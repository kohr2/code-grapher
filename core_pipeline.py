#!/usr/bin/env python3
"""
Core Pipeline for Codebase Analysis and Graph Creation
Simplified to handle the 3 core functions: Review, Update, Query
"""

import sys
import os
import json
import time
import ast
from pathlib import Path
from typing import List, Dict, Any, Set, Tuple, Optional
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

# CodeGraphManager will be accessed through ServiceLocator
from shared.services.service_locator import ServiceLocator
from ai_relationship_extractor import RelationshipExtraction, AIRelationshipExtractor

# Get logger through service locator
logger = ServiceLocator.get_logger("core_pipeline")
from ast_relationship_extractor import extract_ast_relationships
from entity_classifier import classify_entities

# AI Services
import sys
sys.path.append(str(Path(__file__).parent / "ai-services"))
from interfaces.ai_services_interface import AIServicesInterface
from services.ai_service import AIService  
from config.ai_config import AIServiceConfig


def get_ai_service() -> AIServicesInterface:
    """Get or create AI service instance"""
    try:
        # Try to get from service registry first
        registry = ServiceLocator.get_registry()
        if registry.is_registered(AIServicesInterface):
            return registry.get(AIServicesInterface)
    except Exception:
        pass
    
    # Create new AI service
    try:
        # Get AI configuration from environment
        ai_config = AIServiceConfig()
        
        # Override with environment variables
        import os
        if os.getenv("AI_PROVIDER"):
            from models.provider_models import AIProviderType
            try:
                ai_config.default_provider = AIProviderType(os.getenv("AI_PROVIDER").lower())
            except ValueError:
                pass
        
        if os.getenv("OLLAMA_URL"):
            ai_config.ollama_url = os.getenv("OLLAMA_URL")
        
        if os.getenv("GEMINI_API_KEY"):
            ai_config.gemini_api_key = os.getenv("GEMINI_API_KEY")
        
        # Create and initialize AI service
        ai_service = AIService(ai_config, logger)
        ai_service.initialize({})
        
        return ai_service
        
    except Exception as e:
        logger.log_error(f"Failed to create AI service: {e}")
        # Return a mock service or raise error
        raise RuntimeError(f"Could not initialize AI service: {e}")


def load_primer_context(project_root: str = ".") -> str:
    """
    Load business context from PRIMER.md file in the target project directory
    
    Args:
        project_root: Root directory of the project being analyzed (not MCP server directory)
        
    Returns:
        Primer content as string, empty if not found
    """
    primer_paths = [
        os.path.join(project_root, "PRIMER.md"),
        os.path.join(project_root, "primer.md"),
        os.path.join(project_root, "BUSINESS_CONTEXT.md"),
        os.path.join(project_root, "business_context.md")
    ]
    
    # Check environment variable for custom primer path
    custom_primer = os.getenv("PRIMER_FILE_PATH")
    if custom_primer:
        primer_paths.insert(0, custom_primer)
    
    for primer_path in primer_paths:
        try:
            if os.path.exists(primer_path):
                with open(primer_path, 'r', encoding='utf-8') as f:
                    content = f.read().strip()
                    if content:
                        print(f"   📋 Loaded primer context from: {primer_path}")
                        print(f"       (Project root: {project_root})")
                        return content
        except Exception as e:
            print(f"   ⚠️  Could not read primer file {primer_path}: {e}")
    
    print(f"   ➖ No primer context file found in: {project_root}")
    return ""


def generate_code_descriptions(parsed_files: List[Dict[str, Any]], project_root: str = ".") -> Dict[str, Dict[str, str]]:
    """
    Generate AI-powered descriptions for code entities
    
    Args:
        parsed_files: List of parsed file data with entities
        project_root: Root directory for finding primer context
        
    Returns:
        Dict mapping file_path -> entity_name -> description
    """
    print("🤖 Generating AI code descriptions...")
    
    try:
        # Get AI service
        ai_service = get_ai_service()
        
        # Load business context primer  
        primer_context = load_primer_context(project_root)
        
        descriptions = {}
        successful_files = [f for f in parsed_files if f.get("success", False)]
        
        for file_data in successful_files:
            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])
            descriptions[file_path] = {}
            
            print(f"   📄 Describing entities in {file_path}")
            
            # Generate descriptions for functions, classes, and COBOL entities
            for entity in entities:
                if entity.get("type") in ["function", "class", "program", "compilation_unit", "file", "paragraph", "data_item"]:
                    entity_name = entity["name"]
                    
                    # Prepare entity data for AI service
                    entity_data = {
                        "name": entity_name,
                        "type": entity.get("type", "unknown"),
                        "file_path": file_path,
                        "line": entity.get("line", 1),
                        "properties": entity.get("properties", {})
                    }
                    
                    try:
                        # Get the source code snippet for this entity
                        code_snippet = _get_entity_code_snippet(file_path, entity)
                        
                        # Generate description using AI service with source code
                        description = _generate_entity_description(
                            ai_service, entity_name, entity['type'], code_snippet, primer_context
                        )
                        descriptions[file_path][entity_name] = description
                        
                        print(f"     🎯 {entity['type']}: {entity_name}")
                        
                    except Exception as e:
                        print(f"   ⚠️  Could not describe {entity_name}: {e}")
                        descriptions[file_path][entity_name] = f"A {entity['type']} named {entity_name}"
        
        print(f"   ✅ Generated descriptions for entities across {len(successful_files)} files")
        return descriptions
        
    except Exception as e:
        logger.log_error(f"Failed to generate descriptions with AI service: {e}")
        print(f"   ❌ Failed to generate descriptions: {e}")
        
        # Fallback to empty descriptions
        descriptions = {}
        for file_data in parsed_files:
            if file_data.get("success", False):
                descriptions[file_data["file_path"]] = {}
        
        return descriptions


def _get_entity_code_snippet(file_path: str, entity: Dict[str, Any]) -> str:
    """Extract code snippet for an entity"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            lines = f.read().split('\n')
        
        entity_type = entity.get("type", "unknown")
        start_line = entity.get("line", 1) - 1  # Convert to 0-based indexing
        
        # For COBOL programs, show more context to understand the program structure
        if entity_type in ["program", "compilation_unit"]:
            # Show the first 20 lines to get program structure
            end_line = min(20, len(lines))
            snippet = '\n'.join(lines[0:end_line])
        else:
            # For other entities, show around the entity location
            end_line = min(start_line + 10, len(lines))
            snippet = '\n'.join(lines[start_line:end_line])
        
        return snippet
    except Exception:
        return f"{entity.get('type', 'entity')} {entity.get('name', 'unknown')}"


def _generate_entity_description(ai_service, entity_name: str, entity_type: str, code_snippet: str, primer_context: str = "") -> str:
    """Generate a concise description of a code entity using AI with business context"""
    
    # Build prompt with optional business context
    prompt_parts = []
    
    if primer_context:
        prompt_parts.append(f"""BUSINESS CONTEXT:
{primer_context}

---""")
    
    # Determine the code language for syntax highlighting
    code_lang = "cobol" if entity_type in ["program", "compilation_unit"] else "python"
    
    prompt_parts.append(f"""Analyze this {entity_type} and provide a concise 1-2 sentence description of what it does:

{entity_type.title()}: {entity_name}
Code:
```{code_lang}
{code_snippet}
```

Provide a clear, concise description focusing on the purpose and functionality.""")
    
    if primer_context:
        prompt_parts.append("Consider the business context above when describing this code's role and purpose.")
    
    prompt_parts.append("Keep it under 100 words.")
    
    prompt = "\n\n".join(prompt_parts)

    try:
        # Use the AI service to generate description
        entity_data = {
            'name': entity_name,
            'type': entity_type,
            'code_snippet': code_snippet
        }
        description = ai_service.generate_description(entity_data, primer_context)
        if description and description.strip():
            # Remove quotes if the AI wrapped the response in them
            if description.startswith('"') and description.endswith('"'):
                description = description[1:-1]
            return description
        else:
            return f"A {entity_type} named {entity_name}"
    except Exception as e:
        return f"A {entity_type} named {entity_name}"


def clear_neo4j_database():
    """Clear all data from Neo4j database"""
    print("🗑️  Clearing Neo4j database...")
    
    try:
        graph_manager = ServiceLocator.get_graph_manager()
        
        # Clear all nodes and relationships
        clear_query = "MATCH (n) DETACH DELETE n"
        result = graph_manager.graph.run(clear_query)
        
        # Verify it's empty
        count_query = "MATCH (n) RETURN count(n) as count"
        result = graph_manager.graph.run(count_query)
        count = list(result)[0]["count"]
        
        print(f"   ✅ Database cleared. Remaining nodes: {count}")
        
        graph_manager.close()
        return True
        
    except Exception as e:
        print(f"   ❌ Failed to clear database: {e}")
        return False


def parse_and_extract_entities(file_paths: List[str]) -> List[Dict[str, Any]]:
    """Parse files and extract entities using AST parsing"""
    print(f"🔍 Parsing {len(file_paths)} files...")
    
    parsed_files = []
    for file_path in file_paths:
        print(f"   📄 Reading: {file_path}")
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            tree = ast.parse(content)
            entities = []
            
            # Extract functions, classes, and imports with enhanced semantic metadata
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    # Extract parameter types and default values
                    param_info = []
                    for arg in node.args.args:
                        param_data = {"name": arg.arg}
                        if arg.annotation:
                            param_data["type"] = ast.unparse(arg.annotation) if hasattr(ast, 'unparse') else str(arg.annotation)
                        param_info.append(param_data)
                    
                    # Extract return type
                    return_type = None
                    if node.returns:
                        return_type = ast.unparse(node.returns) if hasattr(ast, 'unparse') else str(node.returns)
                    
                    # Extract decorators
                    decorators = []
                    for decorator in node.decorator_list:
                        decorator_name = ast.unparse(decorator) if hasattr(ast, 'unparse') else str(decorator)
                        decorators.append(decorator_name)
                    
                    entities.append({
                        "name": node.name,
                        "type": "function",
                        "line": node.lineno,
                        "args": [arg.arg for arg in node.args.args] if hasattr(node.args, 'args') else [],
                        "parameters": param_info,
                        "return_type": return_type,
                        "decorators": decorators,
                        "is_async": isinstance(node, ast.AsyncFunctionDef),
                        "docstring": ast.get_docstring(node)
                    })
                elif isinstance(node, ast.ClassDef):
                    # Extract class decorators
                    decorators = []
                    for decorator in node.decorator_list:
                        decorator_name = ast.unparse(decorator) if hasattr(ast, 'unparse') else str(decorator)
                        decorators.append(decorator_name)
                    
                    # Extract method information with types
                    methods_info = []
                    class_attributes = []
                    
                    for n in node.body:
                        if isinstance(n, ast.FunctionDef):
                            method_info = {"name": n.name, "line": n.lineno}
                            # Add method decorators
                            method_decorators = []
                            for decorator in n.decorator_list:
                                method_decorators.append(ast.unparse(decorator) if hasattr(ast, 'unparse') else str(decorator))
                            method_info["decorators"] = method_decorators
                            method_info["is_property"] = any("property" in d for d in method_decorators)
                            method_info["is_classmethod"] = any("classmethod" in d for d in method_decorators)
                            method_info["is_staticmethod"] = any("staticmethod" in d for d in method_decorators)
                            methods_info.append(method_info)
                        elif isinstance(n, ast.AnnAssign) and isinstance(n.target, ast.Name):
                            # Class attribute with type annotation
                            attr_type = ast.unparse(n.annotation) if hasattr(ast, 'unparse') else str(n.annotation)
                            class_attributes.append({
                                "name": n.target.id,
                                "type": attr_type,
                                "line": n.lineno
                            })
                    
                    entities.append({
                        "name": node.name,
                        "type": "class",
                        "line": node.lineno,
                        "bases": [ast.unparse(base) if hasattr(ast, 'unparse') else str(base) for base in node.bases],
                        "methods": [n.name for n in node.body if isinstance(n, ast.FunctionDef)],
                        "methods_info": methods_info,
                        "class_attributes": class_attributes,
                        "decorators": decorators,
                        "docstring": ast.get_docstring(node)
                    })
                elif isinstance(node, ast.Import):
                    for alias in node.names:
                        entities.append({
                            "name": alias.asname if alias.asname else alias.name,
                            "type": "import",
                            "line": node.lineno,
                            "module": alias.name
                        })
                elif isinstance(node, ast.ImportFrom):
                    if node.module:
                        for alias in node.names:
                            entities.append({
                                "name": alias.asname if alias.asname else alias.name,
                                "type": "import",
                                "line": node.lineno,
                                "module": f"{node.module}.{alias.name}"
                            })
            
            # Apply intelligent entity classification
            file_context = {
                "file_path": file_path,
                "content": content,
                "line_count": len(content.split('\n'))
            }
            
            classified_entities = classify_entities(entities, file_context)
            
            parsed_files.append({
                "file_path": file_path,
                "entities": classified_entities,
                "content": content,
                "line_count": len(content.split('\n')),
                "success": True
            })
            
        except Exception as e:
            print(f"      ❌ Failed to parse {file_path}: {e}")
            parsed_files.append({
                "file_path": file_path,
                "entities": [],
                "content": "",
                "success": False,
                "error": str(e)
            })
    
    return parsed_files



def extract_enhanced_relationships(parsed_files: List[Dict[str, Any]], 
                                 use_ai: bool = True) -> List[RelationshipExtraction]:
    """Extract relationships using enhanced AI with specialized entity awareness"""
    print(f"🤖 Extracting enhanced relationships (AI enabled: {use_ai})...")
    
    if not use_ai:
        print("   ⚠️  AI disabled, using basic import relationships only")
        return []
    
    try:
        # Get AI service for relationship extraction
        ai_service = get_ai_service()
        
        all_relationships = []
        successful_files = [f for f in parsed_files if f["success"]]
        
        # Extract relationships between all file pairs
        for i, source_file in enumerate(successful_files):
            for j, target_file in enumerate(successful_files):
                if i != j:  # Don't compare file with itself
                    print(f"   🔍 Analyzing: {Path(source_file['file_path']).name} -> {Path(target_file['file_path']).name}")
                    
                    try:
                        # Get entity types for specialized relationship detection
                        source_entities = source_file.get("entities", [])
                        target_entities = target_file.get("entities", [])
                        
                        # Extract relationships using AI service
                        result = ai_service.extract_relationships(
                            source_file['file_path'], 
                            target_file['file_path'],
                            source_file.get('content', ''),
                            target_file.get('content', '')
                        )
                        
                        # Convert to legacy format for compatibility
                        relationships = []
                        if result.success:
                            for rel in result.relationships:
                                # Create RelationshipExtraction object for compatibility
                                rel_extraction = RelationshipExtraction(
                                    source_file=rel.source_file,
                                    target_file=rel.target_file,
                                    source_entity=rel.source_entity,
                                    target_entity=rel.target_entity,
                                    relationship_type=rel.relationship_type,
                                    confidence=rel.confidence,
                                    relationship_strength=rel.confidence_level.value,
                                    line_number=rel.line_number,
                                    context=rel.context
                                )
                                relationships.append(rel_extraction)
                        
                        all_relationships.extend(relationships)
                        
                        if relationships:
                            print(f"      ✅ Found {len(relationships)} enhanced relationships")
                        else:
                            print(f"      ➖ No relationships found")
                    
                    except Exception as e:
                        print(f"      ❌ Error analyzing {Path(source_file['file_path']).name}: {e}")
                        
                        # Log extraction failure
                        logger.logger.error(f"Failed to extract relationships from {Path(source_file['file_path']).name}: {str(e)}")
        
        print(f"   ✅ Total enhanced relationships extracted: {len(all_relationships)}")
        return all_relationships
        
    except Exception as e:
        print(f"   ❌ Enhanced relationship extraction failed: {e}")
        
        # Log overall system failure
        logger.logger.error(f"Enhanced relationship extraction system failed: {str(e)}")
        
        return []


def extract_relationships_for_entities_DEPRECATED():
    """Deprecated function - no longer used"""
    pass


def extract_code_snippet(file_content: str, start_line: int, entity_type: str, max_lines: int = 20) -> str:
    """Extract code snippet for an entity from file content"""
    try:
        lines = file_content.split('\n')
        if start_line <= 0 or start_line > len(lines):
            return ""
        
        # Start from the line where the entity is defined (1-based to 0-based)
        start_idx = start_line - 1
        end_idx = start_idx
        
        # For functions and classes, find the end by looking for the next definition at same indent level
        if entity_type in ["function", "class"]:
            # Find the indentation of the definition
            def_line = lines[start_idx] if start_idx < len(lines) else ""
            base_indent = len(def_line) - len(def_line.lstrip())
            
            # Look for the end of the definition
            for i in range(start_idx + 1, min(len(lines), start_idx + max_lines)):
                line = lines[i]
                if line.strip() == "":  # Skip empty lines
                    continue
                    
                current_indent = len(line) - len(line.lstrip())
                
                # If we find a line at same or less indentation that's not a decorator/comment, we've reached the end
                if current_indent <= base_indent and line.strip() and not line.strip().startswith('@') and not line.strip().startswith('#'):
                    break
                end_idx = i
        else:
            # For other entities, just take a few lines
            end_idx = min(start_idx + 3, len(lines) - 1)
        
        # Extract the snippet
        snippet_lines = lines[start_idx:end_idx + 1]
        return '\n'.join(snippet_lines)
        
    except Exception as e:
        logger.logger.error(f"Failed to extract code snippet: {e}")
        return ""


def create_enhanced_context(file_data: Dict[str, Any], entities: List[Dict[str, Any]]) -> str:
    """Create enhanced context for AI analysis with entity focus"""
    content = file_data["content"]
    file_path = file_data["file_path"]
    
    # Extract import statements for context
    import_lines = []
    for entity in entities:
        if entity.get("type") == "import":
            import_lines.append(f"Line {entity.get('line', 0)}: import {entity.get('module', entity.get('name', ''))}")
    
    # Extract function and class signatures for context
    signatures = []
    for entity in entities:
        if entity.get("type") == "function":
            args = ", ".join(entity.get("args", []))
            signatures.append(f"Line {entity.get('line', 0)}: def {entity.get('name', 'unknown')}({args}):")
            if entity.get("docstring"):
                signatures.append(f"    \"\"\"{entity['docstring'][:200]}...\"\"\"")
        elif entity.get("type") == "class":
            bases = entity.get("bases", [])
            base_str = f"({', '.join(bases)})" if bases else ""
            signatures.append(f"Line {entity.get('line', 0)}: class {entity.get('name', 'unknown')}{base_str}:")
            if entity.get("docstring"):
                signatures.append(f"    \"\"\"{entity['docstring'][:200]}...\"\"\"")
            methods = entity.get("methods", [])
            if methods:
                signatures.append(f"    # Methods: {', '.join(methods[:5])}{'...' if len(methods) > 5 else ''}")
    
    # Build enhanced context with key information first
    enhanced_context = f"""# FILE: {Path(file_path).name}

## IMPORTS:
{chr(10).join(import_lines) if import_lines else "No imports"}

## ENTITY SIGNATURES:
{chr(10).join(signatures) if signatures else "No functions or classes"}

## FULL CODE (for detailed analysis):
{content}"""
    
    return enhanced_context


def create_enhanced_graph_with_entities(parsed_files: List[Dict[str, Any]], 
                                       ai_relationships: List[RelationshipExtraction],
                                       code_descriptions: Dict[str, Dict[str, str]] = None) -> Dict[str, Any]:
    """Create enhanced graph with specialized entities and relationships"""
    print("🏗️  Creating enhanced graph with specialized entities...")
    
    try:
        graph_manager = ServiceLocator.get_graph_manager()
        
        total_entities = 0
        total_files = 0
        entity_type_counts = {}
        relationship_type_counts = {}
        
        # Create file nodes and entities
        for file_data in parsed_files:
            if not (file_data.get("success", False) or file_data.get("parse_success", False)):
                continue
                
            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])
            
            print(f"   📄 Processing: {Path(file_path).name}")
            
            # Create file node
            file_node = graph_manager.create_code_entity(
                "File", 
                file_path,
                {
                    "path": file_path,
                    "name": Path(file_path).name,
                    "line_count": file_data.get("metadata", {}).get("lineCount", 0)
                }
            )
            if file_node:
                total_files += 1
            
            # Create entity nodes with specialized classification
            for entity in entities:
                # Use specialized type if available, otherwise fall back to basic type
                entity_type = entity.get("specialized_type", entity.get("type", "unknown"))
                entity_name = entity.get("name", "unknown")
                
                # Add classification metadata to track specialized types
                classification_info = {
                    "original_type": entity.get("type", "unknown"),
                    "specialized_type": entity.get("specialized_type"),
                    "category": entity.get("category"),
                    "classification_confidence": entity.get("classification_confidence", 0.0),
                    "characteristics": entity.get("characteristics", [])
                }
                
                # Create entity node with enhanced metadata
                entity_properties = {
                    "name": entity_name,
                    "file_path": file_path,
                    "line_number": entity.get("line", 0)
                }
                
                # Add classification information to entity properties
                entity_properties.update(classification_info)
                
                # Add code snippets and signatures based on entity type
                entity_type_str = entity.get("type", "unknown")
                if entity_type_str == "function":
                    # Create enhanced function signature with types
                    params = entity.get("parameters", [])
                    param_strs = []
                    for param in params:
                        param_str = param["name"]
                        if param.get("type"):
                            param_str += f": {param['type']}"
                        param_strs.append(param_str)
                    
                    return_type_str = ""
                    if entity.get("return_type"):
                        return_type_str = f" -> {entity['return_type']}"
                    
                    signature = f"def {entity_name}({', '.join(param_strs)}){return_type_str}:"
                    if entity.get("is_async"):
                        signature = f"async {signature}"
                    
                    entity_properties.update({
                        "signature": signature,
                        "parameters": entity.get("args", []),  # Simple list of parameter names
                        "parameter_count": len(entity.get("args", [])),
                        "return_type": entity.get("return_type", ""),
                        "decorators": entity.get("decorators", []),
                        "is_async": entity.get("is_async", False),
                        "docstring": entity.get("docstring", ""),
                        "code_snippet": extract_code_snippet(file_data["content"], entity.get("line", 0), "function")
                    })
                elif entity_type_str == "class":
                    # Create enhanced class signature
                    bases = entity.get("bases", [])
                    base_str = f"({', '.join(bases)})" if bases else ""
                    signature = f"class {entity_name}{base_str}:"
                    
                    entity_properties.update({
                        "signature": signature,
                        "base_classes": bases,
                        "methods": entity.get("methods", []),  # Simple list of method names
                        "method_count": len(entity.get("methods", [])),
                        "decorators": entity.get("decorators", []),
                        "docstring": entity.get("docstring", ""),
                        "code_snippet": extract_code_snippet(file_data["content"], entity.get("line", 0), "class")
                    })
                elif entity_type_str == "import":
                    # Add import details
                    entity_properties.update({
                        "module": entity.get("module", ""),
                        "signature": f"import {entity.get('module', entity_name)}",
                        "import_type": "from" if "." in entity.get("module", "") else "direct"
                    })
                
                # Add specialized metadata if available
                if entity.get("specialized_type"):
                    entity_properties.update({
                        "specialized_type": entity["specialized_type"],
                        "original_type": entity.get("type")
                    })
                    
                    # Flatten specialized metadata to avoid nested dict issues
                    metadata = entity.get("attributes", {}).get("specialized_metadata", {})
                    for key, value in metadata.items():
                        if isinstance(value, (str, int, float, bool)):
                            entity_properties[f"meta_{key}"] = value
                
                # Add AI-generated description if available
                if code_descriptions and file_path in code_descriptions:
                    entity_description = code_descriptions[file_path].get(entity_name)
                    if entity_description:
                        entity_properties["description"] = entity_description
                        entity_properties["ai_description"] = entity_description
                        # Add code snippet for better search/retrieval
                        entity_properties["code_snippet"] = _get_entity_code_snippet(file_path, entity)
                
                entity_node = graph_manager.create_code_entity(
                    entity_type,
                    entity_name, 
                    entity_properties
                )
                
                if entity_node:
                    total_entities += 1
                    entity_type_counts[entity_type] = entity_type_counts.get(entity_type, 0) + 1
                    
                    # Create CONTAINS relationship
                    graph_manager.create_relationship(
                        file_node, entity_node, "CONTAINS"
                    )
        
        # Create AI-discovered relationships
        print(f"   🔍 DEBUG: Processing {len(ai_relationships)} relationships for graph creation")
        for i, relationship in enumerate(ai_relationships):
            try:
                rel_type = relationship.relationship_type.value if hasattr(relationship.relationship_type, 'value') else str(relationship.relationship_type)
                print(f"   🔍 DEBUG: Processing relationship {i+1}/{len(ai_relationships)}: {relationship.source_entity} -{rel_type}-> {relationship.target_entity}")
                
                # Find source and target nodes using general query (any node type with matching name)
                source_query = "MATCH (n {name: $name}) WHERE n.file_path = $file_path RETURN n LIMIT 1"
                source_result = graph_manager.graph.run(
                    source_query, 
                    name=relationship.source_entity,
                    file_path=relationship.source_file
                ).data()
                
                target_query = "MATCH (n {name: $name}) WHERE n.file_path = $file_path RETURN n LIMIT 1"
                target_result = graph_manager.graph.run(
                    target_query, 
                    name=relationship.target_entity,
                    file_path=relationship.target_file
                ).data()
                
                if source_result and target_result:
                    source_node = source_result[0]["n"]
                    target_node = target_result[0]["n"]
                    
                    relationship_type_counts[rel_type] = relationship_type_counts.get(rel_type, 0) + 1
                    
                    print(f"   ✅ DEBUG: Creating {rel_type} relationship: {relationship.source_entity} -> {relationship.target_entity}")
                    graph_manager.create_relationship(
                        source_node, target_node, rel_type
                    )
                else:
                    print(f"   ❌ DEBUG: Could not find nodes for {relationship.source_entity} -> {relationship.target_entity}")
                    if not source_result:
                        print(f"      Source not found: {relationship.source_entity} in {relationship.source_file}")
                    if not target_result:
                        print(f"      Target not found: {relationship.target_entity} in {relationship.target_file}")
            except Exception as e:
                print(f"         ⚠️  Could not create relationship {relationship.source_entity} -> {relationship.target_entity}: {e}")
        
        graph_manager.close()
        
        return {
            "total_files": total_files,
            "total_entities": total_entities,
            "entity_type_counts": entity_type_counts,
            "relationship_type_counts": relationship_type_counts,
            "total_relationships": sum(relationship_type_counts.values())
        }
        
    except Exception as e:
        print(f"   ❌ Graph creation failed: {e}")
        return {"error": str(e)}


def extract_enhanced_relationships(parsed_files: List[Dict[str, Any]], use_ai: bool = True) -> List[RelationshipExtraction]:
    """
    Extract code relationships using AST analysis
    
    Args:
        parsed_files: List of parsed file data with entities
        use_ai: Ignored - kept for compatibility
        
    Returns:
        List of RelationshipExtraction objects
    """
    print("🔍 DEBUG: extract_enhanced_relationships called with {} files".format(len(parsed_files)))
    print("🔍 Extracting code relationships...")
    
    # AST-based relationship extraction (fast and deterministic)
    print("   📊 AST-based relationship extraction")
    ast_relationships = extract_ast_relationships(parsed_files)
    print(f"   ✅ Extracted {len(ast_relationships)} relationships")
    
    # Debug: Show relationship types
    rel_types = {}
    for rel in ast_relationships:
        rel_type = rel.relationship_type.value if hasattr(rel.relationship_type, 'value') else str(rel.relationship_type)
        rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
    print(f"   🔍 DEBUG: AST relationship types: {rel_types}")
    
    return ast_relationships


def _deduplicate_relationships(relationships: List[RelationshipExtraction]) -> List[RelationshipExtraction]:
    """Remove duplicate relationships, preferring AST over AI for same relationship"""
    seen_relationships = set()
    unique_relationships = []
    
    # Sort to prioritize AST relationships (higher confidence) over AI
    sorted_relationships = sorted(relationships, key=lambda r: r.confidence, reverse=True)
    
    for rel in sorted_relationships:
        # Create a unique key for this relationship
        key = (rel.source_file, rel.target_file, rel.source_entity, rel.target_entity, rel.relationship_type.value)
        
        if key not in seen_relationships:
            seen_relationships.add(key)
            unique_relationships.append(rel)
    
    return unique_relationships


def run_enhanced_pipeline(directory: str = ".", use_ai: bool = True):
    """Run the enhanced pipeline with specialized entity detection"""
    print("🚀 Starting Enhanced AI Pipeline with Specialized Entities")
    print("=" * 60)
    
    start_time = time.time()
    
    # Clear database
    if not clear_neo4j_database():
        print("❌ Failed to clear database, exiting")
        return
    
    # Find Python files
    print("🔍 Finding Python files...")
    python_files = []
    for root, dirs, files in os.walk(directory):
        # Skip hidden directories and common non-source directories
        dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['__pycache__', 'node_modules', '.git']]
        
        for file in files:
            if file.endswith('.py') and not file.startswith('.'):
                python_files.append(os.path.join(root, file))
    
    print(f"   📋 Found {len(python_files)} Python files")
    
    if not python_files:
        print("   ⚠️  No Python files found")
        return
    
    # Parse files with enhanced entity extraction
    parsed_files = parse_and_extract_entities(python_files)
    successful_files = [f for f in parsed_files if f["success"]]
    
    print(f"   ✅ Successfully parsed {len(successful_files)}/{len(python_files)} files")
    
    # Generate AI descriptions for code entities
    code_descriptions = generate_code_descriptions(parsed_files, directory) if use_ai else {}
    
    # Extract enhanced relationships
    ai_relationships = extract_enhanced_relationships(parsed_files, use_ai)
    print(f"   🔍 DEBUG: Received {len(ai_relationships)} relationships from extract_enhanced_relationships")
    
    # Debug: Show relationship types
    rel_types = {}
    for rel in ai_relationships:
        rel_type = rel.relationship_type.value if hasattr(rel.relationship_type, 'value') else str(rel.relationship_type)
        rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
    print(f"   🔍 DEBUG: Relationship types: {rel_types}")
    
    # Create enhanced graph
    graph_stats = create_enhanced_graph_with_entities(parsed_files, ai_relationships, code_descriptions)
    
    # Print results
    end_time = time.time()
    duration = end_time - start_time
    
    print("\n" + "=" * 60)
    print("🎉 Enhanced Pipeline Complete!")
    print(f"⏱️  Duration: {duration:.2f} seconds")
    print(f"📁 Files processed: {graph_stats.get('total_files', 0)}")
    print(f"🏷️  Entities extracted: {graph_stats.get('total_entities', 0)}")
    print(f"🔗 Relationships created: {graph_stats.get('total_relationships', 0)}")
    
    if graph_stats.get('entity_type_counts'):
        print("\n📊 Entity Type Distribution:")
        for entity_type, count in sorted(graph_stats['entity_type_counts'].items()):
            print(f"   {entity_type}: {count}")
    
    if graph_stats.get('relationship_type_counts'):
        print("\n🔗 Relationship Type Distribution:")
        for rel_type, count in sorted(graph_stats['relationship_type_counts'].items()):
            print(f"   {rel_type}: {count}")
    
    # Save results
    results = {
        "timestamp": datetime.now().isoformat(),
        "duration_seconds": duration,
        "files_processed": len(successful_files),
        "total_files": len(python_files),
        "graph_stats": graph_stats,
        "ai_enabled": use_ai
    }
    
    with open("enhanced_pipeline_results.json", "w") as f:
        json.dump(results, f, indent=2)
    
    print(f"\n💾 Results saved to: enhanced_pipeline_results.json")
    print("=" * 60)


if __name__ == "__main__":
    import argparse
    from datetime import datetime
    
    parser = argparse.ArgumentParser(description="Core Pipeline for Codebase Analysis and Graph Creation")
    parser.add_argument("--directory", "-d", default=".", help="Directory to analyze")
    parser.add_argument("--no-ai", action="store_true", help="Disable AI relationship extraction")
    
    args = parser.parse_args()
    
    run_enhanced_pipeline(args.directory, use_ai=not args.no_ai)