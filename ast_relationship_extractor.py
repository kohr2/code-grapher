#!/usr/bin/env python3
"""
AST-Based Relationship Extraction System
Extracts deterministic code relationships from existing AST data without AI
Fast, accurate extraction for structural relationships like INHERITS, DECORATES, CALLS
"""

import ast
import json
from typing import Dict, List, Any, Optional, Set
from pathlib import Path

from ai_relationship_extractor import RelationshipExtraction, RelationshipType
from logger import logger


class ASTRelationshipExtractor:
    """Extract code relationships using AST analysis - fast and deterministic"""
    
    def __init__(self):
        self.logger = logger
        
    def extract_relationships(self, parsed_files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """
        Extract all AST-based relationships from parsed files
        
        Args:
            parsed_files: List of file data with entities from core_pipeline.py
            
        Returns:
            List of RelationshipExtraction objects compatible with existing pipeline
        """
        print("🔍 Starting AST relationship extraction...")
        
        relationships = []
        successful_files = [f for f in parsed_files if f.get("success", False)]
        
        print(f"   📁 Processing {len(successful_files)} files for AST relationships")
        
        # Extract different relationship types
        relationships.extend(self._extract_inherits_relationships(successful_files))
        relationships.extend(self._extract_decorates_relationships(successful_files))
        relationships.extend(self._extract_basic_calls_relationships(successful_files))
        
        # Extract COBOL relationships
        print(f"   🔍 About to extract COBOL relationships from {len(successful_files)} files...")
        cobol_relationships = self._extract_cobol_relationships(successful_files)
        print(f"   📊 COBOL relationships returned: {len(cobol_relationships)}")
        relationships.extend(cobol_relationships)
        
        print(f"   ✅ Extracted {len(relationships)} AST relationships")
        return relationships
    
    def _extract_inherits_relationships(self, files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """Extract INHERITS relationships from class bases"""
        relationships = []
        
        for file_data in files:
            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])
            
            # Find all classes with bases (inheritance)
            for entity in entities:
                if entity.get("type") == "class" and entity.get("bases"):
                    child_class = entity["name"]
                    bases = entity["bases"]
                    
                    for base_class in bases:
                        # Clean up base class name (remove generic type info, etc.)
                        clean_base = self._clean_class_name(base_class)
                        
                        # Create INHERITS relationship
                        relationship = RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,  # May be in different file - we'll handle cross-file later
                            source_entity=child_class,
                            target_entity=clean_base,
                            relationship_type=RelationshipType.INHERITS,
                            confidence=1.0,  # AST parsing is deterministic
                            relationship_strength="strong",
                            line_number=entity.get("line"),
                            context=f"class {child_class}({base_class})"
                        )
                        relationships.append(relationship)
                        
                        print(f"   📈 INHERITS: {child_class} -> {clean_base}")
        
        print(f"   🎯 Found {len(relationships)} INHERITS relationships")
        return relationships
    
    def _extract_decorates_relationships(self, files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """Extract DECORATES relationships from function and class decorators"""
        relationships = []
        
        for file_data in files:
            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])
            
            # Check function decorators
            for entity in entities:
                if entity.get("type") == "function" and entity.get("decorators"):
                    function_name = entity["name"]
                    decorators = entity["decorators"]
                    
                    for decorator in decorators:
                        # Clean up decorator name (remove arguments, etc.)
                        clean_decorator = self._clean_decorator_name(decorator)
                        
                        # Create DECORATES relationship
                        relationship = RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=clean_decorator,
                            target_entity=function_name,
                            relationship_type=RelationshipType.DECORATES,
                            confidence=1.0,
                            relationship_strength="strong",
                            line_number=entity.get("line"),
                            context=f"@{decorator}\\ndef {function_name}()"
                        )
                        relationships.append(relationship)
                        
                        print(f"   🎯 DECORATES: {clean_decorator} -> {function_name}")
                
                # Check class decorators  
                elif entity.get("type") == "class" and entity.get("decorators"):
                    class_name = entity["name"]
                    decorators = entity["decorators"]
                    
                    for decorator in decorators:
                        clean_decorator = self._clean_decorator_name(decorator)
                        
                        relationship = RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=clean_decorator,
                            target_entity=class_name,
                            relationship_type=RelationshipType.DECORATES,
                            confidence=1.0,
                            relationship_strength="strong",
                            line_number=entity.get("line"),
                            context=f"@{decorator}\\nclass {class_name}"
                        )
                        relationships.append(relationship)
                        
                        print(f"   🎯 DECORATES: {clean_decorator} -> {class_name}")
        
        print(f"   🎯 Found {len(relationships)} DECORATES relationships")
        return relationships
    
    def _extract_basic_calls_relationships(self, files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """Extract basic CALLS relationships from function bodies"""
        relationships = []
        
        # This is more complex - we need to parse the actual code to find function calls
        # For now, we'll implement a basic version and enhance later
        
        for file_data in files:
            file_path = file_data["file_path"]
            
            # Read the source code and parse with AST to find calls
            try:
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                
                tree = ast.parse(content)
                
                # Find all function calls within function definitions
                function_calls = self._find_function_calls_in_ast(tree, file_path)
                relationships.extend(function_calls)
                
            except Exception as e:
                print(f"   ⚠️  Could not analyze calls in {file_path}: {e}")
        
        print(f"   🎯 Found {len(relationships)} CALLS relationships")
        return relationships
    
    def _find_function_calls_in_ast(self, tree: ast.AST, file_path: str) -> List[RelationshipExtraction]:
        """Find function calls within function definitions"""
        relationships = []
        
        class CallVisitor(ast.NodeVisitor):
            def __init__(self):
                self.current_function = None
                self.function_calls = []
                
            def visit_FunctionDef(self, node):
                old_function = self.current_function
                self.current_function = node.name
                
                # Visit the function body to find calls
                self.generic_visit(node)
                
                self.current_function = old_function
                
            def visit_Call(self, node):
                if self.current_function:
                    # Try to get the function name being called
                    called_function = self._get_call_name(node)
                    if called_function:
                        self.function_calls.append({
                            "caller": self.current_function,
                            "called": called_function,
                            "line": node.lineno
                        })
                
                self.generic_visit(node)
                
            def _get_call_name(self, call_node):
                """Extract function name from call node"""
                if isinstance(call_node.func, ast.Name):
                    return call_node.func.id
                elif isinstance(call_node.func, ast.Attribute):
                    # For method calls like obj.method(), get just the method name
                    return call_node.func.attr
                return None
        
        visitor = CallVisitor()
        visitor.visit(tree)
        
        # Convert to RelationshipExtraction objects
        for call in visitor.function_calls:
            relationship = RelationshipExtraction(
                source_file=file_path,
                target_file=file_path,  # Assume same file for now
                source_entity=call["caller"],
                target_entity=call["called"],
                relationship_type=RelationshipType.CALLS,
                confidence=0.8,  # Slightly lower confidence for basic call detection
                relationship_strength="medium",
                line_number=call["line"],
                context=f"function {call['caller']} calls {call['called']}"
            )
            relationships.append(relationship)
        
        return relationships
    
    def _clean_class_name(self, base_class: str) -> str:
        """Clean up base class name to get just the class name"""
        # Remove generic type parameters: List[str] -> List
        if '[' in base_class:
            base_class = base_class.split('[')[0]
        
        # Remove module prefixes: typing.List -> List
        if '.' in base_class:
            base_class = base_class.split('.')[-1]
        
        return base_class.strip()
    
    def _clean_decorator_name(self, decorator: str) -> str:
        """Clean up decorator name to get just the decorator name"""
        # Remove arguments: @property() -> property
        if '(' in decorator:
            decorator = decorator.split('(')[0]
        
        # Remove @ if present
        if decorator.startswith('@'):
            decorator = decorator[1:]
        
        # Remove module prefixes: functools.lru_cache -> lru_cache
        if '.' in decorator:
            decorator = decorator.split('.')[-1]
        
        return decorator.strip()
    
    def _extract_cobol_relationships(self, files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """Extract COBOL relationships from parsed COBOL files"""
        relationships = []
        
        print(f"   🔍 Checking {len(files)} files for COBOL relationships...")
        
        for file_data in files:
            language = file_data.get("language", "unknown")
            has_relationships = "relationships" in file_data
            print(f"   📄 File: {file_data.get('file_path', 'unknown')} - Language: {language}, Has relationships: {has_relationships}")
            
            if file_data.get("language") == "cobol" and file_data.get("relationships"):
                # Convert COBOL RelationshipExtraction objects to the format expected by the pipeline
                cobol_relationships = file_data.get("relationships", [])
                print(f"   ✅ Found {len(cobol_relationships)} COBOL relationships in {file_data.get('file_path', 'unknown')}")
                
                for rel in cobol_relationships:
                    # Convert COBOL relationship to pipeline format
                    pipeline_rel = RelationshipExtraction(
                        source_file=file_data.get("file_path", ""),
                        target_file=file_data.get("file_path", ""),  # COBOL relationships are within the same file
                        source_entity=rel.source_entity,
                        target_entity=rel.target_entity,
                        relationship_type=rel.relationship_type,
                        confidence=rel.confidence,
                        context=rel.context,
                        line_number=0,  # COBOL doesn't have line numbers in our current extraction
                        confidence_level=rel.confidence  # Convert float to enum if needed
                    )
                    relationships.append(pipeline_rel)
                    print(f"   🔗 Added {rel.relationship_type.value}: {rel.source_entity} -> {rel.target_entity}")
        
        if relationships:
            print(f"   📊 Extracted {len(relationships)} COBOL relationships")
        else:
            print("   ⚠️  No COBOL relationships found")
        
        return relationships


def extract_ast_relationships(parsed_files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
    """
    Main entry point for AST relationship extraction
    Compatible with existing pipeline interface
    """
    extractor = ASTRelationshipExtractor()
    return extractor.extract_relationships(parsed_files)


if __name__ == "__main__":
    # Test the AST relationship extractor
    print("🧪 Testing AST Relationship Extractor...")
    
    # This would normally be called from core_pipeline.py
    # For testing, we'd need to load some parsed files
    print("   Use this module by calling extract_ast_relationships(parsed_files)")