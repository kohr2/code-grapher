"""
Multi-Language Program Analyzer

Provides comprehensive analysis and description generation for programs written in
multiple programming languages including Python, TypeScript, JavaScript, and COBOL.
"""

import os
import re
import ast
from typing import Dict, List, Any, Optional
from pathlib import Path


class ProgramAnalyzer:
    """Analyzes programs across multiple languages to generate comprehensive descriptions"""
    
    def __init__(self):
        self.language_analyzers = {
            "python": self._analyze_python_program,
            "typescript": self._analyze_typescript_program,
            "javascript": self._analyze_javascript_program,
            "cobol": self._analyze_cobol_program,
            "markdown": self._analyze_markdown_program
        }
    
    def analyze_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze a program file and generate comprehensive descriptions"""
        language = file_data.get("language", "unknown").lower()
        
        if language in self.language_analyzers:
            return self.language_analyzers[language](file_path, file_data)
        else:
            return self._analyze_generic_program(file_path, file_data)
    
    def _analyze_python_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze Python program and generate descriptions"""
        entities = file_data.get("entities", [])
        ast_data = file_data.get("ast_data", {})
        
        # Analyze file purpose
        file_purpose = self._analyze_python_file_purpose(file_path, entities, ast_data)
        
        # Analyze entities
        entity_descriptions = {}
        for entity in entities:
            entity_name = entity.get("name", "unknown")
            entity_type = entity.get("type", "unknown")
            entity_descriptions[entity_name] = self._describe_python_entity(entity, ast_data)
        
        return {
            "file_purpose": file_purpose,
            "entity_descriptions": entity_descriptions,
            "program_structure": self._analyze_python_structure(entities, ast_data),
            "key_functionality": self._extract_python_key_functionality(entities, ast_data)
        }
    
    def _analyze_cobol_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze COBOL program and generate descriptions"""
        entities = file_data.get("entities", [])
        compilation_units = file_data.get("compilation_units", [])
        statements = file_data.get("statements", {})
        paragraphs = file_data.get("paragraphs", {})
        
        # Analyze file purpose
        file_purpose = self._analyze_cobol_file_purpose(file_path, entities, compilation_units)
        
        # Analyze entities
        entity_descriptions = {}
        for entity in entities:
            entity_name = entity.get("name", "unknown")
            entity_type = entity.get("type", "unknown")
            entity_descriptions[entity_name] = self._describe_cobol_entity(entity, statements, paragraphs)
        
        return {
            "file_purpose": file_purpose,
            "entity_descriptions": entity_descriptions,
            "program_structure": self._analyze_cobol_structure(entities, compilation_units, statements),
            "key_functionality": self._extract_cobol_key_functionality(statements, paragraphs)
        }
    
    def _analyze_typescript_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze TypeScript program and generate descriptions"""
        entities = file_data.get("entities", [])
        ast_data = file_data.get("ast_data", {})
        
        # Analyze file purpose
        file_purpose = self._analyze_typescript_file_purpose(file_path, entities, ast_data)
        
        # Analyze entities
        entity_descriptions = {}
        for entity in entities:
            entity_name = entity.get("name", "unknown")
            entity_descriptions[entity_name] = self._describe_typescript_entity(entity, ast_data)
        
        return {
            "file_purpose": file_purpose,
            "entity_descriptions": entity_descriptions,
            "program_structure": self._analyze_typescript_structure(entities, ast_data),
            "key_functionality": self._extract_typescript_key_functionality(entities, ast_data)
        }
    
    def _analyze_javascript_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze JavaScript program and generate descriptions"""
        entities = file_data.get("entities", [])
        ast_data = file_data.get("ast_data", {})
        
        # Analyze file purpose
        file_purpose = self._analyze_javascript_file_purpose(file_path, entities, ast_data)
        
        # Analyze entities
        entity_descriptions = {}
        for entity in entities:
            entity_name = entity.get("name", "unknown")
            entity_descriptions[entity_name] = self._describe_javascript_entity(entity, ast_data)
        
        return {
            "file_purpose": file_purpose,
            "entity_descriptions": entity_descriptions,
            "program_structure": self._analyze_javascript_structure(entities, ast_data),
            "key_functionality": self._extract_javascript_key_functionality(entities, ast_data)
        }
    
    def _analyze_markdown_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze Markdown document and generate descriptions"""
        entities = file_data.get("entities", [])
        
        return {
            "file_purpose": f"Documentation file: {Path(file_path).name}",
            "entity_descriptions": {},
            "program_structure": "Markdown document with sections and content",
            "key_functionality": "Provides documentation and information"
        }
    
    def _analyze_generic_program(self, file_path: str, file_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze generic program file"""
        entities = file_data.get("entities", [])
        
        return {
            "file_purpose": f"Program file: {Path(file_path).name}",
            "entity_descriptions": {},
            "program_structure": "Generic program structure",
            "key_functionality": "Contains program logic and functionality"
        }
    
    # Python Analysis Methods
    def _analyze_python_file_purpose(self, file_path: str, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze Python file purpose"""
        filename = Path(file_path).stem.lower()
        
        # Check for common patterns
        if "test" in filename:
            return f"Test file for {filename.replace('test_', '').replace('_test', '')} functionality"
        elif "main" in filename:
            return "Main entry point for the application"
        elif "config" in filename:
            return "Configuration and settings management"
        elif "utils" in filename or "util" in filename:
            return "Utility functions and helper modules"
        elif "service" in filename:
            return "Service layer providing business logic"
        elif "model" in filename:
            return "Data models and entity definitions"
        elif "api" in filename:
            return "API endpoints and request handling"
        
        # Analyze based on entities
        classes = [e for e in entities if e.get("type") == "class"]
        functions = [e for e in entities if e.get("type") == "function"]
        
        if classes and functions:
            return f"Module containing {len(classes)} classes and {len(functions)} functions"
        elif classes:
            return f"Class definitions module with {len(classes)} classes"
        elif functions:
            return f"Function library with {len(functions)} functions"
        else:
            return "Python module with various components"
    
    def _describe_python_entity(self, entity: Dict, ast_data: Dict) -> str:
        """Describe a Python entity"""
        entity_type = entity.get("type", "unknown")
        entity_name = entity.get("name", "unknown")
        
        if entity_type == "class":
            return f"Class '{entity_name}' - defines object structure and behavior"
        elif entity_type == "function":
            return f"Function '{entity_name}' - performs specific operations"
        elif entity_type == "variable":
            return f"Variable '{entity_name}' - stores data values"
        elif entity_type == "import":
            return f"Import '{entity_name}' - brings in external functionality"
        else:
            return f"{entity_type.title()} '{entity_name}' - program component"
    
    def _analyze_python_structure(self, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze Python program structure"""
        classes = [e for e in entities if e.get("type") == "class"]
        functions = [e for e in entities if e.get("type") == "function"]
        
        if classes and functions:
            return f"Object-oriented module with {len(classes)} classes and {len(functions)} functions"
        elif classes:
            return f"Class-based module with {len(classes)} classes"
        elif functions:
            return f"Functional module with {len(functions)} functions"
        else:
            return "Simple Python module"
    
    def _extract_python_key_functionality(self, entities: List[Dict], ast_data: Dict) -> List[str]:
        """Extract key functionality from Python code"""
        functionality = []
        
        for entity in entities:
            if entity.get("type") == "class":
                functionality.append(f"Class {entity.get('name')} provides object-oriented functionality")
            elif entity.get("type") == "function":
                functionality.append(f"Function {entity.get('name')} performs specific operations")
        
        return functionality[:5]  # Limit to top 5
    
    # COBOL Analysis Methods
    def _analyze_cobol_file_purpose(self, file_path: str, entities: List[Dict], compilation_units: List[Dict]) -> str:
        """Analyze COBOL file purpose"""
        filename = Path(file_path).stem.lower()
        
        # Check for common COBOL patterns
        if "account" in filename:
            return "Banking account management system"
        elif "customer" in filename:
            return "Customer information management system"
        elif "transaction" in filename:
            return "Financial transaction processing system"
        elif "report" in filename:
            return "Report generation and processing system"
        elif "inventory" in filename:
            return "Inventory management system"
        elif "payroll" in filename:
            return "Payroll processing system"
        elif "main" in filename:
            return "Main program entry point"
        
        # Analyze based on compilation units
        if compilation_units:
            cu_name = compilation_units[0].get("name", "UNKNOWN")
            return f"COBOL program '{cu_name}' for business data processing"
        
        return "COBOL business application program"
    
    def _describe_cobol_entity(self, entity: Dict, statements: Dict, paragraphs: Dict) -> str:
        """Describe a COBOL entity"""
        entity_type = entity.get("type", "unknown")
        entity_name = entity.get("name", "unknown")
        
        if entity_type == "program":
            return f"COBOL program '{entity_name}' - main business application"
        elif entity_type == "compilation_unit":
            return f"Compilation unit '{entity_name}' - program module"
        elif entity_type == "paragraph":
            # Try to find paragraph in statements to understand its purpose
            para_purpose = self._analyze_cobol_paragraph_purpose(entity_name, statements)
            return f"Paragraph '{entity_name}' - {para_purpose}"
        else:
            return f"{entity_type.title()} '{entity_name}' - COBOL component"
    
    def _analyze_cobol_paragraph_purpose(self, para_name: str, statements: Dict) -> str:
        """Analyze COBOL paragraph purpose based on name and statements"""
        para_name_lower = para_name.lower()
        
        # Analyze based on paragraph name patterns
        if "main" in para_name_lower:
            return "main program control logic"
        elif "init" in para_name_lower:
            return "initialization and setup procedures"
        elif "read" in para_name_lower:
            return "data reading and input processing"
        elif "write" in para_name_lower:
            return "data writing and output processing"
        elif "validate" in para_name_lower:
            return "data validation and verification"
        elif "update" in para_name_lower:
            return "data update and modification"
        elif "calculate" in para_name_lower:
            return "calculation and computation logic"
        elif "generate" in para_name_lower:
            return "report and output generation"
        elif "end" in para_name_lower:
            return "cleanup and termination procedures"
        elif "process" in para_name_lower:
            return "business logic processing"
        else:
            return "business processing logic"
    
    def _analyze_cobol_structure(self, entities: List[Dict], compilation_units: List[Dict], statements: Dict) -> str:
        """Analyze COBOL program structure"""
        programs = [e for e in entities if e.get("type") == "program"]
        paragraphs = sum(len(para_list) for para_list in statements.values())
        
        if programs and paragraphs > 0:
            return f"COBOL program with {len(programs)} programs and {paragraphs} paragraphs"
        elif programs:
            return f"COBOL program with {len(programs)} programs"
        else:
            return "COBOL business application"
    
    def _extract_cobol_key_functionality(self, statements: Dict, paragraphs: Dict) -> List[str]:
        """Extract key functionality from COBOL code"""
        functionality = []
        
        # Analyze paragraph purposes
        for unit_name, para_dict in statements.items():
            for para_name, stmt_list in para_dict.items():
                para_purpose = self._analyze_cobol_paragraph_purpose(para_name, statements)
                functionality.append(f"Paragraph {para_name}: {para_purpose}")
        
        return functionality[:5]  # Limit to top 5
    
    # TypeScript Analysis Methods
    def _analyze_typescript_file_purpose(self, file_path: str, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze TypeScript file purpose"""
        filename = Path(file_path).stem.lower()
        
        if "component" in filename:
            return "React/UI component definition"
        elif "service" in filename:
            return "TypeScript service layer"
        elif "interface" in filename:
            return "TypeScript interface definitions"
        elif "type" in filename:
            return "TypeScript type definitions"
        elif "test" in filename:
            return "TypeScript test file"
        elif "config" in filename:
            return "TypeScript configuration"
        else:
            return "TypeScript module with type-safe functionality"
    
    def _describe_typescript_entity(self, entity: Dict, ast_data: Dict) -> str:
        """Describe a TypeScript entity"""
        entity_type = entity.get("type", "unknown")
        entity_name = entity.get("name", "unknown")
        
        if entity_type == "class":
            return f"TypeScript class '{entity_name}' with type safety"
        elif entity_type == "interface":
            return f"TypeScript interface '{entity_name}' defining contract"
        elif entity_type == "function":
            return f"TypeScript function '{entity_name}' with typed parameters"
        elif entity_type == "variable":
            return f"TypeScript variable '{entity_name}' with type annotation"
        else:
            return f"TypeScript {entity_type} '{entity_name}'"
    
    def _analyze_typescript_structure(self, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze TypeScript program structure"""
        classes = [e for e in entities if e.get("type") == "class"]
        interfaces = [e for e in entities if e.get("type") == "interface"]
        functions = [e for e in entities if e.get("type") == "function"]
        
        if interfaces and classes:
            return f"TypeScript module with {len(interfaces)} interfaces and {len(classes)} classes"
        elif interfaces:
            return f"TypeScript interface definitions with {len(interfaces)} interfaces"
        elif classes:
            return f"TypeScript class-based module with {len(classes)} classes"
        else:
            return "TypeScript module with type-safe functionality"
    
    def _extract_typescript_key_functionality(self, entities: List[Dict], ast_data: Dict) -> List[str]:
        """Extract key functionality from TypeScript code"""
        functionality = []
        
        for entity in entities:
            if entity.get("type") == "interface":
                functionality.append(f"Interface {entity.get('name')} defines type contract")
            elif entity.get("type") == "class":
                functionality.append(f"Class {entity.get('name')} provides type-safe functionality")
            elif entity.get("type") == "function":
                functionality.append(f"Function {entity.get('name')} with TypeScript typing")
        
        return functionality[:5]
    
    # JavaScript Analysis Methods
    def _analyze_javascript_file_purpose(self, file_path: str, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze JavaScript file purpose"""
        filename = Path(file_path).stem.lower()
        
        if "component" in filename:
            return "JavaScript UI component"
        elif "service" in filename:
            return "JavaScript service layer"
        elif "util" in filename:
            return "JavaScript utility functions"
        elif "test" in filename:
            return "JavaScript test file"
        elif "config" in filename:
            return "JavaScript configuration"
        else:
            return "JavaScript module with dynamic functionality"
    
    def _describe_javascript_entity(self, entity: Dict, ast_data: Dict) -> str:
        """Describe a JavaScript entity"""
        entity_type = entity.get("type", "unknown")
        entity_name = entity.get("name", "unknown")
        
        if entity_type == "class":
            return f"JavaScript class '{entity_name}' with dynamic behavior"
        elif entity_type == "function":
            return f"JavaScript function '{entity_name}' for dynamic operations"
        elif entity_type == "variable":
            return f"JavaScript variable '{entity_name}' with dynamic typing"
        else:
            return f"JavaScript {entity_type} '{entity_name}'"
    
    def _analyze_javascript_structure(self, entities: List[Dict], ast_data: Dict) -> str:
        """Analyze JavaScript program structure"""
        classes = [e for e in entities if e.get("type") == "class"]
        functions = [e for e in entities if e.get("type") == "function"]
        
        if classes and functions:
            return f"JavaScript module with {len(classes)} classes and {len(functions)} functions"
        elif classes:
            return f"JavaScript class-based module with {len(classes)} classes"
        elif functions:
            return f"JavaScript functional module with {len(functions)} functions"
        else:
            return "JavaScript module with dynamic functionality"
    
    def _extract_javascript_key_functionality(self, entities: List[Dict], ast_data: Dict) -> List[str]:
        """Extract key functionality from JavaScript code"""
        functionality = []
        
        for entity in entities:
            if entity.get("type") == "class":
                functionality.append(f"Class {entity.get('name')} provides dynamic functionality")
            elif entity.get("type") == "function":
                functionality.append(f"Function {entity.get('name')} for dynamic operations")
        
        return functionality[:5]
    
    def generate_relationship_context(self, relationship: Any, source_analysis: Dict, target_analysis: Dict) -> str:
        """Generate contextual description for relationships"""
        rel_type = getattr(relationship, 'relationship_type', 'UNKNOWN')
        source_entity = getattr(relationship, 'source_entity', 'unknown')
        target_entity = getattr(relationship, 'target_entity', 'unknown')
        
        # Get entity descriptions
        source_desc = source_analysis.get("entity_descriptions", {}).get(source_entity, f"Entity {source_entity}")
        target_desc = target_analysis.get("entity_descriptions", {}).get(target_entity, f"Entity {target_entity}")
        
        if rel_type == "CALLS":
            return f"{source_desc} calls {target_desc} to perform operations"
        elif rel_type == "CONTAINS":
            return f"{source_desc} contains {target_desc} as a component"
        elif rel_type == "IMPORTS":
            return f"{source_desc} imports {target_desc} for functionality"
        elif rel_type == "USES":
            return f"{source_desc} uses {target_desc} for data or operations"
        else:
            return f"{source_desc} has {rel_type.lower()} relationship with {target_desc}"
