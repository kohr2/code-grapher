#!/usr/bin/env python3
"""
COBOL Relationship Extractor
Simple COBOL relationship extraction for the Code Grapher pipeline
"""

import re
from typing import Dict, List, Any

# Simple relationship types for COBOL
class RelationshipType:
    CALLS = "CALLS"
    IMPORTS = "IMPORTS"
    USES = "USES"
    CONTAINS = "CONTAINS"

# Simple relationship class
class RelationshipExtraction:
    def __init__(self, source_file, target_file, source_entity, target_entity, 
                 relationship_type, confidence=0.8, relationship_strength="medium", 
                 line_number=1, context=""):
        self.source_file = source_file
        self.target_file = target_file
        self.source_entity = source_entity
        self.target_entity = target_entity
        self.relationship_type = relationship_type
        self.confidence = confidence
        self.relationship_strength = relationship_strength
        self.line_number = line_number
        self.context = context


def extract_cobol_relationships(file_data: Dict[str, Any]) -> List[RelationshipExtraction]:
    """
    Extract COBOL relationships from parsed COBOL file data
    
    Args:
        file_data: Parsed COBOL file data
        
    Returns:
        List of RelationshipExtraction objects
    """
    if file_data.get("language") != "cobol":
        return []
        
    relationships = []
    file_path = file_data.get("file_path", "")
    
    try:
        # Extract basic containment relationships
        relationships.extend(_extract_basic_relationships(file_data, file_path))
        
        # Extract relationships from compilation units
        if "compilation_units" in file_data:
            for cu in file_data["compilation_units"]:
                cu_name = cu.get("name", "UNKNOWN")
                
                # Extract CALL relationships
                relationships.extend(_extract_cobol_calls(file_data, cu_name, file_path))
                
                # Extract PERFORM relationships  
                relationships.extend(_extract_cobol_performs(file_data, cu_name, file_path))
                
                # Extract COPY relationships
                relationships.extend(_extract_cobol_copies(file_data, cu_name, file_path))
                
        print(f"   🟦 Extracted {len(relationships)} COBOL relationships from {file_path}")
        
    except Exception as e:
        print(f"   ⚠️  Error extracting COBOL relationships from {file_path}: {e}")
    
    return relationships


def _extract_basic_relationships(file_data: Dict[str, Any], file_path: str) -> List[RelationshipExtraction]:
    """Extract basic containment relationships"""
    relationships = []
    
    try:
        # File contains compilation units
        if "compilation_units" in file_data:
            for cu in file_data["compilation_units"]:
                cu_name = cu.get("name", "UNKNOWN")
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=file_path,
                    target_entity=cu_name,
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"File contains compilation unit {cu_name}"
                ))
        
        # Program contains compilation units
        if "entities" in file_data:
            program_entities = [e for e in file_data["entities"] if e.get("type") == "program"]
            compilation_units = [e for e in file_data["entities"] if e.get("type") == "compilation_unit"]
            
            for program in program_entities:
                for cu in compilation_units:
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=program.get("name", "UNKNOWN"),
                        target_entity=cu.get("name", "UNKNOWN"),
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        relationship_strength="strong",
                        line_number=1,
                        context=f"Program contains compilation unit"
                    ))
        
        # Compilation units contain paragraphs
        if "statements" in file_data:
            for cu_name, paragraphs in file_data["statements"].items():
                for para_name in paragraphs.keys():
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=cu_name,
                        target_entity=para_name,
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        relationship_strength="strong",
                        line_number=1,
                        context=f"Compilation unit {cu_name} contains paragraph {para_name}"
                    ))
                    
    except Exception as e:
        print(f"   ⚠️  Error extracting basic relationships: {e}")
    
    return relationships


def _extract_cobol_calls(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract CALL statement relationships"""
    relationships = []
    
    try:
        # Look for CALL statements in the statements data
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt in stmt_list:
                    # Handle both "CALL program" and "CALLprogram" (no space)
                    call_match = re.search(r'CALL\s*["\']?([A-Z0-9-]+)["\']?', stmt, re.IGNORECASE)
                    if call_match:
                        target_program = call_match.group(1)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=f"{target_program}.cbl",
                            source_entity=para_name,
                            target_entity=target_program,
                            relationship_type=RelationshipType.CALLS,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"CALL statement to {target_program}"
                        ))
    except Exception as e:
        print(f"   ⚠️  Error extracting CALL relationships: {e}")
    
    return relationships


def _extract_cobol_performs(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract PERFORM statement relationships"""
    relationships = []
    
    try:
        # Look for PERFORM statements in the statements data
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt in stmt_list:
                    # Handle both "PERFORM paragraph" and "PERFORMparagraph" (no space)
                    perform_match = re.search(r'PERFORM\s*([A-Z0-9-]+)', stmt, re.IGNORECASE)
                    if perform_match:
                        target_paragraph = perform_match.group(1)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=para_name,
                            target_entity=target_paragraph,
                            relationship_type=RelationshipType.CALLS,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"PERFORM statement to {target_paragraph}"
                        ))
    except Exception as e:
        print(f"   ⚠️  Error extracting PERFORM relationships: {e}")
    
    return relationships


def _extract_cobol_copies(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract COPY statement relationships"""
    relationships = []
    
    try:
        # Look for COPY statements in the statements data
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt in stmt_list:
                    copy_match = re.search(r'COPY\s+([^\s]+)', stmt, re.IGNORECASE)
                    if copy_match:
                        target_copybook = copy_match.group(1)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=f"{target_copybook}.cpy",
                            source_entity=cu_name,
                            target_entity=target_copybook,
                            relationship_type=RelationshipType.IMPORTS,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"COPY statement for {target_copybook}"
                        ))
    except Exception as e:
        print(f"   ⚠️  Error extracting COPY relationships: {e}")
    
    return relationships


if __name__ == "__main__":
    print("🧪 COBOL Relationship Extractor")
    print("   Use: extract_cobol_relationships(file_data)")
