#!/usr/bin/env python3
"""
COBOL Relationship Extractor
Simple COBOL relationship extraction for the Code Grapher pipeline
"""

import re
from typing import Dict, List, Any
from ai_relationship_extractor import RelationshipExtraction, RelationshipType
from logger import logger


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
        # Extract relationships from compilation units
        if "compilation_units" in file_data:
            for cu in file_data["compilation_units"]:
                # Extract CALL relationships
                relationships.extend(_extract_cobol_calls(cu, file_path))
                
                # Extract PERFORM relationships  
                relationships.extend(_extract_cobol_performs(cu, file_path))
                
                # Extract COPY relationships
                relationships.extend(_extract_cobol_copies(cu, file_path))
                
        print(f"   🟦 Extracted {len(relationships)} COBOL relationships from {file_path}")
        
    except Exception as e:
        print(f"   ⚠️  Error extracting COBOL relationships from {file_path}: {e}")
    
    return relationships


def _extract_cobol_calls(cu: Dict[str, Any], file_path: str) -> List[RelationshipExtraction]:
    """Extract CALL statement relationships"""
    relationships = []
    
    try:
        if "divisions" in cu and "procedure" in cu["divisions"]:
            proc_div = cu["divisions"]["procedure"]
            
            # Check paragraphs for CALL statements
            if "paragraphs" in proc_div:
                for para in proc_div["paragraphs"]:
                    if "statements" in para:
                        for stmt in para["statements"]:
                            call_match = re.search(r'CALL\s+["\']?([^"\'\s]+)["\']?', stmt, re.IGNORECASE)
                            if call_match:
                                target_program = call_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=f"{target_program}.cbl",
                                    source_entity=para.get("name", "unknown"),
                                    target_entity=target_program,
                                    relationship_type=RelationshipType.CALLS,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=para.get("line_number", 1),
                                    context=f"CALL statement to {target_program}"
                                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting CALL relationships: {e}")
    
    return relationships


def _extract_cobol_performs(cu: Dict[str, Any], file_path: str) -> List[RelationshipExtraction]:
    """Extract PERFORM statement relationships"""
    relationships = []
    
    try:
        if "divisions" in cu and "procedure" in cu["divisions"]:
            proc_div = cu["divisions"]["procedure"]
            
            # Check paragraphs for PERFORM statements
            if "paragraphs" in proc_div:
                for para in proc_div["paragraphs"]:
                    if "statements" in para:
                        for stmt in para["statements"]:
                            perform_match = re.search(r'PERFORM\s+([^\s]+)', stmt, re.IGNORECASE)
                            if perform_match:
                                target_paragraph = perform_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para.get("name", "unknown"),
                                    target_entity=target_paragraph,
                                    relationship_type=RelationshipType.CALLS,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=para.get("line_number", 1),
                                    context=f"PERFORM statement to {target_paragraph}"
                                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting PERFORM relationships: {e}")
    
    return relationships


def _extract_cobol_copies(cu: Dict[str, Any], file_path: str) -> List[RelationshipExtraction]:
    """Extract COPY statement relationships"""
    relationships = []
    
    try:
        if "divisions" in cu:
            divisions = cu["divisions"]
            
            # Check all divisions for COPY statements
            for div_name, div_content in divisions.items():
                if isinstance(div_content, dict):
                    for section_name, section_content in div_content.items():
                        if isinstance(section_content, list):
                            for item in section_content:
                                if isinstance(item, dict) and "statements" in item:
                                    for stmt in item["statements"]:
                                        copy_match = re.search(r'COPY\s+([^\s]+)', stmt, re.IGNORECASE)
                                        if copy_match:
                                            target_copybook = copy_match.group(1)
                                            relationships.append(RelationshipExtraction(
                                                source_file=file_path,
                                                target_file=f"{target_copybook}.cpy",
                                                source_entity=cu.get("program_id", "unknown"),
                                                target_entity=target_copybook,
                                                relationship_type=RelationshipType.IMPORTS,
                                                confidence=0.95,
                                                relationship_strength="strong",
                                                line_number=item.get("line_number", 1),
                                                context=f"COPY statement for {target_copybook}"
                                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting COPY relationships: {e}")
    
    return relationships


if __name__ == "__main__":
    print("🧪 COBOL Relationship Extractor")
    print("   Use: extract_cobol_relationships(file_data)")
