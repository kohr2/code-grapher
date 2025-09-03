#!/usr/bin/env python3
"""
COBOL Relationship Extractor
Simple COBOL relationship extraction for the Code Grapher pipeline
"""

import re
from typing import Dict, List, Any

# Enhanced relationship types for COBOL
class RelationshipType:
    CALLS = "CALLS"
    IMPORTS = "IMPORTS"
    USES = "USES"
    CONTAINS = "CONTAINS"
    DATA_FLOW = "DATA_FLOW"
    MODIFIES = "MODIFIES"
    READS = "READS"
    WRITES = "WRITES"
    CONDITIONAL = "CONDITIONAL"
    ARITHMETIC = "ARITHMETIC"
    FILE_ACCESS = "FILE_ACCESS"

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
                
                # Extract enhanced relationships from new AST structures
                relationships.extend(_extract_data_flow_relationships(file_data, cu_name, file_path))
                relationships.extend(_extract_arithmetic_relationships(file_data, cu_name, file_path))
                relationships.extend(_extract_conditional_relationships(file_data, cu_name, file_path))
                relationships.extend(_extract_data_item_relationships(file_data, cu_name, file_path))
                
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
                    # Handle both old format (string) and new format (dict)
                    if isinstance(stmt, dict):
                        stmt_text = stmt.get('text', '')
                    else:
                        stmt_text = stmt
                    
                    # Handle both "CALL program" and "CALLprogram" (no space)
                    call_match = re.search(r'CALL\s*["\']?([A-Z0-9-]+)["\']?', stmt_text, re.IGNORECASE)
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
                    # Handle both old format (string) and new format (dict)
                    if isinstance(stmt, dict):
                        stmt_text = stmt.get('text', '')
                    else:
                        stmt_text = stmt
                    
                    # Handle both "PERFORM paragraph" and "PERFORMparagraph" (no space)
                    perform_match = re.search(r'PERFORM\s*([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
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
                    # Handle both old format (string) and new format (dict)
                    if isinstance(stmt, dict):
                        stmt_text = stmt.get('text', '')
                    else:
                        stmt_text = stmt
                    
                    copy_match = re.search(r'COPY\s+([^\s]+)', stmt_text, re.IGNORECASE)
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


def _extract_data_flow_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract data flow relationships from MOVE statements"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    # Handle both old format (string) and new format (dict)
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                        stmt_type = stmt_info.get('type', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract MOVE statement relationships
                        if stmt_type == "MoveStatement" and "MOVE_FROM:" in stmt_details and "MOVE_TO:" in stmt_details:
                            # Parse MOVE_FROM:source:MOVE_TO:target
                            parts = stmt_details.split(":")
                            if len(parts) >= 4:
                                source_var = parts[1]
                                target_var = parts[3]
                                
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=source_var,
                                    target_entity=target_var,
                                    relationship_type=RelationshipType.DATA_FLOW,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"MOVE statement in {para_name}: {source_var} -> {target_var}"
                                ))
                    else:
                        # Handle old string format
                        stmt_text = stmt_info
                        move_match = re.search(r'MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if move_match:
                            source_var = move_match.group(1)
                            target_var = move_match.group(2)
                            
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=source_var,
                                target_entity=target_var,
                                relationship_type=RelationshipType.DATA_FLOW,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=1,
                                context=f"MOVE statement in {para_name}: {source_var} -> {target_var}"
                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting data flow relationships: {e}")
    
    return relationships


def _extract_arithmetic_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract arithmetic operation relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_type = stmt_info.get('type', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract ADD statement relationships
                        if stmt_type == "AddStatement" and "ADD_OPERANDS:" in stmt_details:
                            operands = stmt_details.split("ADD_OPERANDS:")[1].split(",")
                            if len(operands) >= 2:
                                for i in range(1, len(operands)):
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=operands[0].strip(),
                                        target_entity=operands[i].strip(),
                                        relationship_type=RelationshipType.ARITHMETIC,
                                        confidence=0.9,
                                        relationship_strength="medium",
                                        line_number=1,
                                        context=f"ADD operation in {para_name}: {operands[0]} added to {operands[i]}"
                                    ))
                        
                        # Extract SUBTRACT statement relationships
                        elif stmt_type == "SubtractStatement" and "SUBTRACT_OPERANDS:" in stmt_details:
                            operands = stmt_details.split("SUBTRACT_OPERANDS:")[1].split(",")
                            if len(operands) >= 2:
                                for i in range(1, len(operands)):
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=operands[0].strip(),
                                        target_entity=operands[i].strip(),
                                        relationship_type=RelationshipType.ARITHMETIC,
                                        confidence=0.9,
                                        relationship_strength="medium",
                                        line_number=1,
                                        context=f"SUBTRACT operation in {para_name}: {operands[0]} subtracted from {operands[i]}"
                                    ))
                        
                        # Extract COMPUTE statement relationships
                        elif stmt_type == "ComputeStatement" and "COMPUTE_EXPR:" in stmt_details:
                            expr = stmt_details.split("COMPUTE_EXPR:")[1]
                            # Extract variables from expression (simple pattern matching)
                            variables = re.findall(r'([A-Z0-9-]+)', expr)
                            if len(variables) >= 2:
                                # First variable is typically the result
                                result_var = variables[0]
                                for var in variables[1:]:
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=var,
                                        target_entity=result_var,
                                        relationship_type=RelationshipType.ARITHMETIC,
                                        confidence=0.8,
                                        relationship_strength="medium",
                                        line_number=1,
                                        context=f"COMPUTE operation in {para_name}: {var} used in computation for {result_var}"
                                    ))
    except Exception as e:
        print(f"   ⚠️  Error extracting arithmetic relationships: {e}")
    
    return relationships


def _extract_conditional_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract conditional logic relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_type = stmt_info.get('type', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract IF statement relationships
                        if stmt_type == "IfStatement" and "IF_CONDITION:" in stmt_details:
                            condition = stmt_details.split("IF_CONDITION:")[1]
                            # Extract variables from condition
                            variables = re.findall(r'([A-Z0-9-]+)', condition)
                            for var in variables:
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=var,
                                    target_entity=para_name,
                                    relationship_type=RelationshipType.CONDITIONAL,
                                    confidence=0.85,
                                    relationship_strength="medium",
                                    line_number=1,
                                    context=f"IF condition in {para_name}: {var} used in condition"
                                ))
                        
                        # Extract EVALUATE statement relationships
                        elif stmt_type == "EvaluateStatement" and "EVALUATE_SUBJECTS:" in stmt_details:
                            subjects = stmt_details.split("EVALUATE_SUBJECTS:")[1].split(",")
                            for subject in subjects:
                                subject = subject.strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=subject,
                                    target_entity=para_name,
                                    relationship_type=RelationshipType.CONDITIONAL,
                                    confidence=0.85,
                                    relationship_strength="medium",
                                    line_number=1,
                                    context=f"EVALUATE subject in {para_name}: {subject} evaluated"
                                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting conditional relationships: {e}")
    
    return relationships


def _extract_data_item_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract data item definition and usage relationships"""
    relationships = []
    
    try:
        # Extract data item definitions
        data_items = file_data.get("data_items", {})
        if cu_name in data_items:
            for data_item in data_items[cu_name]:
                item_name = data_item.get('name', '')
                item_level = data_item.get('level', '')
                picture_clause = data_item.get('picture_clause', '')
                condition_names = data_item.get('condition_names', '')
                
                # Create relationship between compilation unit and data item
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,
                    target_entity=item_name,
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"Data item definition: {item_name} (Level {item_level}, PIC {picture_clause})"
                ))
                
                # Extract 88-level condition relationships
                if condition_names:
                    conditions = condition_names.split("|")
                    for condition in conditions:
                        if ":" in condition:
                            cond_name, cond_values = condition.split(":", 1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=item_name,
                                target_entity=cond_name.strip(),
                                relationship_type=RelationshipType.CONTAINS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=1,
                                context=f"88-level condition: {cond_name} for {item_name} with values {cond_values}"
                            ))
        
        # Extract linkage item relationships
        linkage_items = file_data.get("linkage_items", {})
        if cu_name in linkage_items:
            for linkage_item in linkage_items[cu_name]:
                item_name = linkage_item.get('name', '')
                item_level = linkage_item.get('level', '')
                
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,
                    target_entity=item_name,
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"Linkage item definition: {item_name} (Level {item_level})"
                ))
        
        # Extract file description relationships
        file_descriptions = file_data.get("file_descriptions", {})
        if cu_name in file_descriptions:
            for file_desc in file_descriptions[cu_name]:
                file_name = file_desc.get('name', '')
                
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,
                    target_entity=file_name,
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"File description: {file_name}"
                ))
                
    except Exception as e:
        print(f"   ⚠️  Error extracting data item relationships: {e}")
    
    return relationships


if __name__ == "__main__":
    print("🧪 Enhanced COBOL Relationship Extractor")
    print("   Use: extract_cobol_relationships(file_data)")
    print("   Supports: Data flow, arithmetic, conditional, and data item relationships")
