#!/usr/bin/env python3
"""
COBOL Relationship Extractor
Simple COBOL relationship extraction for the Code Grapher pipeline
"""

import re
import sys
import os
from typing import Dict, List, Any
from enum import Enum
from dataclasses import dataclass

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Enhanced relationship types for COBOL
class RelationshipType(Enum):
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
    PERFORMS = "PERFORMS"
    INCLUDES = "INCLUDES"
    PASSES_DATA = "PASSES_DATA"
    HANDLES_ERRORS = "HANDLES_ERRORS"
    USES_QUEUE = "USES_QUEUE"
    BINDS_SCREEN = "BINDS_SCREEN"
    REPLACES = "REPLACES"
    WRITTEN_BY = "WRITTEN_BY"

# Simple relationship class - using dataclass for compatibility

@dataclass
class RelationshipExtraction:
    source_file: str
    target_file: str
    source_entity: str
    target_entity: str
    relationship_type: RelationshipType
    confidence: float = 0.8
    relationship_strength: str = "medium"
    line_number: int = 1
    context: str = ""
    
    def __post_init__(self):
        # Debug: Track RelationshipExtraction creation
        print(f"   🔍 DEBUG: RelationshipExtraction created: {self.source_entity} -{self.relationship_type.value}-> {self.target_entity}")


def extract_cobol_relationships(file_data: Dict[str, Any]) -> List[RelationshipExtraction]:
    """
    Extract COBOL relationships from parsed COBOL file data
    
    Args:
        file_data: Parsed COBOL file data
        
    Returns:
        List of RelationshipExtraction objects
    """
    print(f"   🔍 DEBUG: extract_cobol_relationships called for {file_data.get('file_path', 'unknown')}")
    if file_data.get("language") != "cobol":
        print(f"   🔍 DEBUG: Not a COBOL file, returning empty list")
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
                relationships.extend(_extract_author_relationships(file_data, cu_name, file_path))
                
                # Extract additional relationship types
                relationships.extend(_extract_file_operations(file_data, cu_name, file_path))
                relationships.extend(_extract_variable_usage(file_data, cu_name, file_path))
                relationships.extend(_extract_include_statements(file_data, cu_name, file_path))
                relationships.extend(_extract_error_handling(file_data, cu_name, file_path))
                relationships.extend(_extract_screen_operations(file_data, cu_name, file_path))
                relationships.extend(_extract_queue_operations(file_data, cu_name, file_path))
                relationships.extend(_extract_replace_statements(file_data, cu_name, file_path))
        
        # Also extract relationships from line-number-based statement units
        if "statements" in file_data:
            for unit_name in file_data["statements"].keys():
                # Skip if this is already a compilation unit name
                if "compilation_units" in file_data:
                    cu_names = [cu.get("name", "UNKNOWN") for cu in file_data["compilation_units"]]
                    if unit_name in cu_names:
                        continue
                
                # Extract all relationship types for this unit
                print(f"   🔍 DEBUG: Processing unit {unit_name} for all relationship types")
                relationships.extend(_extract_cobol_calls(file_data, unit_name, file_path))
                relationships.extend(_extract_cobol_performs(file_data, unit_name, file_path))
                relationships.extend(_extract_cobol_copies(file_data, unit_name, file_path))
                relationships.extend(_extract_data_flow_relationships(file_data, unit_name, file_path))
                relationships.extend(_extract_arithmetic_relationships(file_data, unit_name, file_path))
                relationships.extend(_extract_conditional_relationships(file_data, unit_name, file_path))
                relationships.extend(_extract_data_item_relationships(file_data, unit_name, file_path))
                relationships.extend(_extract_author_relationships(file_data, unit_name, file_path))
                relationships.extend(_extract_file_operations(file_data, unit_name, file_path))
                relationships.extend(_extract_variable_usage(file_data, unit_name, file_path))
                relationships.extend(_extract_include_statements(file_data, unit_name, file_path))
                relationships.extend(_extract_error_handling(file_data, unit_name, file_path))
                relationships.extend(_extract_screen_operations(file_data, unit_name, file_path))
                relationships.extend(_extract_queue_operations(file_data, unit_name, file_path))
                relationships.extend(_extract_replace_statements(file_data, unit_name, file_path))
                
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
                print(f"   🔍 DEBUG: Creating CONTAINS relationship: {file_path} -> {cu_name}")
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
                    # Skip non-statement items (like paragraph names)
                    if not isinstance(stmt, dict):
                        continue
                        
                    stmt_text = stmt.get('text', '')
                    
                    # Handle both "PERFORM paragraph" and "PERFORMparagraph" (no space)
                    # Check both text and details fields
                    search_text = stmt_text
                    if isinstance(stmt, dict) and 'details' in stmt:
                        search_text = stmt['details']
                    
                    perform_match = re.search(r'PERFORM\s*([A-Z0-9-]+)', search_text, re.IGNORECASE)
                    if perform_match:
                        target_paragraph = perform_match.group(1)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=para_name,
                            target_entity=target_paragraph,
                            relationship_type=RelationshipType.PERFORMS,
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
        print(f"   🔍 DEBUG: _extract_data_flow_relationships called for unit {cu_name}")
        statements = file_data.get("statements", {})
        if cu_name in statements:
            print(f"   🔍 DEBUG: Found unit {cu_name} in statements")
            for para_name, stmt_list in statements[cu_name].items():
                print(f"   🔍 DEBUG: Processing paragraph {para_name} with {len(stmt_list)} statements")
                for stmt_info in stmt_list:
                    # Handle both old format (string) and new format (dict)
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                        stmt_type = stmt_info.get('type', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract MOVE statement relationships
                        if stmt_type in ["MoveStatement", "MoveStatementImpl"]:
                            print(f"   🔍 DEBUG: Found MOVE statement: {stmt_text[:50]}...")
                            print(f"   🔍 DEBUG: Details: {stmt_details}")
                            
                            # Check if we have structured details
                            if "MOVE_FROM:" in stmt_details and "MOVE_TO:" in stmt_details:
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
                                    print(f"   🔍 DEBUG: Created DATA_FLOW relationship: {source_var} -> {target_var}")
                            else:
                                # Parse raw MOVE statement text - handle cases like "MOVETRANS-DATETOCUST-LAST-TRANS-DATE"
                                move_text = stmt_details if stmt_details else stmt_text
                                if move_text:
                                    print(f"   🔍 DEBUG: Parsing MOVE text: {move_text}")
                                    # Look for MOVE ... TO ... pattern - handle both quoted and unquoted values
                                    move_match = re.search(r'MOVE\s*([A-Z0-9-]+|\'[^\']+\')\s*TO\s*([A-Z0-9-]+)', move_text, re.IGNORECASE)
                                    if move_match:
                                        source_var = move_match.group(1).strip()
                                        target_var = move_match.group(2).strip()
                                        
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
                                        print(f"   🔍 DEBUG: Created DATA_FLOW relationship: {source_var} -> {target_var}")
                                    else:
                                        print(f"   🔍 DEBUG: No MOVE pattern found in: {move_text}")
                    else:
                        # Handle old string format
                        stmt_text = stmt_info
                        if 'MOVE' in stmt_text.upper():
                            move_match = re.search(r'MOVE\s*([A-Z0-9-]+|\'[^\']+\')\s*TO\s*([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                            if move_match:
                                source_var = move_match.group(1).strip()
                                target_var = move_match.group(2).strip()
                                
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
                                print(f"   🔍 DEBUG: Created DATA_FLOW relationship: {source_var} -> {target_var}")
    except Exception as e:
        print(f"   ⚠️  Error extracting data flow relationships: {e}")
    
    print(f"   🔍 DEBUG: _extract_data_flow_relationships returning {len(relationships)} relationships")
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
                        stmt_text = stmt_info.get('text', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract COMPUTE statement relationships (arithmetic operations)
                        if stmt_type in ["ComputeStatementImpl"]:
                            # Parse raw COMPUTE statement text
                            compute_text = stmt_details if stmt_details else stmt_text
                            if compute_text and 'COMPUTE' in compute_text.upper():
                                # Look for COMPUTE ... = ... pattern (handle both with and without space)
                                compute_match = re.search(r'COMPUTE\s*([A-Z0-9-]+)\s*=\s*([^=]+)', compute_text, re.IGNORECASE)
                                if compute_match:
                                    target_var = compute_match.group(1).strip()
                                    expression = compute_match.group(2).strip()
                                    
                                    # Extract variables from the expression
                                    variables = re.findall(r'([A-Z0-9-]+)', expression)
                                    for var in variables:
                                        if var != target_var:  # Don't create self-references
                                            relationships.append(RelationshipExtraction(
                                                source_file=file_path,
                                                target_file=file_path,
                                                source_entity=var,
                                                target_entity=target_var,
                                                relationship_type=RelationshipType.ARITHMETIC,
                                                confidence=0.9,
                                                relationship_strength="medium",
                                                line_number=1,
                                                context=f"COMPUTE operation in {para_name}: {var} used in calculation for {target_var}"
                                            ))
                        
                        # Extract ADD statement relationships
                        elif stmt_type in ["AddStatementImpl"]:
                            add_text = stmt_details if stmt_details else stmt_text
                            if add_text and 'ADD' in add_text.upper():
                                # Look for ADD ... TO ... pattern
                                add_match = re.search(r'ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', add_text, re.IGNORECASE)
                                if add_match:
                                    source_var = add_match.group(1).strip()
                                    target_var = add_match.group(2).strip()
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=source_var,
                                        target_entity=target_var,
                                        relationship_type=RelationshipType.ARITHMETIC,
                                        confidence=0.9,
                                        relationship_strength="medium",
                                        line_number=1,
                                        context=f"ADD operation in {para_name}: {source_var} added to {target_var}"
                                    ))
                        
                        # Extract SUBTRACT statement relationships
                        elif stmt_type in ["SubtractStatementImpl"] and "SUBTRACT_OPERANDS:" in stmt_details:
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
                        
                        # Extract COMPUTE statement relationships (structured data)
                        elif stmt_type in ["ComputeStatementImpl"] and "COMPUTE_EXPR:" in stmt_details:
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
                        
                        # Extract COMPUTE statement relationships (raw text)
                        elif stmt_type in ["ComputeStatementImpl"] and not "COMPUTE_EXPR:" in stmt_details:
                            # Parse raw COMPUTE statement text
                            compute_text = stmt_details if stmt_details else stmt_text
                            if compute_text and 'COMPUTE' in compute_text.upper():
                                # Look for COMPUTE ... = ... pattern (handle both with and without space)
                                compute_match = re.search(r'COMPUTE\s*([A-Z0-9-]+)\s*=\s*([^=]+)', compute_text, re.IGNORECASE)
                                if compute_match:
                                    target_var = compute_match.group(1).strip()
                                    expression = compute_match.group(2).strip()
                                    
                                    # Extract variables from the expression
                                    variables = re.findall(r'([A-Z0-9-]+)', expression)
                                    for var in variables:
                                        if var != target_var and len(var) > 1:  # Don't create self-references and skip single chars
                                            relationships.append(RelationshipExtraction(
                                                source_file=file_path,
                                                target_file=file_path,
                                                source_entity=var,
                                                target_entity=target_var,
                                                relationship_type=RelationshipType.ARITHMETIC,
                                                confidence=0.9,
                                                relationship_strength="medium",
                                                line_number=1,
                                                context=f"COMPUTE operation in {para_name}: {var} used in calculation for {target_var}"
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
                        stmt_text = stmt_info.get('text', '')
                        stmt_details = stmt_info.get('details', '')
                        
                        # Extract IF statement relationships
                        if stmt_type in ["IfStatementImpl"]:
                            # Parse raw IF statement text
                            if_text = stmt_details if stmt_details else stmt_text
                            if if_text and 'IF' in if_text.upper():
                                # Look for IF ... THEN pattern (handle both with and without space)
                                if_match = re.search(r'IF\s*([^THEN]+)', if_text, re.IGNORECASE)
                                if if_match:
                                    condition = if_match.group(1).strip()
                                    # Extract variables from condition
                                    variables = re.findall(r'([A-Z0-9-]+)', condition)
                                    for var in variables:
                                        if len(var) > 1:  # Skip single character variables
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
                        
                        # Extract EVALUATE statement relationships (structured data)
                        elif stmt_type in ["EvaluateStatementImpl"] and "EVALUATE_SUBJECTS:" in stmt_details:
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
                        
                        # Extract IF statement relationships (raw text)
                        elif stmt_type in ["IfStatementImpl"] and not "IF_CONDITION:" in stmt_details:
                            # Parse raw IF statement text
                            if_text = stmt_details if stmt_details else stmt_text
                            if if_text and 'IF' in if_text.upper():
                                # Look for IF ... THEN pattern (handle both with and without space)
                                if_match = re.search(r'IF\s*([^THEN]+)', if_text, re.IGNORECASE)
                                if if_match:
                                    condition = if_match.group(1).strip()
                                    # Extract variables from condition
                                    variables = re.findall(r'([A-Z0-9-]+)', condition)
                                    for var in variables:
                                        if len(var) > 1:  # Skip single character variables
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


def _extract_author_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract author and date relationships from identification division"""
    relationships = []
    
    try:
        # Check if identification data is available
        if "identification_data" not in file_data:
            return relationships
            
        identification_data = file_data["identification_data"]
        
        # Extract author relationships for this compilation unit
        if cu_name in identification_data:
            id_data = identification_data[cu_name]
            
            # Create author node and relationship
            if "author" in id_data:
                author = id_data["author"]
                print(f"   🔍 DEBUG: Creating WRITTEN_BY relationship: {cu_name} -> {author}")
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,
                    target_entity=author,
                    relationship_type=RelationshipType.WRITTEN_BY,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"Program written by {author}"
                ))
                
                # Add author as an entity
                if "entities" in file_data:
                    author_entity = {
                        "type": "author",
                        "name": author,
                        "file_path": file_path,
                        "line_number": 1,
                        "context": f"Author of {cu_name}",
                        "language": "cobol"
                    }
                    file_data["entities"].append(author_entity)
            
            # Add date as property to paragraphs
            if "date_written" in id_data:
                date_written = id_data["date_written"]
                
                # Add date property to all paragraphs in this compilation unit
                if "statements" in file_data and cu_name in file_data["statements"]:
                    for para_name, statements in file_data["statements"][cu_name].items():
                        # Add date property to the paragraph
                        if isinstance(statements, list) and len(statements) > 0:
                            # Add date property to the first statement as a paragraph property
                            if isinstance(statements[0], dict):
                                statements[0]["date_written"] = date_written
                                statements[0]["paragraph_date"] = date_written
                                print(f"   🔍 DEBUG: Added date_written property to paragraph {para_name}: {date_written}")
                
                # Also add to paragraphs data if it exists
                if "paragraphs" in file_data and cu_name in file_data["paragraphs"]:
                    for para in file_data["paragraphs"][cu_name]:
                        if isinstance(para, dict):
                            para["date_written"] = date_written
                            para["paragraph_date"] = date_written
                
    except Exception as e:
        print(f"   ⚠️  Error extracting author relationships: {e}")
    
    return relationships


def _extract_file_operations(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract file operation relationships (READS, WRITES, FILE_ACCESS)"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        # Process all statement units, not just the main compilation unit
        for unit_name, unit_data in statements.items():
            if isinstance(unit_data, dict):
                for para_name, stmt_list in unit_data.items():
                    for stmt_info in stmt_list:
                        if isinstance(stmt_info, dict):
                            stmt_type = stmt_info.get('type', '')
                            stmt_details = stmt_info.get('details', '')
                            stmt_text = stmt_info.get('text', '')
                        else:
                            stmt_text = stmt_info
                            stmt_type = ''
                            stmt_details = ''
                        
                        # Extract READ statement relationships
                        if stmt_type in ["ReadStatementImpl"]:
                            if "READ_FILE:" in stmt_details:
                                file_name = stmt_details.split("READ_FILE:")[1].split(",")[0].strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.READS,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"READ statement in {para_name} from {file_name}"
                                ))
                            else:
                                # Parse raw READ statement text
                                read_match = re.search(r'READ\s+([A-Z0-9-]+)', stmt_details, re.IGNORECASE)
                                if read_match:
                                    file_name = read_match.group(1)
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=para_name,
                                        target_entity=file_name,
                                        relationship_type=RelationshipType.READS,
                                        confidence=0.9,
                                        relationship_strength="strong",
                                        line_number=1,
                                        context=f"READ statement in {para_name} from {file_name}"
                                    ))
                        elif "READ" in stmt_text.upper() and "FILE" in stmt_text.upper():
                            # Fallback for old format
                            read_match = re.search(r'READ\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                            if read_match:
                                file_name = read_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.READS,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"READ statement in {para_name} from {file_name}"
                                ))
                        
                        # Extract WRITE statement relationships
                        if stmt_type in ["WriteStatementImpl"]:
                            if "WRITE_FILE:" in stmt_details:
                                file_name = stmt_details.split("WRITE_FILE:")[1].split(",")[0].strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.WRITES,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"WRITE statement in {para_name} to {file_name}"
                                ))
                            else:
                                # Parse raw WRITE statement text
                                write_match = re.search(r'WRITE\s+([A-Z0-9-]+)', stmt_details, re.IGNORECASE)
                                if write_match:
                                    file_name = write_match.group(1)
                                    relationships.append(RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=para_name,
                                        target_entity=file_name,
                                        relationship_type=RelationshipType.WRITES,
                                        confidence=0.9,
                                        relationship_strength="strong",
                                        line_number=1,
                                        context=f"WRITE statement in {para_name} to {file_name}"
                                    ))
                        elif "WRITE" in stmt_text.upper() and "FILE" in stmt_text.upper():
                            # Fallback for old format
                            write_match = re.search(r'WRITE\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                            if write_match:
                                file_name = write_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.WRITES,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"WRITE statement in {para_name} to {file_name}"
                                ))
                        
                        # Extract OPEN statement relationships
                        if stmt_type in ["OpenStatementImpl"] and "OPEN_FILES:" in stmt_details:
                            files = stmt_details.split("OPEN_FILES:")[1].split(",")
                            for file_name in files:
                                file_name = file_name.strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.FILE_ACCESS,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"OPEN statement in {para_name} for {file_name}"
                                ))
                        elif "OPEN" in stmt_text.upper() and "FILE" in stmt_text.upper():
                            # Fallback for old format
                            open_match = re.search(r'OPEN\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                            if open_match:
                                file_name = open_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.FILE_ACCESS,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"OPEN statement in {para_name} for {file_name}"
                                ))
                        
                        # Extract CLOSE statement relationships
                        if stmt_type in ["CloseStatementImpl"] and "CLOSE_FILES:" in stmt_details:
                            files = stmt_details.split("CLOSE_FILES:")[1].split(",")
                            for file_name in files:
                                file_name = file_name.strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.FILE_ACCESS,
                                    confidence=0.95,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"CLOSE statement in {para_name} for {file_name}"
                                ))
                        elif "CLOSE" in stmt_text.upper() and "FILE" in stmt_text.upper():
                            # Fallback for old format
                            close_match = re.search(r'CLOSE\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                            if close_match:
                                file_name = close_match.group(1)
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=file_name,
                                    relationship_type=RelationshipType.FILE_ACCESS,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=1,
                                    context=f"CLOSE statement in {para_name} for {file_name}"
                                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting file operations: {e}")
    
    return relationships


def _extract_variable_usage(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract variable usage relationships (USES, MODIFIES)"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_type = stmt_info.get('type', '')
                        stmt_details = stmt_info.get('details', '')
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                        stmt_type = ''
                        stmt_details = ''
                    
                    # Extract variable usage from various statement types
                    if stmt_type in ["IfStatement", "IfStatementImpl", "EvaluateStatement", "EvaluateStatementImpl", "ComputeStatement", "ComputeStatementImpl", "AddStatement", "AddStatementImpl", "SubtractStatement", "SubtractStatementImpl"]:
                        # Extract variables from conditions and expressions
                        if "IF_CONDITION:" in stmt_details:
                            condition = stmt_details.split("IF_CONDITION:")[1]
                            variables = re.findall(r'([A-Z0-9-]+)', condition)
                            for var in variables:
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=var,
                                    relationship_type=RelationshipType.USES,
                                    confidence=0.85,
                                    relationship_strength="medium",
                                    line_number=1,
                                    context=f"Variable {var} used in condition in {para_name}"
                                ))
                        
                        if "EVALUATE_SUBJECTS:" in stmt_details:
                            subjects = stmt_details.split("EVALUATE_SUBJECTS:")[1].split(",")
                            for subject in subjects:
                                subject = subject.strip()
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=subject,
                                    relationship_type=RelationshipType.USES,
                                    confidence=0.85,
                                    relationship_strength="medium",
                                    line_number=1,
                                    context=f"Variable {subject} used in EVALUATE in {para_name}"
                                ))
                        
                        if "COMPUTE_EXPR:" in stmt_details:
                            expr = stmt_details.split("COMPUTE_EXPR:")[1]
                            variables = re.findall(r'([A-Z0-9-]+)', expr)
                            for var in variables:
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=var,
                                    relationship_type=RelationshipType.USES,
                                    confidence=0.85,
                                    relationship_strength="medium",
                                    line_number=1,
                                    context=f"Variable {var} used in COMPUTE expression in {para_name}"
                                ))
                    
                    # Extract variable modifications from MOVE statements
                    if stmt_type in ["MoveStatement", "MoveStatementImpl"] and "MOVE_TO:" in stmt_details:
                        target_var = stmt_details.split("MOVE_TO:")[1].split(",")[0].strip()
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=para_name,
                            target_entity=target_var,
                            relationship_type=RelationshipType.MODIFIES,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"Variable {target_var} modified in {para_name}"
                        ))
                    elif "MOVE" in stmt_text.upper() and "TO" in stmt_text.upper():
                        # Fallback for old format
                        move_match = re.search(r'MOVE\s+[A-Z0-9-]+\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if move_match:
                            target_var = move_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=target_var,
                                relationship_type=RelationshipType.MODIFIES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=1,
                                context=f"Variable {target_var} modified in {para_name}"
                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting variable usage: {e}")
    
    return relationships


def _extract_include_statements(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract INCLUDE statement relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                    
                    # Look for INCLUDE statements
                    include_match = re.search(r'INCLUDE\s+([^\s]+)', stmt_text, re.IGNORECASE)
                    if include_match:
                        include_file = include_match.group(1)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=f"{include_file}.cbl",
                            source_entity=cu_name,
                            target_entity=include_file,
                            relationship_type=RelationshipType.INCLUDES,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"INCLUDE statement for {include_file}"
                        ))
    except Exception as e:
        print(f"   ⚠️  Error extracting INCLUDE statements: {e}")
    
    return relationships


def _extract_error_handling(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract error handling relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                    
                    # Look for error handling statements
                    if any(keyword in stmt_text.upper() for keyword in ["ON ERROR", "AT END", "INVALID KEY", "NOT INVALID KEY"]):
                        # Extract the statement that handles the error
                        error_handling_match = re.search(r'(ON ERROR|AT END|INVALID KEY|NOT INVALID KEY)', stmt_text, re.IGNORECASE)
                        if error_handling_match:
                            error_type = error_handling_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=f"{error_type}_HANDLER",
                                relationship_type=RelationshipType.HANDLES_ERRORS,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=1,
                                context=f"Error handling {error_type} in {para_name}"
                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting error handling: {e}")
    
    return relationships


def _extract_screen_operations(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract screen operation relationships"""
    relationships = []
    
    try:
        # Check for screen section in data division
        if "screen_sections" in file_data and cu_name in file_data["screen_sections"]:
            for screen_item in file_data["screen_sections"][cu_name]:
                screen_name = screen_item.get('name', '')
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,
                    target_entity=screen_name,
                    relationship_type=RelationshipType.BINDS_SCREEN,
                    confidence=1.0,
                    relationship_strength="strong",
                    line_number=1,
                    context=f"Screen binding for {screen_name}"
                ))
        
        # Check for ACCEPT/DISPLAY statements
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                    
                    # Look for ACCEPT/DISPLAY statements
                    if "ACCEPT" in stmt_text.upper() or "DISPLAY" in stmt_text.upper():
                        screen_match = re.search(r'(ACCEPT|DISPLAY)\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if screen_match:
                            operation = screen_match.group(1)
                            screen_name = screen_match.group(2)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=screen_name,
                                relationship_type=RelationshipType.BINDS_SCREEN,
                                confidence=0.9,
                                relationship_strength="medium",
                                line_number=1,
                                context=f"{operation} operation on screen {screen_name} in {para_name}"
                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting screen operations: {e}")
    
    return relationships


def _extract_queue_operations(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract queue operation relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                    
                    # Look for queue operations
                    if any(keyword in stmt_text.upper() for keyword in ["ENQUEUE", "DEQUEUE", "QUEUE"]):
                        queue_match = re.search(r'(ENQUEUE|DEQUEUE|QUEUE)\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if queue_match:
                            operation = queue_match.group(1)
                            queue_name = queue_match.group(2)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=queue_name,
                                relationship_type=RelationshipType.USES_QUEUE,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=1,
                                context=f"{operation} operation on queue {queue_name} in {para_name}"
                            ))
    except Exception as e:
        print(f"   ⚠️  Error extracting queue operations: {e}")
    
    return relationships


def _extract_replace_statements(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract REPLACE statement relationships"""
    relationships = []
    
    try:
        statements = file_data.get("statements", {})
        if cu_name in statements:
            for para_name, stmt_list in statements[cu_name].items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, dict):
                        stmt_text = stmt_info.get('text', '')
                    else:
                        stmt_text = stmt_info
                    
                    # Look for REPLACE statements
                    replace_match = re.search(r'REPLACE\s+([^\s]+)\s+BY\s+([^\s]+)', stmt_text, re.IGNORECASE)
                    if replace_match:
                        old_text = replace_match.group(1)
                        new_text = replace_match.group(2)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=old_text,
                            target_entity=new_text,
                            relationship_type=RelationshipType.REPLACES,
                            confidence=0.95,
                            relationship_strength="strong",
                            line_number=1,
                            context=f"REPLACE statement in {para_name}: {old_text} -> {new_text}"
                        ))
    except Exception as e:
        print(f"   ⚠️  Error extracting REPLACE statements: {e}")
    
    return relationships


if __name__ == "__main__":
    print("🧪 Enhanced COBOL Relationship Extractor")
    print("   Use: extract_cobol_relationships(file_data)")
    print("   Supports: Data flow, arithmetic, conditional, and data item relationships")
