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
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
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
        # print(f"   🔍 DEBUG: RelationshipExtraction created: {self.source_entity} -{self.relationship_type.value}-> {self.target_entity}")
        pass


def _get_statements_for_unit(file_data: Dict[str, Any], cu_name: str) -> List[Dict[str, Any]]:
    """Helper function to get statements for a compilation unit (ProLeap format)"""
    statements = file_data.get("statements", {})
    unit_statements = []
    
    # ProLeap format: statements are grouped by line number, each group contains paragraph -> statement list
    for line_key, line_data in statements.items():
        if not isinstance(line_data, dict):
            continue
            
        # Each line can have multiple paragraphs with statements
        for para_name, stmt_list in line_data.items():
            if not isinstance(stmt_list, list):
                continue
                
            for stmt in stmt_list:
                if not isinstance(stmt, dict):
                    continue
                    
                # Add paragraph name and line info to statement
                stmt_with_context = stmt.copy()
                stmt_with_context['para_name'] = para_name
                stmt_with_context['unit'] = cu_name
                unit_statements.append(stmt_with_context)
    
    return unit_statements


def _deduplicate_relationships(relationships: List[RelationshipExtraction]) -> List[RelationshipExtraction]:
    """Remove duplicate relationships based on source, target, and type"""
    seen = set()
    unique_relationships = []
    
    for rel in relationships:
        # Create a unique key for the relationship
        key = (
            rel.source_entity,
            rel.target_entity, 
            rel.relationship_type.value,
            rel.source_file,
            rel.target_file
        )
        
        if key not in seen:
            seen.add(key)
            unique_relationships.append(rel)
        else:
            # Optional: Log duplicate relationships for debugging
            # print(f"   🔍 DEBUG: Skipping duplicate relationship: {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
            pass
    
    return unique_relationships


def extract_cobol_relationships(file_data: Dict[str, Any]) -> List[RelationshipExtraction]:
    """
    Extract COBOL relationships from parsed COBOL file data
    
    Args:
        file_data: Parsed COBOL file data
        
    Returns:
        List of RelationshipExtraction objects
    """
    # # print(f"   🔍 DEBUG: extract_cobol_relationships called for {file_data.get('file_path', 'unknown')}")
    if file_data.get("language") != "cobol":
        # print(f"   🔍 DEBUG: Not a COBOL file, returning empty list")
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
                
                # Extract simple USES relationships from statements
                relationships.extend(_extract_simple_uses_relationships(file_data, cu_name, file_path))
                
        # Filter out self-referencing relationships
        filtered_relationships = []
        for rel in relationships:
            if rel.source_entity != rel.target_entity:
                filtered_relationships.append(rel)
            else:
                # print(f"   🔍 DEBUG: Filtered out self-referencing relationship: {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
                pass
        
        # Deduplicate relationships
        unique_relationships = _deduplicate_relationships(filtered_relationships)
        
        print(f"   🟦 Extracted {len(unique_relationships)} COBOL relationships from {file_path} (filtered {len(relationships) - len(filtered_relationships)} self-referencing, {len(filtered_relationships) - len(unique_relationships)} duplicates)")
        
    except Exception as e:
        print(f"   ⚠️  Error extracting COBOL relationships from {file_path}: {e}")
        unique_relationships = []
    
    return unique_relationships


def _extract_basic_relationships(file_data: Dict[str, Any], file_path: str) -> List[RelationshipExtraction]:
    """Extract basic containment relationships"""
    relationships = []
    
    try:
        # File contains compilation units
        if "compilation_units" in file_data:
            for cu in file_data["compilation_units"]:
                cu_name = cu.get("name", "UNKNOWN")
                # print(f"   🔍 DEBUG: Creating CONTAINS relationship: {file_path} -> {cu_name}")
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
        
        # Compilation units contain paragraphs and data items from entities
        if "entities" in file_data:
            for entity in file_data["entities"]:
                entity_type = entity.get("type", "")
                entity_name = entity.get("name", "")
                
                if entity_type in ["paragraph", "data_item"] and entity_name:
                    # Find the compilation unit this entity belongs to
                    cu_name = "UNKNOWN"
                    if "compilation_units" in file_data and file_data["compilation_units"]:
                        cu_name = file_data["compilation_units"][0].get("name", "UNKNOWN")
                    
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=cu_name,
                        target_entity=entity_name,
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=0.9,
                        relationship_strength="strong",
                        line_number=entity.get("start_line", 1),
                        context=f"Compilation unit {cu_name} contains {entity_type} {entity_name}"
                    ))
                    
    except Exception as e:
        print(f"   ⚠️  Error extracting basic relationships: {e}")
    
    return relationships


def _extract_simple_uses_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract simple USES relationships from COBOL statements (ProLeap format)"""
    relationships = []
    
    try:
        # Get statements for this compilation unit - ProLeap format
        statements = file_data.get("statements", {})
        
        # ProLeap format: statements are grouped by line number, each group contains paragraph -> statement list
        for line_key, line_data in statements.items():
            if not isinstance(line_data, dict):
                continue
                
            # Each line can have multiple paragraphs with statements
            for para_name, stmt_list in line_data.items():
                if not isinstance(stmt_list, list):
                    continue
                    
                for stmt in stmt_list:
                    if not isinstance(stmt, dict):
                        continue
                        
                    stmt_text = stmt.get('details', '')  # Use details field for actual statement text
                    stmt_type = stmt.get('type', '')
                    stmt_details = stmt.get('details', '')
                    line_number = stmt.get('start_line', 1)
                    
                    # Extract PERFORM relationships
                    if 'PerformStatementImpl' in stmt_type and 'PERFORM' in stmt_details.upper():
                        # Extract paragraph name from PERFORM statement
                        perform_match = re.search(r'PERFORM\s*([A-Z0-9-]+)', stmt_details, re.IGNORECASE)
                        if perform_match:
                            target_para = perform_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=target_para,
                                relationship_type=RelationshipType.PERFORMS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"PERFORM statement: {para_name} performs {target_para}"
                            ))
                    
                    # Extract CALL relationships
                    elif 'CallStatementImpl' in stmt_type:
                        # Extract program name from CALL statement
                        call_match = re.search(r'CALL\s*([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if call_match:
                            target_program = call_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=target_program,
                                relationship_type=RelationshipType.CALLS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"CALL statement: {para_name} calls {target_program}"
                            ))
                    
                    # Extract variable usage from MOVE statements
                    elif 'MoveStatementImpl' in stmt_type:
                        move_match = re.search(r'MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if move_match:
                            source_var = move_match.group(1)
                            target_var = move_match.group(2)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=source_var,
                                relationship_type=RelationshipType.USES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"MOVE statement: {para_name} uses {source_var}"
                            ))
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=target_var,
                                relationship_type=RelationshipType.MODIFIES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"MOVE statement: {para_name} modifies {target_var}"
                            ))
                    
                    # Extract variable usage from READ statements
                    elif 'ReadStatementImpl' in stmt_type:
                        read_match = re.search(r'READ\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if read_match:
                            file_name = read_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=file_name,
                                relationship_type=RelationshipType.READS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"READ statement: {para_name} reads {file_name}"
                            ))
                    
                    # Extract variable usage from WRITE statements
                    elif 'WriteStatementImpl' in stmt_type:
                        write_match = re.search(r'WRITE([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if write_match:
                            file_name = write_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=file_name,
                                relationship_type=RelationshipType.WRITES,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"WRITE statement: {para_name} writes {file_name}"
                            ))
                    
                    # Extract file access from CLOSE statements
                    elif 'CloseStatementImpl' in stmt_type:
                        close_match = re.search(r'CLOSE([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if close_match:
                            file_name = close_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=file_name,
                                relationship_type=RelationshipType.FILE_ACCESS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"CLOSE statement: {para_name} closes {file_name}"
                            ))
                    
                    # Extract file access from OPEN statements
                    elif 'OpenStatementImpl' in stmt_type:
                        open_match = re.search(r'OPEN(?:INPUT|OUTPUT|I-O)([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if open_match:
                            file_name = open_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=file_name,
                                relationship_type=RelationshipType.FILE_ACCESS,
                                confidence=0.95,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"OPEN statement: {para_name} opens {file_name}"
                            ))
                    
                    # Extract screen binding from DISPLAY statements
                    elif 'DisplayStatementImpl' in stmt_type:
                        # Extract message from DISPLAY statement
                        display_match = re.search(r'DISPLAY"([^"]+)"', stmt_text, re.IGNORECASE)
                        if display_match:
                            message = display_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=message,
                                relationship_type=RelationshipType.BINDS_SCREEN,
                                confidence=0.9,
                                relationship_strength="medium",
                                line_number=line_number,
                                context=f"DISPLAY statement: {para_name} displays {message}"
                            ))
                    
                    # Extract variable usage from IF statements
                    elif 'IfStatementImpl' in stmt_type:
                        if_match = re.search(r'IF\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if if_match:
                            var_name = if_match.group(1)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=var_name,
                                relationship_type=RelationshipType.USES,
                                confidence=0.8,
                                relationship_strength="medium",
                                line_number=line_number,
                                context=f"IF statement: {para_name} uses {var_name}"
                            ))
                    
                    # Extract variable usage from COMPUTE statements
                    elif 'ComputeStatementImpl' in stmt_type:
                        # Extract variables from COMPUTE expressions
                        variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                        for var in variables:
                            if len(var) > 2:  # Filter out short matches
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=para_name,
                                    target_entity=var,
                                    relationship_type=RelationshipType.USES,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=line_number,
                                    context=f"COMPUTE statement: {para_name} uses {var}"
                                ))
                    
                    # Extract variable usage from ADD statements
                    elif 'AddStatementImpl' in stmt_type:
                        add_match = re.search(r'ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                        if add_match:
                            source_var = add_match.group(1)
                            target_var = add_match.group(2)
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=source_var,
                                relationship_type=RelationshipType.USES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"ADD statement: {para_name} uses {source_var}"
                            ))
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=para_name,
                                target_entity=target_var,
                                relationship_type=RelationshipType.MODIFIES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"ADD statement: {para_name} modifies {target_var}"
                            ))
    
    except Exception as e:
        print(f"   ⚠️  Error extracting simple USES relationships: {e}")
    
    return relationships


def _extract_cobol_calls(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract CALL statement relationships"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)
        
        for stmt_data in unit_statements:
            stmt_text = stmt_data.get('text', '')
            stmt_type = stmt_data.get('type', '')
            
            # Only process CALL statements
            if stmt_type != 'CALL':
                continue
                
            # Handle both "CALL program" and "CALLprogram" (no space)
            call_match = re.search(r'CALL\s*["\']?([A-Z0-9-]+)["\']?', stmt_text, re.IGNORECASE)
            if call_match:
                target_program = call_match.group(1)
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=f"{target_program}.cbl",
                    source_entity=cu_name,  # Use compilation unit as source
                    target_entity=target_program,
                    relationship_type=RelationshipType.CALLS,
                    confidence=0.95,
                    relationship_strength="strong",
                    line_number=stmt_data.get('line', 1),
                    context=f"CALL statement to {target_program}"
                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting CALL relationships: {e}")
    
    return relationships


def _extract_cobol_performs(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract PERFORM statement relationships"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)
        
        for stmt_data in unit_statements:
            stmt_text = stmt_data.get('text', '')
            stmt_type = stmt_data.get('type', '')
            
            # Only process PERFORM statements
            if stmt_type != 'PERFORM':
                continue
                    
            # Handle both "PERFORM paragraph" and "PERFORMparagraph" (no space)
            perform_match = re.search(r'PERFORM\s*([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
            if perform_match:
                target_paragraph = perform_match.group(1)
                relationships.append(RelationshipExtraction(
                    source_file=file_path,
                    target_file=file_path,
                    source_entity=cu_name,  # Use compilation unit as source
                    target_entity=target_paragraph,
                    relationship_type=RelationshipType.PERFORMS,
                    confidence=0.95,
                    relationship_strength="strong",
                    line_number=stmt_data.get('line', 1),
                    context=f"PERFORM statement to {target_paragraph}"
                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting PERFORM relationships: {e}")
    
    return relationships


def _extract_cobol_copies(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract COPY statement relationships"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)
        
        for stmt_data in unit_statements:
            stmt_text = stmt_data.get('text', '')
            
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
                    line_number=stmt_data.get('line', 1),
                    context=f"COPY statement for {target_copybook}"
                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting COPY relationships: {e}")
    
    return relationships


def _extract_data_flow_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract data flow relationships from MOVE statements"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)
        
        for stmt_info in unit_statements:
            # Handle both old format (string) and new format (dict)
            if isinstance(stmt_info, dict):
                stmt_text = stmt_info.get('text', '')
                stmt_type = stmt_info.get('type', '')
                stmt_details = stmt_info.get('details', '')
                
                # Extract MOVE statement relationships
                if stmt_type in ["MoveStatement", "MoveStatementImpl"]:
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
                                line_number=stmt_info.get('line', 1),
                                context=f"MOVE statement: {source_var} -> {target_var}"
                            ))
                    else:
                        # Parse raw MOVE statement text
                        move_match = re.search(r'MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_details, re.IGNORECASE)
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
                                line_number=stmt_info.get('line', 1),
                                context=f"MOVE statement: {source_var} -> {target_var}"
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
                        context=f"MOVE statement: {source_var} -> {target_var}"
                    ))
    except Exception as e:
        print(f"   ⚠️  Error extracting data flow relationships: {e}")
    
    return relationships


def _extract_arithmetic_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract arithmetic operation relationships (ProLeap format)"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)
        
        for stmt_info in unit_statements:
            if isinstance(stmt_info, dict):
                stmt_type = stmt_info.get('type', '')
                stmt_text = stmt_info.get('text', '')
                stmt_details = stmt_info.get('details', '')
                para_name = stmt_info.get('para_name', cu_name)
                line_number = stmt_info.get('start_line', 1)
                
                # Extract ADD statement relationships
                if 'ADD' in stmt_text.upper():
                    add_match = re.search(r'ADD\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                    if add_match:
                        source_var = add_match.group(1)
                        target_var = add_match.group(2)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=source_var,
                            target_entity=target_var,
                            relationship_type=RelationshipType.ARITHMETIC,
                            confidence=0.9,
                            relationship_strength="strong",
                            line_number=line_number,
                            context=f"ADD operation in {para_name}: {source_var} added to {target_var}"
                        ))
                
                # Extract SUBTRACT statement relationships
                elif 'SUBTRACT' in stmt_text.upper():
                    sub_match = re.search(r'SUBTRACT\s+([A-Z0-9-]+)\s+FROM\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                    if sub_match:
                        source_var = sub_match.group(1)
                        target_var = sub_match.group(2)
                        relationships.append(RelationshipExtraction(
                            source_file=file_path,
                            target_file=file_path,
                            source_entity=source_var,
                            target_entity=target_var,
                            relationship_type=RelationshipType.ARITHMETIC,
                            confidence=0.9,
                            relationship_strength="strong",
                            line_number=line_number,
                            context=f"SUBTRACT operation in {para_name}: {source_var} subtracted from {target_var}"
                        ))
                
                # Extract COMPUTE statement relationships
                elif 'COMPUTE' in stmt_text.upper():
                    # Extract variables from COMPUTE expressions
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    if len(variables) >= 2:
                        # First variable is typically the result
                        result_var = variables[0]
                        for var in variables[1:]:
                            if len(var) > 2:  # Filter out short matches
                                relationships.append(RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=var,
                                    target_entity=result_var,
                                    relationship_type=RelationshipType.ARITHMETIC,
                                    confidence=0.8,
                                    relationship_strength="medium",
                                    line_number=line_number,
                                    context=f"COMPUTE operation in {para_name}: {var} used in computation for {result_var}"
                                ))
    except Exception as e:
        print(f"   ⚠️  Error extracting arithmetic relationships: {e}")
    
    return relationships


def _extract_conditional_relationships(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract conditional logic relationships (ProLeap format)"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        unit_statements = _get_statements_for_unit(file_data, cu_name)

        for stmt_info in unit_statements:
            if isinstance(stmt_info, dict):
                stmt_type = stmt_info.get('type', '')
                stmt_text = stmt_info.get('text', '')
                stmt_details = stmt_info.get('details', '')
                para_name = stmt_info.get('para_name', cu_name)
                line_number = stmt_info.get('start_line', 1)
                
                # Extract IF statement relationships
                if 'IF' in stmt_text.upper():
                    # Extract variables from IF conditions
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    for var in variables:
                        if len(var) > 2:  # Filter out short matches
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=var,
                                target_entity=para_name,
                                relationship_type=RelationshipType.CONDITIONAL,
                                confidence=0.85,
                                relationship_strength="medium",
                                line_number=line_number,
                                context=f"IF condition in {para_name}: {var} used in condition"
                            ))
                
                # Extract EVALUATE statement relationships
                elif 'EVALUATE' in stmt_text.upper():
                    # Extract variables from EVALUATE statements
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    for var in variables:
                        if len(var) > 2:  # Filter out short matches
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=var,
                                target_entity=para_name,
                                relationship_type=RelationshipType.CONDITIONAL,
                                confidence=0.85,
                                relationship_strength="medium",
                                line_number=line_number,
                                context=f"EVALUATE subject in {para_name}: {var} evaluated"
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
                
                # CONTAINS relationship already created in _extract_basic_relationships
                # Skip to avoid duplication
                pass
                
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
                
                # CONTAINS relationship already created in _extract_basic_relationships
                # Skip to avoid duplication
                pass
        
        # Extract file description relationships
        file_descriptions = file_data.get("file_descriptions", {})
        if cu_name in file_descriptions:
            for file_desc in file_descriptions[cu_name]:
                file_name = file_desc.get('name', '')
                
                # CONTAINS relationship already created in _extract_basic_relationships
                # Skip to avoid duplication
                pass
                
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
                # print(f"   🔍 DEBUG: Creating WRITTEN_BY relationship: {cu_name} -> {author}")
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
                                # print(f"   🔍 DEBUG: Added date_written property to paragraph {para_name}: {date_written}")
                
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
        # Get statements for this compilation unit
        statements = file_data.get("statements", {})
        
        # Process statements in the new format (keyed by statement ID)
        for stmt_id, stmt_data in statements.items():
            if not isinstance(stmt_data, dict):
                continue
                
            stmt_type = stmt_data.get('type', '')
            stmt_text = stmt_data.get('text', '')
            line_number = stmt_data.get('line', 1)
            unit = stmt_data.get('unit', cu_name)
            
            # Extract READ statement relationships
            if stmt_type == "READ":
                read_match = re.search(r'READ\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                if read_match:
                    file_name = read_match.group(1)
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=unit,
                        target_entity=file_name,
                        relationship_type=RelationshipType.READS,
                        confidence=0.95,
                        relationship_strength="strong",
                        line_number=line_number,
                        context=f"READ statement: {unit} reads {file_name}"
                    ))
            
            # Extract WRITE statement relationships
            elif stmt_type == "WRITE":
                write_match = re.search(r'WRITE\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                if write_match:
                    file_name = write_match.group(1)
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=unit,
                        target_entity=file_name,
                        relationship_type=RelationshipType.WRITES,
                        confidence=0.95,
                        relationship_strength="strong",
                        line_number=line_number,
                        context=f"WRITE statement: {unit} writes to {file_name}"
                    ))
            
            # Extract OPEN statement relationships
            elif stmt_type == "OPEN":
                open_match = re.search(r'OPEN\s+(?:INPUT|OUTPUT|I-O)\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                if open_match:
                    file_name = open_match.group(1)
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=unit,
                        target_entity=file_name,
                        relationship_type=RelationshipType.FILE_ACCESS,
                        confidence=0.95,
                        relationship_strength="strong",
                        line_number=line_number,
                        context=f"OPEN statement: {unit} opens {file_name}"
                    ))
            
            # Extract CLOSE statement relationships
            elif stmt_type == "CLOSE":
                close_match = re.search(r'CLOSE\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                if close_match:
                    file_name = close_match.group(1)
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=unit,
                        target_entity=file_name,
                        relationship_type=RelationshipType.FILE_ACCESS,
                        confidence=0.95,
                        relationship_strength="strong",
                        line_number=line_number,
                        context=f"CLOSE statement: {unit} closes {file_name}"
                    ))
    
    except Exception as e:
        print(f"   ⚠️  Error extracting file operations: {e}")
    
    return relationships


def _extract_variable_usage(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract variable usage relationships (USES, MODIFIES)"""
    relationships = []
    
    try:
        # Get statements for this compilation unit
        statements = file_data.get("statements", {})
        
        # Process statements in the new format (keyed by statement ID)
        for stmt_id, stmt_data in statements.items():
            if not isinstance(stmt_data, dict):
                continue
                
            stmt_type = stmt_data.get('type', '')
            stmt_text = stmt_data.get('text', '')
            line_number = stmt_data.get('line', 1)
            unit = stmt_data.get('unit', cu_name)
            
            # Extract variable usage from various statement types
            if stmt_type in ["IF", "EVALUATE", "COMPUTE", "ADD", "SUBTRACT"]:
                # Extract variables from conditions and expressions
                if stmt_type == "IF":
                    # Extract variables from IF conditions
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    for var in variables:
                        if len(var) > 2:  # Filter out short matches
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=unit,
                                target_entity=var,
                                relationship_type=RelationshipType.USES,
                                confidence=0.85,
                                relationship_strength="medium",
                                line_number=line_number,
                                context=f"Variable {var} used in IF condition"
                            ))
                        
                
                elif stmt_type == "COMPUTE":
                    # Extract variables from COMPUTE expressions
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    for var in variables:
                        if len(var) > 2:  # Filter out short matches
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=unit,
                                target_entity=var,
                                relationship_type=RelationshipType.USES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"Variable {var} used in COMPUTE expression"
                            ))
                
                elif stmt_type in ["ADD", "SUBTRACT"]:
                    # Extract variables from ADD/SUBTRACT operations
                    variables = re.findall(r'([A-Z0-9-]+)', stmt_text)
                    for var in variables:
                        if len(var) > 2:  # Filter out short matches
                            relationships.append(RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=unit,
                                target_entity=var,
                                relationship_type=RelationshipType.MODIFIES,
                                confidence=0.9,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"Variable {var} modified in {stmt_type} operation"
                            ))
            
            # Extract variable modifications from MOVE statements
            elif stmt_type == "MOVE":
                move_match = re.search(r'MOVE\s+([A-Z0-9-]+)\s+TO\s+([A-Z0-9-]+)', stmt_text, re.IGNORECASE)
                if move_match:
                    source_var = move_match.group(1)
                    target_var = move_match.group(2)
                    relationships.append(RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=unit,
                        target_entity=target_var,
                        relationship_type=RelationshipType.MODIFIES,
                        confidence=0.95,
                        relationship_strength="strong",
                        line_number=line_number,
                        context=f"Variable {target_var} modified by MOVE from {source_var}"
                    ))
    
    except Exception as e:
        print(f"   ⚠️  Error extracting variable usage: {e}")
    
    return relationships


def _extract_include_statements(file_data: Dict[str, Any], cu_name: str, file_path: str) -> List[RelationshipExtraction]:
    """Extract INCLUDE statement relationships"""
    relationships = []
    
    try:
        # Get statements for this compilation unit

        unit_statements = _get_statements_for_unit(file_data, cu_name)

        

        for stmt_info in unit_statements:
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
        # Get statements for this compilation unit

        unit_statements = _get_statements_for_unit(file_data, cu_name)

        

        for stmt_info in unit_statements:
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
        # Get statements for this compilation unit

        unit_statements = _get_statements_for_unit(file_data, cu_name)

        

        for stmt_info in unit_statements:
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
        # Get statements for this compilation unit

        unit_statements = _get_statements_for_unit(file_data, cu_name)

        

        for stmt_info in unit_statements:
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
        # Get statements for this compilation unit

        unit_statements = _get_statements_for_unit(file_data, cu_name)

        

        for stmt_info in unit_statements:
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
