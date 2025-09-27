"""
COBOL Relationship Extractor

Advanced relationship extraction for COBOL files, focusing on:
- PERFORM relationships (paragraph calls)
- Data flow relationships (MOVE statements)
- File operations relationships
- Control flow relationships
- Data dependencies
"""

import re
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum

from ai_relationship_extractor import RelationshipExtraction, RelationshipType


class COBOLRelationshipType(Enum):
    """COBOL-specific relationship types"""
    PERFORM_CALLS = "PERFORM_CALLS"
    MOVE_DATA_FLOW = "MOVE_DATA_FLOW"
    FILE_OPERATION = "FILE_OPERATION"
    CONDITIONAL_FLOW = "CONDITIONAL_FLOW"
    DATA_DEPENDENCY = "DATA_DEPENDENCY"
    COPY_DEPENDENCY = "COPY_DEPENDENCY"
    CALL_SUBROUTINE = "CALL_SUBROUTINE"


@dataclass
class COBOLRelationship:
    """Represents a COBOL-specific relationship"""
    source_entity: str
    target_entity: str
    relationship_type: COBOLRelationshipType
    line_number: int
    confidence: float
    context: str
    additional_info: Optional[Dict[str, Any]] = None


class COBOLRelationshipExtractor:
    """Extracts relationships from COBOL entities"""

    def __init__(self):
        # Regex patterns for COBOL relationships
        self.patterns = {
            # PERFORM statements - paragraph calls
            'perform': re.compile(r'PERFORM\s+(\w+(?:-\w+)*)', re.IGNORECASE),
            'perform_times': re.compile(r'PERFORM\s+(\w+(?:-\w+)*)\s+(\d+)\s+TIMES', re.IGNORECASE),
            'perform_until': re.compile(r'PERFORM\s+(\w+(?:-\w+)*)\s+UNTIL\s+([^.]*)', re.IGNORECASE),
            'perform_varying': re.compile(r'PERFORM\s+(\w+(?:-\w+)*)\s+VARYING', re.IGNORECASE),
            
            # MOVE statements - data flow
            'move': re.compile(r'MOVE\s+([^.\s]+(?:[^.]*[^.\s])?)\s+TO\s+([^.\s]+(?:[^.]*[^.\s])?)', re.IGNORECASE),
            'move_corresponding': re.compile(r'MOVE\s+CORRESPONDING\s+([^.\s]+)\s+TO\s+([^.\s]+)', re.IGNORECASE),
            
            # File operations
            'read': re.compile(r'READ\s+(\w+(?:-\w+)*)', re.IGNORECASE),
            'write': re.compile(r'WRITE\s+(\w+(?:-\w+)*)', re.IGNORECASE),
            'open': re.compile(r'OPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+(\w+(?:-\w+)*)', re.IGNORECASE),
            'close': re.compile(r'CLOSE\s+(\w+(?:-\w+)*)', re.IGNORECASE),
            
            # CALL statements
            'call': re.compile(r'CALL\s+[\'"]?(\w+(?:-\w+)*)[\'"]?', re.IGNORECASE),
            
            # Arithmetic operations with data flow
            'add': re.compile(r'ADD\s+([^.\s]+)\s+TO\s+([^.\s]+)', re.IGNORECASE),
            'subtract': re.compile(r'SUBTRACT\s+([^.\s]+)\s+FROM\s+([^.\s]+)', re.IGNORECASE),
            'compute': re.compile(r'COMPUTE\s+([^.\s]+)\s*=\s*([^=]+)', re.IGNORECASE),
            
            # Conditional statements
            'if': re.compile(r'IF\s+([^.\s]+)', re.IGNORECASE),
            'when': re.compile(r'WHEN\s+([^.\s]+)', re.IGNORECASE),
            
            # String operations
            'string': re.compile(r'STRING\s+([^.]*)\s+INTO\s+([^.\s]+)', re.IGNORECASE),
            'unstring': re.compile(r'UNSTRING\s+([^.\s]+)\s+INTO\s+([^.]*)', re.IGNORECASE),
            
            # Inspect operations
            'inspect': re.compile(r'INSPECT\s+([^.\s]+)', re.IGNORECASE),
            
            # Initialize operations
            'initialize': re.compile(r'INITIALIZE\s+([^.\s]+)', re.IGNORECASE),
        }

    def extract_relationships(self, cobol_file_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract all relationships from a parsed COBOL file"""
        relationships = []
        entities = cobol_file_data.get("entities", [])
        file_path = cobol_file_data.get("file_path", "")
        
        # Build entity lookup by name
        entity_lookup = {}
        for entity in entities:
            entity_lookup[entity.get("name", "").upper()] = entity
        
        # Extract different types of relationships
        relationships.extend(self._extract_perform_relationships(entities, entity_lookup, file_path))
        relationships.extend(self._extract_data_flow_relationships(entities, entity_lookup, file_path))
        relationships.extend(self._extract_file_relationships(entities, entity_lookup, file_path))
        relationships.extend(self._extract_call_relationships(entities, entity_lookup, file_path))
        relationships.extend(self._extract_arithmetic_relationships(entities, entity_lookup, file_path))
        relationships.extend(self._extract_string_relationships(entities, entity_lookup, file_path))
        
        return relationships

    def _extract_perform_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract PERFORM relationships (paragraph calls)"""
        relationships = []
        
        for entity in entities:
            if entity.get("type") == "statement" and entity.get("context") == "PERFORM":
                content = entity.get("content", "")
                line_number = entity.get("line_number", 0)
                parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
                
                # Match PERFORM patterns
                for pattern_name, pattern in [
                    ("perform", self.patterns["perform"]),
                    ("perform_times", self.patterns["perform_times"]),
                    ("perform_until", self.patterns["perform_until"]),
                    ("perform_varying", self.patterns["perform_varying"])
                ]:
                    matches = pattern.finditer(content)
                    for match in matches:
                        target_paragraph = match.group(1)
                        
                        # Check if target paragraph exists
                        if target_paragraph.upper() in entity_lookup:
                            rel = RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=parent_paragraph,
                                target_entity=target_paragraph,
                                relationship_type=RelationshipType.CALLS,
                                confidence=1.0,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"PERFORM {target_paragraph} from {parent_paragraph} ({pattern_name})"
                            )
                            relationships.append(rel)
        
        return relationships

    def _extract_data_flow_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract data flow relationships from MOVE statements"""
        relationships = []
        
        for entity in entities:
            if entity.get("type") == "statement" and entity.get("context") == "MOVE":
                content = entity.get("content", "")
                line_number = entity.get("line_number", 0)
                parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
                
                # Match MOVE patterns
                for pattern_name, pattern in [
                    ("move", self.patterns["move"]),
                    ("move_corresponding", self.patterns["move_corresponding"])
                ]:
                    matches = pattern.finditer(content)
                    for match in matches:
                        source_data = match.group(1).strip()
                        target_data = match.group(2).strip()
                        
                        # Check if source and target are known data items
                        source_exists = source_data.upper() in entity_lookup or self._is_literal_or_special_value(source_data)
                        target_exists = target_data.upper() in entity_lookup
                        
                        if target_exists:  # At least target should exist
                            rel = RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=source_data,
                                target_entity=target_data,
                                relationship_type=RelationshipType.DATA_FLOW,
                                confidence=0.9 if source_exists else 0.7,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"MOVE from {source_data} to {target_data} in {parent_paragraph}"
                            )
                            relationships.append(rel)
        
        return relationships

    def _extract_file_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract file operation relationships"""
        relationships = []
        
        for entity in entities:
            content = entity.get("content", "")
            line_number = entity.get("line_number", 0)
            parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
            context = entity.get("context", "")
            
            # Match file operation patterns
            file_operations = [
                ("read", self.patterns["read"], RelationshipType.DATA_FLOW),
                ("write", self.patterns["write"], RelationshipType.DATA_FLOW),
                ("open", self.patterns["open"], RelationshipType.CONFIGURES),
                ("close", self.patterns["close"], RelationshipType.CONFIGURES)
            ]
            
            for op_name, pattern, rel_type in file_operations:
                if context == op_name.upper():
                    matches = pattern.finditer(content)
                    for match in matches:
                        if op_name == "open":
                            file_name = match.group(2)
                            operation_type = match.group(1)
                        else:
                            file_name = match.group(1)
                            operation_type = op_name
                        
                        # Check if file exists
                        if file_name.upper() in entity_lookup:
                            rel = RelationshipExtraction(
                                source_file=file_path,
                                target_file=file_path,
                                source_entity=parent_paragraph,
                                target_entity=file_name,
                                relationship_type=rel_type,
                                confidence=1.0,
                                relationship_strength="strong",
                                line_number=line_number,
                                context=f"{operation_type.upper()} operation on {file_name} in {parent_paragraph}"
                            )
                            relationships.append(rel)
        
        return relationships

    def _extract_call_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract CALL relationships (subroutine calls)"""
        relationships = []
        
        for entity in entities:
            if entity.get("type") == "statement" and entity.get("context") == "CALL":
                content = entity.get("content", "")
                line_number = entity.get("line_number", 0)
                parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
                
                matches = self.patterns["call"].finditer(content)
                for match in matches:
                    subroutine_name = match.group(1)
                    
                    rel = RelationshipExtraction(
                        source_file=file_path,
                        target_file=file_path,
                        source_entity=parent_paragraph,
                        target_entity=subroutine_name,
                        relationship_type=RelationshipType.CALLS,
                        confidence=0.8,  # Lower confidence as it might be external
                        relationship_strength="medium",
                        line_number=line_number,
                        context=f"CALL {subroutine_name} from {parent_paragraph}"
                    )
                    relationships.append(rel)
        
        return relationships

    def _extract_arithmetic_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract arithmetic operation relationships"""
        relationships = []
        
        for entity in entities:
            content = entity.get("content", "")
            line_number = entity.get("line_number", 0)
            parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
            context = entity.get("context", "")
            
            # Match arithmetic patterns
            arithmetic_operations = [
                ("add", self.patterns["add"]),
                ("subtract", self.patterns["subtract"]),
                ("compute", self.patterns["compute"])
            ]
            
            for op_name, pattern in arithmetic_operations:
                if context == op_name.upper():
                    matches = pattern.finditer(content)
                    for match in matches:
                        if op_name == "compute":
                            target = match.group(1).strip()
                            source_expression = match.group(2).strip()
                            
                            # Extract variables from expression
                            variables = self._extract_variables_from_expression(source_expression)
                            for var in variables:
                                if var.upper() in entity_lookup:
                                    rel = RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=var,
                                        target_entity=target,
                                        relationship_type=RelationshipType.DATA_FLOW,
                                        confidence=0.8,
                                        relationship_strength="medium",
                                        line_number=line_number,
                                        context=f"COMPUTE {target} using {var} in {parent_paragraph}"
                                    )
                                    relationships.append(rel)
                        else:
                            source = match.group(1).strip()
                            target = match.group(2).strip()
                            
                            if target.upper() in entity_lookup:
                                rel = RelationshipExtraction(
                                    source_file=file_path,
                                    target_file=file_path,
                                    source_entity=source,
                                    target_entity=target,
                                    relationship_type=RelationshipType.DATA_FLOW,
                                    confidence=0.9,
                                    relationship_strength="strong",
                                    line_number=line_number,
                                    context=f"{op_name.upper()} {source} to {target} in {parent_paragraph}"
                                )
                                relationships.append(rel)
        
        return relationships

    def _extract_string_relationships(self, entities: List[Dict], entity_lookup: Dict, file_path: str) -> List[RelationshipExtraction]:
        """Extract string operation relationships"""
        relationships = []
        
        for entity in entities:
            content = entity.get("content", "")
            line_number = entity.get("line_number", 0)
            parent_paragraph = entity.get("parent_paragraph", "UNKNOWN")
            context = entity.get("context", "")
            
            # Match string patterns
            string_operations = [
                ("string", self.patterns["string"]),
                ("unstring", self.patterns["unstring"])
            ]
            
            for op_name, pattern in string_operations:
                if context == op_name.upper():
                    matches = pattern.finditer(content)
                    for match in matches:
                        if op_name == "string":
                            source_expression = match.group(1).strip()
                            target = match.group(2).strip()
                            
                            # Extract variables from source expression
                            variables = self._extract_variables_from_expression(source_expression)
                            for var in variables:
                                if var.upper() in entity_lookup and target.upper() in entity_lookup:
                                    rel = RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=var,
                                        target_entity=target,
                                        relationship_type=RelationshipType.DATA_FLOW,
                                        confidence=0.8,
                                        relationship_strength="medium",
                                        line_number=line_number,
                                        context=f"STRING {var} into {target} in {parent_paragraph}"
                                    )
                                    relationships.append(rel)
                        else:  # unstring
                            source = match.group(1).strip()
                            target_expression = match.group(2).strip()
                            
                            # Extract variables from target expression
                            variables = self._extract_variables_from_expression(target_expression)
                            for var in variables:
                                if source.upper() in entity_lookup and var.upper() in entity_lookup:
                                    rel = RelationshipExtraction(
                                        source_file=file_path,
                                        target_file=file_path,
                                        source_entity=source,
                                        target_entity=var,
                                        relationship_type=RelationshipType.DATA_FLOW,
                                        confidence=0.8,
                                        relationship_strength="medium",
                                        line_number=line_number,
                                        context=f"UNSTRING {source} into {var} in {parent_paragraph}"
                                    )
                                    relationships.append(rel)
        
        return relationships

    def _is_literal_or_special_value(self, value: str) -> bool:
        """Check if a value is a literal or special COBOL value"""
        value = value.strip().upper()
        
        # Common COBOL literals and special values
        special_values = {
            'ZERO', 'ZEROS', 'ZEROES', 'SPACE', 'SPACES', 'HIGH-VALUES', 
            'LOW-VALUES', 'QUOTES', 'ALL', 'NULL', 'NULLS'
        }
        
        # Check if it's a quoted string
        if value.startswith('"') and value.endswith('"'):
            return True
        if value.startswith("'") and value.endswith("'"):
            return True
            
        # Check if it's a numeric literal
        if value.isdigit():
            return True
            
        # Check if it's a special value
        if value in special_values:
            return True
            
        return False

    def _extract_variables_from_expression(self, expression: str) -> List[str]:
        """Extract variable names from a COBOL expression"""
        # Simple variable extraction - look for alphanumeric words
        # This is a basic implementation and could be enhanced
        variables = []
        
        # Split on common operators and delimiters
        import re
        tokens = re.split(r'[\s+\-*/=()<>]', expression)
        
        for token in tokens:
            token = token.strip()
            if token and not self._is_literal_or_special_value(token):
                # Check if it looks like a variable name (alphanumeric with possible hyphens)
                if re.match(r'^[A-Z0-9-]+$', token.upper()):
                    variables.append(token)
        
        return variables


def extract_cobol_relationships(cobol_file_data: Dict[str, Any]) -> List[RelationshipExtraction]:
    """Extract relationships from a parsed COBOL file"""
    extractor = COBOLRelationshipExtractor()
    return extractor.extract_relationships(cobol_file_data)


# Example usage and testing
if __name__ == "__main__":
    # Test the relationship extractor
    extractor = COBOLRelationshipExtractor()
    
    # Sample COBOL file data
    sample_data = {
        "file_path": "/tmp/sample.cbl",
        "entities": [
            {
                "type": "paragraph",
                "name": "1000-MAIN-PROCESSING",
                "line_number": 240,
                "parent_section": "PROCEDURE-DIVISION"
            },
            {
                "type": "paragraph", 
                "name": "2000-INITIALIZE",
                "line_number": 300
            },
            {
                "type": "statement",
                "name": "PERFORM_STATEMENT",
                "line_number": 250,
                "context": "PERFORM",
                "content": "PERFORM 2000-INITIALIZE",
                "parent_paragraph": "1000-MAIN-PROCESSING"
            },
            {
                "type": "statement",
                "name": "MOVE_STATEMENT",
                "line_number": 310,
                "context": "MOVE",
                "content": "MOVE ZERO TO WS-COUNTER",
                "parent_paragraph": "2000-INITIALIZE"
            },
            {
                "type": "data_item",
                "name": "WS-COUNTER",
                "line_number": 200,
                "level_number": 1
            }
        ]
    }
    
    # Extract relationships
    relationships = extractor.extract_relationships(sample_data)
    
    print("COBOL Relationship Extraction Test Results:")
    print(f"Total relationships found: {len(relationships)}")
    
    for rel in relationships:
        print(f"- {rel.source_entity} -> {rel.target_entity} ({rel.relationship_type.value}) - {rel.context}")