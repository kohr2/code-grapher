"""
COBOL Parser Service using Real ProLeap ANTLR4-based parser
Integrates with existing multi-language parser architecture
"""

import os
import json
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
from dataclasses import dataclass

# Import raw ProLeap parser
from .raw_proleap_parser import RawProLeapParser
from cobol_relationship_extractor import extract_cobol_relationships


@dataclass
class COBOLDataItem:
    """Represents a COBOL data item"""
    name: str
    level: int
    data_type: str
    picture: Optional[str]
    value: Optional[str]
    line_number: int
    context: str


@dataclass
class COBOLParagraph:
    """Represents a COBOL paragraph"""
    name: str
    line_number: int
    statements: List[str]
    context: str


@dataclass
class COBOLDivision:
    """Represents a COBOL division"""
    name: str
    sections: Dict[str, Any]
    paragraphs: List[COBOLParagraph]
    data_items: List[COBOLDataItem]


class COBOLParser:
    """COBOL parser using raw ProLeap parser"""
    
    def __init__(self):
        self.parser = RawProLeapParser()
    
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and return AST/ASG data"""
        result = self.parser.parse_file(file_path)
        
        # Extract entities and relationships
        if result.get("parse_success", False):
            # Extract entities from parsed data first
            entities = self._extract_entities(result)
            result["entities"] = entities
            
            # Extract advanced COBOL relationships (now that entities are available)
            relationships = extract_cobol_relationships(result)
            result["relationships"] = relationships
            result["relationship_count"] = len(relationships)
        
        return result
    
    def _extract_entities(self, cobol_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from parsed COBOL data"""
        entities = []
        
        # Add existing entities (program, compilation_unit) with line information
        for entity in cobol_data.get("entities", []):
            if isinstance(entity, dict):
                start_line = entity.get('start_line', 0)
                end_line = entity.get('end_line', start_line)
                line_count = entity.get('line_count', end_line - start_line + 1)
                
                # Add line information to entity properties
                entity_props = entity.get('properties', {})
                entity_props.update({
                    'line': f"{start_line}-{end_line}",
                    'line_count': line_count,
                    'start_line': start_line,
                    'end_line': end_line
                })
                entity['properties'] = entity_props
                
            entities.append(entity)
        
        # Extract paragraph entities
        paragraphs = cobol_data.get("paragraphs", {})
        for unit_name, para_list in paragraphs.items():
            for para_data in para_list:
                if isinstance(para_data, dict):
                    para_name = para_data.get('name', '')
                    if para_name:
                        start_line = para_data.get('start_line', 0)
                        end_line = para_data.get('end_line', start_line)
                        line_count = para_data.get('line_count', end_line - start_line + 1)
                        
                        entities.append({
                            "type": "paragraph",
                            "name": para_name,
                            "properties": {
                                "unit": unit_name,
                                "context": f"Paragraph in {unit_name}",
                                "line": f"{start_line}-{end_line}",
                                "line_count": line_count,
                                "start_line": start_line,
                                "end_line": end_line
                            }
                        })
        
        # Extract data item entities from source code (since ProLeap parser doesn't extract them yet)
        file_path = cobol_data.get("file_path", "")
        if file_path and os.path.exists(file_path):
            data_items = self._extract_data_items_from_source(file_path)
            for item in data_items:
                line_number = item["line_number"]
                entities.append({
                    "type": "data_item",
                    "name": item["name"],
                    "properties": {
                        "unit": cobol_data.get("compilation_units", [{}])[0].get("name", "UNKNOWN"),
                        "level": item["level"],
                        "data_type": item["data_type"],
                        "picture": item["picture"],
                        "context": f"Data item in {item['unit']}",
                        "line": f"{line_number}-{line_number}",
                        "line_count": 1,
                        "start_line": line_number,
                        "end_line": line_number
                    }
                })
        
        # Extract data item entities from parsed data (if available)
        data_items = cobol_data.get("data_items", {})
        for unit_name, item_list in data_items.items():
            for item_data in item_list:
                if isinstance(item_data, dict):
                    item_name = item_data.get('name', '')
                    if item_name:
                        line_number = item_data.get('line_number', 0)
                        entities.append({
                            "type": "data_item",
                            "name": item_name,
                            "properties": {
                                "unit": unit_name,
                                "level": item_data.get('level', 0),
                                "data_type": item_data.get('data_type', 'unknown'),
                                "picture": item_data.get('picture', ''),
                                "context": f"Data item in {unit_name}",
                                "line": f"{line_number}-{line_number}",
                                "line_count": 1,
                                "start_line": line_number,
                                "end_line": line_number
                            }
                        })
        
        return entities
    
    def _extract_data_items_from_source(self, file_path: str) -> List[Dict[str, Any]]:
        """Extract data items from COBOL source code"""
        data_items = []
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            in_data_division = False
            current_unit = "UNKNOWN"
            
            for line_num, line in enumerate(lines, 1):
                # Remove line numbers (first 6 characters) and clean the line
                clean_line = line[6:].strip() if len(line) > 6 else line.strip()
                
                # Check if we're in DATA DIVISION
                if clean_line.upper().startswith('DATA DIVISION'):
                    in_data_division = True
                    continue
                elif clean_line.upper().startswith('PROCEDURE DIVISION'):
                    in_data_division = False
                    continue
                
                if not in_data_division:
                    continue
                
                # Skip empty lines and comments
                if not clean_line or clean_line.startswith('*') or clean_line.startswith('.'):
                    continue
                
                # Extract data items (lines starting with level numbers)
                if clean_line and clean_line[0].isdigit():
                    parts = clean_line.split()
                    if len(parts) >= 2:
                        level = int(parts[0])
                        name = parts[1]
                        
                        # Skip special level numbers
                        if level in [66, 77, 88]:
                            continue
                        
                        # Extract PIC clause
                        picture = ""
                        data_type = "unknown"
                        for i, part in enumerate(parts):
                            if part.upper() == 'PIC':
                                if i + 1 < len(parts):
                                    picture = parts[i + 1]
                                    # Determine data type from picture
                                    if 'X' in picture:
                                        data_type = "alphanumeric"
                                    elif '9' in picture:
                                        data_type = "numeric"
                                    elif 'A' in picture:
                                        data_type = "alphabetic"
                                    elif 'S' in picture:
                                        data_type = "signed_numeric"
                                    break
                        
                        data_items.append({
                            "name": name,
                            "level": level,
                            "data_type": data_type,
                            "picture": picture,
                            "line_number": line_num,
                            "unit": current_unit
                        })
        
        except Exception as e:
            print(f"Error extracting data items from {file_path}: {e}")
        
        return data_items
    
    def is_available(self) -> bool:
        """Check if COBOL parser is available"""
        return self.parser.is_available()
    
    def extract_relationships(self, cobol_data: Dict[str, Any]) -> List[Any]:
        """Extract COBOL relationships from parsed data"""
        return extract_cobol_relationships(cobol_data)
