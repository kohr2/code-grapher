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
        
        # Extract comments from source file for comment-enhanced descriptions
        file_path = cobol_data.get("file_path", "")
        comments = self._extract_comments_from_source(file_path) if file_path else {}
        
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
                
                # Add comments if available for this entity
                entity_name = entity.get('name', '')
                entity_type = entity.get('type', '')
                if entity_name in comments:
                    entity_props['comment'] = comments[entity_name]
                
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
                        
                        # Add comment if available for this paragraph
                        para_props = {
                            "unit": unit_name,
                            "context": f"Paragraph in {unit_name}",
                            "line": f"{start_line}-{end_line}",
                            "line_count": line_count,
                            "start_line": start_line,
                            "end_line": end_line
                        }
                        
                        if para_name in comments:
                            para_props['comment'] = comments[para_name]
                        
                        entities.append({
                            "type": "paragraph",
                            "name": para_name,
                            "properties": para_props
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
                        
                        # Add comment if available for this data item
                        item_props = {
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
                        
                        if item_name in comments:
                            item_props['comment'] = comments[item_name]
                        
                        entities.append({
                            "type": "data_item",
                            "name": item_name,
                            "properties": item_props
                        })
        
        return entities
    
    def _extract_comments_from_source(self, file_path: str) -> Dict[str, str]:
        """Extract comments from COBOL source code and associate them with entities"""
        comments = {}
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                lines = f.readlines()
            
            current_comment_lines = []
            last_entity = None
            
            for line_num, line in enumerate(lines, 1):
                # Remove line numbers (first 6 characters) and clean the line
                # Handle lines that might not have proper line numbers
                try:
                    clean_line = line[6:].strip() if len(line) > 6 else line.strip()
                except:
                    clean_line = line.strip()
                
                # Check for comment lines (starting with *)
                if clean_line.startswith('*') and len(clean_line) > 1:
                    comment_text = clean_line[1:].strip()
                    current_comment_lines.append(comment_text)
                    continue
                
                # Check for division/section headers
                if clean_line.upper().startswith('DIVISION'):
                    division_name = clean_line.upper().replace(' DIVISION', '')
                    if current_comment_lines:
                        comments[division_name] = ' '.join(current_comment_lines)
                        current_comment_lines = []
                    continue
                
                # Check for working storage section
                if clean_line.upper().startswith('WORKING-STORAGE SECTION'):
                    if current_comment_lines:
                        comments['WORKING-STORAGE'] = ' '.join(current_comment_lines)
                        current_comment_lines = []
                    continue
                
                # Check for paragraph names
                if (clean_line and clean_line[0].isalpha() and 
                    clean_line.endswith('.') and 
                    not clean_line.upper().startswith('PROGRAM-ID')):
                    para_name = clean_line.replace('.', '')
                    if current_comment_lines:
                        comments[para_name] = ' '.join(current_comment_lines)
                        current_comment_lines = []
                    continue
                
                # Check for data items (lines starting with level numbers)
                if clean_line and clean_line[0].isdigit():
                    parts = clean_line.split()
                    if len(parts) >= 2:
                        level = int(parts[0])
                        if level in [1, 77, 88]:  # Group level items
                            item_name = parts[1]
                            if current_comment_lines:
                                comments[item_name] = ' '.join(current_comment_lines)
                                current_comment_lines = []
                    continue
                
                # Clear comment buffer if we hit non-comment, non-empty line
                if clean_line:
                    current_comment_lines = []
        
        except Exception as e:
            print(f"Error extracting comments from {file_path}: {e}")
        
        return comments
    
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
