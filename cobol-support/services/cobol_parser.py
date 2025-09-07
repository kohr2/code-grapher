"""
COBOL Parser Service using Real ProLeap ANTLR4-based parser
Integrates with existing multi-language parser architecture
"""

import os
import json
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
from dataclasses import dataclass

# Import real ProLeap parser
from .real_proleap_parser import RealProLeapParser
from .cobol_relationship_extractor import COBOLRelationshipExtractor


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
    """COBOL parser using real ProLeap parser"""
    
    def __init__(self):
        self.parser = RealProLeapParser()
        self.relationship_extractor = COBOLRelationshipExtractor()
    
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and return AST/ASG data"""
        result = self.parser.parse_file(file_path)
        
        # Extract advanced COBOL relationships
        if result.get("parse_success", False):
            relationships = self.relationship_extractor.extract_relationships(result)
            result["relationships"] = relationships
            result["relationship_count"] = len(relationships)
            
            # Extract entities from parsed data
            entities = self._extract_entities(result)
            result["entities"] = entities
        
        return result
    
    def _extract_entities(self, cobol_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from parsed COBOL data"""
        entities = []
        
        # Add existing entities (program, compilation_unit)
        entities.extend(cobol_data.get("entities", []))
        
        # Extract paragraph entities
        paragraphs = cobol_data.get("paragraphs", {})
        for unit_name, para_list in paragraphs.items():
            for para_data in para_list:
                if isinstance(para_data, dict):
                    para_name = para_data.get('name', '')
                    if para_name:
                        entities.append({
                            "type": "paragraph",
                            "name": para_name,
                            "properties": {
                                "unit": unit_name,
                                "context": f"Paragraph in {unit_name}",
                                "line": 0  # Line numbers not available from parser
                            }
                        })
        
        # Extract data item entities (if available)
        data_items = cobol_data.get("data_items", {})
        for unit_name, item_list in data_items.items():
            for item_data in item_list:
                if isinstance(item_data, dict):
                    item_name = item_data.get('name', '')
                    if item_name:
                        entities.append({
                            "type": "data_item",
                            "name": item_name,
                            "properties": {
                                "unit": unit_name,
                                "level": item_data.get('level', 0),
                                "data_type": item_data.get('data_type', 'unknown'),
                                "picture": item_data.get('picture', ''),
                                "context": f"Data item in {unit_name}",
                                "line": item_data.get('line_number', 0)
                            }
                        })
        
        return entities
    
    def is_available(self) -> bool:
        """Check if COBOL parser is available"""
        return self.parser.is_available()
    
    def extract_relationships(self, cobol_data: Dict[str, Any]) -> List[Any]:
        """Extract COBOL relationships from parsed data"""
        return self.relationship_extractor.extract_relationships(cobol_data)
