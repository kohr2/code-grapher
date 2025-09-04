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
        
        return result
    
    def is_available(self) -> bool:
        """Check if COBOL parser is available"""
        return self.parser.is_available()
    
    def extract_relationships(self, cobol_data: Dict[str, Any]) -> List[Any]:
        """Extract COBOL relationships from parsed data"""
        return self.relationship_extractor.extract_relationships(cobol_data)
