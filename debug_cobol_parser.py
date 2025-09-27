#!/usr/bin/env python3
"""
Debug version of COBOL parser to see what's being detected
"""

import re
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from enum import Enum


class COBOLEntityType(Enum):
    PROGRAM = "program"
    DIVISION = "division"
    SECTION = "section"
    PARAGRAPH = "paragraph"
    STATEMENT = "statement"
    DATA_ITEM = "data_item"
    FILE = "file"


@dataclass
class COBOLEntity:
    type: COBOLEntityType
    name: str
    line_number: int
    parent: Optional[str] = None
    children: List[str] = None
    metadata: Dict[str, Any] = None

    def __post_init__(self):
        if self.children is None:
            self.children = []
        if self.metadata is None:
            self.metadata = {}


class DebugCOBOLParser:
    """Debug version of COBOL parser"""

    def __init__(self):
        self.entities: List[COBOLEntity] = []
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None

    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and extract all entities"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            lines = content.splitlines()
            self.entities = []
            self.current_division = None
            self.current_section = None
            self.current_paragraph = None
            
            # Extract all entities
            self._extract_program(lines)
            self._extract_divisions(lines)
            self._extract_sections(lines)
            self._extract_paragraphs(lines)
            
            # Count entities by type
            entity_counts = self._count_entities_by_type()
            
            return {
                'file_path': file_path,
                'entities': [self._entity_to_dict(e) for e in self.entities],
                'entity_counts': entity_counts,
                'parse_success': True,
                'error': None
            }
            
        except Exception as e:
            return {
                'file_path': file_path,
                'entities': [],
                'entity_counts': {},
                'parse_success': False,
                'error': str(e)
            }

    def _extract_program(self, lines: List[str]):
        """Extract PROGRAM-ID"""
        for line_num, line in enumerate(lines, 1):
            match = re.match(r'^\s*\d+\s+PROGRAM-ID\.\s+(\w+)', line, re.IGNORECASE)
            if match:
                program_name = match.group(1)
                entity = COBOLEntity(
                    type=COBOLEntityType.PROGRAM,
                    name=program_name,
                    line_number=line_num,
                    metadata={'cobol_type': 'PROGRAM-ID'}
                )
                self.entities.append(entity)
                print(f"Found PROGRAM: {program_name} at line {line_num}")
                break

    def _extract_divisions(self, lines: List[str]):
        """Extract all divisions"""
        for line_num, line in enumerate(lines, 1):
            match = re.match(r'^\s*\d+\s+(\w+)\s+DIVISION\.', line, re.IGNORECASE)
            if match:
                division_name = match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.DIVISION,
                    name=division_name,
                    line_number=line_num,
                    metadata={'cobol_type': 'DIVISION'}
                )
                self.entities.append(entity)
                self.current_division = division_name
                print(f"Found DIVISION: {division_name} at line {line_num}")

    def _extract_sections(self, lines: List[str]):
        """Extract all sections"""
        for line_num, line in enumerate(lines, 1):
            # More precise section detection - must end with "SECTION."
            section_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', line, re.IGNORECASE)
            if section_match:
                section_name = section_match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.SECTION,
                    name=section_name,
                    line_number=line_num,
                    parent=self.current_division,
                    metadata={'cobol_type': 'SECTION'}
                )
                self.entities.append(entity)
                self.current_section = section_name
                print(f"Found SECTION: {section_name} at line {line_num}")

    def _extract_paragraphs(self, lines: List[str]):
        """Extract all paragraphs"""
        paragraph_count = 0
        for line_num, line in enumerate(lines, 1):
            # Skip if it's a section
            section_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', line, re.IGNORECASE)
            if section_match:
                continue
                
            # Skip if it's a division
            division_match = re.match(r'^\s*\d+\s+(\w+)\s+DIVISION\.', line, re.IGNORECASE)
            if division_match:
                continue
                
            # Skip if it's a program-id
            program_match = re.match(r'^\s*\d+\s+PROGRAM-ID\.\s+(\w+)', line, re.IGNORECASE)
            if program_match:
                continue
                
            # Enhanced paragraph detection - look for any line that starts with a number
            # followed by a paragraph name and ends with a period (but not SECTION or DIVISION)
            paragraph_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s*\.', line)
            if paragraph_match:
                paragraph_name = paragraph_match.group(1).upper()
                # Additional check to ensure it's not a section or division
                if not paragraph_name.endswith('SECTION') and not paragraph_name.endswith('DIVISION'):
                    entity = COBOLEntity(
                        type=COBOLEntityType.PARAGRAPH,
                        name=paragraph_name,
                        line_number=line_num,
                        parent=self.current_section or self.current_division,
                        metadata={'cobol_type': 'PARAGRAPH'}
                    )
                    self.entities.append(entity)
                    self.current_paragraph = paragraph_name
                    paragraph_count += 1
                    if paragraph_count <= 10:  # Print first 10 paragraphs
                        print(f"Found PARAGRAPH: {paragraph_name} at line {line_num}")

    def _count_entities_by_type(self) -> Dict[str, int]:
        """Count entities by type"""
        counts = {}
        for entity in self.entities:
            entity_type = entity.type.value
            counts[entity_type] = counts.get(entity_type, 0) + 1
        return counts

    def _entity_to_dict(self, entity: COBOLEntity) -> Dict[str, Any]:
        """Convert COBOLEntity to dictionary"""
        return {
            'type': entity.type.value,
            'name': entity.name,
            'line_number': entity.line_number,
            'parent': entity.parent,
            'children': entity.children,
            'metadata': entity.metadata
        }


if __name__ == "__main__":
    import sys
    if len(sys.argv) > 1:
        parser = DebugCOBOLParser()
        result = parser.parse_file(sys.argv[1])
        print(f"\nParse success: {result['parse_success']}")
        print(f"Entity counts: {result['entity_counts']}")
        if not result['parse_success']:
            print(f"Error: {result['error']}")
    else:
        print("Usage: python debug_cobol_parser.py <cobol_file>")