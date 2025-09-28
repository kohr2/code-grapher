#!/usr/bin/env python3
"""
Enhanced COBOL Parser

This parser addresses the specific issues identified in the COBOL analysis:
- Missing Divisions as separate entities (should find 4 divisions)
- Missing Sections as separate entities (should find 16 sections)  
- Missing Paragraphs (should find 261, not 103)
- Missing Statements (should find 316 statements)
- Proper hierarchical relationships

Key Improvements:
1. Enhanced regex patterns for all entity types
2. Better validation logic to avoid false positives
3. Hierarchical parent-child relationships
4. Statement-level extraction
5. Comprehensive entity counting
"""

import re
import logging
from typing import Dict, List, Tuple, Set, Optional
from dataclasses import dataclass, field
from enum import Enum
import json

logger = logging.getLogger(__name__)

class COBOLEntityType(Enum):
    PROGRAM = "PROGRAM"
    DIVISION = "DIVISION"
    SECTION = "SECTION"
    PARAGRAPH = "PARAGRAPH"
    STATEMENT = "STATEMENT"
    DATA_ITEM = "DATA_ITEM"
    FILE = "FILE"

@dataclass
class COBOLEntity:
    name: str
    entity_type: COBOLEntityType
    line_number: int
    content: str
    parent: Optional[str] = None
    children: List[str] = field(default_factory=list)
    level: int = 0  # For hierarchical structure

class EnhancedCOBOLParser:
    """
    Enhanced COBOL parser that addresses parsing discrepancies.
    """
    
    def __init__(self):
        # Enhanced regex patterns with better accuracy (line numbers already removed)
        self.patterns = {
            'program': [
                r'^PROGRAM-ID\.\s+([A-Z0-9-]+)',
            ],
            'division': [
                r'^(IDENTIFICATION\s+DIVISION)\.',
                r'^(ENVIRONMENT\s+DIVISION)\.',
                r'^(DATA\s+DIVISION)\.',
                r'^(PROCEDURE\s+DIVISION)\.',
            ],
            'section': [
                r'^([A-Z0-9-]+)\s+SECTION\.\s*$',
                r'^([A-Z0-9-]+)\s+SECTION\s+(\d+)\.',
            ],
            'paragraph': [
                # Paragraphs that end with period and are standalone
                r'^([A-Z0-9-]+)\.\s*$',
                # Paragraphs with period followed by whitespace and content
                r'^([A-Z0-9-]+)\.\s+[^A-Z]',
            ],
            'statement': [
                # Display statements
                r'^(DISPLAY\s+)',
                # Move statements
                r'^(MOVE\s+)',
                # If statements
                r'^(IF\s+)',
                # Perform statements
                r'^(PERFORM\s+)',
                # Read statements
                r'^(READ\s+)',
                # Write statements
                r'^(WRITE\s+)',
                # Compute statements
                r'^(COMPUTE\s+)',
                # Arithmetic statements
                r'^(ADD\s+)',
                r'^(SUBTRACT\s+)',
                r'^(MULTIPLY\s+)',
                r'^(DIVIDE\s+)',
                # I/O statements
                r'^(ACCEPT\s+)',
                r'^(CALL\s+)',
                # Control statements
                r'^(GO\s+TO\s+)',
                r'^(STOP\s+)',
                r'^(EXIT\s+)',
                # Loop statements
                r'^(UNTIL\s+)',
                r'^(WHILE\s+)',
                # String operations
                r'^(STRING\s+)',
                r'^(UNSTRING\s+)',
                # Date/time operations
                r'^(INSPECT\s+)',
                # File operations
                r'^(OPEN\s+)',
                r'^(CLOSE\s+)',
                r'^(DELETE\s+)',
                r'^(REWRITE\s+)',
                r'^(START\s+)',
            ],
            'data_item': [
                r'^(\d+)\s+([A-Z0-9-]+)',
                r'^(\d+)\s+([A-Z0-9-]+)\s+([A-Z0-9-]+)',
            ],
            'file': [
                r'^FD\s+([A-Z0-9-]+)',
                r'^SELECT\s+([A-Z0-9-]+)',
            ]
        }
        
        # Reserved words that should not be treated as paragraphs
        self.reserved_words = {
            'PROGRAM-ID', 'IDENTIFICATION', 'DIVISION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'WORKING-STORAGE', 'LINKAGE', 'FILE', 'SECTION', 'FD', 'SELECT', 'ASSIGN',
            'FILE-CONTROL', 'INPUT-OUTPUT', 'PIC', 'PICTURE', 'VALUE', 'OCCURS',
            'REDEFINES', 'USAGE', 'COMP', 'COMPUTATIONAL', 'INDEXED', 'KEY',
            'LEVEL', 'GROUP', 'ITEM', 'RECORD', 'LABEL', 'BLOCK', 'RECORDING',
            'MODE', 'CODE-SET', 'END', 'OF', 'FILE', 'REPORT', 'REPORTING',
            'REPORTS', 'REPORTING', 'REPORTS', 'REPORTING', 'REPORTS'
        }
        
        self.entities: List[COBOLEntity] = []
        self.hierarchy = {}

    def parse_file(self, file_path: str) -> Dict[str, any]:
        """
        Parse a COBOL file and return comprehensive entity information.
        """
        logger.info(f"Parsing COBOL file: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {e}")
            return {}
        
        # Reset state
        self.entities = []
        self.hierarchy = {}
        
        # Parse the file
        self._parse_content(content)
        
        # Build hierarchy
        self._build_hierarchy()
        
        # Count entities
        counts = self._count_entities()
        
        return {
            'entities': self.entities,
            'counts': counts,
            'hierarchy': self.hierarchy,
            'summary': self._generate_summary(counts)
        }

    def _parse_content(self, content: str) -> None:
        """Parse COBOL content and extract all entities."""
        lines = content.split('\n')
        current_division = None
        current_section = None
        current_paragraph = None
        
        for line_num, line in enumerate(lines, 1):
            if not line.strip():
                continue
            
            # Extract line number and content
            line_number = line[:6].strip() if len(line) >= 6 else ""
            code_content = line[6:].strip() if len(line) >= 6 else line
            
            # Extract entities in order of specificity
            # First check for programs
            program = self._extract_program(code_content, line_num)
            if program:
                continue
            
            # Then check for divisions
            division = self._extract_division(code_content, line_num)
            if division:
                current_division = division.name
                continue
            
            # Then check for sections
            section = self._extract_section(code_content, line_num, current_division)
            if section:
                current_section = section.name
                continue
            
            # Then check for paragraphs
            paragraph = self._extract_paragraph(code_content, line_num, current_division, current_section)
            if paragraph:
                current_paragraph = paragraph.name
                continue
            
            # Extract statements (within paragraphs)
            self._extract_statements(code_content, line_num, current_division, current_section, current_paragraph)
            
            # Extract data items and files
            self._extract_data_items(code_content, line_num, current_division)
            self._extract_files(code_content, line_num, current_division)

    def _extract_program(self, content: str, line_num: int) -> Optional[COBOLEntity]:
        """Extract program entities."""
        for pattern in self.patterns['program']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.PROGRAM,
                    line_number=line_num,
                    content=content,
                    level=0
                )
                self.entities.append(entity)
                return entity
        return None

    def _extract_division(self, content: str, line_num: int) -> Optional[COBOLEntity]:
        """Extract division entities."""
        for pattern in self.patterns['division']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.DIVISION,
                    line_number=line_num,
                    content=content,
                    level=1
                )
                self.entities.append(entity)
                return entity
        return None

    def _extract_section(self, content: str, line_num: int, parent_division: str) -> Optional[COBOLEntity]:
        """Extract section entities."""
        for pattern in self.patterns['section']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.SECTION,
                    line_number=line_num,
                    content=content,
                    parent=parent_division,
                    level=2
                )
                self.entities.append(entity)
                return entity
        return None

    def _extract_paragraph(self, content: str, line_num: int, parent_division: str, parent_section: str) -> Optional[COBOLEntity]:
        """Extract paragraph entities with enhanced validation."""
        # Skip if it's already identified as a division or section
        if any(re.search(pattern, content, re.IGNORECASE) for pattern in 
               self.patterns['division'] + self.patterns['section']):
            return None
        
        # Skip if it's a statement
        if any(re.search(pattern, content, re.IGNORECASE) for pattern in self.patterns['statement']):
            return None
        
        for pattern in self.patterns['paragraph']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                
                # Enhanced validation
                if self._is_valid_paragraph(name, content):
                    entity = COBOLEntity(
                        name=name,
                        entity_type=COBOLEntityType.PARAGRAPH,
                        line_number=line_num,
                        content=content,
                        parent=parent_section or parent_division,
                        level=3
                    )
                    self.entities.append(entity)
                    return entity
        return None

    def _is_valid_paragraph(self, name: str, content: str) -> bool:
        """Enhanced paragraph validation."""
        # Check against reserved words
        if name.upper() in self.reserved_words:
            return False
        
        # Paragraphs should not have multiple periods in the definition line
        if content.count('.') > 1:
            return False
        
        # Skip if it contains COBOL keywords that indicate it's not a paragraph
        non_paragraph_keywords = [
            'PIC', 'PICTURE', 'VALUE', 'OCCURS', 'REDEFINES', 'USAGE',
            'FD', 'SELECT', 'ASSIGN', 'FILE-CONTROL', 'INPUT-OUTPUT'
        ]
        
        for keyword in non_paragraph_keywords:
            if keyword in content.upper():
                return False
        
        # Paragraph names should be reasonable length (not too long)
        if len(name) > 30:
            return False
        
        return True

    def _extract_statements(self, content: str, line_num: int, parent_division: str, parent_section: str, parent_paragraph: str) -> None:
        """Extract statement entities."""
        for pattern in self.patterns['statement']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                statement_type = match.group(1).strip().upper()
                entity = COBOLEntity(
                    name=f"{statement_type}_{line_num}",
                    entity_type=COBOLEntityType.STATEMENT,
                    line_number=line_num,
                    content=content,
                    parent=parent_paragraph or parent_section or parent_division,
                    level=4
                )
                self.entities.append(entity)
                break

    def _extract_data_items(self, content: str, line_num: int, parent_division: str) -> None:
        """Extract data item entities."""
        for pattern in self.patterns['data_item']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(2) if len(match.groups()) > 1 else match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.DATA_ITEM,
                    line_number=line_num,
                    content=content,
                    parent=parent_division,
                    level=3
                )
                self.entities.append(entity)
                break

    def _extract_files(self, content: str, line_num: int, parent_division: str) -> None:
        """Extract file entities."""
        for pattern in self.patterns['file']:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.FILE,
                    line_number=line_num,
                    content=content,
                    parent=parent_division,
                    level=3
                )
                self.entities.append(entity)
                break

    def _build_hierarchy(self) -> None:
        """Build hierarchical relationships between entities."""
        for entity in self.entities:
            if entity.parent:
                if entity.parent not in self.hierarchy:
                    self.hierarchy[entity.parent] = []
                self.hierarchy[entity.parent].append(entity.name)

    def _count_entities(self) -> Dict[str, int]:
        """Count entities by type."""
        counts = {}
        for entity in self.entities:
            entity_type = entity.entity_type.value.lower()
            if entity_type.endswith('s'):
                key = entity_type
            else:
                key = f"{entity_type}s"
            counts[key] = counts.get(key, 0) + 1
        
        return counts

    def _generate_summary(self, counts: Dict[str, int]) -> Dict[str, any]:
        """Generate summary of parsing results."""
        expected = {
            'programs': 1,
            'divisions': 4,
            'sections': 16,
            'paragraphs': 261,
            'statements': 316
        }
        
        discrepancies = {}
        for key, expected_count in expected.items():
            actual_count = counts.get(key, 0)
            discrepancies[key] = {
                'expected': expected_count,
                'actual': actual_count,
                'difference': expected_count - actual_count
            }
        
        return {
            'expected': expected,
            'actual': counts,
            'discrepancies': discrepancies
        }

    def save_results(self, results: Dict[str, any], output_file: str) -> None:
        """Save parsing results to file."""
        # Convert entities to serializable format
        serializable_entities = []
        for entity in results['entities']:
            serializable_entities.append({
                'name': entity.name,
                'type': entity.entity_type.value,
                'line_number': entity.line_number,
                'content': entity.content[:100] + '...' if len(entity.content) > 100 else entity.content,
                'parent': entity.parent,
                'level': entity.level
            })
        
        output_data = {
            'summary': results['summary'],
            'counts': results['counts'],
            'hierarchy': results['hierarchy'],
            'entities': serializable_entities
        }
        
        with open(output_file, 'w') as f:
            json.dump(output_data, f, indent=2)
        
        logger.info(f"Results saved to {output_file}")

def main():
    """Main function to demonstrate the enhanced parser."""
    parser = EnhancedCOBOLParser()
    
    # Example usage
    file_path = "vasu_fraud_management_cobol_reformatted.cbl"
    
    try:
        results = parser.parse_file(file_path)
        
        print("\n=== Enhanced COBOL Parser Results ===")
        print(f"Entity Counts: {results['counts']}")
        
        print("\n=== Discrepancies ===")
        for entity_type, info in results['summary']['discrepancies'].items():
            print(f"{entity_type}: Expected {info['expected']}, Found {info['actual']}, Difference: {info['difference']}")
        
        # Save results
        parser.save_results(results, "enhanced_parser_results.json")
        
    except Exception as e:
        logger.error(f"Parsing failed: {e}")

if __name__ == "__main__":
    main()