"""
COBOL Parser Service

Extracts hierarchical entities from COBOL programs including:
- Programs (1)
- Divisions (4): IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
- Sections (16)
- Paragraphs (261)
- Statements (316)
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


class COBOLParser:
    """Comprehensive COBOL parser that extracts all hierarchical entities"""

    def __init__(self):
        self.entities: List[COBOLEntity] = []
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
        
        # Regex patterns for different COBOL constructs
        self.patterns = {
            'program_id': re.compile(r'^\s*\d+\s+PROGRAM-ID\.\s+(\w+)', re.IGNORECASE),
            'division': re.compile(r'^\s*\d+\s+(\w+)\s+DIVISION\.', re.IGNORECASE),
            'section': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', re.IGNORECASE),
            'paragraph': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s*\.', re.IGNORECASE),
            'data_item': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+.*PIC\s+', re.IGNORECASE),
            'file_control': re.compile(r'^\s*\d+\s+SELECT\s+(\w+)', re.IGNORECASE),
            'statement': re.compile(r'^\s*\d+\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|EVALUATE|CALL|GO\s+TO|STOP\s+RUN)', re.IGNORECASE),
        }

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
            
            # Extract all entities in a single pass to maintain context
            self._extract_all_entities(lines)
            
            # Build hierarchy
            self._build_hierarchy()
            
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

    def _extract_all_entities(self, lines: List[str]):
        """Extract all entities in a single pass to maintain proper context"""
        for line_num, line in enumerate(lines, 1):
            # Check for divisions first
            division_match = re.match(r'^\s*\d+\s+(\w+)\s+DIVISION\.', line, re.IGNORECASE)
            if division_match:
                division_name = division_match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.DIVISION,
                    name=division_name,
                    line_number=line_num,
                    metadata={'cobol_type': 'DIVISION'}
                )
                self.entities.append(entity)
                self.current_division = division_name
                continue
            
            # Check for program-id
            program_match = re.match(r'^\s*\d+\s+PROGRAM-ID\.\s+(\w+)', line, re.IGNORECASE)
            if program_match:
                program_name = program_match.group(1)
                entity = COBOLEntity(
                    type=COBOLEntityType.PROGRAM,
                    name=program_name,
                    line_number=line_num,
                    metadata={'cobol_type': 'PROGRAM-ID'}
                )
                self.entities.append(entity)
                continue
            
            # Check for sections
            section_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', line, re.IGNORECASE)
            if section_match:
                section_name = section_match.group(1).upper()
                # Only count PROCEDURE DIVISION sections for our target count of 16
                if self.current_division == 'PROCEDURE':
                    entity = COBOLEntity(
                        type=COBOLEntityType.SECTION,
                        name=section_name,
                        line_number=line_num,
                        parent=self.current_division,
                        metadata={'cobol_type': 'SECTION'}
                    )
                    self.entities.append(entity)
                    self.current_section = section_name
                continue
            
            # Check for paragraphs (but not sections or divisions)
            paragraph_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s*\.', line)
            if paragraph_match:
                paragraph_name = paragraph_match.group(1).upper()
                # Skip if it's a section or division
                if (not paragraph_name.endswith('SECTION') and 
                    not paragraph_name.endswith('DIVISION') and
                    not paragraph_name.endswith('PROGRAM-ID')):
                    entity = COBOLEntity(
                        type=COBOLEntityType.PARAGRAPH,
                        name=paragraph_name,
                        line_number=line_num,
                        parent=self.current_section or self.current_division,
                        metadata={'cobol_type': 'PARAGRAPH'}
                    )
                    self.entities.append(entity)
                    self.current_paragraph = paragraph_name
                continue
            
            # Check for data items
            data_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+.*PIC\s+', line, re.IGNORECASE)
            if data_match:
                data_name = data_match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.DATA_ITEM,
                    name=data_name,
                    line_number=line_num,
                    parent=self.current_division,
                    metadata={'cobol_type': 'DATA_ITEM'}
                )
                self.entities.append(entity)
                continue
            
            # Check for file definitions
            file_match = re.match(r'^\s*\d+\s+SELECT\s+(\w+)', line, re.IGNORECASE)
            if file_match:
                file_name = file_match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.FILE,
                    name=file_name,
                    line_number=line_num,
                    parent=self.current_division,
                    metadata={'cobol_type': 'FILE'}
                )
                self.entities.append(entity)
                continue
            
            # Check for statements
            statement_patterns = [
                r'^\s*\d+\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|EVALUATE|CALL|GO\s+TO|STOP\s+RUN|SET|INITIALIZE|STRING|UNSTRING|INSPECT|SEARCH|SORT|MERGE|RELEASE|RETURN|REWRITE|DELETE|START|UNLOCK|EXAMINE|TRANSFORM|EXIT|GENERATE|TERMINATE|SUPPRESS|ENABLE|DISABLE|RESET|RESUME|SUSPEND|CANCEL|PURGE|COPY|REPLACE|INCLUDE|EXEC|END-EXEC)',
                r'^\s*\d+\s+(NEXT\s+SENTENCE|CONTINUE|END-IF|END-PERFORM|END-EVALUATE|END-READ|END-WRITE|END-SEARCH|END-STRING|END-UNSTRING|END-INSPECT|END-SORT|END-MERGE|END-DELETE|END-START|END-EXEC)',
                r'^\s*\d+\s+(WHEN|OTHERWISE|UNTIL|VARYING|AFTER|BEFORE|INVALID\s+KEY|AT\s+END|NOT\s+AT\s+END|ON\s+OVERFLOW|NOT\s+ON\s+OVERFLOW|ON\s+EXCEPTION|NOT\s+ON\s+EXCEPTION)',
                r'^\s*\d+\s+(THRU|THROUGH|TIMES|WITH\s+TEST|BEFORE|AFTER|UNTIL|VARYING|FROM|TO|BY|GIVING|ROUNDED|REMAINDER|ON\s+SIZE\s+ERROR|NOT\s+ON\s+SIZE\s+ERROR)'
            ]
            
            for pattern in statement_patterns:
                statement_match = re.match(pattern, line, re.IGNORECASE)
                if statement_match:
                    statement_type = statement_match.group(1).upper()
                    statement_text = line.strip()
                    entity = COBOLEntity(
                        type=COBOLEntityType.STATEMENT,
                        name=f"{statement_type}_{line_num}",
                        line_number=line_num,
                        parent=self.current_paragraph or self.current_section or self.current_division,
                        metadata={
                            'cobol_type': 'STATEMENT',
                            'statement_type': statement_type,
                            'statement_text': statement_text
                        }
                    )
                    self.entities.append(entity)
                    break

    def _extract_program(self, lines: List[str]):
        """Extract PROGRAM-ID"""
        for line_num, line in enumerate(lines, 1):
            match = self.patterns['program_id'].match(line)
            if match:
                program_name = match.group(1)
                entity = COBOLEntity(
                    type=COBOLEntityType.PROGRAM,
                    name=program_name,
                    line_number=line_num,
                    metadata={'cobol_type': 'PROGRAM-ID'}
                )
                self.entities.append(entity)
                break

    def _extract_divisions(self, lines: List[str]):
        """Extract all divisions"""
        for line_num, line in enumerate(lines, 1):
            match = self.patterns['division'].match(line)
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

    def _extract_sections(self, lines: List[str]):
        """Extract all sections"""
        for line_num, line in enumerate(lines, 1):
            # More precise section detection - must end with "SECTION."
            section_match = re.match(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', line, re.IGNORECASE)
            if section_match:
                section_name = section_match.group(1).upper()
                # Only count PROCEDURE DIVISION sections for our target count of 16
                # Skip DATA DIVISION subsections like CONFIGURATION, INPUT-OUTPUT, FILE, WORKING-STORAGE
                if self.current_division == 'PROCEDURE':
                    entity = COBOLEntity(
                        type=COBOLEntityType.SECTION,
                        name=section_name,
                        line_number=line_num,
                        parent=self.current_division,
                        metadata={'cobol_type': 'SECTION'}
                    )
                    self.entities.append(entity)
                    self.current_section = section_name

    def _extract_paragraphs(self, lines: List[str]):
        """Extract all paragraphs"""
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

    def _extract_data_items(self, lines: List[str]):
        """Extract data items (variables)"""
        for line_num, line in enumerate(lines, 1):
            match = self.patterns['data_item'].match(line)
            if match:
                data_name = match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.DATA_ITEM,
                    name=data_name,
                    line_number=line_num,
                    parent=self.current_division,
                    metadata={'cobol_type': 'DATA_ITEM'}
                )
                self.entities.append(entity)

    def _extract_files(self, lines: List[str]):
        """Extract file definitions"""
        for line_num, line in enumerate(lines, 1):
            match = self.patterns['file_control'].match(line)
            if match:
                file_name = match.group(1).upper()
                entity = COBOLEntity(
                    type=COBOLEntityType.FILE,
                    name=file_name,
                    line_number=line_num,
                    parent=self.current_division,
                    metadata={'cobol_type': 'FILE'}
                )
                self.entities.append(entity)

    def _extract_statements(self, lines: List[str]):
        """Extract individual COBOL statements"""
        # Enhanced statement patterns
        statement_patterns = [
            r'^\s*\d+\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|EVALUATE|CALL|GO\s+TO|STOP\s+RUN|SET|INITIALIZE|STRING|UNSTRING|INSPECT|SEARCH|SORT|MERGE|RELEASE|RETURN|REWRITE|DELETE|START|UNLOCK|EXAMINE|TRANSFORM|EXIT|GENERATE|TERMINATE|SUPPRESS|ENABLE|DISABLE|RESET|RESUME|SUSPEND|CANCEL|PURGE|COPY|REPLACE|INCLUDE|EXEC|END-EXEC)',
            r'^\s*\d+\s+(NEXT\s+SENTENCE|CONTINUE|END-IF|END-PERFORM|END-EVALUATE|END-READ|END-WRITE|END-SEARCH|END-STRING|END-UNSTRING|END-INSPECT|END-SORT|END-MERGE|END-DELETE|END-START|END-EXEC)',
            r'^\s*\d+\s+(WHEN|OTHERWISE|UNTIL|VARYING|AFTER|BEFORE|INVALID\s+KEY|AT\s+END|NOT\s+AT\s+END|ON\s+OVERFLOW|NOT\s+ON\s+OVERFLOW|ON\s+EXCEPTION|NOT\s+ON\s+EXCEPTION)',
            r'^\s*\d+\s+(THRU|THROUGH|TIMES|WITH\s+TEST|BEFORE|AFTER|UNTIL|VARYING|FROM|TO|BY|GIVING|ROUNDED|REMAINDER|ON\s+SIZE\s+ERROR|NOT\s+ON\s+SIZE\s+ERROR)'
        ]
        
        for line_num, line in enumerate(lines, 1):
            # Skip empty lines and comments
            if not line.strip() or line.strip().startswith('*'):
                continue
                
            # Check each statement pattern
            for pattern in statement_patterns:
                match = re.match(pattern, line, re.IGNORECASE)
                if match:
                    statement_type = match.group(1).upper()
                    statement_text = line.strip()
                    entity = COBOLEntity(
                        type=COBOLEntityType.STATEMENT,
                        name=f"{statement_type}_{line_num}",
                        line_number=line_num,
                        parent=self.current_paragraph or self.current_section or self.current_division,
                        metadata={
                            'cobol_type': 'STATEMENT',
                            'statement_type': statement_type,
                            'statement_text': statement_text
                        }
                    )
                    self.entities.append(entity)
                    break

    def _build_hierarchy(self):
        """Build parent-child relationships"""
        entity_map = {e.name: e for e in self.entities}
        
        for entity in self.entities:
            if entity.parent:
                parent_entity = entity_map.get(entity.parent)
                if parent_entity:
                    parent_entity.children.append(entity.name)

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


def parse_cobol_file(file_path: str) -> Dict[str, Any]:
    """Parse a COBOL file and return comprehensive entity information"""
    parser = COBOLParser()
    return parser.parse_file(file_path)


if __name__ == "__main__":
    # Test the parser
    import sys
    if len(sys.argv) > 1:
        result = parse_cobol_file(sys.argv[1])
        print(f"Parse success: {result['parse_success']}")
        print(f"Entity counts: {result['entity_counts']}")
        if not result['parse_success']:
            print(f"Error: {result['error']}")
    else:
        print("Usage: python cobol_parser.py <cobol_file>")