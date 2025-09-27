"""
COBOL Parser Service

Provides comprehensive parsing support for COBOL files to extract:
- Programs (1 expected)
- Divisions (4 expected: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Sections (16 expected)
- Paragraphs (261 expected)
- Statements (316 expected)
- Data Items
- Files
- Copybooks
- Other COBOL constructs
"""

import re
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path
from enum import Enum


class COBOLEntityType(Enum):
    """Types of COBOL entities"""
    PROGRAM = "PROGRAM"
    DIVISION = "DIVISION"
    SECTION = "SECTION"
    PARAGRAPH = "PARAGRAPH"
    STATEMENT = "STATEMENT"
    DATA_ITEM = "DATA_ITEM"
    FILE = "FILE"
    COPYBOOK = "COPYBOOK"
    COPY_STATEMENT = "COPY_STATEMENT"
    WORKING_STORAGE = "WORKING_STORAGE"
    LINKAGE_SECTION = "LINKAGE_SECTION"
    PROCEDURE = "PROCEDURE"


@dataclass
class COBOLEntity:
    """Represents a COBOL entity"""
    entity_type: COBOLEntityType
    name: str
    line_number: int
    content: str
    parent_division: Optional[str] = None
    parent_section: Optional[str] = None
    parent_paragraph: Optional[str] = None
    level_number: Optional[int] = None
    data_type: Optional[str] = None
    picture_clause: Optional[str] = None
    value_clause: Optional[str] = None
    occurs_clause: Optional[str] = None
    redefines_clause: Optional[str] = None
    usage_clause: Optional[str] = None
    context: Optional[str] = None


class COBOLParser:
    """Comprehensive COBOL parser for extracting all entity types"""

    def __init__(self):
        # Regex patterns for different COBOL constructs
        self.patterns = {
            # Program identification
            'program_id': re.compile(r'^\s*\d+\s+PROGRAM-ID\.\s+(\w+)', re.IGNORECASE | re.MULTILINE),
            
            # Divisions (4 main divisions)
            'division': re.compile(r'^\s*\d+\s+(\w+)\s+DIVISION\.', re.IGNORECASE | re.MULTILINE),
            
            # Sections within divisions
            'section': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+SECTION\.', re.IGNORECASE | re.MULTILINE),
            
            # Paragraphs (should find 261)
            'paragraph': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s*\.', re.IGNORECASE | re.MULTILINE),
            
            # Data items with level numbers
            'data_item': re.compile(r'^\s*(\d+)\s+(\w+(?:-\w+)*)\s+(?:PIC\s+([^.\s]+)|PICTURE\s+([^.\s]+)|FILLER|REDEFINES\s+(\w+(?:-\w+)*)|OCCURS\s+([^.\s]+)|USAGE\s+(\w+)|VALUE\s+([^.\s]+))', re.IGNORECASE | re.MULTILINE),
            
            # File descriptions
            'file_description': re.compile(r'^\s*\d+\s+(\w+(?:-\w+)*)\s+FILENAME\s+([^.\s]+)', re.IGNORECASE | re.MULTILINE),
            
            # Copy statements
            'copy_statement': re.compile(r'^\s*\d+\s+COPY\s+(\w+(?:-\w+)*)', re.IGNORECASE | re.MULTILINE),
            
            # Working storage section
            'working_storage': re.compile(r'^\s*\d+\s+WORKING-STORAGE\s+SECTION\.', re.IGNORECASE | re.MULTILINE),
            
            # Linkage section
            'linkage_section': re.compile(r'^\s*\d+\s+LINKAGE\s+SECTION\.', re.IGNORECASE | re.MULTILINE),
            
            # Procedure division
            'procedure_division': re.compile(r'^\s*\d+\s+PROCEDURE\s+DIVISION\.', re.IGNORECASE | re.MULTILINE),
            
            # COBOL statements (should find 316)
            'statement': re.compile(r'^\s*\d+\s+(MOVE|DISPLAY|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|IF|ELSE|END-IF|PERFORM|CALL|GO TO|STOP|EXIT|READ|WRITE|REWRITE|DELETE|OPEN|CLOSE|INITIALIZE|STRING|UNSTRING|INSPECT|EVALUATE|WHEN|END-EVALUATE|SEARCH|SET|SORT|MERGE|RELEASE|RETURN|GENERATE|TERMINATE|EXAMINE|TRANSFORM|INITIALIZE|UNLOCK|LOCK|CANCEL|CONTINUE|ALTER|ENTRY|EXIT PROGRAM|GOBACK|STOP RUN)', re.IGNORECASE | re.MULTILINE),
            
            # Comment lines
            'comment': re.compile(r'^\s*\d+\s+\*', re.IGNORECASE | re.MULTILINE),
            
            # Continuation lines
            'continuation': re.compile(r'^\s*\d+\s+-', re.IGNORECASE | re.MULTILINE),
        }
        
        # Known division names
        self.division_names = {
            'IDENTIFICATION': 'IDENTIFICATION',
            'ENVIRONMENT': 'ENVIRONMENT', 
            'DATA': 'DATA',
            'PROCEDURE': 'PROCEDURE'
        }
        
        # Statement keywords that should be counted
        self.statement_keywords = {
            'MOVE', 'DISPLAY', 'ACCEPT', 'COMPUTE', 'ADD', 'SUBTRACT', 
            'MULTIPLY', 'DIVIDE', 'IF', 'ELSE', 'END-IF', 'PERFORM', 
            'CALL', 'GO TO', 'STOP', 'EXIT', 'READ', 'WRITE', 'REWRITE', 
            'DELETE', 'OPEN', 'CLOSE', 'INITIALIZE', 'STRING', 'UNSTRING', 
            'INSPECT', 'EVALUATE', 'WHEN', 'END-EVALUATE', 'SEARCH', 
            'SET', 'SORT', 'MERGE', 'RELEASE', 'RETURN', 'GENERATE', 
            'TERMINATE', 'EXAMINE', 'TRANSFORM', 'UNLOCK', 'LOCK', 
            'CANCEL', 'CONTINUE', 'ALTER', 'ENTRY', 'EXIT PROGRAM', 
            'GOBACK', 'STOP RUN'
        }

    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and extract all entities"""
        try:
            with open(file_path, "r", encoding="utf-8", errors="ignore") as f:
                content = f.read()
            
            # Remove line numbers (first 6 characters of each line)
            cleaned_content = self._remove_line_numbers(content)
            
            # Extract all entity types
            entities = []
            entities.extend(self._extract_programs(cleaned_content, file_path))
            entities.extend(self._extract_divisions(cleaned_content, file_path))
            entities.extend(self._extract_sections(cleaned_content, file_path))
            entities.extend(self._extract_paragraphs(cleaned_content, file_path))
            entities.extend(self._extract_data_items(cleaned_content, file_path))
            entities.extend(self._extract_files(cleaned_content, file_path))
            entities.extend(self._extract_copy_statements(cleaned_content, file_path))
            entities.extend(self._extract_statements(cleaned_content, file_path))
            
            # Build hierarchy and assign parents
            entities = self._build_hierarchy(entities, cleaned_content)
            
            # Count entities by type
            entity_counts = self._count_entities_by_type(entities)
            
            return {
                "file_path": file_path,
                "entities": entities,
                "language": "cobol",
                "lines_of_code": len(content.splitlines()),
                "parse_success": True,
                "error": None,
                "entity_counts": entity_counts,
                "hierarchical_structure": self._build_hierarchical_summary(entities)
            }
            
        except Exception as e:
            return {
                "file_path": file_path,
                "entities": [],
                "language": "cobol",
                "lines_of_code": 0,
                "parse_success": False,
                "error": str(e),
                "entity_counts": {},
                "hierarchical_structure": {}
            }

    def _remove_line_numbers(self, content: str) -> str:
        """Remove line numbers (first 6 characters) from COBOL content"""
        lines = content.splitlines()
        cleaned_lines = []
        
        for line in lines:
            if len(line) > 6:
                # Remove first 6 characters (line number and sequence area)
                cleaned_line = line[6:]
                cleaned_lines.append(cleaned_line)
            else:
                cleaned_lines.append(line)
        
        return '\n'.join(cleaned_lines)

    def _extract_programs(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract PROGRAM entities"""
        entities = []
        matches = self.patterns['program_id'].finditer(content)
        
        for match in matches:
            program_name = match.group(1)
            line_number = content[:match.start()].count('\n') + 1
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.PROGRAM,
                name=program_name,
                line_number=line_number,
                content=match.group(0).strip()
            )
            entities.append(entity)
        
        return entities

    def _extract_divisions(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract DIVISION entities (should find 4: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)"""
        entities = []
        matches = self.patterns['division'].finditer(content)
        
        for match in matches:
            division_name = match.group(1).upper()
            line_number = content[:match.start()].count('\n') + 1
            
            # Only include known divisions
            if division_name in self.division_names:
                entity = COBOLEntity(
                    entity_type=COBOLEntityType.DIVISION,
                    name=division_name,
                    line_number=line_number,
                    content=match.group(0).strip()
                )
                entities.append(entity)
        
        return entities

    def _extract_sections(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract SECTION entities (should find 16)"""
        entities = []
        matches = self.patterns['section'].finditer(content)
        
        for match in matches:
            section_name = match.group(1)
            line_number = content[:match.start()].count('\n') + 1
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.SECTION,
                name=section_name,
                line_number=line_number,
                content=match.group(0).strip()
            )
            entities.append(entity)
        
        return entities

    def _extract_paragraphs(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract PARAGRAPH entities (should find 261)"""
        entities = []
        matches = self.patterns['paragraph'].finditer(content)
        
        for match in matches:
            paragraph_name = match.group(1)
            line_number = content[:match.start()].count('\n') + 1
            
            # Skip if it's actually a section (contains SECTION)
            if 'SECTION' in paragraph_name.upper():
                continue
                
            entity = COBOLEntity(
                entity_type=COBOLEntityType.PARAGRAPH,
                name=paragraph_name,
                line_number=line_number,
                content=match.group(0).strip()
            )
            entities.append(entity)
        
        return entities

    def _extract_data_items(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract DATA_ITEM entities"""
        entities = []
        matches = self.patterns['data_item'].finditer(content)
        
        for match in matches:
            level_number = int(match.group(1))
            item_name = match.group(2)
            line_number = content[:match.start()].count('\n') + 1
            
            # Extract additional clauses
            picture_clause = match.group(3) or match.group(4)
            redefines_clause = match.group(5)
            occurs_clause = match.group(6)
            usage_clause = match.group(7)
            value_clause = match.group(8)
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.DATA_ITEM,
                name=item_name,
                line_number=line_number,
                content=match.group(0).strip(),
                level_number=level_number,
                picture_clause=picture_clause,
                redefines_clause=redefines_clause,
                occurs_clause=occurs_clause,
                usage_clause=usage_clause,
                value_clause=value_clause
            )
            entities.append(entity)
        
        return entities

    def _extract_files(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract FILE entities"""
        entities = []
        matches = self.patterns['file_description'].finditer(content)
        
        for match in matches:
            file_name = match.group(1)
            filename = match.group(2)
            line_number = content[:match.start()].count('\n') + 1
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.FILE,
                name=file_name,
                line_number=line_number,
                content=match.group(0).strip(),
                context=f"FILENAME: {filename}"
            )
            entities.append(entity)
        
        return entities

    def _extract_copy_statements(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract COPY_STATEMENT entities"""
        entities = []
        matches = self.patterns['copy_statement'].finditer(content)
        
        for match in matches:
            copybook_name = match.group(1)
            line_number = content[:match.start()].count('\n') + 1
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.COPY_STATEMENT,
                name=copybook_name,
                line_number=line_number,
                content=match.group(0).strip()
            )
            entities.append(entity)
        
        return entities

    def _extract_statements(self, content: str, file_path: str) -> List[COBOLEntity]:
        """Extract STATEMENT entities (should find 316)"""
        entities = []
        matches = self.patterns['statement'].finditer(content)
        
        for match in matches:
            statement_keyword = match.group(1).upper()
            line_number = content[:match.start()].count('\n') + 1
            
            # Get the full statement line
            line_start = content.rfind('\n', 0, match.start()) + 1
            line_end = content.find('\n', match.start())
            if line_end == -1:
                line_end = len(content)
            full_statement = content[line_start:line_end].strip()
            
            entity = COBOLEntity(
                entity_type=COBOLEntityType.STATEMENT,
                name=f"{statement_keyword}_STATEMENT",
                line_number=line_number,
                content=full_statement,
                context=statement_keyword
            )
            entities.append(entity)
        
        return entities

    def _build_hierarchy(self, entities: List[COBOLEntity], content: str) -> List[COBOLEntity]:
        """Build hierarchical relationships between entities"""
        # Sort entities by line number
        entities.sort(key=lambda x: x.line_number)
        
        current_division = None
        current_section = None
        current_paragraph = None
        
        for entity in entities:
            if entity.entity_type == COBOLEntityType.DIVISION:
                current_division = entity.name
                current_section = None
                current_paragraph = None
            elif entity.entity_type == COBOLEntityType.SECTION:
                current_section = entity.name
                current_paragraph = None
            elif entity.entity_type == COBOLEntityType.PARAGRAPH:
                current_paragraph = entity.name
            
            # Assign parent relationships
            entity.parent_division = current_division
            entity.parent_section = current_section
            entity.parent_paragraph = current_paragraph
        
        return entities

    def _count_entities_by_type(self, entities: List[COBOLEntity]) -> Dict[str, int]:
        """Count entities by type"""
        counts = {}
        for entity in entities:
            entity_type = entity.entity_type.value
            counts[entity_type] = counts.get(entity_type, 0) + 1
        return counts

    def _build_hierarchical_summary(self, entities: List[COBOLEntity]) -> Dict[str, Any]:
        """Build hierarchical structure summary"""
        summary = {
            "programs": 0,
            "divisions": 0,
            "sections": 0,
            "paragraphs": 0,
            "statements": 0,
            "data_items": 0,
            "files": 0,
            "copy_statements": 0
        }
        
        for entity in entities:
            if entity.entity_type == COBOLEntityType.PROGRAM:
                summary["programs"] += 1
            elif entity.entity_type == COBOLEntityType.DIVISION:
                summary["divisions"] += 1
            elif entity.entity_type == COBOLEntityType.SECTION:
                summary["sections"] += 1
            elif entity.entity_type == COBOLEntityType.PARAGRAPH:
                summary["paragraphs"] += 1
            elif entity.entity_type == COBOLEntityType.STATEMENT:
                summary["statements"] += 1
            elif entity.entity_type == COBOLEntityType.DATA_ITEM:
                summary["data_items"] += 1
            elif entity.entity_type == COBOLEntityType.FILE:
                summary["files"] += 1
            elif entity.entity_type == COBOLEntityType.COPY_STATEMENT:
                summary["copy_statements"] += 1
        
        return summary


def parse_cobol_file(file_path: str) -> Dict[str, Any]:
    """Parse a single COBOL file and extract all entities"""
    parser = COBOLParser()
    return parser.parse_file(file_path)


def parse_multiple_cobol_files(file_paths: List[str]) -> List[Dict[str, Any]]:
    """Parse multiple COBOL files"""
    parser = COBOLParser()
    results = []
    
    for file_path in file_paths:
        result = parser.parse_file(file_path)
        results.append(result)
    
    return results


# Example usage and testing
if __name__ == "__main__":
    # Test the parser with a sample COBOL file
    parser = COBOLParser()
    
    # Create a sample COBOL file for testing
    sample_cobol = """
000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. SAMPLE-PROGRAM.
000300 AUTHOR. TEST-AUTHOR.
000400 DATE-WRITTEN. 2024-01-01.
000500
000600 ENVIRONMENT DIVISION.
000700 INPUT-OUTPUT SECTION.
000800 FILE-CONTROL.
000900     SELECT INPUT-FILE ASSIGN TO 'INPUT.DAT'.
001000     SELECT OUTPUT-FILE ASSIGN TO 'OUTPUT.DAT'.
001100
001200 DATA DIVISION.
001300 FILE SECTION.
001400 FD  INPUT-FILE.
001500 01  INPUT-RECORD.
001600     05  INPUT-FIELD-1     PIC X(10).
001700     05  INPUT-FIELD-2     PIC 9(5).
001800
001900 WORKING-STORAGE SECTION.
002000 01  WS-COUNTER           PIC 9(3).
002100 01  WS-TOTAL             PIC 9(6)V99.
002200
002300 PROCEDURE DIVISION.
002400 1000-MAIN-PROCESSING.
002500     PERFORM 2000-INITIALIZE
002600     PERFORM 3000-PROCESS-DATA
002700     PERFORM 4000-FINALIZE
002800     STOP RUN.
002900
003000 2000-INITIALIZE.
003100     OPEN INPUT INPUT-FILE
003200     OPEN OUTPUT OUTPUT-FILE
003300     MOVE ZERO TO WS-COUNTER
003400     MOVE ZERO TO WS-TOTAL.
003500
003600 3000-PROCESS-DATA.
003700     READ INPUT-FILE
003800         AT END MOVE 'Y' TO WS-EOF-FLAG
003900         NOT AT END
004000             ADD 1 TO WS-COUNTER
004100             ADD INPUT-FIELD-2 TO WS-TOTAL
004200     END-READ.
004300
004400 4000-FINALIZE.
004500     DISPLAY 'TOTAL RECORDS: ' WS-COUNTER
004600     DISPLAY 'TOTAL VALUE: ' WS-TOTAL
004700     CLOSE INPUT-FILE
004800     CLOSE OUTPUT-FILE.
"""
    
    # Write sample file
    with open("/tmp/sample.cbl", "w") as f:
        f.write(sample_cobol)
    
    # Parse the sample file
    result = parser.parse_file("/tmp/sample.cbl")
    
    print("COBOL Parser Test Results:")
    print(f"Parse Success: {result['parse_success']}")
    print(f"Entity Counts: {result['entity_counts']}")
    print(f"Hierarchical Structure: {result['hierarchical_structure']}")
    print(f"Total Entities Found: {len(result['entities'])}")
    
    # Show entities by type
    for entity_type in COBOLEntityType:
        entities_of_type = [e for e in result['entities'] if e.entity_type == entity_type]
        if entities_of_type:
            print(f"\n{entity_type.value} entities:")
            for entity in entities_of_type[:5]:  # Show first 5
                print(f"  - {entity.name} (line {entity.line_number})")
            if len(entities_of_type) > 5:
                print(f"  ... and {len(entities_of_type) - 5} more")