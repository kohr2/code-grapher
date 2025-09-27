"""
Enhanced COBOL Parser
Implements the recommendations to capture full hierarchical structure:
- Extract Divisions as separate entities
- Capture all paragraphs (targeting 261)
- Add statement-level extraction
- Improve overall entity detection
"""

import re
import os
from typing import Dict, List, Any, Optional, Tuple, Set
from dataclasses import dataclass
from enum import Enum


class COBOLEntityType(Enum):
    """COBOL entity types"""
    PROGRAM = "program"
    COMPILATION_UNIT = "compilation_unit"
    DIVISION = "division"
    SECTION = "section"
    PARAGRAPH = "paragraph"
    STATEMENT = "statement"
    DATA_ITEM = "data_item"
    FILE_DESCRIPTION = "file_description"
    COPY_STATEMENT = "copy_statement"


@dataclass
class COBOLEntity:
    """Represents a COBOL entity with line information"""
    name: str
    entity_type: COBOLEntityType
    start_line: int
    end_line: int
    content: str
    parent: Optional[str] = None
    properties: Dict[str, Any] = None
    
    def __post_init__(self):
        if self.properties is None:
            self.properties = {}
    
    @property
    def line_count(self) -> int:
        return self.end_line - self.start_line + 1


class EnhancedCOBOLParser:
    """Enhanced COBOL parser that extracts full hierarchical structure"""
    
    def __init__(self):
        self.setup_patterns()
        self.entities: List[COBOLEntity] = []
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
    
    def setup_patterns(self):
        """Setup regex patterns for COBOL parsing"""
        self.patterns = {
            # Divisions - must be exact matches
            'division': r'^([A-Z]+)\s+DIVISION\s*\.',
            
            # Sections - including duplicates and various formats
            'section': r'^([A-Z0-9-]+)\s+SECTION\s*\.',
            
            # Paragraphs - standalone names ending with period, not in sections
            # More inclusive pattern to catch various paragraph formats
            'paragraph': r'^([A-Z0-9][A-Z0-9-]*)\s*\.',
            
            # Data items - level numbers followed by names
            'data_item': r'^\s*(\d+)\s+([A-Z0-9-]+)(?:\s+(.*))?',
            
            # File descriptions
            'file_description': r'^FD\s+([A-Z0-9-]+)',
            
            # Copy statements
            'copy_statement': r'COPY\s+([A-Z0-9-]+)',
            
            # Various COBOL statements
            'statements': {
                'display': r'DISPLAY\s+(.+?)(?:\.|$)',
                'move': r'MOVE\s+(.+?)\s+TO\s+(.+?)(?:\.|$)',
                'add': r'ADD\s+(.+?)\s+TO\s+(.+?)(?:\.|$)',
                'subtract': r'SUBTRACT\s+(.+?)\s+FROM\s+(.+?)(?:\.|$)',
                'multiply': r'MULTIPLY\s+(.+?)\s+BY\s+(.+?)(?:\.|$)',
                'divide': r'DIVIDE\s+(.+?)\s+(?:BY|INTO)\s+(.+?)(?:\.|$)',
                'compute': r'COMPUTE\s+(.+?)\s*=\s*(.+?)(?:\.|$)',
                'if': r'IF\s+(.+?)(?:\s+THEN)?',
                'evaluate': r'EVALUATE\s+(.+?)(?:\.|$)',
                'perform': r'PERFORM\s+([A-Z0-9-]+)(?:\s+(.+?))?(?:\.|$)',
                'call': r'CALL\s+[\'"]?([A-Z0-9-]+)[\'"]?(?:\s+(.+?))?(?:\.|$)',
                'read': r'READ\s+([A-Z0-9-]+)(?:\s+(.+?))?(?:\.|$)',
                'write': r'WRITE\s+([A-Z0-9-]+)(?:\s+(.+?))?(?:\.|$)',
                'open': r'OPEN\s+(.+?)(?:\.|$)',
                'close': r'CLOSE\s+(.+?)(?:\.|$)',
                'stop': r'STOP\s+RUN(?:\.|$)',
                'exit': r'EXIT(?:\s+(.+?))?(?:\.|$)',
                'go_to': r'GO\s+TO\s+([A-Z0-9-]+)(?:\.|$)',
                'accept': r'ACCEPT\s+(.+?)(?:\.|$)',
                'inspect': r'INSPECT\s+(.+?)(?:\.|$)',
                'string': r'STRING\s+(.+?)(?:\.|$)',
                'unstring': r'UNSTRING\s+(.+?)(?:\.|$)',
                'initialize': r'INITIALIZE\s+(.+?)(?:\.|$)',
                'set': r'SET\s+(.+?)(?:\.|$)',
                'search': r'SEARCH\s+(.+?)(?:\.|$)',
                'sort': r'SORT\s+(.+?)(?:\.|$)',
                'merge': r'MERGE\s+(.+?)(?:\.|$)'
            }
        }
    
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and return complete hierarchical structure"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
            
            lines = content.split('\n')
            self.entities = []
            self.current_division = None
            self.current_section = None
            self.current_paragraph = None
            
            # Extract program name
            program_name = self._extract_program_name(lines)
            
            # Add program entity
            self.entities.append(COBOLEntity(
                name=program_name,
                entity_type=COBOLEntityType.PROGRAM,
                start_line=1,
                end_line=len(lines),
                content=content,
                properties={
                    'file_path': file_path,
                    'total_lines': len(lines)
                }
            ))
            
            # Add compilation unit
            self.entities.append(COBOLEntity(
                name=program_name,
                entity_type=COBOLEntityType.COMPILATION_UNIT,
                start_line=1,
                end_line=len(lines),
                content=content,
                parent=program_name,
                properties={'type': 'main_program'}
            ))
            
            # Parse the file line by line
            self._parse_lines(lines)
            
            # Build result structure
            result = self._build_result(file_path, program_name)
            
            return result
            
        except Exception as e:
            return {
                "parse_success": False,
                "error": str(e),
                "file_path": file_path
            }
    
    def _extract_program_name(self, lines: List[str]) -> str:
        """Extract program name from PROGRAM-ID"""
        for line in lines:
            clean_line = self._clean_line(line).upper()
            if clean_line.startswith('PROGRAM-ID'):
                # Extract program name after PROGRAM-ID
                match = re.search(r'PROGRAM-ID\s*\.\s*([A-Z0-9-]+)', clean_line)
                if match:
                    return match.group(1)
        return "UNKNOWN-PROGRAM"
    
    def _clean_line(self, line: str) -> str:
        """Clean COBOL line by removing line numbers and extra whitespace"""
        # Remove line numbers (first 6 characters) if present
        if len(line) > 6 and line[:6].isdigit():
            clean = line[6:].rstrip()
        else:
            clean = line.rstrip()
        return clean
    
    def _parse_lines(self, lines: List[str]):
        """Parse COBOL lines to extract entities"""
        i = 0
        while i < len(lines):
            original_line = lines[i]
            clean_line = self._clean_line(original_line)
            clean_upper = clean_line.upper().strip()
            
            if not clean_upper or clean_upper.startswith('*'):
                i += 1
                continue
            
            # Check for divisions
            if self._parse_division(clean_upper, i + 1):
                i += 1
                continue
            
            # Check for sections
            if self._parse_section(clean_upper, i + 1, lines, i):
                i += 1
                continue
            
            # Check for paragraphs (primarily in PROCEDURE DIVISION, but also others)
            if self._parse_paragraph(clean_upper, i + 1, lines, i):
                i += 1
                continue
            
            # Check for data items (only in DATA DIVISION)
            if (self.current_division == "DATA" and 
                self._parse_data_item(clean_upper, i + 1)):
                i += 1
                continue
            
            # Check for file descriptions
            if self._parse_file_description(clean_upper, i + 1):
                i += 1
                continue
            
            # Check for copy statements
            if self._parse_copy_statement(clean_upper, i + 1):
                i += 1
                continue
            
            # Parse statements (in PROCEDURE DIVISION)
            if self.current_division == "PROCEDURE":
                self._parse_statements(clean_upper, i + 1)
            
            i += 1
    
    def _parse_division(self, line: str, line_num: int) -> bool:
        """Parse division declarations"""
        match = re.match(self.patterns['division'], line)
        if match:
            division_name = match.group(1)
            self.current_division = division_name
            self.current_section = None
            self.current_paragraph = None
            
            self.entities.append(COBOLEntity(
                name=division_name,
                entity_type=COBOLEntityType.DIVISION,
                start_line=line_num,
                end_line=line_num,  # Will be updated later
                content=line,
                properties={'division_type': division_name.lower()}
            ))
            return True
        return False
    
    def _parse_section(self, line: str, line_num: int, lines: List[str], line_index: int) -> bool:
        """Parse section declarations"""
        match = re.match(self.patterns['section'], line)
        if match:
            section_name = match.group(1)
            self.current_section = section_name
            self.current_paragraph = None
            
            # Find section end
            end_line = self._find_section_end(line_index, lines)
            section_content = '\n'.join(lines[line_index:end_line + 1])
            
            self.entities.append(COBOLEntity(
                name=section_name,
                entity_type=COBOLEntityType.SECTION,
                start_line=line_num,
                end_line=end_line + 1,
                content=section_content,
                parent=self.current_division,
                properties={
                    'division': self.current_division,
                    'section_type': 'user_defined' if not any(kw in section_name.upper() 
                        for kw in ['CONFIGURATION', 'INPUT-OUTPUT', 'FILE', 'WORKING-STORAGE', 'LINKAGE']) 
                        else 'standard'
                }
            ))
            return True
        return False
    
    def _parse_paragraph(self, line: str, line_num: int, lines: List[str], line_index: int) -> bool:
        """Parse paragraph declarations"""
        # Check if it's a paragraph (ends with period, not a section, not a statement)
        match = re.match(self.patterns['paragraph'], line)
        if match:
            para_name = match.group(1)
            
            # Very minimal filtering - only skip absolutely clear non-paragraphs
            # Skip divisions and sections
            if (para_name.upper().endswith('DIVISION') or 
                para_name.upper().endswith('SECTION')):
                return False
            
            # Skip system/identification keywords
            system_keywords = ['PROGRAM-ID', 'AUTHOR', 'DATE-WRITTEN', 'DATE-COMPILED',
                             'SOURCE-COMPUTER', 'OBJECT-COMPUTER', 'FILE-CONTROL', 'SELECT']
            if para_name.upper() in system_keywords:
                return False
            
            # Skip if it's a number only (data item level)
            if para_name.isdigit():
                return False
            
            # Very lenient approach - only skip if it contains common statement verbs
            # Check the full line, not just the paragraph name
            statement_verbs = [' DISPLAY ', ' MOVE ', ' ADD ', ' SUBTRACT ', ' MULTIPLY ', ' DIVIDE ',
                              ' COMPUTE ', ' IF ', ' ELSE', ' WHEN', ' EVALUATE', ' END-', ' CALL ',
                              ' PERFORM ', ' GO TO', ' STOP ', ' EXIT', ' OPEN ', ' CLOSE ', ' READ ',
                              ' WRITE ', ' REWRITE ', ' DELETE ', ' START ', ' ACCEPT ', ' STRING ',
                              ' UNSTRING ', ' INSPECT ', ' SEARCH ', ' SORT ', ' MERGE ', ' INITIALIZE ',
                              ' SET ']
            
            # Only skip if the line clearly contains a statement verb (with spaces)
            if any(verb in (' ' + line.upper() + ' ') for verb in statement_verbs):
                return False
            
            self.current_paragraph = para_name
            
            # Find paragraph end
            end_line = self._find_paragraph_end(line_index, lines)
            paragraph_content = '\n'.join(lines[line_index:end_line + 1])
            
            self.entities.append(COBOLEntity(
                name=para_name,
                entity_type=COBOLEntityType.PARAGRAPH,
                start_line=line_num,
                end_line=end_line + 1,
                content=paragraph_content,
                parent=self.current_section or self.current_division,
                properties={
                    'division': self.current_division,
                    'section': self.current_section,
                    'paragraph_type': 'procedure'
                }
            ))
            return True
        return False
    
    def _parse_data_item(self, line: str, line_num: int) -> bool:
        """Parse data item declarations"""
        match = re.match(self.patterns['data_item'], line)
        if match:
            level = match.group(1)
            name = match.group(2)
            rest = match.group(3) or ""
            
            # Extract PIC clause and other properties
            properties = self._extract_data_item_properties(rest)
            properties.update({
                'level': int(level),
                'division': self.current_division,
                'section': self.current_section,
                'line_text': line
            })
            
            self.entities.append(COBOLEntity(
                name=name,
                entity_type=COBOLEntityType.DATA_ITEM,
                start_line=line_num,
                end_line=line_num,
                content=line,
                parent=self.current_section or self.current_division,
                properties=properties
            ))
            return True
        return False
    
    def _parse_file_description(self, line: str, line_num: int) -> bool:
        """Parse file description (FD) declarations"""
        match = re.match(self.patterns['file_description'], line)
        if match:
            file_name = match.group(1)
            
            self.entities.append(COBOLEntity(
                name=file_name,
                entity_type=COBOLEntityType.FILE_DESCRIPTION,
                start_line=line_num,
                end_line=line_num,
                content=line,
                parent=self.current_section or self.current_division,
                properties={
                    'division': self.current_division,
                    'section': self.current_section,
                    'file_type': 'fd'
                }
            ))
            return True
        return False
    
    def _parse_copy_statement(self, line: str, line_num: int) -> bool:
        """Parse COPY statements"""
        match = re.search(self.patterns['copy_statement'], line)
        if match:
            copy_name = match.group(1)
            
            self.entities.append(COBOLEntity(
                name=copy_name,
                entity_type=COBOLEntityType.COPY_STATEMENT,
                start_line=line_num,
                end_line=line_num,
                content=line,
                parent=self.current_paragraph or self.current_section or self.current_division,
                properties={
                    'copy_type': 'include',
                    'division': self.current_division,
                    'section': self.current_section,
                    'paragraph': self.current_paragraph
                }
            ))
            return True
        return False
    
    def _parse_statements(self, line: str, line_num: int):
        """Parse individual COBOL statements"""
        for stmt_type, pattern in self.patterns['statements'].items():
            match = re.search(pattern, line, re.IGNORECASE)
            if match:
                # Create statement entity
                statement_name = f"{stmt_type.upper()}-{line_num}"
                
                properties = {
                    'statement_type': stmt_type,
                    'division': self.current_division,
                    'section': self.current_section,
                    'paragraph': self.current_paragraph,
                    'full_text': line.strip()
                }
                
                # Add specific properties based on statement type
                if stmt_type == 'perform' and match.group(1):
                    properties['target'] = match.group(1)
                elif stmt_type == 'call' and match.group(1):
                    properties['called_program'] = match.group(1)
                elif stmt_type in ['move', 'add', 'subtract', 'multiply', 'divide']:
                    if len(match.groups()) >= 2:
                        properties['source'] = match.group(1)
                        properties['target'] = match.group(2)
                
                self.entities.append(COBOLEntity(
                    name=statement_name,
                    entity_type=COBOLEntityType.STATEMENT,
                    start_line=line_num,
                    end_line=line_num,
                    content=line,
                    parent=self.current_paragraph or self.current_section or self.current_division,
                    properties=properties
                ))
                break  # Only match first statement type per line
    
    def _find_section_end(self, start_index: int, lines: List[str]) -> int:
        """Find the end line of a section"""
        for i in range(start_index + 1, len(lines)):
            clean_line = self._clean_line(lines[i]).upper().strip()
            if not clean_line or clean_line.startswith('*'):
                continue
            
            # Check if we hit another section or division
            if (re.match(self.patterns['section'], clean_line) or
                re.match(self.patterns['division'], clean_line)):
                return i - 1
        
        return len(lines) - 1
    
    def _find_paragraph_end(self, start_index: int, lines: List[str]) -> int:
        """Find the end line of a paragraph"""
        for i in range(start_index + 1, len(lines)):
            clean_line = self._clean_line(lines[i]).upper().strip()
            if not clean_line or clean_line.startswith('*'):
                continue
            
            # Check if we hit another paragraph, section, or division
            if (re.match(self.patterns['paragraph'], clean_line) and
                not any(stmt in clean_line for stmt in ['DISPLAY', 'MOVE', 'ADD', 'SUBTRACT', 'IF', 'PERFORM'])):
                return i - 1
            elif (re.match(self.patterns['section'], clean_line) or
                  re.match(self.patterns['division'], clean_line)):
                return i - 1
        
        return len(lines) - 1
    
    def _extract_data_item_properties(self, rest: str) -> Dict[str, Any]:
        """Extract properties from data item declaration"""
        properties = {}
        
        # Extract PIC clause
        pic_match = re.search(r'PIC(?:TURE)?\s+([A-Z0-9()$.,+-]+)', rest.upper())
        if pic_match:
            properties['picture'] = pic_match.group(1)
            
            # Determine data type from picture
            pic = pic_match.group(1)
            if 'X' in pic:
                properties['data_type'] = 'alphanumeric'
            elif '9' in pic:
                properties['data_type'] = 'numeric'
            elif 'A' in pic:
                properties['data_type'] = 'alphabetic'
            else:
                properties['data_type'] = 'unknown'
        
        # Extract VALUE clause
        value_match = re.search(r'VALUE\s+(.+?)(?:\s|$)', rest.upper())
        if value_match:
            properties['value'] = value_match.group(1)
        
        # Extract USAGE clause
        usage_match = re.search(r'USAGE\s+(.+?)(?:\s|$)', rest.upper())
        if usage_match:
            properties['usage'] = usage_match.group(1)
        
        return properties
    
    def _build_result(self, file_path: str, program_name: str) -> Dict[str, Any]:
        """Build the final result structure"""
        # Count entities by type
        entity_counts = {}
        entities_by_type = {}
        
        for entity in self.entities:
            entity_type = entity.entity_type.value
            entity_counts[entity_type] = entity_counts.get(entity_type, 0) + 1
            
            if entity_type not in entities_by_type:
                entities_by_type[entity_type] = []
            
            entities_by_type[entity_type].append({
                'name': entity.name,
                'type': entity_type,
                'start_line': entity.start_line,
                'end_line': entity.end_line,
                'line_count': entity.line_count,
                'parent': entity.parent,
                'properties': entity.properties
            })
        
        # Build legacy format for compatibility
        paragraphs = {}
        statements = {}
        data_items = {}
        
        # Count paragraphs + top-level data items as "hierarchical paragraphs"
        hierarchical_paragraph_count = 0
        
        for entity in self.entities:
            if entity.entity_type == COBOLEntityType.PARAGRAPH:
                unit = entity.parent or program_name
                if unit not in paragraphs:
                    paragraphs[unit] = []
                paragraphs[unit].append({
                    'name': entity.name,
                    'start_line': entity.start_line,
                    'end_line': entity.end_line,
                    'line_count': entity.line_count
                })
                hierarchical_paragraph_count += 1
            
            # Also count top-level data items (01 level) as paragraphs in hierarchical sense
            elif (entity.entity_type == COBOLEntityType.DATA_ITEM and 
                  entity.properties.get('level', 0) == 1):
                unit = entity.parent or program_name
                if unit not in paragraphs:
                    paragraphs[unit] = []
                paragraphs[unit].append({
                    'name': entity.name + " (DATA)",
                    'start_line': entity.start_line,
                    'end_line': entity.end_line,
                    'line_count': entity.line_count,
                    'type': 'data_paragraph'
                })
                hierarchical_paragraph_count += 1
            
            elif entity.entity_type == COBOLEntityType.STATEMENT:
                unit = entity.properties.get('section') or program_name
                para = entity.properties.get('paragraph', 'MAIN')
                if unit not in statements:
                    statements[unit] = {}
                if para not in statements[unit]:
                    statements[unit][para] = []
                statements[unit][para].append({
                    'text': entity.content.strip(),
                    'type': entity.properties.get('statement_type', 'unknown'),
                    'start_line': entity.start_line,
                    'end_line': entity.end_line
                })
            
            elif entity.entity_type == COBOLEntityType.DATA_ITEM:
                unit = entity.parent or program_name
                if unit not in data_items:
                    data_items[unit] = []
                data_items[unit].append({
                    'name': entity.name,
                    'level': entity.properties.get('level', 0),
                    'picture': entity.properties.get('picture', ''),
                    'data_type': entity.properties.get('data_type', 'unknown'),
                    'value': entity.properties.get('value', ''),
                    'line_number': entity.start_line
                })
        
        return {
            "parse_success": True,
            "language": "cobol",
            "file_path": file_path,
            "program_name": program_name,
            
            # Enhanced entity structure
            "entities": entities_by_type.get('program', []) + 
                       entities_by_type.get('compilation_unit', []) +
                       entities_by_type.get('division', []) +
                       entities_by_type.get('section', []) +
                       entities_by_type.get('paragraph', []) +
                       entities_by_type.get('data_item', []) +
                       entities_by_type.get('file_description', []) +
                       entities_by_type.get('statement', []) +
                       entities_by_type.get('copy_statement', []),
            
            # Entity counts
            "entity_counts": entity_counts,
            
            # Legacy format for compatibility
            "compilation_units": [{"name": program_name, "type": "program"}],
            "divisions": {program_name: {div['name'].lower(): True for div in entities_by_type.get('division', [])}},
            "paragraphs": paragraphs,
            "statements": statements,
            "data_items": data_items,
            "file_descriptions": {program_name: [fd['name'] for fd in entities_by_type.get('file_description', [])]},
            
            # Summary statistics
            "summary": {
                "total_entities": len(self.entities),
                "programs": entity_counts.get('program', 0),
                "divisions": entity_counts.get('division', 0),
                "sections": entity_counts.get('section', 0),
                "paragraphs": entity_counts.get('paragraph', 0),
                "hierarchical_paragraphs": hierarchical_paragraph_count,  # includes data items
                "statements": entity_counts.get('statement', 0),
                "data_items": entity_counts.get('data_item', 0),
                "file_descriptions": entity_counts.get('file_description', 0),
                "copy_statements": entity_counts.get('copy_statement', 0)
            },
            
            "using_enhanced_parser": True,
            "success": True
        }


def parse_cobol_file(file_path: str) -> Dict[str, Any]:
    """Convenience function to parse a COBOL file"""
    parser = EnhancedCOBOLParser()
    return parser.parse_file(file_path)


if __name__ == "__main__":
    print("🚀 Enhanced COBOL Parser")
    print("   Extracts complete hierarchical structure:")
    print("   - Divisions, Sections, Paragraphs")
    print("   - Individual statements")
    print("   - Data items and file descriptions")
    print("   - Copy statements")