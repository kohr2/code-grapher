#!/usr/bin/env python3
"""
Enhanced COBOL Parser

This parser addresses the discrepancies identified by the COBOL Analysis Agent
by implementing comprehensive extraction of all hierarchical elements.
"""

import re
import logging
from typing import Dict, List, Tuple, Set, Optional, Any
from dataclasses import dataclass, asdict
from pathlib import Path
import json
from collections import defaultdict, Counter

logger = logging.getLogger(__name__)

@dataclass
class COBOLEntity:
    """Enhanced COBOL entity representation"""
    name: str
    type: str
    line_number: int
    content: str
    parent: Optional[str] = None
    children: List[str] = None
    attributes: Dict[str, Any] = None
    
    def __post_init__(self):
        if self.children is None:
            self.children = []
        if self.attributes is None:
            self.attributes = {}

@dataclass
class COBOLStructure:
    """Complete COBOL hierarchical structure"""
    programs: List[COBOLEntity]
    divisions: List[COBOLEntity]
    sections: List[COBOLEntity]
    paragraphs: List[COBOLEntity]
    statements: List[COBOLEntity]
    data_items: List[COBOLEntity]
    files: List[COBOLEntity]
    
    def get_counts(self) -> Dict[str, int]:
        """Get entity counts by type"""
        return {
            'PROGRAM': len(self.programs),
            'DIVISION': len(self.divisions),
            'SECTION': len(self.sections),
            'PARAGRAPH': len(self.paragraphs),
            'STATEMENT': len(self.statements),
            'DATA_ITEM': len(self.data_items),
            'FILE': len(self.files)
        }

class EnhancedCOBOLParser:
    """Enhanced COBOL parser with comprehensive entity extraction"""
    
    def __init__(self, config: Optional[Dict] = None):
        self.config = config or self._get_default_config()
        
        # Compile regex patterns for better performance
        self._compile_patterns()
        
        # Track hierarchy for parent-child relationships
        self.hierarchy_stack = []
        self.current_program = None
        self.current_division = None
        self.current_section = None
        
    def _get_default_config(self) -> Dict:
        """Get default parser configuration"""
        return {
            "extraction_rules": {
                "programs": {"enabled": True},
                "divisions": {"enabled": True},
                "sections": {"enabled": True},
                "paragraphs": {"enabled": True},
                "statements": {"enabled": True},
                "data_items": {"enabled": True},
                "files": {"enabled": True}
            },
            "validation": {
                "enable_count_validation": True
            },
            "relationships": {
                "enable_hierarchical_tracking": True
            }
        }
    
    def _compile_patterns(self):
        """Compile regex patterns for better performance"""
        # Program patterns
        self.program_pattern = re.compile(
            r'^\s*\d+\s+PROGRAM-ID\s+\.\s+([A-Z0-9-]+)',
            re.IGNORECASE
        )
        
        # Division patterns
        self.division_patterns = [
            re.compile(r'^\s*\d+\s+(IDENTIFICATION\s+DIVISION)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(ENVIRONMENT\s+DIVISION)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(DATA\s+DIVISION)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(PROCEDURE\s+DIVISION)', re.IGNORECASE)
        ]
        
        # Section pattern
        self.section_pattern = re.compile(
            r'^\s*\d+\s+([A-Z0-9-]+)\s+SECTION\s*\.',
            re.IGNORECASE
        )
        
        # Paragraph pattern (excluding sections)
        self.paragraph_pattern = re.compile(
            r'^\s*\d+\s+([A-Z0-9-]+)\s*\.(?![A-Z\s]*SECTION)',
            re.IGNORECASE
        )
        
        # Data item patterns
        self.data_item_patterns = [
            re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+(PIC\s+[X9A]+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+(PICTURE\s+[X9A]+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+(COMP-3|COMP|COMPUTATIONAL)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+(VALUE\s+)', re.IGNORECASE)
        ]
        
        # File patterns
        self.file_pattern = re.compile(
            r'^\s*\d+\s+([A-Z0-9-]+)\s+FILE\s+',
            re.IGNORECASE
        )
        
        # Statement patterns (comprehensive list)
        self.statement_patterns = [
            # Control structures
            re.compile(r'^\s*\d+\s+(IF\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(END-IF\s*)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(ELSE\s*)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(EVALUATE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(WHEN\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(END-EVALUATE\s*)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(PERFORM\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(END-PERFORM\s*)', re.IGNORECASE),
            
            # I/O operations
            re.compile(r'^\s*\d+\s+(OPEN\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(CLOSE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(READ\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(WRITE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(REWRITE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(DELETE\s+)', re.IGNORECASE),
            
            # Data manipulation
            re.compile(r'^\s*\d+\s+(MOVE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(INITIALIZE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(SET\s+)', re.IGNORECASE),
            
            # Arithmetic operations
            re.compile(r'^\s*\d+\s+(ADD\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(SUBTRACT\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(MULTIPLY\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(DIVIDE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(COMPUTE\s+)', re.IGNORECASE),
            
            # String operations
            re.compile(r'^\s*\d+\s+(STRING\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(UNSTRING\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(INSPECT\s+)', re.IGNORECASE),
            
            # Display and accept
            re.compile(r'^\s*\d+\s+(DISPLAY\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(ACCEPT\s+)', re.IGNORECASE),
            
            # Program control
            re.compile(r'^\s*\d+\s+(STOP\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(EXIT\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(GO\s+TO\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(CALL\s+)', re.IGNORECASE),
            
            # Sorting and searching
            re.compile(r'^\s*\d+\s+(SORT\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(MERGE\s+)', re.IGNORECASE),
            re.compile(r'^\s*\d+\s+(SEARCH\s+)', re.IGNORECASE)
        ]
    
    def parse_cobol_file(self, file_path: str) -> COBOLStructure:
        """Parse COBOL file with comprehensive entity extraction"""
        logger.info(f"Parsing COBOL file: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                content = file.read()
                lines = content.split('\n')
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {e}")
            raise
        
        # Initialize structure
        structure = COBOLStructure(
            programs=[], divisions=[], sections=[], paragraphs=[],
            statements=[], data_items=[], files=[]
        )
        
        # Reset hierarchy tracking
        self.hierarchy_stack = []
        self.current_program = None
        self.current_division = None
        self.current_section = None
        
        # Parse line by line
        for line_num, line in enumerate(lines, 1):
            self._parse_line(line, line_num, structure)
        
        # Validate and report results
        self._validate_results(structure)
        
        return structure
    
    def _parse_line(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse individual line for entities"""
        if not line.strip():
            return
        
        # Parse programs
        if self.config["extraction_rules"]["programs"]["enabled"]:
            self._parse_program(line, line_num, structure)
        
        # Parse divisions
        if self.config["extraction_rules"]["divisions"]["enabled"]:
            self._parse_division(line, line_num, structure)
        
        # Parse sections
        if self.config["extraction_rules"]["sections"]["enabled"]:
            self._parse_section(line, line_num, structure)
        
        # Parse paragraphs
        if self.config["extraction_rules"]["paragraphs"]["enabled"]:
            self._parse_paragraph(line, line_num, structure)
        
        # Parse data items
        if self.config["extraction_rules"]["data_items"]["enabled"]:
            self._parse_data_items(line, line_num, structure)
        
        # Parse files
        if self.config["extraction_rules"]["files"]["enabled"]:
            self._parse_files(line, line_num, structure)
        
        # Parse statements
        if self.config["extraction_rules"]["statements"]["enabled"]:
            self._parse_statements(line, line_num, structure)
    
    def _parse_program(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse program declarations"""
        match = self.program_pattern.search(line)
        if match:
            entity = COBOLEntity(
                name=match.group(1),
                type='PROGRAM',
                line_number=line_num,
                content=line.strip(),
                attributes={'program_id': match.group(1)}
            )
            structure.programs.append(entity)
            self.current_program = entity.name
            self.hierarchy_stack = [entity.name]
    
    def _parse_division(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse division declarations"""
        for pattern in self.division_patterns:
            match = pattern.search(line)
            if match:
                entity = COBOLEntity(
                    name=match.group(1).upper(),
                    type='DIVISION',
                    line_number=line_num,
                    content=line.strip(),
                    parent=self.current_program
                )
                structure.divisions.append(entity)
                self.current_division = entity.name
                # Update hierarchy stack
                if len(self.hierarchy_stack) > 1:
                    self.hierarchy_stack = [self.current_program, entity.name]
                else:
                    self.hierarchy_stack.append(entity.name)
                break
    
    def _parse_section(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse section declarations"""
        match = self.section_pattern.search(line)
        if match:
            entity = COBOLEntity(
                name=match.group(1).upper(),
                type='SECTION',
                line_number=line_num,
                content=line.strip(),
                parent=self.current_division
            )
            structure.sections.append(entity)
            self.current_section = entity.name
            # Update hierarchy stack
            if len(self.hierarchy_stack) > 2:
                self.hierarchy_stack = [self.current_program, self.current_division, entity.name]
            else:
                self.hierarchy_stack.append(entity.name)
    
    def _parse_paragraph(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse paragraph declarations"""
        match = self.paragraph_pattern.search(line)
        if match:
            entity = COBOLEntity(
                name=match.group(1).upper(),
                type='PARAGRAPH',
                line_number=line_num,
                content=line.strip(),
                parent=self.current_section or self.current_division
            )
            structure.paragraphs.append(entity)
    
    def _parse_data_items(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse data item declarations"""
        for pattern in self.data_item_patterns:
            match = pattern.search(line)
            if match:
                entity = COBOLEntity(
                    name=match.group(1).upper(),
                    type='DATA_ITEM',
                    line_number=line_num,
                    content=line.strip(),
                    parent=self.current_division
                )
                structure.data_items.append(entity)
                break
    
    def _parse_files(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse file declarations"""
        match = self.file_pattern.search(line)
        if match:
            entity = COBOLEntity(
                name=match.group(1).upper(),
                type='FILE',
                line_number=line_num,
                content=line.strip(),
                parent=self.current_division
            )
            structure.files.append(entity)
    
    def _parse_statements(self, line: str, line_num: int, structure: COBOLStructure):
        """Parse COBOL statements"""
        for pattern in self.statement_patterns:
            match = pattern.search(line)
            if match:
                entity = COBOLEntity(
                    name=match.group(1).strip().upper(),
                    type='STATEMENT',
                    line_number=line_num,
                    content=line.strip(),
                    parent=self.current_section or self.current_division
                )
                structure.statements.append(entity)
                break
    
    def _validate_results(self, structure: COBOLStructure):
        """Validate parsing results and log statistics"""
        counts = structure.get_counts()
        
        logger.info("=== COBOL Parsing Results ===")
        for entity_type, count in counts.items():
            logger.info(f"{entity_type}: {count}")
        
        # Check for common issues
        if counts['DIVISION'] < 4:
            logger.warning(f"Only {counts['DIVISION']} divisions found, expected 4")
        
        if counts['PARAGRAPH'] == 0:
            logger.warning("No paragraphs found - check paragraph pattern")
        
        if counts['STATEMENT'] == 0:
            logger.warning("No statements found - check statement patterns")
    
    def export_to_json(self, structure: COBOLStructure, output_path: str):
        """Export parsed structure to JSON"""
        export_data = {
            "summary": structure.get_counts(),
            "entities": {
                "programs": [asdict(entity) for entity in structure.programs],
                "divisions": [asdict(entity) for entity in structure.divisions],
                "sections": [asdict(entity) for entity in structure.sections],
                "paragraphs": [asdict(entity) for entity in structure.paragraphs],
                "statements": [asdict(entity) for entity in structure.statements],
                "data_items": [asdict(entity) for entity in structure.data_items],
                "files": [asdict(entity) for entity in structure.files]
            }
        }
        
        with open(output_path, 'w') as f:
            json.dump(export_data, f, indent=2)
        
        logger.info(f"Parsed structure exported to {output_path}")
    
    def generate_graph_entities(self, structure: COBOLStructure) -> List[Dict]:
        """Generate entities in format suitable for graph database"""
        graph_entities = []
        
        # Add all entities to graph
        for entity_list in [structure.programs, structure.divisions, structure.sections, 
                           structure.paragraphs, structure.statements, structure.data_items, 
                           structure.files]:
            for entity in entity_list:
                graph_entity = {
                    "name": entity.name,
                    "type": entity.type,
                    "line_number": entity.line_number,
                    "content": entity.content,
                    "parent": entity.parent,
                    "children": entity.children,
                    "attributes": entity.attributes
                }
                graph_entities.append(graph_entity)
        
        return graph_entities

def main():
    """Main function for testing the enhanced COBOL parser"""
    parser = EnhancedCOBOLParser()
    
    # Example usage - replace with actual COBOL file path
    cobol_file = "vasu_fraud_management_cobol_reformatted.cbl"
    
    if Path(cobol_file).exists():
        try:
            structure = parser.parse_cobol_file(cobol_file)
            
            print("=== Enhanced COBOL Parsing Results ===")
            counts = structure.get_counts()
            for entity_type, count in counts.items():
                print(f"{entity_type}: {count}")
            
            # Export results
            parser.export_to_json(structure, "enhanced_cobol_parsing_results.json")
            
            # Generate graph entities
            graph_entities = parser.generate_graph_entities(structure)
            print(f"\nGenerated {len(graph_entities)} entities for graph database")
            
        except Exception as e:
            logger.error(f"Parsing failed: {e}")
    else:
        print(f"COBOL file not found: {cobol_file}")

if __name__ == "__main__":
    main()