#!/usr/bin/env python3
"""
COBOL Relationship Extractor
Integrates with the existing codebase analysis pipeline to extract COBOL entities and relationships
"""

import re
import json
import logging
from typing import Dict, List, Any, Optional, Set, Tuple
from pathlib import Path
from dataclasses import dataclass
from datetime import datetime

from ai_relationship_extractor import RelationshipExtraction, RelationshipType
from logger import logger


@dataclass
class COBOLEntity:
    """Represents a COBOL entity with its metadata"""
    name: str
    entity_type: str
    line_number: int
    content: str
    parent: str = None
    file_path: str = None


class COBOLRelationshipExtractor:
    """Extract COBOL entities and relationships for integration with the main pipeline"""
    
    def __init__(self):
        self.logger = logger
        self.entities_cache = {}
        
        # Enhanced regex patterns for comprehensive COBOL parsing
        self.patterns = {
            'program_id': re.compile(r'^\s*\d+\s+PROGRAM-ID\.\s+([A-Z0-9-]+)', re.IGNORECASE),
            'division': re.compile(r'^\s*\d+\s+([A-Z]+)\s+DIVISION', re.IGNORECASE),
            'section': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+SECTION\.', re.IGNORECASE),
            'paragraph': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s*\.', re.IGNORECASE),
            'data_item': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+.*PIC\s+', re.IGNORECASE),
            'file_definition': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+.*SELECT\s+', re.IGNORECASE),
            'statement': re.compile(r'^\s*\d+\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|GO\s+TO|CALL|EXIT|STOP|EVALUATE|INITIALIZE|SET|STRING|UNSTRING)', re.IGNORECASE)
        }
        
        # Statement keywords for comprehensive extraction
        self.statement_keywords = {
            'DISPLAY', 'MOVE', 'IF', 'PERFORM', 'READ', 'WRITE', 'OPEN', 'CLOSE',
            'ACCEPT', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'GO TO',
            'CALL', 'EXIT', 'STOP', 'EVALUATE', 'INITIALIZE', 'SET', 'STRING',
            'UNSTRING', 'SEARCH', 'SORT', 'MERGE', 'RELEASE', 'RETURN', 'REWRITE',
            'DELETE', 'START', 'CONTINUE', 'NEXT', 'END-IF', 'END-PERFORM',
            'END-EVALUATE', 'END-SEARCH', 'END-SORT', 'END-MERGE'
        }
    
    def extract_entities_from_file(self, file_path: str) -> Dict[str, List[COBOLEntity]]:
        """Extract all COBOL entities from a file"""
        self.logger.info(f"Extracting COBOL entities from: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                lines = file.readlines()
        except Exception as e:
            self.logger.error(f"Error reading file {file_path}: {e}")
            return {}
        
        entities = {
            'programs': [], 'divisions': [], 'sections': [], 
            'paragraphs': [], 'statements': [], 'data_items': [], 'files': []
        }
        
        current_division = None
        current_section = None
        current_paragraph = None
        
        for line_num, line in enumerate(lines, 1):
            cleaned_line = self.clean_line(line)
            if not cleaned_line or cleaned_line.startswith('*'):
                continue
            
            # Extract programs
            program_match = self.patterns['program_id'].match(line)
            if program_match:
                entities['programs'].append(COBOLEntity(
                    name=program_match.group(1),
                    entity_type='PROGRAM',
                    line_number=line_num,
                    content=cleaned_line,
                    file_path=file_path
                ))
                continue
            
            # Extract divisions
            division_match = self.patterns['division'].match(line)
            if division_match:
                current_division = division_match.group(1)
                entities['divisions'].append(COBOLEntity(
                    name=current_division,
                    entity_type='DIVISION',
                    line_number=line_num,
                    content=cleaned_line,
                    file_path=file_path
                ))
                continue
            
            # Extract sections
            section_match = self.patterns['section'].match(line)
            if section_match:
                current_section = section_match.group(1)
                entities['sections'].append(COBOLEntity(
                    name=current_section,
                    entity_type='SECTION',
                    line_number=line_num,
                    content=cleaned_line,
                    parent=current_division,
                    file_path=file_path
                ))
                continue
            
            # Extract paragraphs
            paragraph_match = self.patterns['paragraph'].match(line)
            if paragraph_match:
                current_paragraph = paragraph_match.group(1)
                entities['paragraphs'].append(COBOLEntity(
                    name=current_paragraph,
                    entity_type='PARAGRAPH',
                    line_number=line_num,
                    content=cleaned_line,
                    parent=current_section,
                    file_path=file_path
                ))
                continue
            
            # Extract data items
            data_match = self.patterns['data_item'].match(line)
            if data_match:
                entities['data_items'].append(COBOLEntity(
                    name=data_match.group(1),
                    entity_type='DATA_ITEM',
                    line_number=line_num,
                    content=cleaned_line,
                    parent=current_paragraph,
                    file_path=file_path
                ))
                continue
            
            # Extract file definitions
            file_match = self.patterns['file_definition'].match(line)
            if file_match:
                entities['files'].append(COBOLEntity(
                    name=file_match.group(1),
                    entity_type='FILE',
                    line_number=line_num,
                    content=cleaned_line,
                    parent=current_division,
                    file_path=file_path
                ))
                continue
            
            # Extract statements
            statement_match = self.patterns['statement'].match(line)
            if statement_match:
                entities['statements'].append(COBOLEntity(
                    name=f"{statement_match.group(1)}_{line_num}",
                    entity_type='STATEMENT',
                    line_number=line_num,
                    content=cleaned_line,
                    parent=current_paragraph,
                    file_path=file_path
                ))
                continue
            
            # Additional statement detection
            for keyword in self.statement_keywords:
                if cleaned_line.upper().startswith(keyword):
                    entities['statements'].append(COBOLEntity(
                        name=f"{keyword}_{line_num}",
                        entity_type='STATEMENT',
                        line_number=line_num,
                        content=cleaned_line,
                        parent=current_paragraph,
                        file_path=file_path
                    ))
                    break
        
        # Cache entities for relationship extraction
        self.entities_cache[file_path] = entities
        
        self.logger.info(f"Extracted {sum(len(entities[et]) for et in entities)} entities from {file_path}")
        return entities
    
    def extract_relationships(self, parsed_files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
        """
        Extract COBOL relationships from parsed files
        Compatible with the existing pipeline interface
        
        Args:
            parsed_files: List of file data from core_pipeline.py
            
        Returns:
            List of RelationshipExtraction objects
        """
        self.logger.info("🔍 Starting COBOL relationship extraction...")
        
        relationships = []
        cobol_files = [f for f in parsed_files if f.get("file_path", "").endswith(('.cbl', '.cob'))]
        
        if not cobol_files:
            self.logger.info("No COBOL files found for relationship extraction")
            return relationships
        
        self.logger.info(f"📁 Processing {len(cobol_files)} COBOL files")
        
        for file_data in cobol_files:
            file_path = file_data.get("file_path")
            if not file_path:
                continue
            
            try:
                # Extract entities if not already cached
                if file_path not in self.entities_cache:
                    self.extract_entities_from_file(file_path)
                
                entities = self.entities_cache[file_path]
                
                # Extract different relationship types
                relationships.extend(self._extract_hierarchical_relationships(entities, file_path))
                relationships.extend(self._extract_data_relationships(entities, file_path))
                relationships.extend(self._extract_control_flow_relationships(entities, file_path))
                
            except Exception as e:
                self.logger.error(f"Error processing COBOL file {file_path}: {e}")
                continue
        
        self.logger.info(f"✅ Extracted {len(relationships)} COBOL relationships")
        return relationships
    
    def _extract_hierarchical_relationships(self, entities: Dict[str, List[COBOLEntity]], file_path: str) -> List[RelationshipExtraction]:
        """Extract hierarchical relationships (CONTAINS, BELONGS_TO)"""
        relationships = []
        
        # Program contains divisions
        programs = entities.get('programs', [])
        divisions = entities.get('divisions', [])
        
        for program in programs:
            for division in divisions:
                relationships.append(RelationshipExtraction(
                    source_entity=program.name,
                    source_type="PROGRAM",
                    target_entity=division.name,
                    target_type="DIVISION",
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=program.line_number,
                    context=f"Program {program.name} contains {division.name} division"
                ))
        
        # Divisions contain sections
        sections = entities.get('sections', [])
        for section in sections:
            if section.parent:  # parent is the division name
                relationships.append(RelationshipExtraction(
                    source_entity=section.parent,
                    source_type="DIVISION",
                    target_entity=section.name,
                    target_type="SECTION",
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=section.line_number,
                    context=f"Division {section.parent} contains section {section.name}"
                ))
        
        # Sections contain paragraphs
        paragraphs = entities.get('paragraphs', [])
        for paragraph in paragraphs:
            if paragraph.parent:  # parent is the section name
                relationships.append(RelationshipExtraction(
                    source_entity=paragraph.parent,
                    source_type="SECTION",
                    target_entity=paragraph.name,
                    target_type="PARAGRAPH",
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=paragraph.line_number,
                    context=f"Section {paragraph.parent} contains paragraph {paragraph.name}"
                ))
        
        return relationships
    
    def _extract_data_relationships(self, entities: Dict[str, List[COBOLEntity]], file_path: str) -> List[RelationshipExtraction]:
        """Extract data relationships (USES, DEFINES)"""
        relationships = []
        
        # Data items belong to paragraphs
        data_items = entities.get('data_items', [])
        for data_item in data_items:
            if data_item.parent:  # parent is the paragraph name
                relationships.append(RelationshipExtraction(
                    source_entity=data_item.parent,
                    source_type="PARAGRAPH",
                    target_entity=data_item.name,
                    target_type="DATA_ITEM",
                    relationship_type=RelationshipType.DEFINES,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=data_item.line_number,
                    context=f"Paragraph {data_item.parent} defines data item {data_item.name}"
                ))
        
        # Files belong to divisions
        files = entities.get('files', [])
        for file_entity in files:
            if file_entity.parent:  # parent is the division name
                relationships.append(RelationshipExtraction(
                    source_entity=file_entity.parent,
                    source_type="DIVISION",
                    target_entity=file_entity.name,
                    target_type="FILE",
                    relationship_type=RelationshipType.DEFINES,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=file_entity.line_number,
                    context=f"Division {file_entity.parent} defines file {file_entity.name}"
                ))
        
        return relationships
    
    def _extract_control_flow_relationships(self, entities: Dict[str, List[COBOLEntity]], file_path: str) -> List[RelationshipExtraction]:
        """Extract control flow relationships (CALLS, PERFORMS)"""
        relationships = []
        
        # Statements belong to paragraphs
        statements = entities.get('statements', [])
        for statement in statements:
            if statement.parent:  # parent is the paragraph name
                relationships.append(RelationshipExtraction(
                    source_entity=statement.parent,
                    source_type="PARAGRAPH",
                    target_entity=statement.name,
                    target_type="STATEMENT",
                    relationship_type=RelationshipType.CONTAINS,
                    confidence=1.0,
                    source_file=file_path,
                    target_file=file_path,
                    line_number=statement.line_number,
                    context=f"Paragraph {statement.parent} contains statement {statement.name}"
                ))
        
        # Extract PERFORM relationships (paragraph calls)
        for statement in statements:
            if 'PERFORM' in statement.content.upper():
                # Look for paragraph names in PERFORM statements
                perform_match = re.search(r'PERFORM\s+([A-Z0-9-]+)', statement.content.upper())
                if perform_match:
                    target_paragraph = perform_match.group(1)
                    relationships.append(RelationshipExtraction(
                        source_entity=statement.parent or "UNKNOWN",
                        source_type="PARAGRAPH",
                        target_entity=target_paragraph,
                        target_type="PARAGRAPH",
                        relationship_type=RelationshipType.CALLS,
                        confidence=0.9,
                        source_file=file_path,
                        target_file=file_path,
                        line_number=statement.line_number,
                        context=f"Paragraph {statement.parent} performs {target_paragraph}"
                    ))
        
        return relationships
    
    def clean_line(self, line: str) -> str:
        """Remove COBOL line numbers and clean the line"""
        if len(line) > 6:
            return line[6:].strip()
        return line.strip()
    
    def get_entity_counts(self, file_path: str) -> Dict[str, int]:
        """Get entity counts for a file"""
        if file_path not in self.entities_cache:
            self.extract_entities_from_file(file_path)
        
        entities = self.entities_cache.get(file_path, {})
        return {
            'programs': len(entities.get('programs', [])),
            'divisions': len(entities.get('divisions', [])),
            'sections': len(entities.get('sections', [])),
            'paragraphs': len(entities.get('paragraphs', [])),
            'statements': len(entities.get('statements', [])),
            'data_items': len(entities.get('data_items', [])),
            'files': len(entities.get('files', []))
        }
    
    def validate_parsing_results(self, file_path: str, expected_counts: Dict[str, int]) -> Dict[str, any]:
        """Validate parsing results against expected counts"""
        actual_counts = self.get_entity_counts(file_path)
        
        discrepancies = {}
        for entity_type, expected_count in expected_counts.items():
            actual_count = actual_counts.get(entity_type, 0)
            if actual_count != expected_count:
                difference = actual_count - expected_count
                percentage_diff = abs(difference) / expected_count * 100 if expected_count > 0 else 0
                
                discrepancies[entity_type] = {
                    'expected': expected_count,
                    'actual': actual_count,
                    'difference': difference,
                    'percentage_diff': percentage_diff,
                    'severity': self._calculate_severity(percentage_diff)
                }
        
        return discrepancies
    
    def _calculate_severity(self, percentage_diff: float) -> str:
        """Calculate severity based on percentage difference"""
        if percentage_diff >= 50:
            return 'critical'
        elif percentage_diff >= 25:
            return 'high'
        elif percentage_diff >= 10:
            return 'medium'
        else:
            return 'low'


# Integration function for the main pipeline
def extract_cobol_relationships(parsed_files: List[Dict[str, Any]]) -> List[RelationshipExtraction]:
    """
    Extract COBOL relationships from parsed files
    This function integrates with the existing pipeline
    
    Args:
        parsed_files: List of file data from core_pipeline.py
        
    Returns:
        List of RelationshipExtraction objects
    """
    extractor = COBOLRelationshipExtractor()
    return extractor.extract_relationships(parsed_files)


if __name__ == "__main__":
    # Test the COBOL relationship extractor
    extractor = COBOLRelationshipExtractor()
    
    # Test with sample file if it exists
    sample_file = "/workspace/sample_fraud_management.cbl"
    if Path(sample_file).exists():
        print("Testing COBOL Relationship Extractor")
        print("=" * 40)
        
        entities = extractor.extract_entities_from_file(sample_file)
        counts = extractor.get_entity_counts(sample_file)
        
        print("Entity Counts:")
        for entity_type, count in counts.items():
            print(f"  {entity_type}: {count}")
        
        # Test relationship extraction
        mock_parsed_files = [{"file_path": sample_file, "success": True}]
        relationships = extractor.extract_relationships(mock_parsed_files)
        
        print(f"\nExtracted {len(relationships)} relationships")
        for rel in relationships[:5]:  # Show first 5
            print(f"  {rel.source_entity} -> {rel.target_entity} ({rel.relationship_type})")
    else:
        print("No sample COBOL file found for testing")