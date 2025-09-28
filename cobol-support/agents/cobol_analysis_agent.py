#!/usr/bin/env python3
"""
COBOL Parsing Analysis Agent

This agent analyzes COBOL files and identifies discrepancies between expected
hierarchical structure and actual parsed entities. It provides detailed analysis
and recommendations for improving COBOL parsing accuracy.

Expected Structure:
- Programs: 1
- Divisions: 4 (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Sections: 16
- Paragraphs: 261
- Statements: 316

Current Issues:
- Missing Divisions as separate entities
- Only 103 paragraphs found instead of 261
- No statement-level extraction
- Sections not properly captured as entities
"""

import re
import logging
from typing import Dict, List, Tuple, Set
from dataclasses import dataclass
from enum import Enum
import json

# Configure logging
logging.basicConfig(level=logging.INFO)
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
    parent: str = None
    children: List[str] = None

    def __post_init__(self):
        if self.children is None:
            self.children = []

@dataclass
class ParsingAnalysis:
    expected_counts: Dict[str, int]
    actual_counts: Dict[str, int]
    missing_entities: List[str]
    extra_entities: List[str]
    discrepancies: Dict[str, int]
    recommendations: List[str]

class COBOLAnalysisAgent:
    """
    Background agent for analyzing COBOL parsing discrepancies and providing fixes.
    """
    
    def __init__(self):
        self.entities: List[COBOLEntity] = []
        self.entity_patterns = {
            'program': [
                r'^[0-9]{6}\s+(PROGRAM-ID\.\s+([A-Z0-9-]+))',
                r'^[0-9]{6}\s+(IDENTIFICATION\s+DIVISION\.)',
            ],
            'division': [
                r'^[0-9]{6}\s+(IDENTIFICATION\s+DIVISION\.)',
                r'^[0-9]{6}\s+(ENVIRONMENT\s+DIVISION\.)',
                r'^[0-9]{6}\s+(DATA\s+DIVISION\.)',
                r'^[0-9]{6}\s+(PROCEDURE\s+DIVISION\.)',
            ],
            'section': [
                r'^[0-9]{6}\s+([A-Z0-9-]+)\s+SECTION\.',
                r'^[0-9]{6}\s+([A-Z0-9-]+)\s+SECTION\s+(\d+)\.',
            ],
            'paragraph': [
                r'^[0-9]{6}\s+([A-Z0-9-]+)\s*\.\s*$',
                r'^[0-9]{6}\s+([A-Z0-9-]+)\s*\.\s*[^.]',
            ],
            'statement': [
                r'^[0-9]{6}\s+(DISPLAY\s+)',
                r'^[0-9]{6}\s+(MOVE\s+)',
                r'^[0-9]{6}\s+(IF\s+)',
                r'^[0-9]{6}\s+(PERFORM\s+)',
                r'^[0-9]{6}\s+(READ\s+)',
                r'^[0-9]{6}\s+(WRITE\s+)',
                r'^[0-9]{6}\s+(COMPUTE\s+)',
                r'^[0-9]{6}\s+(ADD\s+)',
                r'^[0-9]{6}\s+(SUBTRACT\s+)',
                r'^[0-9]{6}\s+(MULTIPLY\s+)',
                r'^[0-9]{6}\s+(DIVIDE\s+)',
                r'^[0-9]{6}\s+(ACCEPT\s+)',
                r'^[0-9]{6}\s+(CALL\s+)',
                r'^[0-9]{6}\s+(GO\s+TO\s+)',
                r'^[0-9]{6}\s+(STOP\s+)',
                r'^[0-9]{6}\s+(EXIT\s+)',
            ],
            'data_item': [
                r'^[0-9]{6}\s+(\d+)\s+([A-Z0-9-]+)',
                r'^[0-9]{6}\s+(\d+)\s+([A-Z0-9-]+)\s+([A-Z0-9-]+)',
            ],
            'file': [
                r'^[0-9]{6}\s+(FD\s+([A-Z0-9-]+))',
                r'^[0-9]{6}\s+(SELECT\s+([A-Z0-9-]+))',
            ]
        }

    def analyze_cobol_file(self, file_path: str) -> ParsingAnalysis:
        """
        Analyze a COBOL file and identify parsing discrepancies.
        """
        logger.info(f"Analyzing COBOL file: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {e}")
            return None

        # Extract entities using enhanced patterns
        self.extract_entities(content)
        
        # Count entities by type
        actual_counts = self.count_entities()
        
        # Expected counts (from your analysis)
        expected_counts = {
            'programs': 1,
            'divisions': 4,
            'sections': 16,
            'paragraphs': 261,
            'statements': 316
        }
        
        # Analyze discrepancies
        discrepancies = {}
        missing_entities = []
        extra_entities = []
        
        for entity_type, expected in expected_counts.items():
            actual = actual_counts.get(entity_type, 0)
            discrepancies[entity_type] = expected - actual
            
            if expected > actual:
                missing_entities.append(f"{entity_type}: missing {expected - actual}")
            elif actual > expected:
                extra_entities.append(f"{entity_type}: extra {actual - expected}")
        
        # Generate recommendations
        recommendations = self.generate_recommendations(discrepancies, actual_counts)
        
        return ParsingAnalysis(
            expected_counts=expected_counts,
            actual_counts=actual_counts,
            missing_entities=missing_entities,
            extra_entities=extra_entities,
            discrepancies=discrepancies,
            recommendations=recommendations
        )

    def extract_entities(self, content: str) -> None:
        """
        Extract all COBOL entities from file content using enhanced patterns.
        """
        lines = content.split('\n')
        current_division = None
        current_section = None
        
        for line_num, line in enumerate(lines, 1):
            line = line.strip()
            if not line:
                continue
            
            # Extract line number (first 6 characters)
            line_number = line[:6].strip() if len(line) >= 6 else ""
            code_content = line[6:].strip() if len(line) >= 6 else line
            
            # Extract programs
            self._extract_programs(code_content, line_num)
            
            # Extract divisions
            division = self._extract_divisions(code_content, line_num)
            if division:
                current_division = division.name
            
            # Extract sections
            section = self._extract_sections(code_content, line_num, current_division)
            if section:
                current_section = section.name
            
            # Extract paragraphs
            self._extract_paragraphs(code_content, line_num, current_division, current_section)
            
            # Extract statements
            self._extract_statements(code_content, line_num, current_division, current_section)
            
            # Extract data items
            self._extract_data_items(code_content, line_num, current_division)
            
            # Extract files
            self._extract_files(code_content, line_num, current_division)

    def _extract_programs(self, content: str, line_num: int) -> None:
        """Extract program entities."""
        patterns = self.entity_patterns['program']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(2) if len(match.groups()) > 1 else match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.PROGRAM,
                    line_number=line_num,
                    content=content
                )
                self.entities.append(entity)
                break

    def _extract_divisions(self, content: str, line_num: int) -> COBOLEntity:
        """Extract division entities."""
        patterns = self.entity_patterns['division']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.DIVISION,
                    line_number=line_num,
                    content=content
                )
                self.entities.append(entity)
                return entity
        return None

    def _extract_sections(self, content: str, line_num: int, parent_division: str) -> COBOLEntity:
        """Extract section entities."""
        patterns = self.entity_patterns['section']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.SECTION,
                    line_number=line_num,
                    content=content,
                    parent=parent_division
                )
                self.entities.append(entity)
                return entity
        return None

    def _extract_paragraphs(self, content: str, line_num: int, parent_division: str, parent_section: str) -> None:
        """Extract paragraph entities with enhanced logic."""
        patterns = self.entity_patterns['paragraph']
        
        # Skip if it's already identified as a division, section, or statement
        if any(re.search(pattern, content, re.IGNORECASE) for pattern in 
               self.entity_patterns['division'] + self.entity_patterns['section'] + self.entity_patterns['statement']):
            return
        
        # Check for paragraph patterns
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(1)
                
                # Additional validation to ensure it's a paragraph
                if self._is_valid_paragraph(name, content):
                    entity = COBOLEntity(
                        name=name,
                        entity_type=COBOLEntityType.PARAGRAPH,
                        line_number=line_num,
                        content=content,
                        parent=parent_section or parent_division
                    )
                    self.entities.append(entity)
                break

    def _is_valid_paragraph(self, name: str, content: str) -> bool:
        """Validate if a name is actually a paragraph definition."""
        # Paragraphs should not be reserved words
        reserved_words = {
            'PROGRAM-ID', 'IDENTIFICATION', 'DIVISION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'WORKING-STORAGE', 'LINKAGE', 'FILE', 'SECTION', 'FD', 'SELECT', 'ASSIGN',
            'FILE-CONTROL', 'INPUT-OUTPUT', 'PIC', 'PICTURE', 'VALUE', 'OCCURS',
            'REDEFINES', 'USAGE', 'COMP', 'COMPUTATIONAL', 'INDEXED', 'KEY'
        }
        
        if name.upper() in reserved_words:
            return False
        
        # Paragraphs typically end with a period and have no additional content on the same line
        # or have minimal content
        if content.count('.') > 1:
            return False
            
        return True

    def _extract_statements(self, content: str, line_num: int, parent_division: str, parent_section: str) -> None:
        """Extract statement entities."""
        patterns = self.entity_patterns['statement']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                # Extract the statement type
                statement_type = match.group(1).strip().upper()
                entity = COBOLEntity(
                    name=f"{statement_type}_{line_num}",
                    entity_type=COBOLEntityType.STATEMENT,
                    line_number=line_num,
                    content=content,
                    parent=parent_section or parent_division
                )
                self.entities.append(entity)
                break

    def _extract_data_items(self, content: str, line_num: int, parent_division: str) -> None:
        """Extract data item entities."""
        patterns = self.entity_patterns['data_item']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(2) if len(match.groups()) > 1 else match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.DATA_ITEM,
                    line_number=line_num,
                    content=content,
                    parent=parent_division
                )
                self.entities.append(entity)
                break

    def _extract_files(self, content: str, line_num: int, parent_division: str) -> None:
        """Extract file entities."""
        patterns = self.entity_patterns['file']
        for pattern in patterns:
            match = re.search(pattern, content, re.IGNORECASE)
            if match:
                name = match.group(2) if len(match.groups()) > 1 else match.group(1)
                entity = COBOLEntity(
                    name=name,
                    entity_type=COBOLEntityType.FILE,
                    line_number=line_num,
                    content=content,
                    parent=parent_division
                )
                self.entities.append(entity)
                break

    def count_entities(self) -> Dict[str, int]:
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

    def generate_recommendations(self, discrepancies: Dict[str, int], actual_counts: Dict[str, int]) -> List[str]:
        """Generate recommendations for fixing parsing issues."""
        recommendations = []
        
        if discrepancies.get('divisions', 0) > 0:
            recommendations.append(
                "Missing Divisions: Ensure parser extracts all 4 main divisions "
                "(IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE) as separate entities"
            )
        
        if discrepancies.get('sections', 0) > 0:
            recommendations.append(
                "Missing Sections: Improve section detection regex patterns to capture "
                "all section definitions, including those with numbers"
            )
        
        if discrepancies.get('paragraphs', 0) > 0:
            recommendations.append(
                "Missing Paragraphs: Review paragraph validation logic and ensure "
                "all paragraph definitions are captured, not just PERFORM statements"
            )
        
        if discrepancies.get('statements', 0) > 0:
            recommendations.append(
                "Missing Statements: Add comprehensive statement extraction for all "
                "COBOL verbs (DISPLAY, MOVE, IF, PERFORM, etc.)"
            )
        
        # Additional recommendations
        recommendations.extend([
            "Implement hierarchical parent-child relationships between entities",
            "Add line number preservation for better debugging",
            "Create entity validation to avoid duplicates",
            "Add support for nested paragraph structures",
            "Implement statement-level granularity for detailed analysis"
        ])
        
        return recommendations

    def generate_enhanced_parser_code(self) -> str:
        """Generate enhanced COBOL parser code with fixes."""
        return '''
# Enhanced COBOL Parser with Improved Entity Extraction

class EnhancedCOBOLParser:
    def __init__(self):
        # Enhanced regex patterns
        self.patterns = {
            'program': r'^[0-9]{6}\\s+(PROGRAM-ID\\.\\s+([A-Z0-9-]+))',
            'division': r'^[0-9]{6}\\s+(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\\s+DIVISION\\.',
            'section': r'^[0-9]{6}\\s+([A-Z0-9-]+)\\s+SECTION(?:\\.|\\s+\\d+\\.)',
            'paragraph': r'^[0-9]{6}\\s+([A-Z0-9-]+)\\s*\\.(?:\\s*$|\\s+[^A-Z])',
            'statement': r'^[0-9]{6}\\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|ACCEPT|CALL|GO\\s+TO|STOP|EXIT)\\s+',
        }
        
        # Validation rules
        self.reserved_words = {
            'PROGRAM-ID', 'IDENTIFICATION', 'DIVISION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'WORKING-STORAGE', 'LINKAGE', 'FILE', 'SECTION', 'FD', 'SELECT', 'ASSIGN'
        }
    
    def extract_all_entities(self, content):
        """Extract all COBOL entities with improved accuracy."""
        entities = []
        lines = content.split('\\n')
        
        for line_num, line in enumerate(lines, 1):
            if not line.strip():
                continue
                
            # Remove line numbers
            code_content = line[6:].strip() if len(line) >= 6 else line
            
            # Extract all entity types
            entities.extend(self._extract_entity_type(code_content, line_num, 'program'))
            entities.extend(self._extract_entity_type(code_content, line_num, 'division'))
            entities.extend(self._extract_entity_type(code_content, line_num, 'section'))
            entities.extend(self._extract_entity_type(code_content, line_num, 'paragraph'))
            entities.extend(self._extract_entity_type(code_content, line_num, 'statement'))
        
        return entities
    
    def _extract_entity_type(self, content, line_num, entity_type):
        """Extract specific entity type with validation."""
        entities = []
        pattern = self.patterns.get(entity_type)
        
        if not pattern:
            return entities
            
        match = re.search(pattern, content, re.IGNORECASE)
        if match:
            if entity_type == 'paragraph':
                # Additional validation for paragraphs
                if not self._is_valid_paragraph(match.group(1), content):
                    return entities
            
            entities.append({
                'type': entity_type,
                'name': match.group(1) if entity_type != 'program' else match.group(2),
                'line_number': line_num,
                'content': content
            })
        
        return entities
    
    def _is_valid_paragraph(self, name, content):
        """Enhanced paragraph validation."""
        # Check against reserved words
        if name.upper() in self.reserved_words:
            return False
        
        # Paragraphs should not have multiple periods or complex content
        if content.count('.') > 1:
            return False
        
        # Skip if it's actually a section or division
        if any(re.search(pattern, content, re.IGNORECASE) for pattern in 
               [self.patterns['section'], self.patterns['division']]):
            return False
        
        return True
'''

    def save_analysis_report(self, analysis: ParsingAnalysis, output_file: str) -> None:
        """Save analysis report to file."""
        report = {
            'summary': {
                'expected_counts': analysis.expected_counts,
                'actual_counts': analysis.actual_counts,
                'discrepancies': analysis.discrepancies
            },
            'missing_entities': analysis.missing_entities,
            'extra_entities': analysis.extra_entities,
            'recommendations': analysis.recommendations,
            'detailed_entities': [
                {
                    'name': entity.name,
                    'type': entity.entity_type.value,
                    'line_number': entity.line_number,
                    'parent': entity.parent
                }
                for entity in self.entities
            ]
        }
        
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        logger.info(f"Analysis report saved to {output_file}")

def main():
    """Main function to run the COBOL analysis agent."""
    agent = COBOLAnalysisAgent()
    
    # Example usage (replace with actual file path)
    file_path = "vasu_fraud_management_cobol_reformatted.cbl"
    
    try:
        analysis = agent.analyze_cobol_file(file_path)
        if analysis:
            print("\n=== COBOL Parsing Analysis Report ===")
            print(f"Expected: {analysis.expected_counts}")
            print(f"Actual: {analysis.actual_counts}")
            print(f"Discrepancies: {analysis.discrepancies}")
            
            print("\n=== Missing Entities ===")
            for missing in analysis.missing_entities:
                print(f"- {missing}")
            
            print("\n=== Recommendations ===")
            for i, rec in enumerate(analysis.recommendations, 1):
                print(f"{i}. {rec}")
            
            # Save report
            agent.save_analysis_report(analysis, "cobol_analysis_report.json")
            
            # Generate enhanced parser code
            enhanced_code = agent.generate_enhanced_parser_code()
            with open("enhanced_cobol_parser.py", "w") as f:
                f.write(enhanced_code)
            
            print(f"\nEnhanced parser code saved to enhanced_cobol_parser.py")
            
    except Exception as e:
        logger.error(f"Analysis failed: {e}")

if __name__ == "__main__":
    main()