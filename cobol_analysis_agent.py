#!/usr/bin/env python3
"""
COBOL Parsing Analysis Agent

This agent analyzes COBOL files to identify discrepancies between expected
hierarchical structure and actual parsed entities. It provides detailed
analysis and fixes for missing entity extraction.
"""

import re
import logging
from typing import Dict, List, Tuple, Set, Optional
from dataclasses import dataclass
from pathlib import Path
import json
from collections import defaultdict, Counter

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class COBOLStructure:
    """Represents the expected COBOL hierarchical structure"""
    programs: int = 0
    divisions: int = 0
    sections: int = 0
    paragraphs: int = 0
    statements: int = 0

@dataclass
class ParsedEntity:
    """Represents a parsed COBOL entity"""
    name: str
    type: str
    line_number: int
    content: str
    parent: Optional[str] = None

@dataclass
class AnalysisResult:
    """Results of COBOL structure analysis"""
    expected: COBOLStructure
    actual: Dict[str, int]
    discrepancies: Dict[str, int]
    missing_entities: List[str]
    extra_entities: List[str]
    recommendations: List[str]

class COBOLAnalysisAgent:
    """Agent for analyzing COBOL file structure and parsing discrepancies"""
    
    def __init__(self):
        self.division_patterns = [
            r'^\s*\d+\s+(IDENTIFICATION\s+DIVISION)',
            r'^\s*\d+\s+(ENVIRONMENT\s+DIVISION)',
            r'^\s*\d+\s+(DATA\s+DIVISION)',
            r'^\s*\d+\s+(PROCEDURE\s+DIVISION)'
        ]
        
        self.section_pattern = r'^\s*\d+\s+([A-Z0-9-]+)\s+SECTION\s*\.'
        self.paragraph_pattern = r'^\s*\d+\s+([A-Z0-9-]+)\s*\.'
        
        # Statement patterns for common COBOL statements
        self.statement_patterns = [
            r'^\s*\d+\s+(DISPLAY\s+)',
            r'^\s*\d+\s+(MOVE\s+)',
            r'^\s*\d+\s+(IF\s+)',
            r'^\s*\d+\s+(PERFORM\s+)',
            r'^\s*\d+\s+(OPEN\s+)',
            r'^\s*\d+\s+(CLOSE\s+)',
            r'^\s*\d+\s+(READ\s+)',
            r'^\s*\d+\s+(WRITE\s+)',
            r'^\s*\d+\s+(ACCEPT\s+)',
            r'^\s*\d+\s+(COMPUTE\s+)',
            r'^\s*\d+\s+(ADD\s+)',
            r'^\s*\d+\s+(SUBTRACT\s+)',
            r'^\s*\d+\s+(MULTIPLY\s+)',
            r'^\s*\d+\s+(DIVIDE\s+)',
            r'^\s*\d+\s+(STOP\s+)',
            r'^\s*\d+\s+(EXIT\s+)',
            r'^\s*\d+\s+(CALL\s+)',
            r'^\s*\d+\s+(GO\s+TO\s+)',
            r'^\s*\d+\s+(END-IF\s*)',
            r'^\s*\d+\s+(END-PERFORM\s*)',
            r'^\s*\d+\s+(END-EVALUATE\s*)',
            r'^\s*\d+\s+(EVALUATE\s+)',
            r'^\s*\d+\s+(WHEN\s+)',
            r'^\s*\d+\s+(INITIALIZE\s+)',
            r'^\s*\d+\s+(STRING\s+)',
            r'^\s*\d+\s+(UNSTRING\s+)',
            r'^\s*\d+\s+(INSPECT\s+)',
            r'^\s*\d+\s+(SET\s+)',
            r'^\s*\d+\s+(SEARCH\s+)',
            r'^\s*\d+\s+(SORT\s+)',
            r'^\s*\d+\s+(MERGE\s+)'
        ]
        
        self.program_pattern = r'^\s*\d+\s+PROGRAM-ID\s+\.\s+([A-Z0-9-]+)'
        
    def analyze_cobol_file(self, file_path: str) -> AnalysisResult:
        """Analyze a COBOL file and return detailed structure analysis"""
        logger.info(f"Analyzing COBOL file: {file_path}")
        
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                content = file.read()
                lines = content.split('\n')
        except Exception as e:
            logger.error(f"Error reading file {file_path}: {e}")
            raise
        
        # Extract expected structure from file
        expected = self._extract_expected_structure(lines)
        
        # Parse actual entities
        actual_entities = self._parse_entities(lines)
        
        # Count actual entities
        actual_counts = self._count_entities(actual_entities)
        
        # Calculate discrepancies
        discrepancies = self._calculate_discrepancies(expected, actual_counts)
        
        # Identify missing and extra entities
        missing, extra = self._identify_missing_extra_entities(expected, actual_counts)
        
        # Generate recommendations
        recommendations = self._generate_recommendations(discrepancies, missing, extra)
        
        return AnalysisResult(
            expected=expected,
            actual=actual_counts,
            discrepancies=discrepancies,
            missing_entities=missing,
            extra_entities=extra,
            recommendations=recommendations
        )
    
    def _extract_expected_structure(self, lines: List[str]) -> COBOLStructure:
        """Extract expected structure from COBOL file"""
        structure = COBOLStructure()
        
        # Count programs
        programs = set()
        for line in lines:
            match = re.search(self.program_pattern, line, re.IGNORECASE)
            if match:
                programs.add(match.group(1))
        structure.programs = len(programs)
        
        # Count divisions
        divisions = set()
        for pattern in self.division_patterns:
            for line in lines:
                match = re.search(pattern, line, re.IGNORECASE)
                if match:
                    divisions.add(match.group(1).upper())
        structure.divisions = len(divisions)
        
        # Count sections
        sections = set()
        for line in lines:
            match = re.search(self.section_pattern, line, re.IGNORECASE)
            if match:
                sections.add(match.group(1).upper())
        structure.sections = len(sections)
        
        # Count paragraphs
        paragraphs = set()
        for line in lines:
            match = re.search(self.paragraph_pattern, line, re.IGNORECASE)
            if match:
                # Exclude sections from paragraph count
                if 'SECTION' not in line.upper():
                    paragraphs.add(match.group(1).upper())
        structure.paragraphs = len(paragraphs)
        
        # Count statements
        statements = 0
        for pattern in self.statement_patterns:
            for line in lines:
                if re.search(pattern, line, re.IGNORECASE):
                    statements += 1
        structure.statements = statements
        
        return structure
    
    def _parse_entities(self, lines: List[str]) -> List[ParsedEntity]:
        """Parse all entities from COBOL file"""
        entities = []
        
        for line_num, line in enumerate(lines, 1):
            # Parse programs
            match = re.search(self.program_pattern, line, re.IGNORECASE)
            if match:
                entities.append(ParsedEntity(
                    name=match.group(1),
                    type='PROGRAM',
                    line_number=line_num,
                    content=line.strip()
                ))
            
            # Parse divisions
            for pattern in self.division_patterns:
                match = re.search(pattern, line, re.IGNORECASE)
                if match:
                    entities.append(ParsedEntity(
                        name=match.group(1).upper(),
                        type='DIVISION',
                        line_number=line_num,
                        content=line.strip()
                    ))
                    break
            
            # Parse sections
            match = re.search(self.section_pattern, line, re.IGNORECASE)
            if match:
                entities.append(ParsedEntity(
                    name=match.group(1).upper(),
                    type='SECTION',
                    line_number=line_num,
                    content=line.strip()
                ))
            
            # Parse paragraphs
            match = re.search(self.paragraph_pattern, line, re.IGNORECASE)
            if match:
                # Exclude sections from paragraphs
                if 'SECTION' not in line.upper():
                    entities.append(ParsedEntity(
                        name=match.group(1).upper(),
                        type='PARAGRAPH',
                        line_number=line_num,
                        content=line.strip()
                    ))
            
            # Parse statements
            for pattern in self.statement_patterns:
                match = re.search(pattern, line, re.IGNORECASE)
                if match:
                    entities.append(ParsedEntity(
                        name=match.group(1).strip().upper(),
                        type='STATEMENT',
                        line_number=line_num,
                        content=line.strip()
                    ))
                    break
        
        return entities
    
    def _count_entities(self, entities: List[ParsedEntity]) -> Dict[str, int]:
        """Count entities by type"""
        counts = defaultdict(int)
        for entity in entities:
            counts[entity.type] += 1
        return dict(counts)
    
    def _calculate_discrepancies(self, expected: COBOLStructure, actual: Dict[str, int]) -> Dict[str, int]:
        """Calculate discrepancies between expected and actual counts"""
        expected_dict = {
            'PROGRAM': expected.programs,
            'DIVISION': expected.divisions,
            'SECTION': expected.sections,
            'PARAGRAPH': expected.paragraphs,
            'STATEMENT': expected.statements
        }
        
        discrepancies = {}
        for entity_type in expected_dict:
            expected_count = expected_dict[entity_type]
            actual_count = actual.get(entity_type, 0)
            discrepancies[entity_type] = actual_count - expected_count
        
        return discrepancies
    
    def _identify_missing_extra_entities(self, expected: COBOLStructure, actual: Dict[str, int]) -> Tuple[List[str], List[str]]:
        """Identify missing and extra entities"""
        expected_dict = {
            'PROGRAM': expected.programs,
            'DIVISION': expected.divisions,
            'SECTION': expected.sections,
            'PARAGRAPH': expected.paragraphs,
            'STATEMENT': expected.statements
        }
        
        missing = []
        extra = []
        
        for entity_type in expected_dict:
            expected_count = expected_dict[entity_type]
            actual_count = actual.get(entity_type, 0)
            
            if actual_count < expected_count:
                missing.append(f"{entity_type}: Expected {expected_count}, found {actual_count}")
            elif actual_count > expected_count:
                extra.append(f"{entity_type}: Expected {expected_count}, found {actual_count}")
        
        return missing, extra
    
    def _generate_recommendations(self, discrepancies: Dict[str, int], missing: List[str], extra: List[str]) -> List[str]:
        """Generate recommendations for fixing parsing issues"""
        recommendations = []
        
        if discrepancies.get('DIVISION', 0) < 0:
            recommendations.append("Enhance division extraction - ensure all 4 standard COBOL divisions are captured")
        
        if discrepancies.get('SECTION', 0) < 0:
            recommendations.append("Improve section detection - check regex patterns for section identification")
        
        if discrepancies.get('PARAGRAPH', 0) < 0:
            recommendations.append("Fix paragraph extraction - review filtering logic and regex patterns")
        
        if discrepancies.get('STATEMENT', 0) < 0:
            recommendations.append("Add statement-level parsing - implement comprehensive statement extraction")
        
        if missing:
            recommendations.append("Review entity filtering - ensure no valid entities are being excluded")
        
        if extra:
            recommendations.append("Review duplicate detection - check for over-counting of entities")
        
        recommendations.append("Implement hierarchical relationship tracking between divisions, sections, and paragraphs")
        recommendations.append("Add validation system to compare expected vs actual entity counts")
        
        return recommendations
    
    def generate_enhanced_parser_config(self, analysis_result: AnalysisResult) -> Dict:
        """Generate configuration for enhanced COBOL parser"""
        config = {
            "extraction_rules": {
                "programs": {
                    "enabled": True,
                    "pattern": self.program_pattern,
                    "description": "Extract PROGRAM-ID declarations"
                },
                "divisions": {
                    "enabled": True,
                    "patterns": self.division_patterns,
                    "description": "Extract all 4 standard COBOL divisions"
                },
                "sections": {
                    "enabled": True,
                    "pattern": self.section_pattern,
                    "description": "Extract SECTION declarations"
                },
                "paragraphs": {
                    "enabled": True,
                    "pattern": self.paragraph_pattern,
                    "description": "Extract paragraph declarations",
                    "exclude_sections": True
                },
                "statements": {
                    "enabled": True,
                    "patterns": self.statement_patterns,
                    "description": "Extract individual COBOL statements"
                }
            },
            "validation": {
                "enable_count_validation": True,
                "expected_counts": {
                    "programs": analysis_result.expected.programs,
                    "divisions": analysis_result.expected.divisions,
                    "sections": analysis_result.expected.sections,
                    "paragraphs": analysis_result.expected.paragraphs,
                    "statements": analysis_result.expected.statements
                }
            },
            "relationships": {
                "enable_hierarchical_tracking": True,
                "parent_child_mapping": {
                    "PROGRAM": ["DIVISION"],
                    "DIVISION": ["SECTION"],
                    "SECTION": ["PARAGRAPH"],
                    "PARAGRAPH": ["STATEMENT"]
                }
            }
        }
        
        return config
    
    def save_analysis_report(self, analysis_result: AnalysisResult, output_path: str):
        """Save analysis results to JSON file"""
        report = {
            "summary": {
                "expected": {
                    "programs": analysis_result.expected.programs,
                    "divisions": analysis_result.expected.divisions,
                    "sections": analysis_result.expected.sections,
                    "paragraphs": analysis_result.expected.paragraphs,
                    "statements": analysis_result.expected.statements
                },
                "actual": analysis_result.actual,
                "discrepancies": analysis_result.discrepancies
            },
            "issues": {
                "missing_entities": analysis_result.missing_entities,
                "extra_entities": analysis_result.extra_entities
            },
            "recommendations": analysis_result.recommendations,
            "enhanced_parser_config": self.generate_enhanced_parser_config(analysis_result)
        }
        
        with open(output_path, 'w') as f:
            json.dump(report, f, indent=2)
        
        logger.info(f"Analysis report saved to {output_path}")

def main():
    """Main function for testing the COBOL analysis agent"""
    agent = COBOLAnalysisAgent()
    
    # Example usage - replace with actual COBOL file path
    cobol_file = "vasu_fraud_management_cobol_reformatted.cbl"
    
    if Path(cobol_file).exists():
        try:
            result = agent.analyze_cobol_file(cobol_file)
            
            print("=== COBOL Structure Analysis ===")
            print(f"Expected: {result.expected}")
            print(f"Actual: {result.actual}")
            print(f"Discrepancies: {result.discrepancies}")
            print(f"Missing: {result.missing_entities}")
            print(f"Extra: {result.extra_entities}")
            print("\nRecommendations:")
            for rec in result.recommendations:
                print(f"- {rec}")
            
            # Save report
            agent.save_analysis_report(result, "cobol_analysis_report.json")
            
        except Exception as e:
            logger.error(f"Analysis failed: {e}")
    else:
        print(f"COBOL file not found: {cobol_file}")

if __name__ == "__main__":
    main()