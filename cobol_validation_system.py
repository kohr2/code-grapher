#!/usr/bin/env python3
"""
COBOL Validation System

This system validates COBOL parsing results against expected hierarchical structure
and provides detailed analysis of discrepancies.

Expected Structure:
- Programs: 1
- Divisions: 4 (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Sections: 16
- Paragraphs: 261
- Statements: 316
"""

import json
import logging
from typing import Dict, List, Tuple, Set
from dataclasses import dataclass
from enum import Enum
import re

logger = logging.getLogger(__name__)

@dataclass
class ValidationResult:
    entity_type: str
    expected: int
    actual: int
    difference: int
    accuracy_percentage: float
    issues: List[str]
    recommendations: List[str]

class COBOLValidationSystem:
    """
    System for validating COBOL parsing results against expected structure.
    """
    
    def __init__(self):
        # Expected structure based on analysis
        self.expected_structure = {
            'programs': 1,
            'divisions': 4,
            'sections': 16,
            'paragraphs': 261,
            'statements': 316
        }
        
        # Division names that should be found
        self.expected_divisions = [
            'IDENTIFICATION DIVISION',
            'ENVIRONMENT DIVISION', 
            'DATA DIVISION',
            'PROCEDURE DIVISION'
        ]
        
        # Common issues and their solutions
        self.issue_patterns = {
            'missing_divisions': {
                'description': 'Missing division entities',
                'solution': 'Ensure parser extracts all 4 main divisions as separate entities'
            },
            'missing_sections': {
                'description': 'Missing section entities', 
                'solution': 'Improve section detection regex patterns'
            },
            'missing_paragraphs': {
                'description': 'Missing paragraph entities',
                'solution': 'Review paragraph validation logic and regex patterns'
            },
            'missing_statements': {
                'description': 'Missing statement entities',
                'solution': 'Add comprehensive statement extraction for all COBOL verbs'
            },
            'false_positives': {
                'description': 'Incorrect entity classifications',
                'solution': 'Improve validation logic to avoid false positives'
            }
        }

    def validate_parsing_results(self, actual_counts: Dict[str, int], entities: List[Dict]) -> Dict[str, ValidationResult]:
        """
        Validate parsing results against expected structure.
        """
        logger.info("Validating COBOL parsing results")
        
        validation_results = {}
        
        for entity_type, expected_count in self.expected_structure.items():
            actual_count = actual_counts.get(entity_type, 0)
            difference = expected_count - actual_count
            
            # Calculate accuracy percentage
            accuracy = (actual_count / expected_count * 100) if expected_count > 0 else 0
            
            # Identify issues
            issues = self._identify_issues(entity_type, expected_count, actual_count, entities)
            
            # Generate recommendations
            recommendations = self._generate_recommendations(entity_type, issues, difference)
            
            validation_results[entity_type] = ValidationResult(
                entity_type=entity_type,
                expected=expected_count,
                actual=actual_count,
                difference=difference,
                accuracy_percentage=accuracy,
                issues=issues,
                recommendations=recommendations
            )
        
        return validation_results

    def _identify_issues(self, entity_type: str, expected: int, actual: int, entities: List[Dict]) -> List[str]:
        """Identify specific issues with entity extraction."""
        issues = []
        
        if actual < expected:
            issues.append(f"Missing {expected - actual} {entity_type}")
            
            # Specific issues for each entity type
            if entity_type == 'divisions':
                issues.extend(self._identify_missing_divisions(entities))
            elif entity_type == 'sections':
                issues.extend(self._identify_missing_sections(entities))
            elif entity_type == 'paragraphs':
                issues.extend(self._identify_missing_paragraphs(entities))
            elif entity_type == 'statements':
                issues.extend(self._identify_missing_statements(entities))
        
        elif actual > expected:
            issues.append(f"Extra {actual - expected} {entity_type} (possible false positives)")
        
        return issues

    def _identify_missing_divisions(self, entities: List[Dict]) -> List[str]:
        """Identify which specific divisions are missing."""
        found_divisions = set()
        for entity in entities:
            if hasattr(entity, 'entity_type'):
                if entity.entity_type.value == 'DIVISION':
                    found_divisions.add(entity.name)
            elif isinstance(entity, dict) and entity.get('type') == 'DIVISION':
                found_divisions.add(entity['name'])
        
        missing_divisions = set(self.expected_divisions) - found_divisions
        issues = []
        
        for division in missing_divisions:
            issues.append(f"Missing division: {division}")
        
        return issues

    def _identify_missing_sections(self, entities: List[Dict]) -> List[str]:
        """Identify potential missing sections."""
        issues = []
        # Handle both dict and COBOLEntity objects
        sections = []
        for e in entities:
            if hasattr(e, 'entity_type'):
                if e.entity_type.value == 'SECTION':
                    sections.append(e)
            elif isinstance(e, dict) and e.get('type') == 'SECTION':
                sections.append(e)
        
        if len(sections) < 16:
            issues.append(f"Only found {len(sections)} sections, expected 16")
            issues.append("Check section detection regex patterns")
            issues.append("Verify section naming conventions")
        
        return issues

    def _identify_missing_paragraphs(self, entities: List[Dict]) -> List[str]:
        """Identify potential missing paragraphs."""
        issues = []
        # Handle both dict and COBOLEntity objects
        paragraphs = []
        for e in entities:
            if hasattr(e, 'entity_type'):
                if e.entity_type.value == 'PARAGRAPH':
                    paragraphs.append(e)
            elif isinstance(e, dict) and e.get('type') == 'PARAGRAPH':
                paragraphs.append(e)
        
        if len(paragraphs) < 261:
            issues.append(f"Only found {len(paragraphs)} paragraphs, expected 261")
            issues.append("Check paragraph validation logic")
            issues.append("Review reserved word filtering")
            issues.append("Verify paragraph naming patterns")
        
        # Check for common paragraph patterns that might be missed
        potential_paragraphs = []
        for entity in entities:
            if hasattr(entity, 'entity_type'):
                if entity.entity_type.value == 'STATEMENT':
                    content = entity.content
                    if 'PERFORM' in content.upper():
                        # Extract paragraph name from PERFORM statement
                        match = re.search(r'PERFORM\s+([A-Z0-9-]+)', content.upper())
                        if match:
                            potential_paragraphs.append(match.group(1))
            elif isinstance(entity, dict) and entity.get('type') == 'STATEMENT':
                content = entity.get('content', '')
                if 'PERFORM' in content.upper():
                    # Extract paragraph name from PERFORM statement
                    match = re.search(r'PERFORM\s+([A-Z0-9-]+)', content.upper())
                    if match:
                        potential_paragraphs.append(match.group(1))
        
        if potential_paragraphs:
            issues.append(f"Found {len(set(potential_paragraphs))} unique paragraphs referenced in PERFORM statements")
        
        return issues

    def _identify_missing_statements(self, entities: List[Dict]) -> List[str]:
        """Identify potential missing statements."""
        issues = []
        # Handle both dict and COBOLEntity objects
        statements = []
        for e in entities:
            if hasattr(e, 'entity_type'):
                if e.entity_type.value == 'STATEMENT':
                    statements.append(e)
            elif isinstance(e, dict) and e.get('type') == 'STATEMENT':
                statements.append(e)
        
        if len(statements) < 316:
            issues.append(f"Only found {len(statements)} statements, expected 316")
            issues.append("Check statement detection regex patterns")
            issues.append("Verify all COBOL verbs are covered")
        
        # Check for common statement types
        statement_types = set()
        for entity in entities:
            if hasattr(entity, 'entity_type'):
                if entity.entity_type.value == 'STATEMENT':
                    name = entity.name
                    if '_' in name:
                        stmt_type = name.split('_')[0]
                        statement_types.add(stmt_type)
            elif isinstance(entity, dict) and entity.get('type') == 'STATEMENT':
                name = entity.get('name', '')
                if '_' in name:
                    stmt_type = name.split('_')[0]
                    statement_types.add(stmt_type)
        
        expected_stmt_types = {
            'DISPLAY', 'MOVE', 'IF', 'PERFORM', 'READ', 'WRITE', 'COMPUTE',
            'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'ACCEPT', 'CALL'
        }
        
        missing_stmt_types = expected_stmt_types - statement_types
        if missing_stmt_types:
            issues.append(f"Missing statement types: {missing_stmt_types}")
        
        return issues

    def _generate_recommendations(self, entity_type: str, issues: List[str], difference: int) -> List[str]:
        """Generate specific recommendations for fixing issues."""
        recommendations = []
        
        if difference > 0:  # Missing entities
            if entity_type == 'divisions':
                recommendations.extend([
                    "Ensure parser extracts all 4 main divisions as separate entities",
                    "Add specific regex patterns for each division type",
                    "Verify division detection logic handles all variations"
                ])
            elif entity_type == 'sections':
                recommendations.extend([
                    "Improve section detection regex patterns",
                    "Add support for numbered sections (e.g., SECTION 1)",
                    "Verify section naming conventions are properly handled"
                ])
            elif entity_type == 'paragraphs':
                recommendations.extend([
                    "Review paragraph validation logic to avoid over-filtering",
                    "Check reserved word list for completeness",
                    "Improve paragraph detection regex patterns",
                    "Add support for nested paragraph structures"
                ])
            elif entity_type == 'statements':
                recommendations.extend([
                    "Add comprehensive statement extraction for all COBOL verbs",
                    "Implement statement-level granularity",
                    "Add support for multi-line statements",
                    "Verify statement detection patterns cover all variations"
                ])
        
        elif difference < 0:  # Extra entities (false positives)
            recommendations.extend([
                "Review entity classification logic",
                "Improve validation to avoid false positives",
                "Check for duplicate entity detection",
                "Verify entity filtering rules"
            ])
        
        # General recommendations
        recommendations.extend([
            "Implement hierarchical parent-child relationships",
            "Add line number preservation for debugging",
            "Create entity validation to ensure uniqueness",
            "Add support for nested structures"
        ])
        
        return recommendations

    def generate_fix_script(self, validation_results: Dict[str, ValidationResult]) -> str:
        """Generate a script to fix identified issues."""
        script = '''#!/usr/bin/env python3
"""
Auto-generated fix script for COBOL parser issues
"""

import re

class COBOLParserFixes:
    """Fixes for identified COBOL parser issues."""
    
    def __init__(self):
        # Enhanced patterns based on validation results
        self.enhanced_patterns = {
            'division': [
                r'^[0-9]{6}\\s+(IDENTIFICATION\\s+DIVISION)\\.',
                r'^[0-9]{6}\\s+(ENVIRONMENT\\s+DIVISION)\\.', 
                r'^[0-9]{6}\\s+(DATA\\s+DIVISION)\\.',
                r'^[0-9]{6}\\s+(PROCEDURE\\s+DIVISION)\\.',
            ],
            'section': [
                r'^[0-9]{6}\\s+([A-Z0-9-]+)\\s+SECTION\\.\\s*$',
                r'^[0-9]{6}\\s+([A-Z0-9-]+)\\s+SECTION\\s+(\\d+)\\.',
            ],
            'paragraph': [
                r'^[0-9]{6}\\s+([A-Z0-9-]+)\\.\\s*$',
                r'^[0-9]{6}\\s+([A-Z0-9-]+)\\.\\s+[^A-Z]',
            ],
            'statement': [
                r'^[0-9]{6}\\s+(DISPLAY\\s+)',
                r'^[0-9]{6}\\s+(MOVE\\s+)',
                r'^[0-9]{6}\\s+(IF\\s+)',
                r'^[0-9]{6}\\s+(PERFORM\\s+)',
                r'^[0-9]{6}\\s+(READ\\s+)',
                r'^[0-9]{6}\\s+(WRITE\\s+)',
                r'^[0-9]{6}\\s+(COMPUTE\\s+)',
                r'^[0-9]{6}\\s+(ADD\\s+)',
                r'^[0-9]{6}\\s+(SUBTRACT\\s+)',
                r'^[0-9]{6}\\s+(MULTIPLY\\s+)',
                r'^[0-9]{6}\\s+(DIVIDE\\s+)',
                r'^[0-9]{6}\\s+(ACCEPT\\s+)',
                r'^[0-9]{6}\\s+(CALL\\s+)',
            ]
        }
        
        # Reduced reserved words list
        self.reserved_words = {
            'PROGRAM-ID', 'IDENTIFICATION', 'DIVISION', 'ENVIRONMENT', 'DATA', 'PROCEDURE',
            'WORKING-STORAGE', 'LINKAGE', 'FILE', 'SECTION', 'FD', 'SELECT', 'ASSIGN'
        }
    
    def apply_fixes(self, content):
        """Apply all identified fixes to COBOL content."""
        # Implementation of fixes would go here
        pass
'''
        
        return script

    def save_validation_report(self, validation_results: Dict[str, ValidationResult], output_file: str) -> None:
        """Save validation report to file."""
        report = {
            'summary': {
                'total_entities_expected': sum(self.expected_structure.values()),
                'total_entities_found': sum(r.actual for r in validation_results.values()),
                'overall_accuracy': sum(r.accuracy_percentage for r in validation_results.values()) / len(validation_results)
            },
            'validation_results': {
                entity_type: {
                    'expected': result.expected,
                    'actual': result.actual,
                    'difference': result.difference,
                    'accuracy_percentage': result.accuracy_percentage,
                    'issues': result.issues,
                    'recommendations': result.recommendations
                }
                for entity_type, result in validation_results.items()
            },
            'fix_script': self.generate_fix_script(validation_results)
        }
        
        with open(output_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        logger.info(f"Validation report saved to {output_file}")

def main():
    """Main function to demonstrate validation system."""
    validator = COBOLValidationSystem()
    
    # Example validation results
    example_counts = {
        'programs': 1,
        'divisions': 2,  # Missing 2 divisions
        'sections': 8,   # Missing 8 sections
        'paragraphs': 103,  # Missing 158 paragraphs
        'statements': 150   # Missing 166 statements
    }
    
    example_entities = [
        {'name': 'IDENTIFICATION DIVISION', 'type': 'DIVISION'},
        {'name': 'DATA DIVISION', 'type': 'DIVISION'},
        # Missing ENVIRONMENT and PROCEDURE divisions
    ]
    
    validation_results = validator.validate_parsing_results(example_counts, example_entities)
    
    print("\n=== COBOL Validation Report ===")
    for entity_type, result in validation_results.items():
        print(f"\n{entity_type.upper()}:")
        print(f"  Expected: {result.expected}")
        print(f"  Actual: {result.actual}")
        print(f"  Difference: {result.difference}")
        print(f"  Accuracy: {result.accuracy_percentage:.1f}%")
        
        if result.issues:
            print(f"  Issues:")
            for issue in result.issues:
                print(f"    - {issue}")
        
        if result.recommendations:
            print(f"  Recommendations:")
            for rec in result.recommendations[:3]:  # Show first 3
                print(f"    - {rec}")
    
    # Save report
    validator.save_validation_report(validation_results, "cobol_validation_report.json")

if __name__ == "__main__":
    main()