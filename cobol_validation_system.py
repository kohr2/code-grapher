#!/usr/bin/env python3
"""
COBOL Validation System

Comprehensive validation system to compare expected vs actual entity counts
and provide detailed analysis of COBOL parsing discrepancies.
"""

import json
import logging
import re
from pathlib import Path
from typing import Dict, List, Tuple, Optional
from dataclasses import dataclass, asdict
from datetime import datetime
import difflib

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

@dataclass
class ValidationResult:
    """Result of validation analysis"""
    file_path: str
    expected_counts: Dict[str, int]
    actual_counts: Dict[str, int]
    discrepancies: Dict[str, Dict[str, any]]
    accuracy_score: float
    recommendations: List[str]
    timestamp: datetime
    parser_version: str = "enhanced_v1.0"

@dataclass
class ParsingRecommendation:
    """Recommendation for improving parsing"""
    recommendation_id: str
    entity_type: str
    issue_description: str
    suggested_fix: str
    priority: str  # 'low', 'medium', 'high', 'critical'
    estimated_impact: str

class COBOLValidationSystem:
    """Comprehensive validation system for COBOL parsing"""
    
    def __init__(self):
        self.validation_history = []
        self.recommendations_db = []
        
        # Enhanced regex patterns for comprehensive analysis
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
        
        # Known COBOL file patterns and their expected structures
        self.file_patterns = {
            'fraud_management': {
                'expected': {'programs': 1, 'divisions': 4, 'sections': 16, 'paragraphs': 261, 'statements': 316},
                'tolerance': {'programs': 0, 'divisions': 0, 'sections': 2, 'paragraphs': 20, 'statements': 50}
            },
            'banking_system': {
                'expected': {'programs': 1, 'divisions': 4, 'sections': 20, 'paragraphs': 300, 'statements': 400},
                'tolerance': {'programs': 0, 'divisions': 0, 'sections': 5, 'paragraphs': 30, 'statements': 100}
            },
            'default': {
                'expected': {'programs': 1, 'divisions': 4, 'sections': 10, 'paragraphs': 100, 'statements': 200},
                'tolerance': {'programs': 0, 'divisions': 0, 'sections': 3, 'paragraphs': 20, 'statements': 50}
            }
        }
    
    def identify_file_pattern(self, file_path: str) -> str:
        """Identify the pattern type of a COBOL file"""
        filename = Path(file_path).name.lower()
        
        if 'fraud' in filename:
            return 'fraud_management'
        elif 'banking' in filename or 'bank' in filename:
            return 'banking_system'
        else:
            return 'default'
    
    def get_expected_counts(self, file_path: str) -> Dict[str, int]:
        """Get expected entity counts for a file"""
        pattern_type = self.identify_file_pattern(file_path)
        return self.file_patterns[pattern_type]['expected']
    
    def get_tolerance_limits(self, file_path: str) -> Dict[str, int]:
        """Get tolerance limits for entity counts"""
        pattern_type = self.identify_file_pattern(file_path)
        return self.file_patterns[pattern_type]['tolerance']
    
    def analyze_cobol_file_detailed(self, file_path: str) -> Dict[str, any]:
        """Perform detailed analysis of a COBOL file"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                lines = file.readlines()
            
            analysis = {
                'file_info': {
                    'path': file_path,
                    'size_bytes': Path(file_path).stat().st_size,
                    'total_lines': len(lines),
                    'non_comment_lines': 0
                },
                'entities': {
                    'programs': [], 'divisions': [], 'sections': [], 
                    'paragraphs': [], 'statements': [], 'data_items': [], 'files': []
                },
                'counts': {
                    'programs': 0, 'divisions': 0, 'sections': 0,
                    'paragraphs': 0, 'statements': 0, 'data_items': 0, 'files': 0
                },
                'structure': {
                    'nested_levels': 0,
                    'complexity_score': 0,
                    'main_sections': []
                }
            }
            
            current_division = None
            current_section = None
            current_paragraph = None
            nesting_level = 0
            
            for line_num, line in enumerate(lines, 1):
                cleaned_line = self.clean_line(line)
                
                # Skip comments and empty lines
                if not cleaned_line or cleaned_line.startswith('*'):
                    continue
                
                analysis['file_info']['non_comment_lines'] += 1
                
                # Analyze programs
                program_match = self.patterns['program_id'].match(line)
                if program_match:
                    analysis['entities']['programs'].append({
                        'name': program_match.group(1),
                        'line': line_num,
                        'content': cleaned_line
                    })
                    analysis['counts']['programs'] += 1
                
                # Analyze divisions
                division_match = self.patterns['division'].match(line)
                if division_match:
                    current_division = division_match.group(1)
                    analysis['entities']['divisions'].append({
                        'name': current_division,
                        'line': line_num,
                        'content': cleaned_line
                    })
                    analysis['counts']['divisions'] += 1
                    analysis['structure']['main_sections'].append(current_division)
                
                # Analyze sections
                section_match = self.patterns['section'].match(line)
                if section_match:
                    current_section = section_match.group(1)
                    analysis['entities']['sections'].append({
                        'name': current_section,
                        'line': line_num,
                        'content': cleaned_line,
                        'parent_division': current_division
                    })
                    analysis['counts']['sections'] += 1
                    nesting_level += 1
                
                # Analyze paragraphs
                paragraph_match = self.patterns['paragraph'].match(line)
                if paragraph_match:
                    current_paragraph = paragraph_match.group(1)
                    analysis['entities']['paragraphs'].append({
                        'name': current_paragraph,
                        'line': line_num,
                        'content': cleaned_line,
                        'parent_section': current_section
                    })
                    analysis['counts']['paragraphs'] += 1
                    nesting_level += 1
                
                # Analyze data items
                data_match = self.patterns['data_item'].match(line)
                if data_match:
                    analysis['entities']['data_items'].append({
                        'name': data_match.group(1),
                        'line': line_num,
                        'content': cleaned_line,
                        'parent_paragraph': current_paragraph
                    })
                    analysis['counts']['data_items'] += 1
                
                # Analyze file definitions
                file_match = self.patterns['file_definition'].match(line)
                if file_match:
                    analysis['entities']['files'].append({
                        'name': file_match.group(1),
                        'line': line_num,
                        'content': cleaned_line,
                        'parent_division': current_division
                    })
                    analysis['counts']['files'] += 1
                
                # Analyze statements
                statement_match = self.patterns['statement'].match(line)
                if statement_match:
                    analysis['entities']['statements'].append({
                        'name': statement_match.group(1),
                        'line': line_num,
                        'content': cleaned_line,
                        'parent_paragraph': current_paragraph
                    })
                    analysis['counts']['statements'] += 1
                else:
                    # Additional statement detection
                    for keyword in self.statement_keywords:
                        if cleaned_line.upper().startswith(keyword):
                            analysis['entities']['statements'].append({
                                'name': keyword,
                                'line': line_num,
                                'content': cleaned_line,
                                'parent_paragraph': current_paragraph
                            })
                            analysis['counts']['statements'] += 1
                            break
            
            analysis['structure']['nested_levels'] = nesting_level
            analysis['structure']['complexity_score'] = self.calculate_complexity_score(analysis)
            
            return analysis
            
        except Exception as e:
            logger.error(f"Error analyzing file {file_path}: {e}")
            return {}
    
    def calculate_complexity_score(self, analysis: Dict[str, any]) -> float:
        """Calculate complexity score for a COBOL file"""
        counts = analysis['counts']
        file_info = analysis['file_info']
        
        # Base complexity from entity counts
        entity_complexity = (
            counts['programs'] * 1 +
            counts['divisions'] * 2 +
            counts['sections'] * 3 +
            counts['paragraphs'] * 4 +
            counts['statements'] * 5 +
            counts['data_items'] * 2 +
            counts['files'] * 3
        )
        
        # Size complexity
        size_complexity = file_info['non_comment_lines'] / 100
        
        # Nesting complexity
        nesting_complexity = analysis['structure']['nested_levels'] * 2
        
        return entity_complexity + size_complexity + nesting_complexity
    
    def validate_parsing_results(self, file_path: str, actual_counts: Dict[str, int]) -> ValidationResult:
        """Validate parsing results against expected counts"""
        expected_counts = self.get_expected_counts(file_path)
        tolerance_limits = self.get_tolerance_limits(file_path)
        
        discrepancies = {}
        recommendations = []
        
        for entity_type, expected_count in expected_counts.items():
            actual_count = actual_counts.get(entity_type, 0)
            tolerance = tolerance_limits.get(entity_type, 0)
            
            if actual_count != expected_count:
                difference = actual_count - expected_count
                percentage_diff = abs(difference) / expected_count * 100 if expected_count > 0 else 0
                
                discrepancy_info = {
                    'expected': expected_count,
                    'actual': actual_count,
                    'difference': difference,
                    'percentage_diff': percentage_diff,
                    'within_tolerance': abs(difference) <= tolerance,
                    'severity': self.calculate_severity(percentage_diff)
                }
                
                discrepancies[entity_type] = discrepancy_info
                
                # Generate recommendations
                recommendation = self.generate_recommendation(entity_type, discrepancy_info)
                if recommendation:
                    recommendations.append(recommendation)
        
        # Calculate overall accuracy score
        accuracy_score = self.calculate_accuracy_score(expected_counts, actual_counts, tolerance_limits)
        
        return ValidationResult(
            file_path=file_path,
            expected_counts=expected_counts,
            actual_counts=actual_counts,
            discrepancies=discrepancies,
            accuracy_score=accuracy_score,
            recommendations=recommendations,
            timestamp=datetime.now()
        )
    
    def calculate_severity(self, percentage_diff: float) -> str:
        """Calculate severity based on percentage difference"""
        if percentage_diff >= 50:
            return 'critical'
        elif percentage_diff >= 25:
            return 'high'
        elif percentage_diff >= 10:
            return 'medium'
        else:
            return 'low'
    
    def generate_recommendation(self, entity_type: str, discrepancy_info: Dict[str, any]) -> Optional[str]:
        """Generate recommendation for fixing a discrepancy"""
        severity = discrepancy_info['severity']
        difference = discrepancy_info['difference']
        
        if entity_type == 'paragraphs' and difference < 0:
            if severity in ['high', 'critical']:
                return f"CRITICAL: Missing {abs(difference)} paragraphs. Check regex pattern for paragraph detection. Consider case sensitivity and line number handling."
            else:
                return f"Review paragraph extraction logic. Missing {abs(difference)} paragraphs may indicate filtering issues."
        
        elif entity_type == 'statements' and difference < 0:
            if severity in ['high', 'critical']:
                return f"CRITICAL: Missing {abs(difference)} statements. Expand statement keyword list and improve regex patterns."
            else:
                return f"Enhance statement detection. Missing {abs(difference)} statements may require additional keyword patterns."
        
        elif entity_type == 'sections' and difference < 0:
            return f"Missing {abs(difference)} sections. Verify section detection regex and line number handling."
        
        elif entity_type == 'divisions' and difference < 0:
            return f"Missing {abs(difference)} divisions. Check division detection pattern and file structure."
        
        return None
    
    def calculate_accuracy_score(self, expected: Dict[str, int], actual: Dict[str, int], tolerance: Dict[str, int]) -> float:
        """Calculate overall accuracy score"""
        total_score = 0
        total_weight = 0
        
        # Weight different entity types by importance
        weights = {
            'programs': 10,
            'divisions': 8,
            'sections': 6,
            'paragraphs': 4,
            'statements': 3,
            'data_items': 2,
            'files': 2
        }
        
        for entity_type, expected_count in expected.items():
            actual_count = actual.get(entity_type, 0)
            tolerance_limit = tolerance.get(entity_type, 0)
            weight = weights.get(entity_type, 1)
            
            if expected_count == 0:
                continue
            
            # Calculate accuracy for this entity type
            if abs(actual_count - expected_count) <= tolerance_limit:
                accuracy = 100
            else:
                accuracy = max(0, 100 - (abs(actual_count - expected_count) / expected_count * 100))
            
            total_score += accuracy * weight
            total_weight += weight
        
        return total_score / total_weight if total_weight > 0 else 0
    
    def clean_line(self, line: str) -> str:
        """Remove COBOL line numbers and clean the line"""
        if len(line) > 6:
            return line[6:].strip()
        return line.strip()
    
    def generate_validation_report(self, validation_result: ValidationResult) -> str:
        """Generate a comprehensive validation report"""
        report = f"""
COBOL Parsing Validation Report
==============================

File: {validation_result.file_path}
Analysis Date: {validation_result.timestamp.strftime('%Y-%m-%d %H:%M:%S')}
Parser Version: {validation_result.parser_version}
Overall Accuracy Score: {validation_result.accuracy_score:.1f}%

EXPECTED STRUCTURE:
{json.dumps(validation_result.expected_counts, indent=2)}

ACTUAL STRUCTURE:
{json.dumps(validation_result.actual_counts, indent=2)}

DISCREPANCIES ANALYSIS:
"""
        
        for entity_type, discrepancy in validation_result.discrepancies.items():
            report += f"""
{entity_type.upper()}:
  Expected: {discrepancy['expected']}
  Actual: {discrepancy['actual']}
  Difference: {discrepancy['difference']}
  Percentage Difference: {discrepancy['percentage_diff']:.1f}%
  Severity: {discrepancy['severity']}
  Within Tolerance: {discrepancy['within_tolerance']}
"""
        
        if validation_result.recommendations:
            report += "\nRECOMMENDATIONS:\n"
            for i, recommendation in enumerate(validation_result.recommendations, 1):
                report += f"{i}. {recommendation}\n"
        
        return report
    
    def save_validation_result(self, validation_result: ValidationResult, output_path: str):
        """Save validation result to file"""
        result_data = asdict(validation_result)
        result_data['timestamp'] = validation_result.timestamp.isoformat()
        
        with open(output_path, 'w') as f:
            json.dump(result_data, f, indent=2)
        
        logger.info(f"Validation result saved to: {output_path}")
    
    def load_validation_result(self, input_path: str) -> ValidationResult:
        """Load validation result from file"""
        with open(input_path, 'r') as f:
            data = json.load(f)
        
        data['timestamp'] = datetime.fromisoformat(data['timestamp'])
        
        return ValidationResult(**data)

def main():
    """Main function to demonstrate the validation system"""
    validator = COBOLValidationSystem()
    
    print("COBOL Validation System")
    print("=" * 30)
    print("This system provides comprehensive validation of COBOL parsing results.")
    print("\nFeatures:")
    print("- Detailed entity analysis")
    print("- Discrepancy detection")
    print("- Accuracy scoring")
    print("- Recommendations generation")
    print("- Comprehensive reporting")
    
    # Example usage
    example_file = "/workspace/example.cbl"
    if Path(example_file).exists():
        print(f"\nAnalyzing example file: {example_file}")
        analysis = validator.analyze_cobol_file_detailed(example_file)
        validation_result = validator.validate_parsing_results(example_file, analysis['counts'])
        report = validator.generate_validation_report(validation_result)
        print(report)
    else:
        print("\nNo example file found. Create a COBOL file to test the validation system.")

if __name__ == "__main__":
    main()