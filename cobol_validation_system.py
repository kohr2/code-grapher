#!/usr/bin/env python3
"""
COBOL Validation System

This system validates COBOL parsing results against expected structure
and provides detailed discrepancy analysis and recommendations.
"""

import re
import logging
from typing import Dict, List, Tuple, Set, Optional, Any
from dataclasses import dataclass, asdict
from pathlib import Path
import json
from collections import defaultdict, Counter
from datetime import datetime

logger = logging.getLogger(__name__)

@dataclass
class ValidationRule:
    """Represents a validation rule for COBOL entities"""
    entity_type: str
    min_count: int
    max_count: int
    expected_count: Optional[int] = None
    description: str = ""
    severity: str = "WARNING"  # INFO, WARNING, ERROR

@dataclass
class ValidationResult:
    """Results of validation check"""
    rule: ValidationRule
    actual_count: int
    passed: bool
    message: str
    recommendations: List[str]

@dataclass
class ValidationReport:
    """Complete validation report"""
    timestamp: str
    file_path: str
    overall_status: str
    total_rules: int
    passed_rules: int
    failed_rules: int
    warnings: int
    errors: int
    results: List[ValidationResult]
    summary: Dict[str, Any]
    recommendations: List[str]

class COBOLValidationSystem:
    """System for validating COBOL parsing results"""
    
    def __init__(self, config: Optional[Dict] = None):
        self.config = config or self._get_default_config()
        self.validation_rules = self._load_validation_rules()
        
    def _get_default_config(self) -> Dict:
        """Get default validation configuration"""
        return {
            "strict_mode": False,
            "enable_count_validation": True,
            "enable_structure_validation": True,
            "enable_relationship_validation": True,
            "tolerance_percentage": 10  # 10% tolerance for count variations
        }
    
    def _load_validation_rules(self) -> List[ValidationRule]:
        """Load validation rules for COBOL structure"""
        rules = [
            # Program validation
            ValidationRule(
                entity_type="PROGRAM",
                min_count=1,
                max_count=1,
                expected_count=1,
                description="Each COBOL file should have exactly one PROGRAM-ID",
                severity="ERROR"
            ),
            
            # Division validation
            ValidationRule(
                entity_type="DIVISION",
                min_count=4,
                max_count=4,
                expected_count=4,
                description="Standard COBOL programs should have 4 divisions: IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE",
                severity="ERROR"
            ),
            
            # Section validation
            ValidationRule(
                entity_type="SECTION",
                min_count=0,
                max_count=None,
                description="Sections are optional but should be properly structured",
                severity="WARNING"
            ),
            
            # Paragraph validation
            ValidationRule(
                entity_type="PARAGRAPH",
                min_count=1,
                max_count=None,
                description="COBOL programs should have at least one paragraph",
                severity="WARNING"
            ),
            
            # Statement validation
            ValidationRule(
                entity_type="STATEMENT",
                min_count=1,
                max_count=None,
                description="COBOL programs should have executable statements",
                severity="INFO"
            ),
            
            # Data item validation
            ValidationRule(
                entity_type="DATA_ITEM",
                min_count=0,
                max_count=None,
                description="Data items should be properly declared",
                severity="INFO"
            ),
            
            # File validation
            ValidationRule(
                entity_type="FILE",
                min_count=0,
                max_count=None,
                description="File declarations should be present if files are used",
                severity="INFO"
            )
        ]
        
        return rules
    
    def validate_structure(self, structure: Any, expected_counts: Optional[Dict[str, int]] = None) -> ValidationReport:
        """Validate COBOL structure against rules and expected counts"""
        logger.info("Starting COBOL structure validation")
        
        # Get actual counts
        if hasattr(structure, 'get_counts'):
            actual_counts = structure.get_counts()
        else:
            actual_counts = structure
        
        # Apply expected counts if provided
        if expected_counts:
            self._update_rules_with_expected_counts(expected_counts)
        
        # Run validation
        results = []
        for rule in self.validation_rules:
            result = self._validate_rule(rule, actual_counts)
            results.append(result)
        
        # Generate report
        report = self._generate_report(results, actual_counts, expected_counts)
        
        logger.info(f"Validation completed: {report.overall_status}")
        return report
    
    def _update_rules_with_expected_counts(self, expected_counts: Dict[str, int]):
        """Update validation rules with expected counts"""
        for rule in self.validation_rules:
            if rule.entity_type in expected_counts:
                expected_count = expected_counts[rule.entity_type]
                rule.expected_count = expected_count
                rule.min_count = max(rule.min_count, expected_count - self.config["tolerance_percentage"])
                if rule.max_count is not None:
                    rule.max_count = min(rule.max_count, expected_count + self.config["tolerance_percentage"])
    
    def _validate_rule(self, rule: ValidationRule, actual_counts: Dict[str, int]) -> ValidationResult:
        """Validate a single rule against actual counts"""
        actual_count = actual_counts.get(rule.entity_type, 0)
        
        # Check if count is within bounds
        passed = True
        message = ""
        recommendations = []
        
        if actual_count < rule.min_count:
            passed = False
            message = f"Count {actual_count} is below minimum {rule.min_count}"
            recommendations.append(f"Increase {rule.entity_type} extraction - check parsing patterns")
        elif rule.max_count is not None and actual_count > rule.max_count:
            passed = False
            message = f"Count {actual_count} exceeds maximum {rule.max_count}"
            recommendations.append(f"Review {rule.entity_type} filtering - possible over-counting")
        else:
            message = f"Count {actual_count} is within acceptable range"
        
        # Check against expected count if specified
        if rule.expected_count is not None:
            if actual_count != rule.expected_count:
                if passed:  # Only add warning if not already failed
                    message += f" (expected {rule.expected_count})"
                    recommendations.append(f"Verify {rule.entity_type} extraction accuracy")
        
        return ValidationResult(
            rule=rule,
            actual_count=actual_count,
            passed=passed,
            message=message,
            recommendations=recommendations
        )
    
    def _generate_report(self, results: List[ValidationResult], actual_counts: Dict[str, int], expected_counts: Optional[Dict[str, int]]) -> ValidationReport:
        """Generate comprehensive validation report"""
        total_rules = len(results)
        passed_rules = sum(1 for r in results if r.passed)
        failed_rules = total_rules - passed_rules
        
        # Count by severity
        warnings = sum(1 for r in results if not r.passed and r.rule.severity == "WARNING")
        errors = sum(1 for r in results if not r.passed and r.rule.severity == "ERROR")
        
        # Determine overall status
        if errors > 0:
            overall_status = "FAILED"
        elif warnings > 0:
            overall_status = "WARNING"
        else:
            overall_status = "PASSED"
        
        # Generate summary
        summary = {
            "actual_counts": actual_counts,
            "expected_counts": expected_counts or {},
            "discrepancies": self._calculate_discrepancies(actual_counts, expected_counts),
            "coverage_analysis": self._analyze_coverage(actual_counts),
            "quality_metrics": self._calculate_quality_metrics(results)
        }
        
        # Generate recommendations
        recommendations = self._generate_recommendations(results, actual_counts, expected_counts)
        
        return ValidationReport(
            timestamp=datetime.now().isoformat(),
            file_path="",
            overall_status=overall_status,
            total_rules=total_rules,
            passed_rules=passed_rules,
            failed_rules=failed_rules,
            warnings=warnings,
            errors=errors,
            results=results,
            summary=summary,
            recommendations=recommendations
        )
    
    def _calculate_discrepancies(self, actual_counts: Dict[str, int], expected_counts: Optional[Dict[str, int]]) -> Dict[str, int]:
        """Calculate discrepancies between actual and expected counts"""
        discrepancies = {}
        
        if expected_counts:
            for entity_type in set(actual_counts.keys()) | set(expected_counts.keys()):
                actual = actual_counts.get(entity_type, 0)
                expected = expected_counts.get(entity_type, 0)
                discrepancies[entity_type] = actual - expected
        
        return discrepancies
    
    def _analyze_coverage(self, actual_counts: Dict[str, int]) -> Dict[str, Any]:
        """Analyze entity coverage and completeness"""
        total_entities = sum(actual_counts.values())
        
        coverage = {
            "total_entities": total_entities,
            "entity_distribution": {k: (v/total_entities*100) if total_entities > 0 else 0 
                                   for k, v in actual_counts.items()},
            "missing_standard_entities": [],
            "unexpected_entities": []
        }
        
        # Check for missing standard entities
        standard_entities = ["PROGRAM", "DIVISION", "PARAGRAPH"]
        for entity in standard_entities:
            if actual_counts.get(entity, 0) == 0:
                coverage["missing_standard_entities"].append(entity)
        
        return coverage
    
    def _calculate_quality_metrics(self, results: List[ValidationResult]) -> Dict[str, Any]:
        """Calculate quality metrics from validation results"""
        metrics = {
            "compliance_rate": (sum(1 for r in results if r.passed) / len(results)) * 100,
            "error_rate": (sum(1 for r in results if not r.passed and r.rule.severity == "ERROR") / len(results)) * 100,
            "warning_rate": (sum(1 for r in results if not r.passed and r.rule.severity == "WARNING") / len(results)) * 100,
            "critical_issues": [r for r in results if not r.passed and r.rule.severity == "ERROR"],
            "improvement_areas": [r.rule.entity_type for r in results if not r.passed]
        }
        
        return metrics
    
    def _generate_recommendations(self, results: List[ValidationResult], actual_counts: Dict[str, int], expected_counts: Optional[Dict[str, int]]) -> List[str]:
        """Generate actionable recommendations based on validation results"""
        recommendations = []
        
        # Collect all recommendations from failed rules
        for result in results:
            if not result.passed:
                recommendations.extend(result.recommendations)
        
        # Add specific recommendations based on discrepancies
        if expected_counts:
            discrepancies = self._calculate_discrepancies(actual_counts, expected_counts)
            
            for entity_type, discrepancy in discrepancies.items():
                if discrepancy < 0:
                    recommendations.append(f"Investigate missing {entity_type} entities - {abs(discrepancy)} fewer than expected")
                elif discrepancy > 0:
                    recommendations.append(f"Review {entity_type} extraction - {discrepancy} more than expected")
        
        # Add general recommendations
        recommendations.extend([
            "Implement hierarchical relationship tracking between entities",
            "Add validation checks to parser configuration",
            "Consider implementing entity deduplication if duplicates are found",
            "Review regex patterns for better entity detection",
            "Add statement-level parsing for comprehensive analysis"
        ])
        
        # Remove duplicates while preserving order
        unique_recommendations = []
        for rec in recommendations:
            if rec not in unique_recommendations:
                unique_recommendations.append(rec)
        
        return unique_recommendations
    
    def save_validation_report(self, report: ValidationReport, output_path: str):
        """Save validation report to JSON file"""
        report_dict = asdict(report)
        
        with open(output_path, 'w') as f:
            json.dump(report_dict, f, indent=2)
        
        logger.info(f"Validation report saved to {output_path}")
    
    def print_validation_summary(self, report: ValidationReport):
        """Print a summary of validation results"""
        print("\n" + "="*60)
        print("COBOL STRUCTURE VALIDATION REPORT")
        print("="*60)
        print(f"Overall Status: {report.overall_status}")
        print(f"Timestamp: {report.timestamp}")
        print(f"Total Rules: {report.total_rules}")
        print(f"Passed: {report.passed_rules}")
        print(f"Failed: {report.failed_rules}")
        print(f"Warnings: {report.warnings}")
        print(f"Errors: {report.errors}")
        
        print("\nENTITY COUNTS:")
        print("-" * 30)
        for entity_type, count in report.summary["actual_counts"].items():
            expected = report.summary["expected_counts"].get(entity_type, "N/A")
            print(f"{entity_type:12}: {count:4} (expected: {expected})")
        
        if report.summary["discrepancies"]:
            print("\nDISCREPANCIES:")
            print("-" * 30)
            for entity_type, discrepancy in report.summary["discrepancies"].items():
                if discrepancy != 0:
                    print(f"{entity_type:12}: {discrepancy:+4}")
        
        print("\nQUALITY METRICS:")
        print("-" * 30)
        metrics = report.summary["quality_metrics"]
        print(f"Compliance Rate: {metrics['compliance_rate']:.1f}%")
        print(f"Error Rate: {metrics['error_rate']:.1f}%")
        print(f"Warning Rate: {metrics['warning_rate']:.1f}%")
        
        if report.recommendations:
            print("\nRECOMMENDATIONS:")
            print("-" * 30)
            for i, rec in enumerate(report.recommendations, 1):
                print(f"{i:2}. {rec}")
        
        print("\n" + "="*60)

def main():
    """Main function for testing the validation system"""
    validator = COBOLValidationSystem()
    
    # Example usage with mock data
    mock_actual_counts = {
        "PROGRAM": 1,
        "DIVISION": 4,
        "SECTION": 16,
        "PARAGRAPH": 261,
        "STATEMENT": 316,
        "DATA_ITEM": 33,
        "FILE": 2
    }
    
    mock_expected_counts = {
        "PROGRAM": 1,
        "DIVISION": 4,
        "SECTION": 16,
        "PARAGRAPH": 261,
        "STATEMENT": 316
    }
    
    try:
        report = validator.validate_structure(mock_actual_counts, mock_expected_counts)
        validator.print_validation_summary(report)
        validator.save_validation_report(report, "cobol_validation_report.json")
        
    except Exception as e:
        logger.error(f"Validation failed: {e}")

if __name__ == "__main__":
    main()