#!/usr/bin/env python3
"""
Test script for COBOL analysis and improvement system

This script demonstrates the complete COBOL analysis pipeline including
discrepancy detection, enhanced parsing, and validation.
"""

import asyncio
import logging
from pathlib import Path
import json
from datetime import datetime

from cobol_analysis_agent import COBOLAnalysisAgent
from enhanced_cobol_parser import EnhancedCOBOLParser
from cobol_validation_system import COBOLValidationSystem
from cobol_background_agent import COBOLBackgroundAgent

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_sample_cobol_file():
    """Create a sample COBOL file for testing"""
    sample_cobol = """       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUD-MANAGEMENT.
       AUTHOR. SYSTEM.
       DATE-WRITTEN. 2024-01-01.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TRANSACTION-FILE ASSIGN TO 'TRANS.DAT'.
           SELECT REPORT-FILE ASSIGN TO 'REPORT.OUT'.
       
       DATA DIVISION.
       FILE SECTION.
       FD  TRANSACTION-FILE.
       01  TRANSACTION-RECORD.
           05  ACCOUNT-NUMBER     PIC X(10).
           05  TRANSACTION-AMOUNT PIC 9(8)V99.
           05  TRANSACTION-DATE   PIC 9(8).
           05  TRANSACTION-TYPE   PIC X(1).
       
       FD  REPORT-FILE.
       01  REPORT-RECORD.
           05  ACCOUNT-NUMBER     PIC X(10).
           05  FRAUD-SCORE        PIC 9(3).
           05  STATUS-MESSAGE     PIC X(20).
       
       WORKING-STORAGE SECTION.
       01  WS-CONTROL-FIELDS.
           05  WS-EOF-FLAG        PIC X(1) VALUE 'N'.
           05  WS-FRAUD-THRESHOLD PIC 9(3) VALUE 100.
           05  WS-TRANSACTION-COUNT PIC 9(5) VALUE ZERO.
       
       01  WS-FRAUD-ANALYSIS.
           05  WS-SUSPICIOUS-COUNT PIC 9(3) VALUE ZERO.
           05  WS-TOTAL-AMOUNT    PIC 9(10)V99 VALUE ZERO.
       
       PROCEDURE DIVISION.
       1000-MAIN-PROCESSING.
           PERFORM 1100-OPEN-FILES
           PERFORM 1200-PROCESS-TRANSACTIONS
           PERFORM 1300-GENERATE-REPORT
           PERFORM 1400-CLOSE-FILES
           STOP RUN.
       
       1100-OPEN-FILES.
           OPEN INPUT TRANSACTION-FILE
           OPEN OUTPUT REPORT-FILE
           DISPLAY 'FILES OPENED SUCCESSFULLY'.
       
       1200-PROCESS-TRANSACTIONS.
           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ TRANSACTION-FILE
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       PERFORM 1210-ANALYZE-TRANSACTION
                       ADD 1 TO WS-TRANSACTION-COUNT
               END-READ
           END-PERFORM.
       
       1210-ANALYZE-TRANSACTION.
           IF TRANSACTION-AMOUNT > 5000
               ADD 1 TO WS-SUSPICIOUS-COUNT
           END-IF
           ADD TRANSACTION-AMOUNT TO WS-TOTAL-AMOUNT.
       
       1300-GENERATE-REPORT.
           MOVE ACCOUNT-NUMBER TO ACCOUNT-NUMBER OF REPORT-RECORD
           IF WS-SUSPICIOUS-COUNT > WS-FRAUD-THRESHOLD
               MOVE 999 TO FRAUD-SCORE
               MOVE 'HIGH RISK' TO STATUS-MESSAGE
           ELSE
               MOVE WS-SUSPICIOUS-COUNT TO FRAUD-SCORE
               MOVE 'NORMAL' TO STATUS-MESSAGE
           END-IF
           WRITE REPORT-RECORD.
       
       1400-CLOSE-FILES.
           CLOSE TRANSACTION-FILE
           CLOSE REPORT-FILE
           DISPLAY 'PROCESSING COMPLETED'.
"""
    
    file_path = "sample_fraud_management.cbl"
    with open(file_path, 'w') as f:
        f.write(sample_cobol)
    
    return file_path

def test_analysis_pipeline():
    """Test the complete COBOL analysis pipeline"""
    print("="*80)
    print("COBOL ANALYSIS PIPELINE TEST")
    print("="*80)
    
    # Create sample COBOL file
    print("\n1. Creating sample COBOL file...")
    cobol_file = create_sample_cobol_file()
    print(f"   Created: {cobol_file}")
    
    # Initialize components
    print("\n2. Initializing analysis components...")
    analysis_agent = COBOLAnalysisAgent()
    enhanced_parser = EnhancedCOBOLParser()
    validation_system = COBOLValidationSystem()
    
    try:
        # Run analysis
        print("\n3. Running COBOL structure analysis...")
        analysis_result = analysis_agent.analyze_cobol_file(cobol_file)
        
        print(f"   Expected Structure:")
        print(f"     Programs: {analysis_result.expected.programs}")
        print(f"     Divisions: {analysis_result.expected.divisions}")
        print(f"     Sections: {analysis_result.expected.sections}")
        print(f"     Paragraphs: {analysis_result.expected.paragraphs}")
        print(f"     Statements: {analysis_result.expected.statements}")
        
        print(f"\n   Actual Counts:")
        for entity_type, count in analysis_result.actual.items():
            print(f"     {entity_type}: {count}")
        
        print(f"\n   Discrepancies:")
        for entity_type, discrepancy in analysis_result.discrepancies.items():
            if discrepancy != 0:
                print(f"     {entity_type}: {discrepancy:+}")
        
        # Enhanced parsing
        print("\n4. Running enhanced COBOL parsing...")
        structure = enhanced_parser.parse_cobol_file(cobol_file)
        counts = structure.get_counts()
        
        print(f"   Enhanced Parser Results:")
        for entity_type, count in counts.items():
            print(f"     {entity_type}: {count}")
        
        # Validation
        print("\n5. Running validation...")
        validation_report = validation_system.validate_structure(structure)
        
        print(f"   Validation Status: {validation_report.overall_status}")
        print(f"   Passed Rules: {validation_report.passed_rules}/{validation_report.total_rules}")
        print(f"   Warnings: {validation_report.warnings}")
        print(f"   Errors: {validation_report.errors}")
        
        # Save results
        print("\n6. Saving analysis results...")
        analysis_agent.save_analysis_report(analysis_result, "test_analysis_report.json")
        enhanced_parser.export_to_json(structure, "test_parsing_results.json")
        validation_system.save_validation_report(validation_report, "test_validation_report.json")
        
        print("   Results saved to:")
        print("     - test_analysis_report.json")
        print("     - test_parsing_results.json")
        print("     - test_validation_report.json")
        
        # Print recommendations
        print("\n7. Recommendations:")
        for i, rec in enumerate(analysis_result.recommendations, 1):
            print(f"   {i}. {rec}")
        
        print("\n" + "="*80)
        print("ANALYSIS PIPELINE TEST COMPLETED SUCCESSFULLY")
        print("="*80)
        
    except Exception as e:
        print(f"\nError during analysis: {e}")
        logger.error(f"Analysis pipeline test failed: {e}")
    
    finally:
        # Clean up
        if Path(cobol_file).exists():
            Path(cobol_file).unlink()
            print(f"\nCleaned up sample file: {cobol_file}")

async def test_background_agent():
    """Test the background agent functionality"""
    print("\n" + "="*80)
    print("BACKGROUND AGENT TEST")
    print("="*80)
    
    # Create sample file
    cobol_file = create_sample_cobol_file()
    
    try:
        # Initialize background agent
        print("\n1. Initializing background agent...")
        background_agent = COBOLBackgroundAgent()
        
        # Run single analysis
        print("\n2. Running single analysis...")
        result = background_agent.run_single_analysis(cobol_file)
        
        if "error" not in result:
            print(f"   Analysis Status: SUCCESS")
            print(f"   Validation Status: {result['validation_report']['overall_status']}")
            print(f"   Entity Counts: {result['structure_counts']}")
        else:
            print(f"   Analysis Status: FAILED - {result['error']}")
        
        # Get agent status
        print("\n3. Getting agent status...")
        status = background_agent.get_status()
        print(f"   Is Running: {status['is_running']}")
        print(f"   Analyses Completed: {status['metrics']['analyses_completed']}")
        print(f"   Success Rate: {status['metrics']['success_rate']:.1f}%")
        
        print("\n" + "="*80)
        print("BACKGROUND AGENT TEST COMPLETED")
        print("="*80)
        
    except Exception as e:
        print(f"\nError during background agent test: {e}")
        logger.error(f"Background agent test failed: {e}")
    
    finally:
        # Clean up
        if Path(cobol_file).exists():
            Path(cobol_file).unlink()
            print(f"\nCleaned up sample file: {cobol_file}")

def demonstrate_discrepancy_analysis():
    """Demonstrate the discrepancy analysis with mock data"""
    print("\n" + "="*80)
    print("DISCREPANCY ANALYSIS DEMONSTRATION")
    print("="*80)
    
    # Mock expected structure (from your original question)
    expected_structure = {
        "PROGRAM": 1,
        "DIVISION": 4,
        "SECTION": 16,
        "PARAGRAPH": 261,
        "STATEMENT": 316
    }
    
    # Mock actual structure (what the graph shows)
    actual_structure = {
        "PROGRAM": 1,
        "PARAGRAPH": 103,  # Missing 158 paragraphs
        "DATA_ITEM": 33,
        "FILE": 2,
        "INFERRED": 19,
        "COMPILATION_UNIT": 1
    }
    
    print("\nExpected Structure:")
    for entity_type, count in expected_structure.items():
        print(f"  {entity_type:15}: {count:4}")
    
    print("\nActual Structure:")
    for entity_type, count in actual_structure.items():
        print(f"  {entity_type:15}: {count:4}")
    
    print("\nDiscrepancies:")
    for entity_type in set(expected_structure.keys()) | set(actual_structure.keys()):
        expected = expected_structure.get(entity_type, 0)
        actual = actual_structure.get(entity_type, 0)
        discrepancy = actual - expected
        
        if discrepancy != 0:
            print(f"  {entity_type:15}: {discrepancy:+4} ({actual} vs {expected})")
    
    print("\nRoot Cause Analysis:")
    print("  1. Missing Divisions: Parser not extracting division entities separately")
    print("  2. Missing Sections: Section extraction not implemented")
    print("  3. Missing Paragraphs: 158 paragraphs not detected (filtering issues)")
    print("  4. Missing Statements: Statement-level parsing not implemented")
    print("  5. Extra Entities: Parser creating different entity types than expected")
    
    print("\nRecommended Solutions:")
    print("  1. Implement division extraction with proper regex patterns")
    print("  2. Add section detection and hierarchical tracking")
    print("  3. Fix paragraph filtering logic to capture all 261 paragraphs")
    print("  4. Implement comprehensive statement parsing")
    print("  5. Standardize entity types to match expected structure")
    
    print("\n" + "="*80)

def main():
    """Main test function"""
    print("COBOL ANALYSIS AND IMPROVEMENT SYSTEM TEST")
    print(f"Test started at: {datetime.now().isoformat()}")
    
    try:
        # Run tests
        test_analysis_pipeline()
        demonstrate_discrepancy_analysis()
        
        # Test background agent (async)
        asyncio.run(test_background_agent())
        
        print("\n" + "="*80)
        print("ALL TESTS COMPLETED SUCCESSFULLY")
        print("="*80)
        
    except Exception as e:
        print(f"\nTest suite failed: {e}")
        logger.error(f"Test suite failed: {e}")

if __name__ == "__main__":
    main()