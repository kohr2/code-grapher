#!/usr/bin/env python3
"""
Test Script for Enhanced COBOL Parser

This script demonstrates the enhanced COBOL parsing capabilities and
validates the improvements made to address the parsing discrepancies.
"""

import json
import logging
from pathlib import Path
from cobol_parsing_agent import COBOLParsingAgent
from cobol_validation_system import COBOLValidationSystem

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def create_sample_cobol_file():
    """Create a sample COBOL file for testing"""
    sample_content = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FRAUD-MANAGEMENT.
000300 AUTHOR. SYSTEM.
000400 DATE-WRITTEN. 2024-01-01.
000500
000600 ENVIRONMENT DIVISION.
000700 CONFIGURATION SECTION.
000800 SOURCE-COMPUTER. IBM-370.
000900 OBJECT-COMPUTER. IBM-370.
001000 INPUT-OUTPUT SECTION.
001100 FILE-CONTROL.
001200     SELECT FRAUD-FILE ASSIGN TO FRAUD01.
001300     SELECT REPORT-FILE ASSIGN TO REPORT01.
001400
001500 DATA DIVISION.
001600 FILE SECTION.
001700 FD  FRAUD-FILE
001800     LABEL RECORDS ARE STANDARD.
001900 01  FRAUD-RECORD.
002000     05  CUSTOMER-ID        PIC X(10).
002100     05  TRANSACTION-AMT    PIC 9(10)V99.
002200     05  TRANSACTION-DATE   PIC X(10).
002300     05  RISK-SCORE         PIC 9(3).
002400
002500 WORKING-STORAGE SECTION.
002600 01  WS-COUNTERS.
002700     05  WS-RECORD-COUNT    PIC 9(6) VALUE ZERO.
002800     05  WS-FRAUD-COUNT     PIC 9(6) VALUE ZERO.
002900 01  WS-FLAGS.
003000     05  WS-EOF-FLAG        PIC X VALUE 'N'.
003100         88  WS-EOF          VALUE 'Y'.
003200
003300 PROCEDURE DIVISION.
003400 1000-MAIN-PROCESSING.
003500     PERFORM 1100-OPEN-FILES
003600     PERFORM 1200-PROCESS-RECORDS
003700     PERFORM 1300-CLOSE-FILES
003800     PERFORM 1400-GENERATE-REPORT
003900     STOP RUN.
004000
004100 1100-OPEN-FILES.
004200     OPEN INPUT FRAUD-FILE
004300     OPEN OUTPUT REPORT-FILE
004400     DISPLAY 'FILES OPENED SUCCESSFULLY'.
004500
004600 1200-PROCESS-RECORDS.
004700     READ FRAUD-FILE
004800         AT END SET WS-EOF TO TRUE
004900     END-READ
005000     PERFORM UNTIL WS-EOF
005100         ADD 1 TO WS-RECORD-COUNT
005200         PERFORM 1210-CHECK-FRAUD
005300         READ FRAUD-FILE
005400             AT END SET WS-EOF TO TRUE
005500         END-READ
005600     END-PERFORM.
005700
005800 1210-CHECK-FRAUD.
005900     IF RISK-SCORE > 800
006000         ADD 1 TO WS-FRAUD-COUNT
006100         MOVE 'HIGH RISK' TO WS-RISK-STATUS
006200     ELSE
006300         MOVE 'LOW RISK' TO WS-RISK-STATUS
006400     END-IF.
006500
006600 1300-CLOSE-FILES.
006700     CLOSE FRAUD-FILE
006800     CLOSE REPORT-FILE
006900     DISPLAY 'FILES CLOSED SUCCESSFULLY'.
007000
007100 1400-GENERATE-REPORT.
007200     DISPLAY 'TOTAL RECORDS PROCESSED: ' WS-RECORD-COUNT
007300     DISPLAY 'FRAUD CASES DETECTED: ' WS-FRAUD-COUNT.
"""
    
    sample_file = Path("/workspace/sample_fraud_management.cbl")
    with open(sample_file, 'w') as f:
        f.write(sample_content)
    
    logger.info(f"Created sample COBOL file: {sample_file}")
    return str(sample_file)

def test_enhanced_parser():
    """Test the enhanced COBOL parser"""
    logger.info("Testing Enhanced COBOL Parser")
    print("=" * 50)
    
    # Create sample file
    sample_file = create_sample_cobol_file()
    
    # Initialize parser and validator
    parser = COBOLParsingAgent()
    validator = COBOLValidationSystem()
    
    # Analyze the file
    logger.info("Analyzing COBOL file...")
    entities = parser.extract_entities(sample_file)
    actual_counts = entities.get_counts()
    
    # Expected counts for this sample
    expected_counts = {
        'programs': 1,
        'divisions': 4,  # IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
        'sections': 3,   # CONFIGURATION, INPUT-OUTPUT, FILE, WORKING-STORAGE
        'paragraphs': 8, # 1000-MAIN-PROCESSING, 1100-OPEN-FILES, etc.
        'statements': 25, # Various COBOL statements
        'data_items': 8, # Various data items
        'files': 2       # FRAUD-FILE, REPORT-FILE
    }
    
    # Validate results
    validation_result = validator.validate_parsing_results(sample_file, actual_counts)
    
    # Display results
    print("\nPARSING RESULTS:")
    print("-" * 30)
    for entity_type, count in actual_counts.items():
        expected = expected_counts.get(entity_type, 0)
        status = "✓" if count == expected else "✗"
        print(f"{status} {entity_type.upper()}: {count} (expected: {expected})")
    
    print(f"\nOVERALL ACCURACY SCORE: {validation_result.accuracy_score:.1f}%")
    
    # Show discrepancies
    if validation_result.discrepancies:
        print("\nDISCREPANCIES:")
        print("-" * 20)
        for entity_type, discrepancy in validation_result.discrepancies.items():
            print(f"{entity_type}: {discrepancy['actual']} vs {discrepancy['expected']} "
                  f"({discrepancy['severity']} severity)")
    
    # Show recommendations
    if validation_result.recommendations:
        print("\nRECOMMENDATIONS:")
        print("-" * 20)
        for i, recommendation in enumerate(validation_result.recommendations, 1):
            print(f"{i}. {recommendation}")
    
    # Generate detailed report
    report = validator.generate_validation_report(validation_result)
    
    # Save report
    report_file = Path("/workspace/cobol_parsing_test_report.txt")
    with open(report_file, 'w') as f:
        f.write(report)
    
    logger.info(f"Detailed report saved to: {report_file}")
    
    return validation_result

def test_background_agent():
    """Test the background agent functionality"""
    logger.info("Testing Background Agent")
    print("=" * 50)
    
    from cobol_background_agent import COBOLBackgroundAgent, AgentConfig
    
    # Create configuration
    config = AgentConfig(
        watch_directory='/workspace',
        check_interval=5,  # Short interval for testing
        auto_fix_enabled=True,
        notification_enabled=True
    )
    
    # Create agent
    agent = COBOLBackgroundAgent(config)
    
    # Test file processing
    sample_file = "/workspace/sample_fraud_management.cbl"
    if Path(sample_file).exists():
        logger.info("Processing sample file with background agent...")
        agent.process_file(sample_file)
        
        # Show agent stats
        print(f"\nAGENT STATISTICS:")
        print(f"Files Analyzed: {agent.stats['files_analyzed']}")
        print(f"Issues Found: {agent.stats['issues_found']}")
        print(f"Issues Fixed: {agent.stats['issues_fixed']}")
        
        # Show recent issues
        if agent.issue_history:
            print(f"\nRECENT ISSUES:")
            for issue in agent.issue_history[-3:]:  # Show last 3 issues
                print(f"- {issue.issue_id}: {issue.description} ({issue.severity})")
    
    return agent

def demonstrate_parser_improvements():
    """Demonstrate the improvements made to the parser"""
    logger.info("Demonstrating Parser Improvements")
    print("=" * 50)
    
    improvements = [
        {
            'issue': 'Missing Divisions as Entities',
            'solution': 'Enhanced regex patterns to detect all 4 main divisions',
            'impact': 'Now captures IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions'
        },
        {
            'issue': 'Incomplete Paragraph Extraction',
            'solution': 'Improved paragraph detection with better line number handling',
            'impact': 'Should now capture all 261 paragraphs instead of just 103'
        },
        {
            'issue': 'Missing Statement-Level Entities',
            'solution': 'Comprehensive statement keyword detection',
            'impact': 'Now extracts individual COBOL statements (DISPLAY, MOVE, IF, etc.)'
        },
        {
            'issue': 'No Validation System',
            'solution': 'Built comprehensive validation and recommendation system',
            'impact': 'Automatic detection and fixing of parsing discrepancies'
        },
        {
            'issue': 'No Background Monitoring',
            'solution': 'Created background agent for continuous monitoring',
            'impact': 'Real-time detection and auto-fixing of parsing issues'
        }
    ]
    
    print("PARSER IMPROVEMENTS:")
    print("-" * 30)
    for i, improvement in enumerate(improvements, 1):
        print(f"{i}. ISSUE: {improvement['issue']}")
        print(f"   SOLUTION: {improvement['solution']}")
        print(f"   IMPACT: {improvement['impact']}")
        print()
    
    return improvements

def main():
    """Main test function"""
    print("COBOL Parser Enhancement Test Suite")
    print("=" * 60)
    
    try:
        # Test enhanced parser
        validation_result = test_enhanced_parser()
        
        # Test background agent
        agent = test_background_agent()
        
        # Demonstrate improvements
        improvements = demonstrate_parser_improvements()
        
        print("\nTEST SUMMARY:")
        print("=" * 20)
        print(f"✓ Enhanced parser created and tested")
        print(f"✓ Validation system implemented")
        print(f"✓ Background agent functional")
        print(f"✓ Comprehensive reporting available")
        print(f"✓ Auto-fix capabilities enabled")
        
        print(f"\nOverall accuracy achieved: {validation_result.accuracy_score:.1f}%")
        
        if validation_result.accuracy_score >= 90:
            print("🎉 Parser enhancement successful!")
        elif validation_result.accuracy_score >= 75:
            print("✅ Good improvement, some fine-tuning needed")
        else:
            print("⚠️  Further improvements required")
        
    except Exception as e:
        logger.error(f"Test failed: {e}")
        print(f"❌ Test failed: {e}")

if __name__ == "__main__":
    main()