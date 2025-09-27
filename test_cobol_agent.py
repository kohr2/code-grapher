#!/usr/bin/env python3
"""
Test script for COBOL Background Agent

This script demonstrates the COBOL analysis and parsing system.
"""

import asyncio
import logging
import json
from pathlib import Path

# Configure logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(name)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)

async def test_cobol_analysis():
    """Test the COBOL analysis system."""
    logger.info("Testing COBOL Analysis System")
    
    # Create a sample COBOL file for testing
    sample_cobol = '''000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FRAUD-MANAGEMENT.
000300 AUTHOR. SYSTEM.
000400 DATE-WRITTEN. 2024-01-01.
000500
000600 ENVIRONMENT DIVISION.
000700 INPUT-OUTPUT SECTION.
000800 FILE-CONTROL.
000900     SELECT FRAUD-FILE ASSIGN TO 'FRAUD.DAT'.
001000
001100 DATA DIVISION.
001200 FILE SECTION.
001300 FD FRAUD-FILE.
001400 01 FRAUD-RECORD.
001500     05 CUSTOMER-ID PIC X(10).
001600     05 TRANSACTION-AMOUNT PIC 9(10)V99.
001700
001800 WORKING-STORAGE SECTION.
001900 01 WS-COUNTER PIC 9(5) VALUE ZERO.
002000 01 WS-TOTAL PIC 9(12)V99 VALUE ZERO.
002100
002200 PROCEDURE DIVISION.
002300 1000-INITIALIZE-PROGRAM.
002400     DISPLAY 'INITIALIZING FRAUD MANAGEMENT'.
002500     MOVE ZERO TO WS-COUNTER.
002600     MOVE ZERO TO WS-TOTAL.
002700
002800 2000-PROCESS-FRAUD.
002900     PERFORM 3000-READ-FRAUD-FILE.
003000     IF WS-COUNTER > 0
003100         PERFORM 4000-ANALYZE-FRAUD
003200     END-IF.
003300
003400 3000-READ-FRAUD-FILE.
003500     READ FRAUD-FILE.
003600     IF NOT EOF
003700         ADD 1 TO WS-COUNTER
003800         ADD TRANSACTION-AMOUNT TO WS-TOTAL
003900     END-IF.
004000
004100 4000-ANALYZE-FRAUD.
004200     DISPLAY 'ANALYZING FRAUD PATTERNS'.
004300     COMPUTE WS-TOTAL = WS-TOTAL / WS-COUNTER.
004400
004500 9999-END-PROGRAM.
004600     DISPLAY 'FRAUD ANALYSIS COMPLETE'.
004700     STOP RUN.
'''
    
    # Write sample file
    test_file = "test_sample.cbl"
    with open(test_file, 'w') as f:
        f.write(sample_cobol)
    
    try:
        # Test the analysis agent
        from cobol_analysis_agent import COBOLAnalysisAgent
        analysis_agent = COBOLAnalysisAgent()
        
        print("\n=== Testing COBOL Analysis Agent ===")
        analysis = analysis_agent.analyze_cobol_file(test_file)
        
        if analysis:
            print(f"Expected counts: {analysis.expected_counts}")
            print(f"Actual counts: {analysis.actual_counts}")
            print(f"Discrepancies: {analysis.discrepancies}")
            
            print("\nRecommendations:")
            for i, rec in enumerate(analysis.recommendations, 1):
                print(f"{i}. {rec}")
        
        # Test the enhanced parser
        from enhanced_cobol_parser import EnhancedCOBOLParser
        enhanced_parser = EnhancedCOBOLParser()
        
        print("\n=== Testing Enhanced COBOL Parser ===")
        parse_results = enhanced_parser.parse_file(test_file)
        
        if parse_results:
            print(f"Entity counts: {parse_results['counts']}")
            print(f"Hierarchy: {parse_results['hierarchy']}")
            
            # Show some entities
            print(f"\nFound {len(parse_results['entities'])} entities:")
            for entity in parse_results['entities'][:10]:  # Show first 10
                print(f"  {entity.entity_type.value}: {entity.name} (line {entity.line_number})")
        
        # Test the validation system
        from cobol_validation_system import COBOLValidationSystem
        validation_system = COBOLValidationSystem()
        
        print("\n=== Testing Validation System ===")
        if parse_results:
            validation_results = validation_system.validate_parsing_results(
                parse_results['counts'],
                parse_results['entities']
            )
            
            for entity_type, result in validation_results.items():
                print(f"{entity_type.upper()}:")
                print(f"  Expected: {result.expected}, Actual: {result.actual}")
                print(f"  Accuracy: {result.accuracy_percentage:.1f}%")
                if result.issues:
                    print(f"  Issues: {result.issues[:2]}")  # Show first 2 issues
        
        # Test the background agent
        from cobol_background_agent import COBOLBackgroundAgent
        background_agent = COBOLBackgroundAgent(max_workers=1)
        
        print("\n=== Testing Background Agent ===")
        
        # Start the agent
        monitor_task = await background_agent.start()
        
        # Submit a task
        from cobol_background_agent import COBOLProcessingTask
        task = COBOLProcessingTask(
            file_path=test_file,
            priority=1,
            metadata={'test': True}
        )
        background_agent.submit_task(task)
        
        # Let it process
        await asyncio.sleep(5)
        
        # Get statistics
        stats = background_agent.get_stats()
        queue_status = background_agent.get_queue_status()
        
        print(f"Tasks processed: {stats['tasks_processed']}")
        print(f"Successful: {stats['successful_tasks']}")
        print(f"Average accuracy: {stats['average_accuracy']:.1f}%")
        print(f"Pending tasks: {queue_status['pending_tasks']}")
        
        # Stop the agent
        await background_agent.stop()
        
        print("\n=== Test Complete ===")
        print("Check the 'cobol_processing_results' directory for generated reports")
        
    except Exception as e:
        logger.error(f"Test failed: {e}")
        import traceback
        traceback.print_exc()
    
    finally:
        # Clean up test file
        if Path(test_file).exists():
            Path(test_file).unlink()

def test_expected_vs_actual():
    """Test with the expected structure from the user's analysis."""
    print("\n=== Expected vs Actual Analysis ===")
    
    expected = {
        'programs': 1,
        'divisions': 4,
        'sections': 16,
        'paragraphs': 261,
        'statements': 316
    }
    
    # Simulate current parser results
    actual = {
        'programs': 1,
        'divisions': 0,  # Missing divisions
        'sections': 0,   # Missing sections
        'paragraphs': 103,  # Missing 158 paragraphs
        'statements': 0   # Missing statements
    }
    
    print("Expected Structure:")
    for entity_type, count in expected.items():
        print(f"  {entity_type}: {count}")
    
    print("\nCurrent Parser Results:")
    for entity_type, count in actual.items():
        print(f"  {entity_type}: {count}")
    
    print("\nDiscrepancies:")
    for entity_type in expected:
        expected_count = expected[entity_type]
        actual_count = actual.get(entity_type, 0)
        difference = expected_count - actual_count
        accuracy = (actual_count / expected_count * 100) if expected_count > 0 else 0
        
        print(f"  {entity_type}: Expected {expected_count}, Found {actual_count}, "
              f"Missing {difference}, Accuracy {accuracy:.1f}%")
    
    print("\nKey Issues Identified:")
    print("1. Missing Divisions: Parser not extracting IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE divisions")
    print("2. Missing Sections: Parser not extracting section entities")
    print("3. Missing Paragraphs: Only finding 103 out of 261 paragraphs")
    print("4. Missing Statements: Not extracting individual COBOL statements")
    
    print("\nRecommended Fixes:")
    print("1. Add specific regex patterns for each division type")
    print("2. Improve section detection to handle all section formats")
    print("3. Review paragraph validation logic to avoid over-filtering")
    print("4. Add comprehensive statement extraction for all COBOL verbs")
    print("5. Implement hierarchical parent-child relationships")

if __name__ == "__main__":
    # Run the expected vs actual analysis
    test_expected_vs_actual()
    
    # Run the full test
    asyncio.run(test_cobol_analysis())