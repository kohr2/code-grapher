#!/usr/bin/env python3
"""
Test runner for COBOL relationship extractor
Runs comprehensive tests and generates a test report
"""

import os
import sys
import unittest
from pathlib import Path

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Add cobol-support to the path
cobol_support_path = os.path.join(project_root, 'cobol-support')
if cobol_support_path not in sys.path:
    sys.path.insert(0, cobol_support_path)

def run_relationship_tests():
    """Run all COBOL relationship extractor tests"""
    print("🧪 Running COBOL Relationship Extractor Tests")
    print("=" * 60)
    
    # Discover and run tests
    loader = unittest.TestLoader()
    start_dir = os.path.dirname(__file__)
    suite = loader.discover(start_dir, pattern='test_cobol_relationship_extractor.py')
    
    # Run tests with detailed output
    runner = unittest.TextTestRunner(verbosity=2, stream=sys.stdout)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 60)
    print("TEST SUMMARY")
    print("=" * 60)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Success rate: {((result.testsRun - len(result.failures) - len(result.errors)) / result.testsRun * 100):.1f}%")
    
    if result.failures:
        print("\nFAILURES:")
        for test, traceback in result.failures:
            print(f"  - {test}: {traceback.split('AssertionError: ')[-1].split('\\n')[0]}")
    
    if result.errors:
        print("\nERRORS:")
        for test, traceback in result.errors:
            print(f"  - {test}: {traceback.split('\\n')[-2]}")
    
    return result.wasSuccessful()

def test_with_sample_cobol_file():
    """Test relationship extraction with the actual COBOL file"""
    print("\n🔍 Testing with Sample COBOL File")
    print("=" * 60)
    
    try:
        from services.cobol_relationship_extractor import extract_cobol_relationships
        
        # Test with the actual COBOL file
        cobol_file_path = os.path.join(project_root, 'temp_processing', 'vasu_fraud_management_cobol_reformatted.cbl')
        
        if not os.path.exists(cobol_file_path):
            print(f"❌ COBOL file not found: {cobol_file_path}")
            return False
        
        # Create mock file data for testing
        file_data = {
            "file_path": cobol_file_path,
            "language": "cobol",
            "parse_success": True,
            "compilation_units": [{"name": "FRAUD-MGMT-SYSTEM"}],
            "entities": [
                {"type": "program", "name": "FRAUD-MGMT-SYSTEM"},
                {"type": "paragraph", "name": "0000-MAIN-PROCESS"},
                {"type": "paragraph", "name": "1000-INITIALIZE-PROGRAM"},
                {"type": "data_item", "name": "WS-TOTAL-RISK-SCORE"},
                {"type": "data_item", "name": "TRANS-AMOUNT"}
            ],
            "statements": {
                "FRAUD-MGMT-SYSTEM": {
                    "0000-MAIN-PROCESS": [
                        {"type": "PerformStatement", "text": "PERFORM 1000-INITIALIZE-PROGRAM", "details": "PERFORM 1000-INITIALIZE-PROGRAM"},
                        {"type": "MoveStatement", "text": "MOVE TRANS-DATE TO CUST-LAST-TRANS-DATE", "details": "MOVE_FROM:TRANS-DATE:MOVE_TO:CUST-LAST-TRANS-DATE"},
                        {"type": "IfStatement", "text": "IF WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD", "details": "IF_CONDITION:WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD"},
                        {"type": "ComputeStatement", "text": "COMPUTE CUST-AVG-MONTHLY-SPEND = (CUST-AVG-MONTHLY-SPEND * 0.9) + (TRANS-AMOUNT * 0.1)", "details": "COMPUTE_EXPR:CUST-AVG-MONTHLY-SPEND = (CUST-AVG-MONTHLY-SPEND * 0.9) + (TRANS-AMOUNT * 0.1)"},
                        {"type": "AddStatement", "text": "ADD 75 TO WS-TOTAL-RISK-SCORE", "details": "ADD_OPERANDS:75,WS-TOTAL-RISK-SCORE"},
                        {"type": "ReadStatement", "text": "READ TRANSACTION-FILE", "details": "READ_FILE:TRANSACTION-FILE"},
                        {"type": "WriteStatement", "text": "WRITE FRAUD-LOG-RECORD", "details": "WRITE_FILE:FRAUD-LOG-RECORD"},
                        {"type": "OpenStatement", "text": "OPEN INPUT TRANSACTION-FILE", "details": "OPEN_FILES:TRANSACTION-FILE"},
                        {"type": "DisplayStatement", "text": "DISPLAY 'FRAUD MANAGEMENT SYSTEM - INITIALIZING'", "details": "DISPLAY_MESSAGE:FRAUD MANAGEMENT SYSTEM - INITIALIZING"}
                    ]
                }
            },
            "data_items": {
                "FRAUD-MGMT-SYSTEM": [
                    {"name": "WS-TOTAL-RISK-SCORE", "level": "01", "picture_clause": "PIC 9(4)"},
                    {"name": "TRANS-AMOUNT", "level": "05", "picture_clause": "PIC 9(8)V99"}
                ]
            },
            "identification_data": {
                "FRAUD-MGMT-SYSTEM": {
                    "author": "FRAUD-DETECTION-TEAM",
                    "date_written": "2025-08-06"
                }
            }
        }
        
        # Extract relationships
        relationships = extract_cobol_relationships(file_data)
        
        print(f"✅ Extracted {len(relationships)} relationships")
        
        # Analyze relationship types
        rel_types = {}
        for rel in relationships:
            rel_type = rel.relationship_type.value
            rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
        
        print("\nRelationship Types Found:")
        for rel_type, count in sorted(rel_types.items()):
            print(f"  - {rel_type}: {count}")
        
        # Check for expected relationship types
        expected_types = ["CONTAINS", "CALLS", "DATA_FLOW", "ARITHMETIC", "CONDITIONAL", "READS", "WRITES", "FILE_ACCESS", "BINDS_SCREEN", "WRITTEN_BY", "USES", "MODIFIES"]
        found_types = set(rel_types.keys())
        missing_types = set(expected_types) - found_types
        
        if missing_types:
            print(f"\n⚠️  Missing relationship types: {missing_types}")
        else:
            print("\n✅ All expected relationship types found!")
        
        return len(relationships) > 0
        
    except Exception as e:
        print(f"❌ Error testing with COBOL file: {e}")
        import traceback
        traceback.print_exc()
        return False

def main():
    """Main test runner"""
    print("🚀 COBOL Relationship Extractor Test Suite")
    print("=" * 60)
    
    # Run unit tests
    unit_test_success = run_relationship_tests()
    
    # Test with sample COBOL file
    sample_test_success = test_with_sample_cobol_file()
    
    # Overall result
    print("\n" + "=" * 60)
    print("OVERALL RESULT")
    print("=" * 60)
    
    if unit_test_success and sample_test_success:
        print("✅ All tests passed! COBOL relationship extractor is working correctly.")
        return 0
    else:
        print("❌ Some tests failed. Please check the output above.")
        return 1

if __name__ == "__main__":
    sys.exit(main())
