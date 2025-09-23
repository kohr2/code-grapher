#!/usr/bin/env python3
"""
Main COBOL Test Runner

This script runs all COBOL tests and provides comprehensive reporting.
Supports different test modes: unit, integration, performance, and all.
"""

import os
import sys
import argparse
import time
from pathlib import Path

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))


def run_parser_tests():
    """Run parser tests"""
    print("🧪 Running Parser Tests")
    print("=" * 50)
    
    try:
        from test_parser import run_parser_tests
        return run_parser_tests()
    except ImportError as e:
        print(f"❌ Failed to import parser tests: {e}")
        return False


def run_relationship_tests():
    """Run relationship tests"""
    print("🧪 Running Relationship Tests")
    print("=" * 50)
    
    try:
        from test_relationships import run_relationship_tests
        return run_relationship_tests()
    except ImportError as e:
        print(f"❌ Failed to import relationship tests: {e}")
        return False


def run_integration_tests():
    """Run integration tests"""
    print("🧪 Running Integration Tests")
    print("=" * 50)
    
    try:
        from test_integration import run_integration_tests
        return run_integration_tests()
    except ImportError as e:
        print(f"❌ Failed to import integration tests: {e}")
        return False


def run_utility_tests():
    """Run utility tests"""
    print("🧪 Running Utility Tests")
    print("=" * 50)
    
    try:
        from test_utilities import run_utility_tests
        return run_utility_tests()
    except ImportError as e:
        print(f"❌ Failed to import utility tests: {e}")
        return False


def run_performance_tests():
    """Run performance tests"""
    print("🧪 Running Performance Tests")
    print("=" * 50)
    
    try:
        from test_integration import TestCOBOLPerformance
        import unittest
        
        # Create test suite for performance tests only
        loader = unittest.TestLoader()
        suite = loader.loadTestsFromTestCase(TestCOBOLPerformance)
        
        # Run tests
        runner = unittest.TextTestRunner(verbosity=2)
        result = runner.run(suite)
        
        return result.wasSuccessful()
    except ImportError as e:
        print(f"❌ Failed to import performance tests: {e}")
        return False


def run_all_tests():
    """Run all tests"""
    print("🚀 Running All COBOL Tests")
    print("=" * 60)
    
    start_time = time.time()
    
    test_suites = [
        ("Parser Tests", run_parser_tests),
        ("Relationship Tests", run_relationship_tests),
        ("Integration Tests", run_integration_tests),
        ("Utility Tests", run_utility_tests),
    ]
    
    results = []
    
    for suite_name, suite_func in test_suites:
        print(f"\n{'='*60}")
        print(f"Running {suite_name}")
        print('='*60)
        
        suite_start = time.time()
        success = suite_func()
        suite_end = time.time()
        
        results.append({
            'name': suite_name,
            'success': success,
            'duration': suite_end - suite_start
        })
        
        status = "✅ PASSED" if success else "❌ FAILED"
        print(f"\n{suite_name}: {status} ({suite_end - suite_start:.2f}s)")
    
    end_time = time.time()
    total_duration = end_time - start_time
    
    # Print summary
    print(f"\n{'='*60}")
    print("TEST SUMMARY")
    print('='*60)
    
    passed = sum(1 for r in results if r['success'])
    total = len(results)
    
    print(f"Total test suites: {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {total - passed}")
    print(f"Total duration: {total_duration:.2f}s")
    
    print(f"\nDetailed Results:")
    for result in results:
        status = "✅ PASSED" if result['success'] else "❌ FAILED"
        print(f"  {result['name']}: {status} ({result['duration']:.2f}s)")
    
    if passed == total:
        print(f"\n🎉 All test suites passed!")
        return True
    else:
        print(f"\n❌ {total - passed} test suite(s) failed!")
        return False


def main():
    """Main test runner"""
    parser = argparse.ArgumentParser(description="COBOL Test Runner")
    parser.add_argument(
        "--mode", 
        choices=["all", "parser", "relationships", "integration", "utilities", "performance"],
        default="all",
        help="Test mode to run"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose output"
    )
    
    args = parser.parse_args()
    
    print("🚀 COBOL Test Runner")
    print("=" * 60)
    print(f"Mode: {args.mode}")
    print(f"Verbose: {args.verbose}")
    print("=" * 60)
    
    success = False
    
    if args.mode == "all":
        success = run_all_tests()
    elif args.mode == "parser":
        success = run_parser_tests()
    elif args.mode == "relationships":
        success = run_relationship_tests()
    elif args.mode == "integration":
        success = run_integration_tests()
    elif args.mode == "utilities":
        success = run_utility_tests()
    elif args.mode == "performance":
        success = run_performance_tests()
    
    print(f"\n{'='*60}")
    if success:
        print("🎉 All tests completed successfully!")
        return 0
    else:
        print("❌ Some tests failed!")
        return 1


if __name__ == "__main__":
    sys.exit(main())




