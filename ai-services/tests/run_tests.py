#!/usr/bin/env python3
"""
Test runner for AI Services
Runs all tests and provides coverage reporting
"""
import unittest
import sys
import os
from pathlib import Path

# Add necessary paths to Python path
current_path = Path(__file__).parent
ai_services_path = current_path.parent
parent_path = ai_services_path.parent

sys.path.insert(0, str(parent_path))  # For shared modules
sys.path.insert(0, str(ai_services_path))  # For ai-services modules

# Change working directory to enable relative imports
os.chdir(str(parent_path))

def run_tests():
    """Run all tests in the test suite"""
    # Discover and run tests
    loader = unittest.TestLoader()
    start_dir = Path(__file__).parent
    suite = loader.discover(str(start_dir), pattern='test_*.py')
    
    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "="*60)
    print("TEST SUMMARY")
    print("="*60)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Skipped: {len(result.skipped) if hasattr(result, 'skipped') else 0}")
    
    # Print details of failures and errors
    if result.failures:
        print("\nFAILURES:")
        for test, traceback in result.failures:
            print(f"- {test}: {traceback}")
    
    if result.errors:
        print("\nERRORS:")
        for test, traceback in result.errors:
            print(f"- {test}: {traceback}")
    
    # Return success status
    return len(result.failures) == 0 and len(result.errors) == 0

if __name__ == '__main__':
    success = run_tests()
    sys.exit(0 if success else 1)