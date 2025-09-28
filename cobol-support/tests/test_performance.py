#!/usr/bin/env python3
"""
COBOL Performance Tests

This module contains performance and stress tests for COBOL functionality:
- Large file processing
- Memory usage
- Speed benchmarks
- Stress testing
"""

import os
import sys
import unittest
import time
import psutil
import gc
from pathlib import Path
from typing import Dict, List, Any

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser


class TestCOBOLPerformance(unittest.TestCase):
    """Test COBOL parsing performance"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.test_files = [
            ("Small file", "fixtures/test_cobol_banking.cbl"),
            ("Large file", "fixtures/vasu/vasu_fraud_management_cobol_reformatted.cbl")
        ]
    
    def test_parsing_performance(self):
        """Test parsing performance with different file sizes"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        results = []
        
        for test_name, test_file in self.test_files:
            if not os.path.exists(test_file):
                test_file = os.path.join("cobol-support", "tests", test_file)
            
            if not os.path.exists(test_file):
                print(f"⚠️  Skipping {test_name}: file not found")
                continue
            
            print(f"📄 Testing {test_name}: {test_file}")
            
            start_time = time.time()
            result = self.parser.parse_file(test_file)
            end_time = time.time()
            
            if result.get("parse_success", False):
                entities = result.get("entities", [])
                parse_time = end_time - start_time
                entities_per_second = len(entities) / parse_time if parse_time > 0 else 0
                
                print(f"✅ Parsed {len(entities)} entities in {parse_time:.2f}s ({entities_per_second:.1f} entities/s)")
                
                results.append({
                    "name": test_name,
                    "entities": len(entities),
                    "time": parse_time,
                    "entities_per_second": entities_per_second
                })
            else:
                print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
        
        # Verify we got some results
        self.assertGreater(len(results), 0, "Should have at least one successful parse")
        
        # Print performance summary
        if results:
            print(f"\n📈 Performance Summary:")
            for result in results:
                print(f"  {result['name']}: {result['entities']} entities in {result['time']:.2f}s "
                      f"({result['entities_per_second']:.1f} entities/s)")
    
    def test_memory_usage(self):
        """Test memory usage during parsing"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        # Get initial memory usage
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB
        
        # Parse file
        test_file = self.test_files[0][1]
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            self.skipTest(f"Test file not found: {test_file}")
        
        result = self.parser.parse_file(test_file)
        
        # Get memory usage after parsing
        after_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_used = after_memory - initial_memory
        
        print(f"Memory usage: {memory_used:.2f} MB")
        
        # Memory usage should be reasonable (less than 100MB for small file)
        self.assertLess(memory_used, 100, f"Memory usage should be reasonable (used {memory_used:.2f} MB)")
        
        # Clean up
        del result
        gc.collect()
    
    def test_large_file_performance(self):
        """Test performance with large files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        large_file = self.test_files[1][1]
        if not os.path.exists(large_file):
            large_file = os.path.join("cobol-support", "tests", large_file)
        
        if not os.path.exists(large_file):
            self.skipTest(f"Large test file not found: {large_file}")
        
        print(f"📄 Testing large file: {large_file}")
        
        start_time = time.time()
        result = self.parser.parse_file(large_file)
        end_time = time.time()
        
        parse_time = end_time - start_time
        
        if result.get("parse_success", False):
            entities = result.get("entities", [])
            entities_per_second = len(entities) / parse_time if parse_time > 0 else 0
            
            print(f"✅ Large file: {len(entities)} entities in {parse_time:.2f}s ({entities_per_second:.1f} entities/s)")
            
            # Performance should be reasonable (less than 30 seconds for this file)
            self.assertLess(parse_time, 30, f"Large file parsing should complete in reasonable time (took {parse_time:.2f}s)")
            
            # Should extract substantial number of entities
            self.assertGreater(len(entities), 100, "Should extract many entities from large file")
        else:
            self.fail(f"Large file parsing failed: {result.get('error', 'Unknown error')}")
    
    def test_concurrent_parsing(self):
        """Test concurrent parsing performance"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        test_file = self.test_files[0][1]
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            self.skipTest(f"Test file not found: {test_file}")
        
        # Test multiple rapid accesses
        num_concurrent = 5
        start_time = time.time()
        
        results = []
        for i in range(num_concurrent):
            result = self.parser.parse_file(test_file)
            results.append(result)
        
        end_time = time.time()
        total_time = end_time - start_time
        
        # All should succeed
        successful = sum(1 for r in results if r.get("parse_success", False))
        print(f"Concurrent parsing: {successful}/{num_concurrent} successful in {total_time:.2f}s")
        
        self.assertEqual(successful, num_concurrent, "All concurrent parses should succeed")
        
        # Should complete in reasonable time
        self.assertLess(total_time, 10, f"Concurrent parsing should complete in reasonable time (took {total_time:.2f}s)")


class TestCOBOLStress(unittest.TestCase):
    """Test COBOL parsing under stress conditions"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
    
    def test_repeated_parsing(self):
        """Test repeated parsing of the same file"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        test_file = "fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            self.skipTest(f"Test file not found: {test_file}")
        
        num_iterations = 10
        start_time = time.time()
        
        for i in range(num_iterations):
            result = self.parser.parse_file(test_file)
            self.assertTrue(result.get("parse_success", False), f"Iteration {i+1} should succeed")
            
            # Clean up
            del result
            gc.collect()
        
        end_time = time.time()
        total_time = end_time - start_time
        avg_time = total_time / num_iterations
        
        print(f"Repeated parsing: {num_iterations} iterations in {total_time:.2f}s (avg: {avg_time:.2f}s)")
        
        # Should maintain consistent performance
        self.assertLess(avg_time, 5, f"Average parsing time should be reasonable (avg: {avg_time:.2f}s)")
    
    def test_memory_leak_detection(self):
        """Test for memory leaks during repeated parsing"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        test_file = "fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            self.skipTest(f"Test file not found: {test_file}")
        
        process = psutil.Process()
        
        # Get initial memory
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB
        
        # Parse multiple times
        num_iterations = 20
        for i in range(num_iterations):
            result = self.parser.parse_file(test_file)
            self.assertTrue(result.get("parse_success", False), f"Iteration {i+1} should succeed")
            
            # Clean up
            del result
            gc.collect()
        
        # Get final memory
        final_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_increase = final_memory - initial_memory
        
        print(f"Memory leak test: {memory_increase:.2f} MB increase over {num_iterations} iterations")
        
        # Memory increase should be minimal (less than 50MB)
        self.assertLess(memory_increase, 50, f"Memory increase should be minimal (increased {memory_increase:.2f} MB)")
    
    def test_error_recovery(self):
        """Test error recovery and resilience"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        # Test with various error conditions
        error_tests = [
            ("Non-existent file", "non_existent_file.cbl"),
            ("Empty file", ""),
            ("Invalid content", "invalid_content.cbl")
        ]
        
        for test_name, test_input in error_tests:
            with self.subTest(test=test_name):
                if test_input == "":
                    # Create empty file
                    empty_file = "empty_test.cbl"
                    try:
                        with open(empty_file, 'w') as f:
                            pass
                        result = self.parser.parse_file(empty_file)
                    finally:
                        if os.path.exists(empty_file):
                            os.remove(empty_file)
                else:
                    result = self.parser.parse_file(test_input)
                
                # Should handle errors gracefully
                self.assertIsInstance(result, dict, f"{test_name} should return a dictionary")
                self.assertIn("parse_success", result, f"{test_name} should have parse_success field")
                
                # Should not crash
                self.assertIsNotNone(result, f"{test_name} should not return None")


def run_performance_tests():
    """Run all performance tests"""
    print("🧪 Running COBOL Performance Tests")
    print("=" * 60)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLPerformance))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLStress))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 60)
    print(f"📊 Performance Test Results: {result.testsRun} tests run")
    print(f"✅ Passed: {result.testsRun - len(result.failures) - len(result.errors)}")
    print(f"❌ Failed: {len(result.failures)}")
    print(f"💥 Errors: {len(result.errors)}")
    
    if result.failures:
        print("\n❌ FAILURES:")
        for test, traceback in result.failures:
            print(f"  - {test}: {traceback}")
    
    if result.errors:
        print("\n💥 ERRORS:")
        for test, traceback in result.errors:
            print(f"  - {test}: {traceback}")
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_performance_tests()
    sys.exit(0 if success else 1)




