#!/usr/bin/env python3
"""
COBOL Integration Tests

This module contains system integration tests for COBOL functionality:
- Multi-language parser integration
- Pipeline integration
- End-to-end functionality
- Performance benchmarks
"""

import os
import sys
import unittest
import time
import json
from pathlib import Path
from typing import Dict, List, Any

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser


class TestCOBOLSystemIntegration(unittest.TestCase):
    """Test COBOL integration with the broader system"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.test_file = "fixtures/test_cobol_banking.cbl"
        self.vasu_file = "fixtures/vasu/vasu_fraud_management_cobol_reformatted.cbl"
        
        # Ensure test files exist
        if not os.path.exists(self.test_file):
            self.test_file = os.path.join("cobol-support", "tests", self.test_file)
        if not os.path.exists(self.vasu_file):
            self.vasu_file = os.path.join("cobol-support", "tests", self.vasu_file)
    
    def test_multi_language_parser_integration(self):
        """Test COBOL integration with multi-language parser"""
        try:
            from shared.services.multi_language_parser import MultiLanguageParser
            
            parser = MultiLanguageParser()
            
            # Test language detection
            language = parser.detect_language(self.test_file)
            self.assertEqual(language, "cobol", "Language should be detected as COBOL")
            
            # Test parsing through multi-language parser
            result = parser.parse_file(self.test_file)
            
            self.assertIsNotNone(result, "Multi-language parser should return result")
            self.assertTrue(result.get("parse_success", False), "Multi-language parsing should succeed")
            self.assertEqual(result.get("language"), "cobol", "Language should be COBOL")
            
            # Verify entities were extracted
            entities = result.get("entities", [])
            self.assertGreater(len(entities), 0, "Should extract entities")
            
        except ImportError:
            self.skipTest("Multi-language parser not available")
    
    def test_ast_relationship_extractor_integration(self):
        """Test COBOL parser integration with AST relationship extractor"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        try:
            from ast_relationship_extractor import ASTRelationshipExtractor
            
            # Parse the file
            parse_result = self.parser.parse_file(self.test_file)
            self.assertTrue(parse_result.get("parse_success", False), "Parsing should succeed")
            
            # Extract relationships
            extractor = ASTRelationshipExtractor()
            relationships = extractor.extract_relationships(parse_result)
            
            self.assertIsInstance(relationships, list, "Relationships should be a list")
            self.assertGreaterEqual(len(relationships), 0, "Should extract relationships (may be empty)")
            
        except ImportError:
            self.skipTest("AST relationship extractor not available")
    
    def test_cobol_relationship_extractor_integration(self):
        """Test COBOL-specific relationship extractor integration"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        try:
            from services.cobol_relationship_extractor import extract_cobol_relationships
            
            # Parse the file
            parse_result = self.parser.parse_file(self.test_file)
            self.assertTrue(parse_result.get("parse_success", False), "Parsing should succeed")
            
            # Extract COBOL relationships
            relationships = extract_cobol_relationships(parse_result)
            
            self.assertIsInstance(relationships, list, "Relationships should be a list")
            self.assertGreaterEqual(len(relationships), 0, "Should extract relationships (may be empty)")
            
            # Check relationship structure
            for rel in relationships:
                self.assertIn("source_entity", rel.__dict__, "Relationship should have source_entity")
                self.assertIn("target_entity", rel.__dict__, "Relationship should have target_entity")
                self.assertIn("relationship_type", rel.__dict__, "Relationship should have relationship_type")
            
        except ImportError:
            self.skipTest("COBOL relationship extractor not available")
    
    def test_end_to_end_pipeline(self):
        """Test complete end-to-end pipeline"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        # Step 1: Parse COBOL file
        result = self.parser.parse_file(self.test_file)
        self.assertTrue(result.get("parse_success", False), "File parsing should succeed")
        
        # Step 2: Verify language detection
        self.assertEqual(result.get("language"), "cobol", "Language should be COBOL")
        
        # Step 3: Verify entities extraction
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 0, "Should extract entities")
        
        # Step 4: Verify compilation units
        compilation_units = result.get("compilation_units", [])
        self.assertGreater(len(compilation_units), 0, "Should extract compilation units")
        
        # Step 5: Verify statements extraction
        statements = result.get("statements", {})
        self.assertIsInstance(statements, dict, "Statements should be a dictionary")
        
        # Step 6: Verify data items extraction
        data_items = result.get("data_items", {})
        self.assertIsInstance(data_items, dict, "Data items should be a dictionary")
        
        # Step 7: Test JSON serialization
        try:
            json_str = json.dumps(result, indent=2, default=str)
            parsed_back = json.loads(json_str)
            self.assertEqual(parsed_back.get("parse_success"), result.get("parse_success"))
            self.assertEqual(parsed_back.get("language"), result.get("language"))
        except Exception as e:
            self.fail(f"JSON serialization failed: {e}")
    
    def test_large_file_processing(self):
        """Test processing of large COBOL files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        if not os.path.exists(self.vasu_file):
            self.skipTest(f"Test file not found: {self.vasu_file}")
        
        start_time = time.time()
        result = self.parser.parse_file(self.vasu_file)
        end_time = time.time()
        
        # Verify parsing succeeded
        self.assertTrue(result.get("parse_success", False), "Large file parsing should succeed")
        
        # Verify reasonable performance
        parse_time = end_time - start_time
        self.assertLess(parse_time, 30, f"Parsing should complete in reasonable time (took {parse_time:.2f}s)")
        
        # Verify substantial entity extraction
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 100, "Should extract many entities from large file")
        
        print(f"Large file processing: {len(entities)} entities in {parse_time:.2f}s")
    
    def test_error_handling(self):
        """Test error handling for various failure scenarios"""
        # Test with non-existent file
        result = self.parser.parse_file("non_existent_file.cbl")
        self.assertFalse(result.get("parse_success", True), "Non-existent file should fail")
        
        # Test with empty file
        empty_file = "empty_test.cbl"
        try:
            with open(empty_file, 'w') as f:
                f.write("")
            
            result = self.parser.parse_file(empty_file)
            # Empty file might succeed or fail, but shouldn't crash
            self.assertIsInstance(result, dict, "Should return a dictionary")
            
        finally:
            if os.path.exists(empty_file):
                os.remove(empty_file)
    
    def test_parser_availability_check(self):
        """Test parser availability checking"""
        # Test availability check
        is_available = self.parser.is_available()
        self.assertIsInstance(is_available, bool, "is_available should return boolean")
        
        if is_available:
            # If available, should be able to parse
            result = self.parser.parse_file(self.test_file)
            self.assertIsInstance(result, dict, "Should return result dictionary")
        else:
            # If not available, should handle gracefully
            result = self.parser.parse_file(self.test_file)
            self.assertIsInstance(result, dict, "Should return result dictionary even when not available")


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
        
        import psutil
        import gc
        
        # Get initial memory usage
        process = psutil.Process()
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB
        
        # Parse file
        result = self.parser.parse_file(self.test_files[0][1])
        
        # Get memory usage after parsing
        after_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_used = after_memory - initial_memory
        
        print(f"Memory usage: {memory_used:.2f} MB")
        
        # Memory usage should be reasonable (less than 100MB for small file)
        self.assertLess(memory_used, 100, f"Memory usage should be reasonable (used {memory_used:.2f} MB)")
        
        # Clean up
        del result
        gc.collect()


class TestCOBOLDataConsistency(unittest.TestCase):
    """Test data consistency and integrity"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.test_file = "fixtures/test_cobol_banking.cbl"
        
        if not os.path.exists(self.test_file):
            self.test_file = os.path.join("cobol-support", "tests", self.test_file)
    
    def test_entity_data_consistency(self):
        """Test that entity data is consistent"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        for entity in entities:
            with self.subTest(entity=entity.get("name", "unknown")):
                # Check required fields
                self.assertIn("type", entity, "Entity should have type")
                self.assertIn("name", entity, "Entity should have name")
                self.assertIn("file_path", entity, "Entity should have file_path")
                
                # Check properties structure
                properties = entity.get("properties", {})
                self.assertIsInstance(properties, dict, "Properties should be a dictionary")
                
                # Check line information consistency
                if "line" in properties:
                    line_value = properties["line"]
                    if "-" in line_value:
                        start, end = line_value.split("-", 1)
                        if start.isdigit() and end.isdigit():
                            self.assertLessEqual(int(start), int(end), "Start line should be <= end line")
    
    def test_relationship_data_consistency(self):
        """Test that relationship data is consistent"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        try:
            from services.cobol_relationship_extractor import extract_cobol_relationships
            
            result = self.parser.parse_file(self.test_file)
            relationships = extract_cobol_relationships(result)
            
            for rel in relationships:
                with self.subTest(relationship=rel):
                    # Check required fields
                    self.assertIsNotNone(rel.source_entity, "Relationship should have source_entity")
                    self.assertIsNotNone(rel.target_entity, "Relationship should have target_entity")
                    self.assertIsNotNone(rel.relationship_type, "Relationship should have relationship_type")
                    self.assertIsNotNone(rel.context, "Relationship should have context")
                    
                    # Check confidence score
                    self.assertGreaterEqual(rel.confidence, 0.0, "Confidence should be >= 0")
                    self.assertLessEqual(rel.confidence, 1.0, "Confidence should be <= 1")
                    
                    # Check relationship strength
                    self.assertIn(rel.relationship_strength, ["weak", "medium", "strong"], 
                                "Relationship strength should be valid")
        
        except ImportError:
            self.skipTest("COBOL relationship extractor not available")
    
    def test_json_serialization_consistency(self):
        """Test that data can be serialized and deserialized consistently"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        
        # Test JSON serialization
        try:
            json_str = json.dumps(result, indent=2, default=str)
            parsed_back = json.loads(json_str)
            
            # Verify key fields are preserved
            self.assertEqual(parsed_back.get("parse_success"), result.get("parse_success"))
            self.assertEqual(parsed_back.get("language"), result.get("language"))
            self.assertEqual(parsed_back.get("file_path"), result.get("file_path"))
            
            # Verify entities are preserved
            original_entities = result.get("entities", [])
            parsed_entities = parsed_back.get("entities", [])
            self.assertEqual(len(parsed_entities), len(original_entities))
            
        except Exception as e:
            self.fail(f"JSON serialization failed: {e}")


def run_integration_tests():
    """Run all integration tests"""
    print("🧪 Running COBOL Integration Tests")
    print("=" * 60)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLSystemIntegration))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLPerformance))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLDataConsistency))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 60)
    print(f"📊 Test Results: {result.testsRun} tests run")
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
    success = run_integration_tests()
    sys.exit(0 if success else 1)




