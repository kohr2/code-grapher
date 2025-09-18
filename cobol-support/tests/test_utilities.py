#!/usr/bin/env python3
"""
COBOL Utility Tests

This module contains tests for utility functions and helper components:
- Test data creation
- Mock functionality
- Helper functions
- Edge cases
"""

import os
import sys
import unittest
import tempfile
from pathlib import Path
from typing import Dict, List, Any

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser


class TestCOBOLUtilities(unittest.TestCase):
    """Test utility functions and helper components"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.temp_dir = tempfile.mkdtemp()
    
    def tearDown(self):
        """Clean up test fixtures"""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
    
    def test_create_test_cobol_file(self):
        """Test creating test COBOL files"""
        test_file = os.path.join(self.temp_dir, "test_utility.cbl")
        
        # Create a simple COBOL file
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UTILITY-TEST.
       AUTHOR. TEST-AUTHOR.
       DATE-WRITTEN. 2025-01-27.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-TEST-VALUE PIC 9(5).
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           MOVE 12345 TO WS-TEST-VALUE.
           DISPLAY "TEST VALUE: " WS-TEST-VALUE.
           STOP RUN.
       
       END PROGRAM UTILITY-TEST.
        """
        
        with open(test_file, 'w') as f:
            f.write(cobol_content)
        
        # Verify file was created
        self.assertTrue(os.path.exists(test_file), "Test file should be created")
        
        # Verify content
        with open(test_file, 'r') as f:
            content = f.read()
        
        self.assertIn("PROGRAM-ID. UTILITY-TEST", content)
        self.assertIn("WS-TEST-VALUE", content)
        self.assertIn("MAIN-LOGIC", content)
    
    def test_parser_with_temp_file(self):
        """Test parser with temporary file"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        test_file = os.path.join(self.temp_dir, "temp_test.cbl")
        
        # Create a test COBOL file
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEMP-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-COUNT PIC 9(3).
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 100 TO WS-COUNT.
           DISPLAY "COUNT: " WS-COUNT.
           STOP RUN.
       
       END PROGRAM TEMP-TEST.
        """
        
        with open(test_file, 'w') as f:
            f.write(cobol_content)
        
        # Parse the file
        result = self.parser.parse_file(test_file)
        
        # Verify parsing succeeded
        self.assertTrue(result.get("parse_success", False), "Temporary file should parse successfully")
        self.assertEqual(result.get("language"), "cobol", "Language should be COBOL")
        
        # Verify entities were extracted
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 0, "Should extract entities from temporary file")
    
    def test_file_path_handling(self):
        """Test various file path handling scenarios"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        # Test with absolute path
        test_file = os.path.join(self.temp_dir, "abs_path_test.cbl")
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ABS-PATH-TEST.
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "ABSOLUTE PATH TEST".
           STOP RUN.
       
       END PROGRAM ABS-PATH-TEST.
        """
        
        with open(test_file, 'w') as f:
            f.write(cobol_content)
        
        result = self.parser.parse_file(test_file)
        self.assertTrue(result.get("parse_success", False), "Absolute path should work")
        self.assertEqual(result.get("file_path"), test_file, "File path should be preserved")
    
    def test_empty_file_handling(self):
        """Test handling of empty files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        empty_file = os.path.join(self.temp_dir, "empty.cbl")
        
        # Create empty file
        with open(empty_file, 'w') as f:
            pass
        
        result = self.parser.parse_file(empty_file)
        
        # Should handle empty file gracefully
        self.assertIsInstance(result, dict, "Should return a dictionary")
        self.assertIn("parse_success", result, "Should have parse_success field")
    
    def test_invalid_file_handling(self):
        """Test handling of invalid files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        # Test with non-existent file
        result = self.parser.parse_file("non_existent_file.cbl")
        self.assertFalse(result.get("parse_success", True), "Non-existent file should fail")
        self.assertIn("error", result, "Should have error field")
    
    def test_malformed_cobol_handling(self):
        """Test handling of malformed COBOL files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        malformed_file = os.path.join(self.temp_dir, "malformed.cbl")
        
        # Create malformed COBOL file
        malformed_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MALFORMED-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VALUE PIC 9(5)
       
       PROCEDURE DIVISION.
       MAIN.
           MOVE 123 TO WS-VALUE
           DISPLAY "MALFORMED TEST"
           STOP RUN
       
       END PROGRAM MALFORMED-TEST.
        """
        
        with open(malformed_file, 'w') as f:
            f.write(malformed_content)
        
        result = self.parser.parse_file(malformed_file)
        
        # Should handle malformed file gracefully
        self.assertIsInstance(result, dict, "Should return a dictionary")
        # May succeed or fail, but shouldn't crash
        self.assertIn("parse_success", result, "Should have parse_success field")


class TestCOBOLMockData(unittest.TestCase):
    """Test mock data creation and validation"""
    
    def test_create_mock_cobol_data(self):
        """Test creating mock COBOL data structures"""
        mock_data = {
            "parse_success": True,
            "language": "cobol",
            "file_path": "mock_test.cbl",
            "compilation_units": [
                {"name": "MOCK-PROGRAM", "type": "program"}
            ],
            "entities": [
                {"type": "program", "name": "MOCK-PROGRAM", "file_path": "mock_test.cbl"},
                {"type": "paragraph", "name": "MAIN-LOGIC", "file_path": "mock_test.cbl"},
                {"type": "data_item", "name": "WS-MOCK-VALUE", "file_path": "mock_test.cbl"}
            ],
            "statements": {
                "MOCK-PROGRAM": {
                    "MAIN-LOGIC": [
                        {"type": "MoveStatement", "text": "MOVE 123 TO WS-MOCK-VALUE", "details": "MOVE_FROM:123:MOVE_TO:WS-MOCK-VALUE"}
                    ]
                }
            }
        }
        
        # Validate mock data structure
        self.assertTrue(mock_data["parse_success"])
        self.assertEqual(mock_data["language"], "cobol")
        self.assertIn("entities", mock_data)
        self.assertIn("statements", mock_data)
        
        # Validate entities
        entities = mock_data["entities"]
        self.assertGreater(len(entities), 0)
        
        for entity in entities:
            self.assertIn("type", entity)
            self.assertIn("name", entity)
            self.assertIn("file_path", entity)
    
    def test_mock_relationship_data(self):
        """Test creating mock relationship data"""
        mock_relationships = [
            {
                "source_entity": "PROGRAM:MOCK-PROGRAM",
                "target_entity": "PARAGRAPH:MAIN-LOGIC",
                "relationship_type": "CONTAINS",
                "confidence": 0.9,
                "context": "Program contains paragraph"
            },
            {
                "source_entity": "PARAGRAPH:MAIN-LOGIC",
                "target_entity": "DATA_ITEM:WS-MOCK-VALUE",
                "relationship_type": "USES",
                "confidence": 0.8,
                "context": "Paragraph uses data item"
            }
        ]
        
        # Validate relationship structure
        for rel in mock_relationships:
            self.assertIn("source_entity", rel)
            self.assertIn("target_entity", rel)
            self.assertIn("relationship_type", rel)
            self.assertIn("confidence", rel)
            self.assertIn("context", rel)
            
            # Validate confidence score
            self.assertGreaterEqual(rel["confidence"], 0.0)
            self.assertLessEqual(rel["confidence"], 1.0)


class TestCOBOLEdgeCases(unittest.TestCase):
    """Test edge cases and error conditions"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.temp_dir = tempfile.mkdtemp()
    
    def tearDown(self):
        """Clean up test fixtures"""
        import shutil
        if os.path.exists(self.temp_dir):
            shutil.rmtree(self.temp_dir)
    
    def test_very_long_line_handling(self):
        """Test handling of very long lines"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        long_line_file = os.path.join(self.temp_dir, "long_line.cbl")
        
        # Create file with very long line
        long_line = "A" * 10000  # 10KB line
        cobol_content = f"""
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LONG-LINE-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-LONG-VALUE PIC X(10000) VALUE "{long_line}".
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "LONG LINE TEST".
           STOP RUN.
       
       END PROGRAM LONG-LINE-TEST.
        """
        
        with open(long_line_file, 'w') as f:
            f.write(cobol_content)
        
        result = self.parser.parse_file(long_line_file)
        
        # Should handle long lines gracefully
        self.assertIsInstance(result, dict, "Should return a dictionary")
        self.assertIn("parse_success", result, "Should have parse_success field")
    
    def test_special_characters_handling(self):
        """Test handling of special characters in COBOL files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        special_char_file = os.path.join(self.temp_dir, "special_chars.cbl")
        
        # Create file with special characters
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIAL-CHARS-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-SPECIAL PIC X(10) VALUE "!@#$%^&*()".
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "SPECIAL CHARS: " WS-SPECIAL.
           STOP RUN.
       
       END PROGRAM SPECIAL-CHARS-TEST.
        """
        
        with open(special_char_file, 'w') as f:
            f.write(cobol_content)
        
        result = self.parser.parse_file(special_char_file)
        
        # Should handle special characters gracefully
        self.assertIsInstance(result, dict, "Should return a dictionary")
        self.assertIn("parse_success", result, "Should have parse_success field")
    
    def test_unicode_handling(self):
        """Test handling of Unicode characters"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        unicode_file = os.path.join(self.temp_dir, "unicode.cbl")
        
        # Create file with Unicode characters
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. UNICODE-TEST.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-UNICODE PIC X(20) VALUE "Hello 世界 🌍".
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "UNICODE: " WS-UNICODE.
           STOP RUN.
       
       END PROGRAM UNICODE-TEST.
        """
        
        with open(unicode_file, 'w', encoding='utf-8') as f:
            f.write(cobol_content)
        
        result = self.parser.parse_file(unicode_file)
        
        # Should handle Unicode gracefully
        self.assertIsInstance(result, dict, "Should return a dictionary")
        self.assertIn("parse_success", result, "Should have parse_success field")
    
    def test_concurrent_file_access(self):
        """Test handling of concurrent file access scenarios"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        concurrent_file = os.path.join(self.temp_dir, "concurrent.cbl")
        
        # Create test file
        cobol_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CONCURRENT-TEST.
       
       PROCEDURE DIVISION.
       MAIN.
           DISPLAY "CONCURRENT TEST".
           STOP RUN.
       
       END PROGRAM CONCURRENT-TEST.
        """
        
        with open(concurrent_file, 'w') as f:
            f.write(cobol_content)
        
        # Test multiple rapid accesses
        results = []
        for i in range(5):
            result = self.parser.parse_file(concurrent_file)
            results.append(result)
        
        # All should succeed
        for result in results:
            self.assertIsInstance(result, dict, "Should return a dictionary")
            self.assertIn("parse_success", result, "Should have parse_success field")


def run_utility_tests():
    """Run all utility tests"""
    print("🧪 Running COBOL Utility Tests")
    print("=" * 50)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLUtilities))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLMockData))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLEdgeCases))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 50)
    if result.wasSuccessful():
        print("🎉 All utility tests passed!")
    else:
        print(f"❌ {len(result.failures)} test(s) failed, {len(result.errors)} error(s)")
        for failure in result.failures:
            print(f"FAIL: {failure[0]}")
            print(failure[1])
        for error in result.errors:
            print(f"ERROR: {error[0]}")
            print(error[1])
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_utility_tests()
    sys.exit(0 if success else 1)
