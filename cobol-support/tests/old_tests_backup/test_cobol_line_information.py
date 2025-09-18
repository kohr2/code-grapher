#!/usr/bin/env python3
"""
Unit Tests for COBOL Entity Line Information

This test suite verifies that COBOL entities are properly extracted with
accurate line range information and line count calculations.
"""

import os
import sys
import unittest
from pathlib import Path
from typing import Dict, List, Any

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser
from cobol_section_extractor import COBOLSectionExtractor, COBOLSection


class TestCOBOLLineInformation(unittest.TestCase):
    """Test cases for COBOL entity line information functionality"""
    
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
    
    def test_parser_availability(self):
        """Test that COBOL parser is available"""
        self.assertTrue(
            self.parser.is_available(),
            "COBOL parser should be available for testing"
        )
    
    def test_entity_line_range_format(self):
        """Test that entities have proper line range format"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        self.assertTrue(result.get("parse_success", False), "COBOL parsing should succeed")
        
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 0, "Should extract at least some entities")
        
        for entity in entities:
            with self.subTest(entity=entity.get("name", "unknown")):
                properties = entity.get("properties", {})
                
                # Check that line property exists and has correct format
                self.assertIn("line", properties, f"Entity {entity.get('name')} should have line property")
                line_value = properties["line"]
                
                # Line should be in format "start-end" or single number
                if "-" in line_value:
                    start, end = line_value.split("-", 1)
                    self.assertTrue(start.isdigit(), f"Start line should be numeric: {start}")
                    self.assertTrue(end.isdigit(), f"End line should be numeric: {end}")
                    self.assertLessEqual(int(start), int(end), "Start line should be <= end line")
                else:
                    self.assertTrue(line_value.isdigit(), f"Line should be numeric: {line_value}")
    
    def test_entity_line_count_calculation(self):
        """Test that line_count is calculated correctly"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        for entity in entities:
            with self.subTest(entity=entity.get("name", "unknown")):
                properties = entity.get("properties", {})
                
                # Check that line_count property exists
                self.assertIn("line_count", properties, f"Entity {entity.get('name')} should have line_count property")
                line_count = properties["line_count"]
                
                # line_count should be a positive integer
                self.assertIsInstance(line_count, int, "line_count should be an integer")
                self.assertGreater(line_count, 0, "line_count should be positive")
                
                # Verify line_count matches calculated value
                start_line = properties.get("start_line", 0)
                end_line = properties.get("end_line", start_line)
                expected_count = end_line - start_line + 1
                self.assertEqual(line_count, expected_count, 
                               f"line_count {line_count} should equal {expected_count} (end_line - start_line + 1)")
    
    def test_paragraph_entities_have_line_ranges(self):
        """Test that paragraph entities have proper line ranges"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        paragraph_entities = [e for e in entities if e.get("type") == "paragraph"]
        self.assertGreater(len(paragraph_entities), 0, "Should have at least one paragraph entity")
        
        for paragraph in paragraph_entities:
            with self.subTest(paragraph=paragraph.get("name", "unknown")):
                properties = paragraph.get("properties", {})
                
                # Paragraphs should have line ranges (not single lines)
                line_value = properties.get("line", "")
                self.assertIn("-", line_value, f"Paragraph {paragraph.get('name')} should have line range")
                
                # Should have start_line and end_line properties
                self.assertIn("start_line", properties, "Should have start_line property")
                self.assertIn("end_line", properties, "Should have end_line property")
                
                start_line = properties["start_line"]
                end_line = properties["end_line"]
                
                # End line should be >= start line
                self.assertLessEqual(start_line, end_line, "End line should be >= start line")
                
                # For paragraphs, end line should typically be > start line
                if paragraph.get("name") != "unknown":
                    self.assertGreater(end_line, start_line, 
                                     f"Paragraph {paragraph.get('name')} should span multiple lines")
    
    def test_data_item_entities_have_line_info(self):
        """Test that data item entities have line information"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        data_entities = [e for e in entities if e.get("type") == "data_item"]
        
        if len(data_entities) > 0:  # Skip if no data items found
            for data_item in data_entities:
                with self.subTest(data_item=data_item.get("name", "unknown")):
                    properties = data_item.get("properties", {})
                    
                    # Data items should have line information
                    self.assertIn("line", properties, "Data item should have line property")
                    self.assertIn("line_count", properties, "Data item should have line_count property")
                    self.assertIn("start_line", properties, "Data item should have start_line property")
                    self.assertIn("end_line", properties, "Data item should have end_line property")
                    
                    # Data items typically span single lines
                    line_count = properties["line_count"]
                    self.assertEqual(line_count, 1, "Data items should typically span 1 line")
                    
                    start_line = properties["start_line"]
                    end_line = properties["end_line"]
                    self.assertEqual(start_line, end_line, "Data item start and end lines should be equal")
    
    def test_vasu_file_comprehensive_parsing(self):
        """Test comprehensive parsing of the Vasu fraud management file"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        if not os.path.exists(self.vasu_file):
            self.skipTest(f"Test file not found: {self.vasu_file}")
        
        result = self.parser.parse_file(self.vasu_file)
        self.assertTrue(result.get("parse_success", False), "Vasu file parsing should succeed")
        
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 100, "Should extract many entities from Vasu file")
        
        # Check entity type distribution
        entity_types = {}
        for entity in entities:
            entity_type = entity.get("type", "unknown")
            entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
        
        print(f"Entity type distribution: {entity_types}")
        
        # Should have multiple types of entities
        self.assertGreater(len(entity_types), 2, "Should have multiple entity types")
        
        # Verify all entities have proper line information
        for entity in entities:
            with self.subTest(entity=entity.get("name", "unknown")):
                properties = entity.get("properties", {})
                
                # All entities should have line information
                self.assertIn("line", properties, f"Entity {entity.get('name')} should have line property")
                self.assertIn("line_count", properties, f"Entity {entity.get('name')} should have line_count property")
                self.assertIn("start_line", properties, f"Entity {entity.get('name')} should have start_line property")
                self.assertIn("end_line", properties, f"Entity {entity.get('name')} should have end_line property")
                
                # Line count should be positive
                line_count = properties["line_count"]
                self.assertGreater(line_count, 0, "Line count should be positive")
    
    def test_section_extractor_line_info(self):
        """Test that COBOL section extractor provides line information"""
        if not os.path.exists(self.vasu_file):
            self.skipTest(f"Test file not found: {self.vasu_file}")
        
        with open(self.vasu_file, 'r', encoding='utf-8', errors='ignore') as f:
            cobol_content = f.read()
        
        extractor = COBOLSectionExtractor()
        sections = extractor.extract_relevant_sections(cobol_content)
        
        self.assertGreater(len(sections), 0, "Should extract some sections")
        
        for section_name, section in sections.items():
            with self.subTest(section=section_name):
                # Test COBOLSection properties
                self.assertIsInstance(section, COBOLSection, "Should be COBOLSection instance")
                
                # Test line range property
                line_range = section.line_range
                self.assertIn("-", line_range, "Line range should contain dash")
                
                # Test line count property
                line_count = section.line_count
                self.assertGreater(line_count, 0, "Line count should be positive")
                
                # Test that line count matches calculated value
                expected_count = section.line_end - section.line_start + 1
                self.assertEqual(line_count, expected_count, "Line count should match calculated value")
                
                # Test that start <= end
                self.assertLessEqual(section.line_start, section.line_end, "Start line should be <= end line")
    
    def test_line_information_consistency(self):
        """Test that line information is consistent across all properties"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        for entity in entities:
            with self.subTest(entity=entity.get("name", "unknown")):
                properties = entity.get("properties", {})
                
                start_line = properties.get("start_line", 0)
                end_line = properties.get("end_line", 0)
                line_count = properties.get("line_count", 0)
                line_range = properties.get("line", "")
                
                # Test consistency between properties
                expected_count = end_line - start_line + 1
                self.assertEqual(line_count, expected_count, 
                               "line_count should equal end_line - start_line + 1")
                
                # Test line range format
                if "-" in line_range:
                    range_start, range_end = line_range.split("-", 1)
                    self.assertEqual(int(range_start), start_line, "Line range start should match start_line")
                    self.assertEqual(int(range_end), end_line, "Line range end should match end_line")
                else:
                    self.assertEqual(int(line_range), start_line, "Single line should match start_line")
                    self.assertEqual(start_line, end_line, "Start and end should be equal for single line")
    
    def test_edge_cases(self):
        """Test edge cases for line information"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        # Test that we handle entities with line 0 gracefully
        for entity in entities:
            properties = entity.get("properties", {})
            start_line = properties.get("start_line", 0)
            end_line = properties.get("end_line", 0)
            
            # Even if lines are 0, line_count should be calculated correctly
            if start_line == 0 and end_line == 0:
                line_count = properties.get("line_count", 0)
                self.assertEqual(line_count, 1, "Line count should be 1 when start and end are both 0")
    
    def test_performance_with_large_file(self):
        """Test performance with a larger COBOL file"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        if not os.path.exists(self.vasu_file):
            self.skipTest(f"Test file not found: {self.vasu_file}")
        
        import time
        
        start_time = time.time()
        result = self.parser.parse_file(self.vasu_file)
        end_time = time.time()
        
        self.assertTrue(result.get("parse_success", False), "Large file parsing should succeed")
        
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 0, "Should extract entities from large file")
        
        # Performance should be reasonable (less than 30 seconds for this file)
        parse_time = end_time - start_time
        self.assertLess(parse_time, 30, f"Parsing should complete in reasonable time (took {parse_time:.2f}s)")
        
        print(f"Parsed {len(entities)} entities in {parse_time:.2f} seconds")


def run_tests():
    """Run all tests"""
    print("🧪 Running COBOL Line Information Tests")
    print("=" * 50)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(TestCOBOLLineInformation)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 50)
    if result.wasSuccessful():
        print("🎉 All tests passed!")
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
    success = run_tests()
    sys.exit(0 if success else 1)
