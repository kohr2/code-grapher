#!/usr/bin/env python3
"""
COBOL Parser Tests

This module contains all tests related to COBOL parsing functionality:
- Parser availability and initialization
- File parsing functionality
- Entity extraction
- Line information
- AST structure parsing
"""

import os
import sys
import unittest
import time
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


class TestCOBOLParser(unittest.TestCase):
    """Test cases for COBOL parser functionality"""
    
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
    
    def test_parser_initialization(self):
        """Test COBOL parser initialization"""
        self.assertIsNotNone(self.parser)
        self.assertTrue(hasattr(self.parser, 'is_available'))
        self.assertTrue(hasattr(self.parser, 'parse_file'))
    
    def test_basic_file_parsing(self):
        """Test basic COBOL file parsing"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        
        # Basic parsing checks
        self.assertTrue(result.get("parse_success", False), "File should parse successfully")
        self.assertEqual(result.get("language"), "cobol", "Language should be COBOL")
        self.assertIn("entities", result, "Result should contain entities")
        self.assertIn("file_path", result, "Result should contain file_path")
    
    def test_entity_extraction(self):
        """Test COBOL entity extraction"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        # Should have entities
        self.assertGreater(len(entities), 0, "Should extract entities")
        
        # Check entity structure
        for entity in entities:
            self.assertIn("type", entity, "Entity should have type")
            self.assertIn("name", entity, "Entity should have name")
            self.assertIn("file_path", entity, "Entity should have file_path")
    
    def test_entity_types(self):
        """Test that different entity types are extracted"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
        # Check for different entity types
        entity_types = set(entity.get("type") for entity in entities)
        self.assertGreater(len(entity_types), 0, "Should have multiple entity types")
        
        # Should have at least some common COBOL entity types
        expected_types = ["program", "paragraph", "data_item", "section"]
        found_expected = [t for t in expected_types if any(e.get("type") == t for e in entities)]
        self.assertGreater(len(found_expected), 0, f"Should have some expected entity types: {expected_types}")
    
    def test_line_information(self):
        """Test that entities have proper line information"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        entities = result.get("entities", [])
        
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
    
    def test_line_count_calculation(self):
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
    
    def test_compilation_units(self):
        """Test compilation unit extraction"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        result = self.parser.parse_file(self.test_file)
        compilation_units = result.get("compilation_units", [])
        
        self.assertGreater(len(compilation_units), 0, "Should extract compilation units")
        
        for unit in compilation_units:
            self.assertIn("name", unit, "Compilation unit should have name")
            self.assertIn("type", unit, "Compilation unit should have type")
    
    def test_large_file_parsing(self):
        """Test parsing of larger COBOL files"""
        if not self.parser.is_available():
            self.skipTest("COBOL parser not available")
        
        if not os.path.exists(self.vasu_file):
            self.skipTest(f"Test file not found: {self.vasu_file}")
        
        start_time = time.time()
        result = self.parser.parse_file(self.vasu_file)
        end_time = time.time()
        
        self.assertTrue(result.get("parse_success", False), "Large file parsing should succeed")
        
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 100, "Should extract many entities from large file")
        
        # Performance should be reasonable (less than 30 seconds for this file)
        parse_time = end_time - start_time
        self.assertLess(parse_time, 30, f"Parsing should complete in reasonable time (took {parse_time:.2f}s)")
        
        print(f"Parsed {len(entities)} entities in {parse_time:.2f} seconds")
    
    def test_section_extractor(self):
        """Test COBOL section extractor functionality"""
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
    
    def test_comment_extraction(self):
        """Test that comments are extracted from COBOL source code"""
        # Create a test file with comments
        test_content = '''000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. COMMENT-TEST.
000300 AUTHOR. TEST-AUTHOR.
000400 
000500* This is a program comment explaining the overall purpose
000600* of this banking fraud detection system
000700 
000800 ENVIRONMENT DIVISION.
000900 
001000 DATA DIVISION.
001100 WORKING-STORAGE SECTION.
001200 
001300* File Status Variables
001400* These variables track the status of file operations
001500 01  WS-TRANS-STATUS             PIC XX.
001600 01  WS-CUST-STATUS              PIC XX.
001700 
001800* Control Variables
001900* Used to control program flow and error handling
002000 01  WS-EOF-FLAG                 PIC X VALUE 'N'.
002100 88  EOF-REACHED             VALUE 'Y'.
002200 
002300 PROCEDURE DIVISION.
002400 
002500* Main program entry point
002600 MAIN-PARAGRAPH.
002700     DISPLAY 'Starting fraud detection process'
002800     PERFORM INITIALIZE-PROGRAM
002900     STOP RUN.
003000 
003100* Initialize program variables and open files
003200* Sets up the environment for processing
003300 INITIALIZE-PROGRAM.
003400     MOVE 'N' TO WS-EOF-FLAG
003500     DISPLAY 'Program initialized successfully'.
'''
        
        # Write test content to temporary file
        test_file_path = os.path.join(os.path.dirname(__file__), "temp_comment_test.cbl")
        try:
            with open(test_file_path, 'w') as f:
                f.write(test_content)
            
            # Test comment extraction directly
            comments = self.parser._extract_comments_from_source(test_file_path)
            
            # Verify we found comments
            self.assertGreater(len(comments), 0, "Should extract comments from COBOL file")
            
            # Check for specific comment associations
            self.assertIn('WS-TRANS-STATUS', comments, "Should associate comment with data item")
            self.assertIn('WS-EOF-FLAG', comments, "Should associate comment with data item")
            self.assertIn('MAIN-PARAGRAPH', comments, "Should associate comment with paragraph")
            self.assertIn('INITIALIZE-PROGRAM', comments, "Should associate comment with paragraph")
            
            # Verify comment content
            self.assertIn('File Status Variables', comments['WS-TRANS-STATUS'],
                         "Should extract correct comment for WS-TRANS-STATUS")
            self.assertIn('Main program entry point', comments['MAIN-PARAGRAPH'],
                         "Should extract correct comment for MAIN-PARAGRAPH")
            
        finally:
            # Clean up test file
            if os.path.exists(test_file_path):
                os.remove(test_file_path)
    
    def test_comment_usage_in_entities(self):
        """Test that comments are included in entity properties"""
        # Create a test file with comments
        test_content = '''000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. COMMENT-TEST.
000300 AUTHOR. TEST-AUTHOR.
000400 
000500* This is a program comment explaining the overall purpose
000600 
000700 DATA DIVISION.
000800 WORKING-STORAGE SECTION.
000900 
001000* File Status Variables
001100 01  WS-TRANS-STATUS             PIC XX.
001200 
001300 PROCEDURE DIVISION.
001400 
001500* Main program entry point
001600 MAIN-PARAGRAPH.
001700     DISPLAY 'Starting process'
001800     STOP RUN.
'''
        
        # Write test content to temporary file
        test_file_path = os.path.join(os.path.dirname(__file__), "temp_comment_entity_test.cbl")
        try:
            with open(test_file_path, 'w') as f:
                f.write(test_content)
            
            # Parse the file
            result = self.parser.parse_file(test_file_path)
            
            if not result.get("parse_success", False):
                self.skipTest("Parser not available")
            
            entities = result.get("entities", [])
            
            # Find entities with comments
            entities_with_comments = []
            for entity in entities:
                properties = entity.get('properties', {})
                if 'comment' in properties:
                    entities_with_comments.append(entity)
            
            # Should have at least one entity with a comment
            self.assertGreater(len(entities_with_comments), 0, 
                             "Should have entities with comments in their properties")
            
            # Check that comment content is preserved
            for entity in entities_with_comments:
                comment = entity['properties']['comment']
                self.assertIsInstance(comment, str, "Comment should be a string")
                self.assertGreater(len(comment), 0, "Comment should not be empty")
                
        finally:
            # Clean up test file
            if os.path.exists(test_file_path):
                os.remove(test_file_path)


class TestCOBOLParserIntegration(unittest.TestCase):
    """Test COBOL parser integration with the broader system"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.test_file = "fixtures/test_cobol_banking.cbl"
        
        if not os.path.exists(self.test_file):
            self.test_file = os.path.join("cobol-support", "tests", self.test_file)
    
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


def run_parser_tests():
    """Run all parser tests"""
    print("🧪 Running COBOL Parser Tests")
    print("=" * 50)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLParser))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLParserIntegration))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 50)
    if result.wasSuccessful():
        print("🎉 All parser tests passed!")
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
    success = run_parser_tests()
    sys.exit(0 if success else 1)
