#!/usr/bin/env python3
"""
Unit Test for COBOL Parser Integration
Tests the full pipeline from COBOL file parsing to relationship extraction
"""

import os
import sys
import unittest
from typing import Dict, List, Any

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Add cobol-support to the path
cobol_support_path = os.path.join(project_root, 'cobol-support')
if cobol_support_path not in sys.path:
    sys.path.insert(0, cobol_support_path)

from services.cobol_parser import COBOLParser
from services.cobol_relationship_extractor import RelationshipType


class TestCOBOLParserIntegration(unittest.TestCase):
    """Test COBOL parser integration with relationship extraction"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.parser = COBOLParser()
        self.test_file = os.path.join(project_root, 'temp_processing', 'vasu_fraud_management_cobol_reformatted.cbl')
        
    def test_parser_availability(self):
        """Test that COBOL parser is available"""
        self.assertTrue(self.parser.is_available(), "COBOL parser should be available")
    
    def test_parse_cobol_file(self):
        """Test parsing a COBOL file"""
        result = self.parser.parse_file(self.test_file)
        
        # Basic parsing checks
        self.assertTrue(result.get("parse_success", False), "File should parse successfully")
        self.assertEqual(result.get("language"), "cobol", "Language should be COBOL")
        self.assertIn("entities", result, "Result should contain entities")
        self.assertIn("relationships", result, "Result should contain relationships")
        
        # Check entity count
        entities = result.get("entities", [])
        self.assertGreater(len(entities), 0, "Should have entities")
        print(f"✅ Found {len(entities)} entities")
        
        # Check relationship count
        relationships = result.get("relationships", [])
        self.assertGreater(len(relationships), 0, "Should have relationships")
        print(f"✅ Found {len(relationships)} relationships")
    
    def test_relationship_types_extracted(self):
        """Test that multiple relationship types are extracted"""
        result = self.parser.parse_file(self.test_file)
        relationships = result.get("relationships", [])
        
        # Count relationship types
        rel_types = {}
        for rel in relationships:
            rel_type = rel.relationship_type.value if hasattr(rel.relationship_type, 'value') else str(rel.relationship_type)
            rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
        
        print(f"📊 Relationship types found: {rel_types}")
        
        # Should have more than just CONTAINS relationships
        self.assertGreater(len(rel_types), 1, f"Should have multiple relationship types, got: {list(rel_types.keys())}")
        
        # Check for specific relationship types that should be present
        expected_types = [
            "CALLS", "PERFORMS", "DATA_FLOW", "ARITHMETIC", 
            "CONDITIONAL", "USES", "MODIFIES", "READS", "WRITES"
        ]
        
        found_types = list(rel_types.keys())
        missing_types = [t for t in expected_types if t not in found_types]
        
        if missing_types:
            print(f"⚠️  Missing relationship types: {missing_types}")
            print(f"   Found types: {found_types}")
        
        # At least some of these should be present
        found_expected = [t for t in expected_types if t in found_types]
        self.assertGreater(len(found_expected), 0, f"Should have at least some expected relationship types. Found: {found_expected}")
    
    def test_parsed_data_structure(self):
        """Test the structure of parsed data"""
        result = self.parser.parse_file(self.test_file)
        
        # Check required keys
        required_keys = ["parse_success", "language", "file_path", "entities", "relationships"]
        for key in required_keys:
            self.assertIn(key, result, f"Result should contain {key}")
        
        # Check statements structure
        statements = result.get("statements", {})
        self.assertIsInstance(statements, dict, "Statements should be a dictionary")
        
        if statements:
            print(f"📝 Found statements in {len(statements)} units")
            # Show first few statement units
            for i, (unit, stmts) in enumerate(list(statements.items())[:3]):
                print(f"   Unit {unit}: {len(stmts)} statements")
                if stmts and len(stmts) > 0:
                    print(f"     First statement: {stmts[0]}")
        
        # Check paragraphs structure
        paragraphs = result.get("paragraphs", {})
        self.assertIsInstance(paragraphs, dict, "Paragraphs should be a dictionary")
        
        if paragraphs:
            print(f"📄 Found paragraphs in {len(paragraphs)} units")
            # Show first few paragraph units
            for i, (unit, paras) in enumerate(list(paragraphs.items())[:3]):
                print(f"   Unit {unit}: {len(paras)} paragraphs")
                if paras and len(paras) > 0:
                    print(f"     First paragraph: {paras[0]}")
    
    def test_relationship_extraction_debug(self):
        """Debug relationship extraction process"""
        result = self.parser.parse_file(self.test_file)
        
        print("\n🔍 DEBUGGING RELATIONSHIP EXTRACTION")
        print("=" * 50)
        
        # Check what data is available for relationship extraction
        print(f"📊 Available data keys: {list(result.keys())}")
        
        # Check statements data
        statements = result.get("statements", {})
        print(f"📝 Statements data: {len(statements)} units")
        
        # Check paragraphs data
        paragraphs = result.get("paragraphs", {})
        print(f"📄 Paragraphs data: {len(paragraphs)} units")
        
        # Check data items
        data_items = result.get("data_items", {})
        print(f"📦 Data items: {len(data_items)} units")
        
        # Check divisions
        divisions = result.get("divisions", {})
        print(f"🏗️  Divisions: {len(divisions)} units")
        
        # Show sample statement data
        if statements:
            print(f"\n📝 Sample statement data:")
            for unit, stmts in list(statements.items())[:2]:
                print(f"   Unit: {unit}")
                print(f"   Statement count: {len(stmts)}")
                if stmts:
                    print(f"   First statement: {stmts[0]}")
                    if len(stmts) > 1:
                        print(f"   Second statement: {stmts[1]}")
                print()
        
        # Show sample paragraph data
        if paragraphs:
            print(f"📄 Sample paragraph data:")
            for unit, paras in list(paragraphs.items())[:2]:
                print(f"   Unit: {unit}")
                print(f"   Paragraph count: {len(paras)}")
                if paras:
                    print(f"   First paragraph: {paras[0]}")
                    if len(paras) > 1:
                        print(f"   Second paragraph: {paras[1]}")
                print()
    
    def test_individual_relationship_functions(self):
        """Test individual relationship extraction functions"""
        result = self.parser.parse_file(self.test_file)
        
        # Import the individual functions
        from services.cobol_relationship_extractor import (
            _extract_cobol_calls,
            _extract_cobol_performs,
            _extract_data_flow_relationships,
            _extract_arithmetic_relationships,
            _extract_conditional_relationships,
            _extract_variable_usage,
            _extract_file_operations
        )
        
        print("\n🧪 TESTING INDIVIDUAL RELATIONSHIP FUNCTIONS")
        print("=" * 50)
        
        # Test each function
        functions_to_test = [
            ("CALLS", _extract_cobol_calls),
            ("PERFORMS", _extract_cobol_performs),
            ("DATA_FLOW", _extract_data_flow_relationships),
            ("ARITHMETIC", _extract_arithmetic_relationships),
            ("CONDITIONAL", _extract_conditional_relationships),
            ("USES/MODIFIES", _extract_variable_usage),
            ("FILE_OPERATIONS", _extract_file_operations)
        ]
        
        for func_name, func in functions_to_test:
            try:
                relationships = func(result)
                rel_count = len(relationships)
                print(f"✅ {func_name}: {rel_count} relationships")
                
                if relationships:
                    # Show first relationship
                    first_rel = relationships[0]
                    print(f"   Example: {first_rel.source_entity} -{first_rel.relationship_type.value}-> {first_rel.target_entity}")
            except Exception as e:
                print(f"❌ {func_name}: Error - {e}")


def run_integration_tests():
    """Run all integration tests"""
    print("🧪 Running COBOL Parser Integration Tests")
    print("=" * 60)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = loader.loadTestsFromTestCase(TestCOBOLParserIntegration)
    
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
