#!/usr/bin/env python3
"""
COBOL Relationship Extraction Tests

This module contains all tests related to COBOL relationship extraction:
- Basic relationship extraction
- Advanced COBOL relationships (CALLS, INCLUDES, etc.)
- Mock data relationship testing
- Relationship type validation
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

from services.cobol_relationship_extractor import (
    extract_cobol_relationships, 
    RelationshipType, 
    RelationshipExtraction,
    _extract_basic_relationships,
    _extract_cobol_calls,
    _extract_cobol_performs,
    _extract_cobol_copies,
    _extract_data_flow_relationships,
    _extract_arithmetic_relationships,
    _extract_conditional_relationships,
    _extract_data_item_relationships,
    _extract_author_relationships,
    _extract_file_operations,
    _extract_variable_usage,
    _extract_include_statements,
    _extract_error_handling,
    _extract_screen_operations,
    _extract_queue_operations,
    _extract_replace_statements
)


class TestCOBOLRelationshipExtractor(unittest.TestCase):
    """Test cases for COBOL relationship extractor"""
    
    def setUp(self):
        """Set up test data"""
        self.sample_file_data = {
            "file_path": "test_fraud_mgmt.cbl",
            "language": "cobol",
            "parse_success": True,
            "compilation_units": [
                {
                    "name": "FRAUD-MGMT-SYSTEM",
                    "type": "program"
                }
            ],
            "entities": [
                {
                    "type": "program",
                    "name": "FRAUD-MGMT-SYSTEM",
                    "file_path": "test_fraud_mgmt.cbl",
                    "line_number": 1
                },
                {
                    "type": "paragraph",
                    "name": "0000-MAIN-PROCESS",
                    "file_path": "test_fraud_mgmt.cbl",
                    "line_number": 217
                },
                {
                    "type": "paragraph", 
                    "name": "1000-INITIALIZE-PROGRAM",
                    "file_path": "test_fraud_mgmt.cbl",
                    "line_number": 223
                },
                {
                    "type": "data_item",
                    "name": "WS-TOTAL-RISK-SCORE",
                    "file_path": "test_fraud_mgmt.cbl",
                    "line_number": 150
                },
                {
                    "type": "data_item",
                    "name": "TRANS-AMOUNT",
                    "file_path": "test_fraud_mgmt.cbl",
                    "line_number": 51
                }
            ],
            "statements": {
                "FRAUD-MGMT-SYSTEM": {
                    "0000-MAIN-PROCESS": [
                        {
                            "type": "PerformStatement",
                            "text": "PERFORM 1000-INITIALIZE-PROGRAM",
                            "details": "PERFORM 1000-INITIALIZE-PROGRAM"
                        },
                        {
                            "type": "MoveStatement", 
                            "text": "MOVE TRANS-DATE TO CUST-LAST-TRANS-DATE",
                            "details": "MOVE_FROM:TRANS-DATE:MOVE_TO:CUST-LAST-TRANS-DATE"
                        },
                        {
                            "type": "IfStatement",
                            "text": "IF WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD",
                            "details": "IF_CONDITION:WS-TOTAL-RISK-SCORE >= HIGH-RISK-THRESHOLD"
                        },
                        {
                            "type": "ComputeStatement",
                            "text": "COMPUTE CUST-AVG-MONTHLY-SPEND = (CUST-AVG-MONTHLY-SPEND * 0.9) + (TRANS-AMOUNT * 0.1)",
                            "details": "COMPUTE_EXPR:CUST-AVG-MONTHLY-SPEND = (CUST-AVG-MONTHLY-SPEND * 0.9) + (TRANS-AMOUNT * 0.1)"
                        },
                        {
                            "type": "AddStatement",
                            "text": "ADD 75 TO WS-TOTAL-RISK-SCORE",
                            "details": "ADD_OPERANDS:75,WS-TOTAL-RISK-SCORE"
                        },
                        {
                            "type": "ReadStatement",
                            "text": "READ TRANSACTION-FILE",
                            "details": "READ_FILE:TRANSACTION-FILE"
                        },
                        {
                            "type": "WriteStatement",
                            "text": "WRITE FRAUD-LOG-RECORD",
                            "details": "WRITE_FILE:FRAUD-LOG-RECORD"
                        },
                        {
                            "type": "OpenStatement",
                            "text": "OPEN INPUT TRANSACTION-FILE",
                            "details": "OPEN_FILES:TRANSACTION-FILE"
                        },
                        {
                            "type": "DisplayStatement",
                            "text": "DISPLAY 'FRAUD MANAGEMENT SYSTEM - INITIALIZING'",
                            "details": "DISPLAY_MESSAGE:FRAUD MANAGEMENT SYSTEM - INITIALIZING"
                        }
                    ]
                }
            },
            "screen_sections": {
                "FRAUD-MGMT-SYSTEM": [
                    {
                        "name": "MAIN-SCREEN",
                        "value": "FRAUD-MGMT-SCREEN",
                        "from": "WS-INPUT-FIELD",
                        "to": "WS-OUTPUT-FIELD"
                    }
                ]
            },
            "data_items": {
                "FRAUD-MGMT-SYSTEM": [
                    {
                        "name": "WS-TOTAL-RISK-SCORE",
                        "level": "01",
                        "picture_clause": "PIC 9(4)",
                        "line_number": 150
                    },
                    {
                        "name": "TRANS-AMOUNT",
                        "level": "05", 
                        "picture_clause": "PIC 9(8)V99",
                        "line_number": 51
                    }
                ]
            },
            "identification_data": {
                "FRAUD-MGMT-SYSTEM": {
                    "author": "FRAUD-DETECTION-TEAM",
                    "date_written": "2025-08-06"
                }
            }
        }

    def test_extract_cobol_relationships_main_function(self):
        """Test the main extract_cobol_relationships function"""
        relationships = extract_cobol_relationships(self.sample_file_data)
        
        # Should return a list
        self.assertIsInstance(relationships, list)
        
        # Should have relationships
        self.assertGreater(len(relationships), 0)
        
        # All relationships should be RelationshipExtraction objects
        for rel in relationships:
            self.assertIsInstance(rel, RelationshipExtraction)
            self.assertIsInstance(rel.relationship_type, RelationshipType)

    def test_extract_basic_relationships(self):
        """Test basic containment relationships"""
        relationships = _extract_basic_relationships(self.sample_file_data, "test_fraud_mgmt.cbl")
        
        # Should have CONTAINS relationships
        contains_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONTAINS]
        self.assertGreater(len(contains_rels), 0)
        
        # Check that file contains compilation units
        file_contains_cu = any(
            r.source_entity == "test_fraud_mgmt.cbl" and 
            r.target_entity == "FRAUD-MGMT-SYSTEM" and
            r.relationship_type == RelationshipType.CONTAINS
            for r in relationships
        )
        self.assertTrue(file_contains_cu)

    def test_extract_cobol_performs(self):
        """Test PERFORM statement relationships"""
        relationships = _extract_cobol_performs(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have PERFORM relationships
        perform_rels = [r for r in relationships if r.relationship_type == RelationshipType.CALLS]
        self.assertGreater(len(perform_rels), 0)
        
        # Check specific PERFORM relationship
        perform_rel = next(
            (r for r in relationships if "1000-INITIALIZE-PROGRAM" in r.target_entity), 
            None
        )
        self.assertIsNotNone(perform_rel)
        self.assertEqual(perform_rel.relationship_type, RelationshipType.CALLS)

    def test_extract_data_flow_relationships(self):
        """Test data flow relationships from MOVE statements"""
        relationships = _extract_data_flow_relationships(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have DATA_FLOW relationships
        data_flow_rels = [r for r in relationships if r.relationship_type == RelationshipType.DATA_FLOW]
        self.assertGreater(len(data_flow_rels), 0)
        
        # Check specific MOVE relationship
        move_rel = next(
            (r for r in relationships if "TRANS-DATE" in r.source_entity and "CUST-LAST-TRANS-DATE" in r.target_entity),
            None
        )
        self.assertIsNotNone(move_rel)
        self.assertEqual(move_rel.relationship_type, RelationshipType.DATA_FLOW)

    def test_extract_arithmetic_relationships(self):
        """Test arithmetic operation relationships"""
        relationships = _extract_arithmetic_relationships(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have ARITHMETIC relationships
        arithmetic_rels = [r for r in relationships if r.relationship_type == RelationshipType.ARITHMETIC]
        self.assertGreater(len(arithmetic_rels), 0)
        
        # Check ADD relationship
        add_rel = next(
            (r for r in relationships if "75" in r.source_entity and "WS-TOTAL-RISK-SCORE" in r.target_entity),
            None
        )
        self.assertIsNotNone(add_rel)
        self.assertEqual(add_rel.relationship_type, RelationshipType.ARITHMETIC)

    def test_extract_conditional_relationships(self):
        """Test conditional logic relationships"""
        relationships = _extract_conditional_relationships(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have CONDITIONAL relationships
        conditional_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONDITIONAL]
        self.assertGreater(len(conditional_rels), 0)
        
        # Check IF relationship
        if_rel = next(
            (r for r in relationships if "WS-TOTAL-RISK-SCORE" in r.source_entity),
            None
        )
        self.assertIsNotNone(if_rel)
        self.assertEqual(if_rel.relationship_type, RelationshipType.CONDITIONAL)

    def test_extract_data_item_relationships(self):
        """Test data item definition relationships"""
        relationships = _extract_data_item_relationships(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have CONTAINS relationships for data items
        data_item_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONTAINS and r.target_entity in ["WS-TOTAL-RISK-SCORE", "TRANS-AMOUNT"]]
        self.assertGreater(len(data_item_rels), 0)
        
        # Check specific data item relationship
        data_item_rel = next(
            (r for r in relationships if "WS-TOTAL-RISK-SCORE" in r.target_entity),
            None
        )
        self.assertIsNotNone(data_item_rel)

    def test_extract_author_relationships(self):
        """Test author and date relationships"""
        relationships = _extract_author_relationships(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have WRITTEN_BY relationship
        author_rels = [r for r in relationships if r.relationship_type == RelationshipType.WRITTEN_BY]
        self.assertGreater(len(author_rels), 0)
        
        # Check specific author relationship
        author_rel = next(
            (r for r in relationships if "FRAUD-DETECTION-TEAM" in r.target_entity),
            None
        )
        self.assertIsNotNone(author_rel)
        self.assertEqual(author_rel.relationship_type, RelationshipType.WRITTEN_BY)

    def test_extract_file_operations(self):
        """Test file operation relationships"""
        relationships = _extract_file_operations(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have file operation relationships
        file_rels = [r for r in relationships if r.relationship_type in [RelationshipType.READS, RelationshipType.WRITES, RelationshipType.FILE_ACCESS]]
        self.assertGreater(len(file_rels), 0)
        
        # Check READ relationship
        read_rel = next(
            (r for r in relationships if r.relationship_type == RelationshipType.READS and "TRANSACTION-FILE" in r.target_entity),
            None
        )
        self.assertIsNotNone(read_rel)
        
        # Check WRITE relationship
        write_rel = next(
            (r for r in relationships if r.relationship_type == RelationshipType.WRITES and "FRAUD-LOG-RECORD" in r.target_entity),
            None
        )
        self.assertIsNotNone(write_rel)

    def test_extract_variable_usage(self):
        """Test variable usage relationships"""
        relationships = _extract_variable_usage(self.sample_file_data, "FRAUD-MGMT-SYSTEM", "test_fraud_mgmt.cbl")
        
        # Should have USES and MODIFIES relationships
        usage_rels = [r for r in relationships if r.relationship_type in [RelationshipType.USES, RelationshipType.MODIFIES]]
        self.assertGreater(len(usage_rels), 0)

    def test_relationship_types_coverage(self):
        """Test that all expected relationship types are covered"""
        relationships = extract_cobol_relationships(self.sample_file_data)
        
        # Get all relationship types found
        found_types = set(rel.relationship_type for rel in relationships)
        
        # Expected relationship types that should be present
        expected_types = {
            RelationshipType.CONTAINS,
            RelationshipType.CALLS,  # From PERFORM statements
            RelationshipType.DATA_FLOW,  # From MOVE statements
            RelationshipType.ARITHMETIC,  # From ADD/COMPUTE statements
            RelationshipType.CONDITIONAL,  # From IF statements
            RelationshipType.READS,  # From READ statements
            RelationshipType.WRITES,  # From WRITE statements
            RelationshipType.FILE_ACCESS,  # From OPEN statements
            RelationshipType.BINDS_SCREEN,  # From DISPLAY statements
            RelationshipType.WRITTEN_BY,  # From author data
            RelationshipType.USES,  # From variable usage
            RelationshipType.MODIFIES  # From variable modifications
        }
        
        # Check that we have at least some of the expected types
        intersection = found_types.intersection(expected_types)
        self.assertGreater(len(intersection), 0, f"Expected at least some of {expected_types}, but found {found_types}")

    def test_relationship_confidence_scores(self):
        """Test that relationships have appropriate confidence scores"""
        relationships = extract_cobol_relationships(self.sample_file_data)
        
        for rel in relationships:
            # Confidence should be between 0 and 1
            self.assertGreaterEqual(rel.confidence, 0.0)
            self.assertLessEqual(rel.confidence, 1.0)
            
            # Relationship strength should be a valid string
            self.assertIn(rel.relationship_strength, ["weak", "medium", "strong"])

    def test_relationship_context(self):
        """Test that relationships have meaningful context"""
        relationships = extract_cobol_relationships(self.sample_file_data)
        
        for rel in relationships:
            # Context should not be empty
            self.assertIsNotNone(rel.context)
            self.assertGreater(len(rel.context), 0)

    def test_empty_file_data(self):
        """Test behavior with empty file data"""
        empty_data = {"file_path": "empty.cbl", "language": "cobol"}
        relationships = extract_cobol_relationships(empty_data)
        
        # Should return empty list, not crash
        self.assertIsInstance(relationships, list)
        self.assertEqual(len(relationships), 0)

    def test_non_cobol_file(self):
        """Test behavior with non-COBOL file"""
        non_cobol_data = {"file_path": "test.py", "language": "python"}
        relationships = extract_cobol_relationships(non_cobol_data)
        
        # Should return empty list
        self.assertIsInstance(relationships, list)
        self.assertEqual(len(relationships), 0)
    
    def test_comment_enhanced_description_generation(self):
        """Test that comments are used when generating descriptions for COBOL entities"""
        # Mock COBOL data with comments in entity properties
        mock_cobol_data = {
            "parse_success": True,
            "language": "cobol",
            "file_path": "test.cbl",
            "entities": [
                {
                    "type": "paragraph",
                    "name": "MAIN-PARAGRAPH",
                    "properties": {
                        "comment": "Main program entry point that initializes the system and starts processing",
                        "unit": "TEST-PROGRAM",
                        "line": "100-150"
                    }
                },
                {
                    "type": "data_item", 
                    "name": "WS-TRANS-STATUS",
                    "properties": {
                        "comment": "File status variables that track the status of file operations",
                        "data_type": "alphanumeric",
                        "picture": "XX",
                        "unit": "TEST-PROGRAM",
                        "line": "50-50"
                    }
                },
                {
                    "type": "paragraph",
                    "name": "PROCESS-DATA",
                    "properties": {
                        "comment": "Processes transaction data and validates business rules",
                        "unit": "TEST-PROGRAM", 
                        "line": "200-300"
                    }
                }
            ],
            "paragraphs": {},
            "statements": {},
            "data_items": {}
        }
        
        # Mock AI service that uses comments in descriptions
        class MockAIService:
            def generate_description(self, entity_data, context=None):
                entity_name = entity_data.get('name', '')
                entity_type = entity_data.get('type', '')
                properties = entity_data.get('properties', {})
                comment = properties.get('comment', '')
                
                if comment:
                    return f"{comment}. This {entity_type} handles {entity_name.lower().replace('-', ' ')} operations."
                else:
                    return f"This {entity_type} named {entity_name} handles {entity_name.lower().replace('-', ' ')} operations."
        
        # Test description generation with comments
        mock_ai_service = MockAIService()
        
        descriptions = {}
        for entity in mock_cobol_data["entities"]:
            description = mock_ai_service.generate_description(entity)
            descriptions[entity["name"]] = description
        
        # Verify that descriptions incorporate comments
        self.assertIn("MAIN-PARAGRAPH", descriptions)
        main_desc = descriptions["MAIN-PARAGRAPH"]
        self.assertIn("Main program entry point", main_desc, 
                     "Description should include the comment about main entry point")
        
        self.assertIn("WS-TRANS-STATUS", descriptions)
        status_desc = descriptions["WS-TRANS-STATUS"]
        self.assertIn("File status variables", status_desc,
                     "Description should include the comment about file status variables")
        
        self.assertIn("PROCESS-DATA", descriptions)
        process_desc = descriptions["PROCESS-DATA"]
        self.assertIn("Processes transaction data", process_desc,
                     "Description should include the comment about processing transaction data")
        
        # Verify descriptions are more informative with comments
        for entity_name, description in descriptions.items():
            self.assertGreater(len(description), 50, 
                             f"Description for {entity_name} should be detailed when comment is available")
            self.assertIn(entity_name.lower().replace('-', ' '), description.lower(),
                         f"Description should mention {entity_name}")


class TestCOBOLRelationshipTypes(unittest.TestCase):
    """Test that all relationship types are properly defined"""
    
    def test_all_relationship_types_defined(self):
        """Test that all expected relationship types are defined"""
        expected_types = [
            "CALLS", "IMPORTS", "USES", "CONTAINS", "DATA_FLOW", "MODIFIES",
            "READS", "WRITES", "CONDITIONAL", "ARITHMETIC", "FILE_ACCESS",
            "PERFORMS", "INCLUDES", "PASSES_DATA", "HANDLES_ERRORS", "USES_QUEUE",
            "BINDS_SCREEN", "REPLACES", "WRITTEN_BY"
        ]
        
        for rel_type in expected_types:
            self.assertTrue(hasattr(RelationshipType, rel_type), f"RelationshipType.{rel_type} not defined")
            self.assertEqual(getattr(RelationshipType, rel_type).value, rel_type)

    def test_relationship_type_values(self):
        """Test that relationship type values are correct"""
        self.assertEqual(RelationshipType.CALLS.value, "CALLS")
        self.assertEqual(RelationshipType.DATA_FLOW.value, "DATA_FLOW")
        self.assertEqual(RelationshipType.CONTAINS.value, "CONTAINS")
        self.assertEqual(RelationshipType.PERFORMS.value, "PERFORMS")


class TestAdvancedCOBOLRelationships(unittest.TestCase):
    """Test advanced COBOL relationship extraction features"""
    
    def setUp(self):
        """Set up test data with advanced COBOL features"""
        self.advanced_file_data = {
            "parse_success": True,
            "language": "cobol",
            "file_path": "test_advanced.cbl",
            "copy_statements": {
                "ENHANCED-TEST": [
                    {"name": "BANKING-COPYBOOK", "library": "COMMON", "unit": "ENHANCED-TEST"}
                ]
            },
            "replacing_phrases": {
                "ENHANCED-TEST": {
                    "BANKING-COPYBOOK": [
                        {"replaceable": "ACCOUNT", "replacement": "CUSTOMER"}
                    ]
                }
            },
            "call_statements": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "unit": "ENHANCED-TEST"}
                    ]
                }
            },
            "call_parameters": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "param_type": "REFERENCE", "param_name": "WS-ACCOUNT-NUMBER"}
                    ]
                }
            },
            "use_statements": {
                "ENHANCED-TEST": [
                    {"use_type": "ERROR", "file_name": "ACCOUNT-FILE", "procedure_name": "", "unit": "ENHANCED-TEST"}
                ]
            },
            "communication": {
                "ENHANCED-TEST": [
                    {"name": "COMM-AREA", "type": "INPUT", "symbolic_queue": "WS-QUEUE", "symbolic_destination": "WS-DEST", "unit": "ENHANCED-TEST"}
                ]
            },
            "screens": {
                "ENHANCED-TEST": [
                    {"name": "MAIN-SCREEN", "value": "ACCOUNT NUMBER:", "from": "WS-ACCOUNT-NUMBER", "to": "WS-ACCOUNT-NUMBER", "unit": "ENHANCED-TEST"}
                ]
            }
        }

    def test_copy_relationships(self):
        """Test COPY statement relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have INCLUDES relationships
        includes_rels = [r for r in relationships if r.relationship_type == RelationshipType.INCLUDES]
        self.assertGreater(len(includes_rels), 0)
        
        # Check specific COPY relationship
        copy_rel = next(
            (r for r in relationships if "BANKING-COPYBOOK" in r.target_entity),
            None
        )
        self.assertIsNotNone(copy_rel)
        self.assertEqual(copy_rel.relationship_type, RelationshipType.INCLUDES)

    def test_call_relationships(self):
        """Test CALL statement relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have CALLS relationships
        calls_rels = [r for r in relationships if r.relationship_type == RelationshipType.CALLS]
        self.assertGreater(len(calls_rels), 0)
        
        # Check specific CALL relationship
        call_rel = next(
            (r for r in relationships if "INTEREST-CALCULATOR" in r.target_entity),
            None
        )
        self.assertIsNotNone(call_rel)
        self.assertEqual(call_rel.relationship_type, RelationshipType.CALLS)

    def test_parameter_relationships(self):
        """Test parameter passing relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have PASSES_DATA relationships
        passes_data_rels = [r for r in relationships if r.relationship_type == RelationshipType.PASSES_DATA]
        self.assertGreater(len(passes_data_rels), 0)
        
        # Check specific parameter relationship
        param_rel = next(
            (r for r in relationships if "WS-ACCOUNT-NUMBER" in r.source_entity),
            None
        )
        self.assertIsNotNone(param_rel)
        self.assertEqual(param_rel.relationship_type, RelationshipType.PASSES_DATA)

    def test_error_handling_relationships(self):
        """Test error handling relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have HANDLES_ERRORS relationships
        error_rels = [r for r in relationships if r.relationship_type == RelationshipType.HANDLES_ERRORS]
        self.assertGreater(len(error_rels), 0)
        
        # Check specific error handling relationship
        error_rel = next(
            (r for r in relationships if "ACCOUNT-FILE" in r.target_entity),
            None
        )
        self.assertIsNotNone(error_rel)
        self.assertEqual(error_rel.relationship_type, RelationshipType.HANDLES_ERRORS)

    def test_communication_relationships(self):
        """Test communication relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have USES_QUEUE relationships
        queue_rels = [r for r in relationships if r.relationship_type == RelationshipType.USES_QUEUE]
        self.assertGreater(len(queue_rels), 0)
        
        # Check specific communication relationship
        comm_rel = next(
            (r for r in relationships if "WS-QUEUE" in r.target_entity),
            None
        )
        self.assertIsNotNone(comm_rel)
        self.assertEqual(comm_rel.relationship_type, RelationshipType.USES_QUEUE)

    def test_screen_relationships(self):
        """Test screen relationship extraction"""
        relationships = extract_cobol_relationships(self.advanced_file_data)
        
        # Should have BINDS_SCREEN relationships
        screen_rels = [r for r in relationships if r.relationship_type == RelationshipType.BINDS_SCREEN]
        self.assertGreater(len(screen_rels), 0)
        
        # Check specific screen relationship
        screen_rel = next(
            (r for r in relationships if "WS-ACCOUNT-NUMBER" in r.target_entity),
            None
        )
        self.assertIsNotNone(screen_rel)
        self.assertEqual(screen_rel.relationship_type, RelationshipType.BINDS_SCREEN)


def run_relationship_tests():
    """Run all relationship tests"""
    print("🧪 Running COBOL Relationship Tests")
    print("=" * 50)
    
    # Create test suite
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLRelationshipExtractor))
    suite.addTests(loader.loadTestsFromTestCase(TestCOBOLRelationshipTypes))
    suite.addTests(loader.loadTestsFromTestCase(TestAdvancedCOBOLRelationships))
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    # Print summary
    print("\n" + "=" * 50)
    if result.wasSuccessful():
        print("🎉 All relationship tests passed!")
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
    success = run_relationship_tests()
    sys.exit(0 if success else 1)
