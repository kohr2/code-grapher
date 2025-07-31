"""
Comprehensive unit tests for Relationship Service
Tests the critical validation logic that addresses surgical updater issues
"""
import unittest
from unittest.mock import Mock, MagicMock
import sys
from pathlib import Path

# Add ai-services to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from services.relationship_service import RelationshipService
from providers.mock_provider import MockProvider
from models.relationship_models import (
    Relationship, RelationshipExtractionResult, RelationshipType, ConfidenceLevel
)


class TestRelationshipService(unittest.TestCase):
    """Test suite for RelationshipService - focuses on validation logic"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.mock_logger = Mock()
        self.mock_provider = Mock()
        self.service = RelationshipService(self.mock_provider, self.mock_logger)
    
    def test_initialization(self):
        """Test service initialization"""
        self.assertIsNotNone(self.service)
        self.assertEqual(self.service.provider, self.mock_provider)
        self.assertEqual(self.service.logger, self.mock_logger)
        self.assertIsNotNone(self.service._validation_rules)
        self.assertIsNotNone(self.service._entity_patterns)
    
    def test_extract_relationships_success(self):
        """Test successful relationship extraction with validation"""
        # Mock provider response
        self.mock_provider.extract_relationships.return_value = [
            {
                'source_entity': 'func1',
                'target_entity': 'func2',
                'relationship_type': 'CALLS',
                'confidence': 0.9
            }
        ]
        
        source_code = "def func1(): func2()"
        target_code = "def func2(): pass"
        
        result = self.service.extract_relationships(
            "source.py", "target.py", source_code, target_code
        )
        
        self.assertIsInstance(result, RelationshipExtractionResult)
        self.assertTrue(result.success)
        self.assertEqual(result.source_file, "source.py")
        self.assertEqual(result.target_file, "target.py")
        self.assertGreater(result.extraction_time, 0)
    
    def test_extract_relationships_provider_failure(self):
        """Test handling of provider failure"""
        self.mock_provider.extract_relationships.side_effect = Exception("Provider failed")
        
        result = self.service.extract_relationships(
            "source.py", "target.py", "code1", "code2"
        )
        
        self.assertFalse(result.success)
        self.assertIsNotNone(result.error)
        self.assertEqual(len(result.relationships), 0)
    
    def test_validate_relationships_valid(self):
        """Test validation of valid relationships"""
        relationships = [
            Relationship(
                source_entity='func1',
                target_entity='func2',
                relationship_type=RelationshipType.CALLS,
                confidence=0.8,
                source_file='source.py',
                target_file='target.py'
            )
        ]
        
        context = {
            'source_code': 'def func1(): func2()',
            'target_code': 'def func2(): pass',
            'source_file': 'source.py',
            'target_file': 'target.py'
        }
        
        validated = self.service.validate_relationships(relationships, context)
        
        self.assertEqual(len(validated), 1)
        self.assertEqual(validated[0].source_entity, 'func1')
    
    def test_validate_relationships_entity_not_found(self):
        """Test validation fails when entity not found"""
        relationships = [
            Relationship(
                source_entity='nonexistent_func',
                target_entity='func2',
                relationship_type=RelationshipType.CALLS,
                confidence=0.8,
                source_file='source.py',
                target_file='target.py'
            )
        ]
        
        context = {
            'source_code': 'def func1(): pass',
            'target_code': 'def func2(): pass',
            'source_file': 'source.py',
            'target_file': 'target.py'
        }
        
        validated = self.service.validate_relationships(relationships, context)
        
        self.assertEqual(len(validated), 0)
        self.mock_logger.log_warning.assert_called()
    
    def test_validate_relationships_low_confidence(self):
        """Test validation fails for low confidence relationships"""
        relationships = [
            Relationship(
                source_entity='func1',
                target_entity='func2',
                relationship_type=RelationshipType.CALLS,
                confidence=0.1,  # Very low confidence
                source_file='source.py',
                target_file='target.py'
            )
        ]
        
        context = {
            'source_code': 'def func1(): func2()',
            'target_code': 'def func2(): pass',
            'source_file': 'source.py',
            'target_file': 'target.py'
        }
        
        validated = self.service.validate_relationships(relationships, context)
        
        self.assertEqual(len(validated), 0)
    
    def test_extract_entities_from_code(self):
        """Test entity extraction from code"""
        code = """
def function1():
    pass

class TestClass:
    def method1(self):
        pass

variable1 = 10
"""
        
        entities = self.service._extract_entities_from_code(code)
        
        self.assertIn('function1', entities)
        self.assertIn('TestClass', entities)
        self.assertIn('method1', entities)
        self.assertIn('variable1', entities)
    
    def test_validate_entity_exists_direct_match(self):
        """Test entity validation with direct match"""
        entities = {'func1', 'func2', 'Class1'}
        
        self.assertTrue(self.service._validate_entity_exists('func1', entities, 'test.py'))
        self.assertFalse(self.service._validate_entity_exists('nonexistent', entities, 'test.py'))
    
    def test_validate_entity_exists_method_call(self):
        """Test entity validation with method call pattern"""
        entities = {'func1', 'method1'}
        
        self.assertTrue(self.service._validate_entity_exists('obj.method1', entities, 'test.py'))
        self.assertFalse(self.service._validate_entity_exists('obj.nonexistent', entities, 'test.py'))
    
    def test_validate_entity_exists_class_method(self):
        """Test entity validation with class method pattern"""
        entities = {'Class1', 'method1'}
        
        self.assertTrue(self.service._validate_entity_exists('Class1.method1', entities, 'test.py'))
        self.assertFalse(self.service._validate_entity_exists('NonexistentClass.method1', entities, 'test.py'))
    
    def test_parse_single_relationship_valid(self):
        """Test parsing valid relationship data"""
        raw_rel = {
            'source_entity': 'func1',
            'target_entity': 'func2',
            'relationship_type': 'CALLS',
            'confidence': 0.8,
            'line_number': 10,
            'context': 'func1 calls func2'
        }
        
        relationship = self.service._parse_single_relationship(raw_rel, 'source.py', 'target.py')
        
        self.assertIsNotNone(relationship)
        self.assertEqual(relationship.source_entity, 'func1')
        self.assertEqual(relationship.target_entity, 'func2')
        self.assertEqual(relationship.relationship_type, RelationshipType.CALLS)
        self.assertEqual(relationship.confidence, 0.8)
    
    def test_parse_single_relationship_invalid(self):
        """Test parsing invalid relationship data"""
        raw_rel = {
            'source_entity': '',  # Empty source entity
            'target_entity': 'func2',
            'relationship_type': 'CALLS',
            'confidence': 0.8
        }
        
        relationship = self.service._parse_single_relationship(raw_rel, 'source.py', 'target.py')
        
        self.assertIsNone(relationship)
    
    def test_map_relationship_type_variations(self):
        """Test mapping of relationship type variations"""
        self.assertEqual(
            self.service._map_relationship_type('CALL'),
            RelationshipType.CALLS
        )
        self.assertEqual(
            self.service._map_relationship_type('INHERIT'),
            RelationshipType.INHERITS
        )
        self.assertEqual(
            self.service._map_relationship_type('EXTENDS'),
            RelationshipType.INHERITS
        )
        self.assertIsNone(self.service._map_relationship_type('UNKNOWN'))
    
    def test_filter_relationships(self):
        """Test relationship filtering"""
        relationships = [
            Relationship(
                source_entity='func1',
                target_entity='func2',
                relationship_type=RelationshipType.CALLS,
                confidence=0.8,
                source_file='test.py',
                target_file='test.py'
            ),
            Relationship(
                source_entity='func3',
                target_entity='func4',
                relationship_type=RelationshipType.CALLS,
                confidence=0.1,  # Too low confidence
                source_file='test.py',
                target_file='test.py'
            ),
            Relationship(
                source_entity='func5',
                target_entity='func5',  # Self-relationship
                relationship_type=RelationshipType.CALLS,
                confidence=0.9,
                source_file='test.py',
                target_file='test.py'
            )
        ]
        
        filtered = self.service._filter_relationships(relationships)
        
        self.assertEqual(len(filtered), 1)
        self.assertEqual(filtered[0].source_entity, 'func1')
    
    def test_is_callable_entity(self):
        """Test callable entity detection"""
        context = {
            'source_code': 'def func1(): pass',
            'target_code': 'def func2(): pass'
        }
        
        self.assertTrue(self.service._is_callable_entity('func1', context))
        self.assertTrue(self.service._is_callable_entity('func2', context))
        self.assertFalse(self.service._is_callable_entity('nonexistent', context))
    
    def test_is_class_entity(self):
        """Test class entity detection"""
        context = {
            'source_code': 'class TestClass: pass',
            'target_code': 'class AnotherClass: pass'
        }
        
        self.assertTrue(self.service._is_class_entity('TestClass', context))
        self.assertTrue(self.service._is_class_entity('AnotherClass', context))
        self.assertFalse(self.service._is_class_entity('nonexistent', context))
    
    def test_validate_cross_file_relationship_calls(self):
        """Test cross-file relationship validation for CALLS"""
        relationship = Relationship(
            source_entity='func1',
            target_entity='func2',
            relationship_type=RelationshipType.CALLS,
            confidence=0.8,
            source_file='source.py',
            target_file='target.py'
        )
        
        # Test with import present
        context = {
            'source_code': 'from target import func2\ndef func1(): func2()',
            'target_code': 'def func2(): pass'
        }
        
        self.assertTrue(self.service._validate_cross_file_relationship(relationship, context))
        
        # Test with direct call present
        context = {
            'source_code': 'def func1(): func2()',
            'target_code': 'def func2(): pass'
        }
        
        self.assertTrue(self.service._validate_cross_file_relationship(relationship, context))
    
    def test_get_supported_relationship_types(self):
        """Test getting supported relationship types"""
        types = self.service.get_supported_relationship_types()
        
        self.assertIsInstance(types, list)
        self.assertIn('CALLS', types)
        self.assertIn('INHERITS', types)
        self.assertIn('USES', types)
    
    def test_validation_comprehensive_scenario(self):
        """Test comprehensive validation scenario that addresses surgical updater issues"""
        # This test simulates the scenario where surgical updater was failing
        # due to incorrect relationship extraction
        
        source_code = """
class SourceClass:
    def method1(self):
        target_func()
        
    def method2(self):
        AnotherClass.static_method()
"""
        
        target_code = """
def target_func():
    return "hello"

class AnotherClass:
    @staticmethod
    def static_method():
        return "static"
"""
        
        # Mock provider to return some relationships, including invalid ones
        self.mock_provider.extract_relationships.return_value = [
            {
                'source_entity': 'method1',
                'target_entity': 'target_func',
                'relationship_type': 'CALLS',
                'confidence': 0.9
            },
            {
                'source_entity': 'method2',
                'target_entity': 'static_method',
                'relationship_type': 'CALLS',
                'confidence': 0.8
            },
            {
                'source_entity': 'nonexistent_method',  # This should be filtered out
                'target_entity': 'target_func',
                'relationship_type': 'CALLS',
                'confidence': 0.7
            },
            {
                'source_entity': 'method1',
                'target_entity': 'nonexistent_func',  # This should be filtered out
                'relationship_type': 'CALLS',
                'confidence': 0.6
            }
        ]
        
        result = self.service.extract_relationships(
            'source.py', 'target.py', source_code, target_code
        )
        
        # Should succeed overall
        self.assertTrue(result.success)
        
        # Should have filtered out invalid relationships
        self.assertLess(result.total_valid, result.total_found)
        
        # Should have validation errors reported
        self.assertGreater(len(result.validation_errors), 0)
        
        # Valid relationships should remain
        valid_relationships = result.relationships
        self.assertGreater(len(valid_relationships), 0)
        
        # All remaining relationships should be valid
        for rel in valid_relationships:
            self.assertGreaterEqual(rel.confidence, 0.3)
            self.assertNotEqual(rel.source_entity, rel.target_entity)


if __name__ == '__main__':
    unittest.main()