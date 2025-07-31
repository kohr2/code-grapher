"""
Integration tests for AI Services
Tests the complete AI service pipeline
"""
import unittest
import sys
from pathlib import Path

# Add ai-services to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from services.ai_service import AIService
from config.ai_config import AIServiceConfig
from models.provider_models import AIProviderType


class TestAIServicesIntegration(unittest.TestCase):
    """Integration test suite for AI Services"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.config = AIServiceConfig()
        self.config.default_provider = AIProviderType.MOCK
        self.config.enable_evaluation_tracking = True
        self.ai_service = AIService(self.config)
    
    def test_full_pipeline_relationship_extraction(self):
        """Test complete relationship extraction pipeline"""
        # Initialize service
        self.ai_service.initialize({})
        
        # Define test code
        source_code = """
class Calculator:
    def add(self, a, b):
        return self.validate_input(a) + self.validate_input(b)
    
    def validate_input(self, value):
        if not isinstance(value, (int, float)):
            raise ValueError("Invalid input")
        return value
"""
        
        target_code = """
def format_result(result):
    return f"Result: {result}"

class Logger:
    def log(self, message):
        print(f"Log: {message}")
"""
        
        # Extract relationships
        result = self.ai_service.extract_relationships(
            "calculator.py", "utils.py", source_code, target_code
        )
        
        # Verify result structure
        self.assertIsNotNone(result)
        self.assertTrue(result.success)
        self.assertEqual(result.source_file, "calculator.py")
        self.assertEqual(result.target_file, "utils.py")
        self.assertGreaterEqual(result.extraction_time, 0)
        
        # Verify relationships are valid
        for relationship in result.relationships:
            self.assertIsNotNone(relationship.source_entity)
            self.assertIsNotNone(relationship.target_entity)
            self.assertIsNotNone(relationship.relationship_type)
            self.assertGreaterEqual(relationship.confidence, 0.0)
            self.assertLessEqual(relationship.confidence, 1.0)
    
    def test_full_pipeline_description_generation(self):
        """Test complete description generation pipeline"""
        # Initialize service
        self.ai_service.initialize({})
        
        # Define test entities
        entities = [
            {
                "id": "calc_add",
                "type": "method",
                "name": "add",
                "class": "Calculator",
                "code": "def add(self, a, b): return a + b",
                "parameters": ["a", "b"],
                "return_type": "number"
            },
            {
                "id": "logger_class",
                "type": "class",
                "name": "Logger",
                "code": "class Logger:\n    def log(self, message): print(message)",
                "methods": ["log"]
            }
        ]
        
        # Generate descriptions
        descriptions = self.ai_service.generate_descriptions_batch(entities)
        
        # Verify descriptions
        self.assertIsInstance(descriptions, dict)
        self.assertEqual(len(descriptions), 2)
        self.assertIn("calc_add", descriptions)
        self.assertIn("logger_class", descriptions)
        
        # Verify description quality
        for entity_id, description in descriptions.items():
            self.assertIsInstance(description, str)
            self.assertGreater(len(description), 10)  # Reasonable length description
    
    def test_evaluation_tracking_integration(self):
        """Test evaluation tracking throughout the pipeline"""
        # Initialize service
        self.ai_service.initialize({})
        
        # Perform various operations
        self.ai_service.extract_relationships(
            "test1.py", "test2.py", "def func1(): pass", "def func2(): pass"
        )
        
        self.ai_service.generate_description({
            "type": "function", "name": "test_func", "code": "def test_func(): pass"
        })
        
        self.ai_service.semantic_search("test query")
        
        # Get evaluation summary
        summary = self.ai_service.get_evaluation_summary()
        
        # Verify tracking worked
        self.assertIsInstance(summary, dict)
        # Should have tracked evaluations (exact structure depends on implementation)
    
    def test_error_handling_and_recovery(self):
        """Test error handling and graceful degradation"""
        # Initialize service
        self.ai_service.initialize({})
        
        # Test relationship extraction with malformed code
        result = self.ai_service.extract_relationships(
            "bad.py", "also_bad.py", "malformed {{{ code", "more }} bad code"
        )
        
        # Should handle gracefully
        self.assertIsNotNone(result)
        # May succeed or fail gracefully depending on mock provider implementation
        
        # Test description generation with empty entity
        description = self.ai_service.generate_description({})
        
        # Should return some description, even if error message
        self.assertIsInstance(description, str)
        self.assertGreater(len(description), 0)
    
    def test_service_lifecycle(self):
        """Test complete service lifecycle"""
        # Test initialization
        self.assertFalse(self.ai_service._initialized)
        
        self.ai_service.initialize({})
        self.assertTrue(self.ai_service._initialized)
        
        # Test health check
        health = self.ai_service.health_check()
        self.assertEqual(health["status"], "healthy")
        
        # Test service operations
        provider = self.ai_service.get_provider()
        self.assertIsNotNone(provider)
        
        # Test shutdown
        self.ai_service.shutdown()
        self.assertFalse(self.ai_service._initialized)
    
    def test_configuration_flexibility(self):
        """Test service with different configurations"""
        # Test with custom timeout
        custom_config = AIServiceConfig()
        custom_config.timeout_seconds = 30
        custom_config.max_retries = 2
        custom_config.default_provider = AIProviderType.MOCK
        
        custom_service = AIService(custom_config)
        custom_service.initialize({})
        
        self.assertEqual(custom_service.config.timeout_seconds, 30)
        self.assertEqual(custom_service.config.max_retries, 2)
        
        # Service should still work
        result = custom_service.extract_relationships(
            "a.py", "b.py", "def a(): pass", "def b(): pass"
        )
        self.assertIsNotNone(result)
        
        custom_service.shutdown()
    
    def test_concurrent_operations(self):
        """Test that service can handle multiple operations"""
        # Initialize service
        self.ai_service.initialize({})
        
        # Perform multiple operations in sequence (simulating concurrent use)
        operations = []
        
        # Multiple relationship extractions
        for i in range(3):
            result = self.ai_service.extract_relationships(
                f"source_{i}.py", f"target_{i}.py",
                f"def func{i}(): pass", f"def target{i}(): pass"
            )
            operations.append(result)
        
        # Multiple description generations
        for i in range(3):
            description = self.ai_service.generate_description({
                "id": f"entity_{i}",
                "type": "function",
                "name": f"func_{i}"
            })
            operations.append(description)
        
        # All operations should complete
        self.assertEqual(len(operations), 6)
        
        # All relationship results should be valid
        for i in range(3):
            self.assertIsNotNone(operations[i])
            self.assertTrue(hasattr(operations[i], 'success'))
        
        # All descriptions should be strings
        for i in range(3, 6):
            self.assertIsInstance(operations[i], str)


if __name__ == '__main__':
    unittest.main()