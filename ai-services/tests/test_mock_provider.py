"""
Unit tests for Mock Provider
"""
import unittest
from unittest.mock import Mock
import sys
from pathlib import Path

# Add ai-services to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from providers.mock_provider import MockProvider
from models.provider_models import ProviderConfig, AIProviderType


class TestMockProvider(unittest.TestCase):
    """Test suite for MockProvider"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.mock_logger = Mock()
        self.provider = MockProvider(logger=self.mock_logger)
    
    def test_initialization(self):
        """Test provider initialization"""
        self.assertIsNotNone(self.provider)
        self.assertEqual(self.provider.logger, self.mock_logger)
    
    def test_initialize_service(self):
        """Test service initialization"""
        self.provider.initialize({})
        # Mock provider should always succeed
        self.assertTrue(True)
    
    def test_is_available(self):
        """Test availability check"""
        self.assertTrue(self.provider.is_available())
    
    def test_get_model_info(self):
        """Test model info retrieval"""
        info = self.provider.get_model_info()
        
        self.assertIsInstance(info, dict)
        self.assertIn('model_name', info)
        self.assertIn('provider_type', info)
        self.assertEqual(info['provider_type'], 'mock')
    
    def test_generate_text(self):
        """Test text generation"""
        text = self.provider.generate_text("Test prompt")
        
        self.assertIsInstance(text, str)
        self.assertGreater(len(text), 0)
        self.assertIn("Test prompt", text)
    
    def test_extract_relationships(self):
        """Test relationship extraction"""
        source_code = "def func1(): func2()"
        target_code = "def func2(): pass"
        
        relationships = self.provider.extract_relationships(source_code, target_code)
        
        self.assertIsInstance(relationships, list)
        if len(relationships) > 0:
            rel = relationships[0]
            self.assertIn('source_entity', rel)
            self.assertIn('target_entity', rel)
            self.assertIn('relationship_type', rel)
            self.assertIn('confidence', rel)
    
    def test_generate_entity_description(self):
        """Test entity description generation"""
        entity = {"type": "function", "name": "test_func", "code": "def test_func(): pass"}
        
        description = self.provider.generate_entity_description(entity)
        
        self.assertIsInstance(description, str)
        self.assertGreater(len(description), 0)
    
    def test_shutdown(self):
        """Test provider shutdown"""
        self.provider.shutdown()
        # Mock provider shutdown should succeed
        self.assertTrue(True)
    
    def test_error_handling_graceful(self):
        """Test that mock provider handles errors gracefully"""
        # Mock provider should not raise exceptions
        try:
            self.provider.generate_text("")
            self.provider.extract_relationships("invalid", "code")
            self.provider.generate_entity_description({})
        except Exception as e:
            self.fail(f"Mock provider should not raise exceptions: {e}")


if __name__ == '__main__':
    unittest.main()