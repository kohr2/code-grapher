"""
Comprehensive unit tests for AI Service
"""
import unittest
from unittest.mock import Mock, patch, MagicMock
import sys
from pathlib import Path

# Add paths to enable imports
ai_services_path = Path(__file__).parent.parent
sys.path.insert(0, str(ai_services_path))
sys.path.insert(0, str(ai_services_path.parent))

from ai_services.services.ai_service import AIService
from ai_services.config.ai_config import AIServiceConfig
from ai_services.providers.mock_provider import MockProvider
from ai_services.providers.openai_provider import OpenAIProvider
from ai_services.models.provider_models import AIProviderType
from ai_services.models.relationship_models import RelationshipExtractionResult, Relationship, RelationshipType
from ai_services.models.evaluation_models import EvaluationCategory


class TestAIService(unittest.TestCase):
    """Test suite for AIService"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.mock_logger = Mock()
        self.config = AIServiceConfig()
        self.config.default_provider = AIProviderType.MOCK
        self.ai_service = AIService(self.config, self.mock_logger)
    
    def test_initialization(self):
        """Test AI service initialization"""
        self.assertIsNotNone(self.ai_service)
        self.assertEqual(self.ai_service.config, self.config)
        self.assertEqual(self.ai_service.logger, self.mock_logger)
        self.assertFalse(self.ai_service._initialized)
    
    def test_initialize_success(self):
        """Test successful initialization"""
        self.ai_service.initialize({})
        
        self.assertTrue(self.ai_service._initialized)
        self.assertGreater(len(self.ai_service._providers), 0)
        self.assertIsNotNone(self.ai_service._relationship_service)
        self.assertIsNotNone(self.ai_service._description_service)
    
    def test_initialize_with_config_override(self):
        """Test initialization with config override"""
        override_config = {
            'timeout_seconds': 60,
            'max_retries': 5
        }
        
        self.ai_service.initialize(override_config)
        
        self.assertTrue(self.ai_service._initialized)
        self.assertEqual(self.ai_service.config.timeout_seconds, 60)
        self.assertEqual(self.ai_service.config.max_retries, 5)
    
    def test_get_provider_default(self):
        """Test getting default provider"""
        self.ai_service.initialize({})
        
        provider = self.ai_service.get_provider()
        
        self.assertIsNotNone(provider)
        self.assertIsInstance(provider, MockProvider)
    
    def test_get_provider_specific(self):
        """Test getting specific provider"""
        self.ai_service.initialize({})
        
        provider = self.ai_service.get_provider("mock")
        
        self.assertIsNotNone(provider)
        self.assertIsInstance(provider, MockProvider)
    
    def test_get_provider_not_initialized(self):
        """Test getting provider when not initialized raises error"""
        with self.assertRaises(RuntimeError):
            self.ai_service.get_provider()
    
    def test_extract_relationships_success(self):
        """Test successful relationship extraction"""
        self.ai_service.initialize({})
        
        result = self.ai_service.extract_relationships(
            "source.py", "target.py", "def foo(): pass", "def bar(): foo()"
        )
        
        self.assertIsInstance(result, RelationshipExtractionResult)
        self.assertTrue(result.success)
        self.assertEqual(result.source_file, "source.py")
        self.assertEqual(result.target_file, "target.py")
    
    def test_extract_relationships_service_not_initialized(self):
        """Test relationship extraction when service not initialized"""
        self.ai_service.initialize({})
        self.ai_service._relationship_service = None
        
        result = self.ai_service.extract_relationships(
            "source.py", "target.py", "code1", "code2"
        )
        
        self.assertFalse(result.success)
        self.assertIsNotNone(result.error)
    
    def test_generate_description_success(self):
        """Test successful description generation"""
        self.ai_service.initialize({})
        
        entity = {"type": "function", "name": "test_func", "code": "def test_func(): pass"}
        
        description = self.ai_service.generate_description(entity)
        
        self.assertIsInstance(description, str)
        self.assertGreater(len(description), 0)
    
    def test_generate_description_service_not_initialized(self):
        """Test description generation when service not initialized"""
        self.ai_service.initialize({})
        self.ai_service._description_service = None
        
        entity = {"type": "function", "name": "test_func"}
        
        description = self.ai_service.generate_description(entity)
        
        self.assertIn("Error generating description", description)
    
    def test_generate_descriptions_batch(self):
        """Test batch description generation"""
        self.ai_service.initialize({})
        
        entities = [
            {"id": "1", "type": "function", "name": "func1"},
            {"id": "2", "type": "class", "name": "Class1"}
        ]
        
        descriptions = self.ai_service.generate_descriptions_batch(entities)
        
        self.assertIsInstance(descriptions, dict)
        self.assertEqual(len(descriptions), 2)
        self.assertIn("1", descriptions)
        self.assertIn("2", descriptions)
    
    def test_track_evaluation_success(self):
        """Test successful evaluation tracking"""
        self.ai_service.initialize({})
        
        self.ai_service.track_evaluation("relationship_extraction", {"success": True})
        
        # Should not raise exception
        self.mock_logger.log_error.assert_not_called()
    
    def test_track_evaluation_invalid_category(self):
        """Test evaluation tracking with invalid category"""
        self.ai_service.initialize({})
        
        self.ai_service.track_evaluation("invalid_category", {"success": True})
        
        self.mock_logger.log_warning.assert_called()
    
    def test_track_evaluation_no_service(self):
        """Test evaluation tracking when service not available"""
        self.ai_service.initialize({})
        self.ai_service._evaluation_service = None
        
        self.ai_service.track_evaluation("relationship_extraction", {"success": True})
        
        self.mock_logger.log_warning.assert_called()
    
    def test_get_evaluation_summary_success(self):
        """Test getting evaluation summary"""
        self.ai_service.initialize({})
        
        summary = self.ai_service.get_evaluation_summary()
        
        self.assertIsInstance(summary, dict)
    
    def test_get_evaluation_summary_no_service(self):
        """Test getting evaluation summary when service not available"""
        self.ai_service.initialize({})
        self.ai_service._evaluation_service = None
        
        summary = self.ai_service.get_evaluation_summary()
        
        self.assertIn("error", summary)
    
    def test_semantic_search_placeholder(self):
        """Test semantic search placeholder implementation"""
        self.ai_service.initialize({})
        
        results = self.ai_service.semantic_search("test query", limit=3)
        
        self.assertIsInstance(results, list)
        self.assertLessEqual(len(results), 3)
    
    def test_health_check_healthy(self):
        """Test health check when service is healthy"""
        self.ai_service.initialize({})
        
        health = self.ai_service.health_check()
        
        self.assertEqual(health["status"], "healthy")
        self.assertTrue(health["initialized"])
        self.assertIn("providers", health)
        self.assertIn("services", health)
    
    def test_health_check_unhealthy(self):
        """Test health check when service is unhealthy"""
        health = self.ai_service.health_check()
        
        self.assertEqual(health["status"], "unhealthy")
        self.assertFalse(health["initialized"])
    
    def test_shutdown(self):
        """Test service shutdown"""
        self.ai_service.initialize({})
        self.assertTrue(self.ai_service._initialized)
        
        self.ai_service.shutdown()
        
        self.assertFalse(self.ai_service._initialized)
        self.assertEqual(len(self.ai_service._providers), 0)
    
    def test_get_service_name(self):
        """Test service name retrieval"""
        name = self.ai_service.get_service_name()
        self.assertEqual(name, "AIService")
    
    @patch('services.ai_service.OllamaProvider')
    def test_provider_initialization_failure_continues(self, mock_ollama_provider):
        """Test that provider initialization failure doesn't stop service"""
        mock_ollama_provider.side_effect = Exception("Provider failed")
        
        # Should still initialize successfully with mock provider
        self.ai_service.initialize({})
        
        self.assertTrue(self.ai_service._initialized)
        self.mock_logger.log_warning.assert_called()
    
    def test_no_providers_available_raises_error(self):
        """Test that having no providers raises error"""
        # Mock all providers to fail
        with patch('services.ai_service.OllamaProvider', side_effect=Exception("Failed")), \
             patch('services.ai_service.GeminiProvider', side_effect=Exception("Failed")), \
             patch('services.ai_service.OpenAIProvider', side_effect=Exception("Failed")), \
             patch('services.ai_service.MockProvider', side_effect=Exception("Failed")):
            
            with self.assertRaises(RuntimeError):
                self.ai_service.initialize({})
    
    def test_openai_provider_initialization(self):
        """Test OpenAI provider initialization when API key is provided"""
        config = AIServiceConfig()
        config.openai_api_key = "test-api-key"
        config.openai_model = "gpt-4o-mini"
        config.default_provider = AIProviderType.OPENAI
        
        ai_service = AIService(config, self.mock_logger)
        
        with patch('services.ai_service.OpenAIProvider') as mock_openai_provider_class:
            mock_provider = Mock()
            mock_openai_provider_class.return_value = mock_provider
            
            ai_service.initialize({})
            
            # Check that OpenAI provider was initialized
            mock_openai_provider_class.assert_called_once()
            mock_provider.initialize.assert_called_once()
    
    def test_openai_provider_not_initialized_without_key(self):
        """Test that OpenAI provider is not initialized without API key"""
        config = AIServiceConfig()
        config.openai_api_key = None
        config.default_provider = AIProviderType.OPENAI
        
        ai_service = AIService(config, self.mock_logger)
        
        with patch('services.ai_service.OpenAIProvider') as mock_openai_provider_class:
            ai_service.initialize({})
            
            # OpenAI provider should not be called without API key
            mock_openai_provider_class.assert_not_called()
    
    def test_get_openai_provider(self):
        """Test getting OpenAI provider"""
        config = AIServiceConfig()
        config.openai_api_key = "test-api-key"
        config.default_provider = AIProviderType.OPENAI
        
        ai_service = AIService(config, self.mock_logger)
        
        with patch('services.ai_service.OpenAIProvider') as mock_openai_provider_class:
            mock_provider = Mock()
            mock_openai_provider_class.return_value = mock_provider
            
            ai_service.initialize({})
            
            provider = ai_service.get_provider("openai")
            
            self.assertEqual(provider, mock_provider)


if __name__ == '__main__':
    unittest.main()