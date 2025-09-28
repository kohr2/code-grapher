"""
Unit tests for OpenAI Provider
"""
import unittest
from unittest.mock import Mock, patch, MagicMock
import sys
from pathlib import Path

# Add ai-services to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from providers.openai_provider import OpenAIProvider
from models.provider_models import ProviderConfig, AIProviderType


class TestOpenAIProvider(unittest.TestCase):
    """Test suite for OpenAIProvider"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.mock_logger = Mock()
        self.config = ProviderConfig(
            provider_type=AIProviderType.OPENAI,
            api_key="test-api-key",
            model_name="gpt-4o-mini",
            base_url="https://api.openai.com/v1"
        )
        self.provider = OpenAIProvider(self.config, self.mock_logger)
    
    def test_initialization(self):
        """Test provider initialization"""
        self.assertIsNotNone(self.provider)
        self.assertEqual(self.provider.config, self.config)
        self.assertEqual(self.provider.logger, self.mock_logger)
    
    @patch('providers.openai_provider.OpenAI')
    def test_initialize_client_success(self, mock_openai_class):
        """Test successful client initialization"""
        mock_client = Mock()
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        mock_openai_class.assert_called_once_with(
            api_key="test-api-key",
            base_url="https://api.openai.com/v1"
        )
        self.assertEqual(self.provider._client, mock_client)
    
    @patch('providers.openai_provider.OpenAI')
    def test_initialize_client_without_base_url(self, mock_openai_class):
        """Test client initialization without base_url"""
        config = ProviderConfig(
            provider_type=AIProviderType.OPENAI,
            api_key="test-api-key",
            model_name="gpt-4o-mini"
        )
        provider = OpenAIProvider(config, self.mock_logger)
        
        mock_client = Mock()
        mock_openai_class.return_value = mock_client
        
        provider._initialize_client()
        
        mock_openai_class.assert_called_once_with(api_key="test-api-key")
    
    def test_initialize_client_import_error(self):
        """Test client initialization with import error"""
        with patch('providers.openai_provider.OpenAI', side_effect=ImportError("No module named 'openai'")):
            with self.assertRaises(RuntimeError) as context:
                self.provider._initialize_client()
            
            self.assertIn("OpenAI client not installed", str(context.exception))
    
    @patch('providers.openai_provider.OpenAI')
    def test_initialize_client_general_error(self, mock_openai_class):
        """Test client initialization with general error"""
        mock_openai_class.side_effect = Exception("API error")
        
        with self.assertRaises(Exception):
            self.provider._initialize_client()
    
    @patch('providers.openai_provider.OpenAI')
    def test_test_connection_success(self, mock_openai_class):
        """Test successful connection test"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "Hello"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        result = self.provider._test_connection()
        
        self.assertTrue(result)
        mock_client.chat.completions.create.assert_called_once()
    
    @patch('providers.openai_provider.OpenAI')
    def test_test_connection_failure(self, mock_openai_class):
        """Test connection test failure"""
        mock_client = Mock()
        mock_client.chat.completions.create.side_effect = Exception("API error")
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        result = self.provider._test_connection()
        
        self.assertFalse(result)
    
    def test_test_connection_no_client(self):
        """Test connection test with no client"""
        result = self.provider._test_connection()
        self.assertFalse(result)
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_text_success(self, mock_openai_class):
        """Test successful text generation"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "Generated response"
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = Mock()
        mock_response.usage.dict.return_value = {"total_tokens": 100}
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        response = self.provider.generate_text("Test prompt")
        
        self.assertTrue(response.success)
        self.assertEqual(response.content, "Generated response")
        self.assertEqual(response.provider, "OpenAI")
        self.assertEqual(response.model, "gpt-4o-mini")
        self.assertIsNotNone(response.metadata)
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_text_with_context(self, mock_openai_class):
        """Test text generation with context"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "Generated response"
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        context = {"key": "value", "number": 42}
        response = self.provider.generate_text("Test prompt", context)
        
        # Check that context was added to messages
        call_args = mock_client.chat.completions.create.call_args
        messages = call_args[1]['messages']
        
        self.assertEqual(len(messages), 2)
        self.assertEqual(messages[0]['role'], 'system')
        self.assertIn('Context:', messages[0]['content'])
        self.assertEqual(messages[1]['role'], 'user')
        self.assertEqual(messages[1]['content'], 'Test prompt')
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_text_failure(self, mock_openai_class):
        """Test text generation failure"""
        mock_client = Mock()
        mock_client.chat.completions.create.side_effect = Exception("API error")
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        response = self.provider.generate_text("Test prompt")
        
        self.assertFalse(response.success)
        self.assertIn("API error", response.error)
    
    def test_generate_text_no_client(self):
        """Test text generation with no client"""
        response = self.provider.generate_text("Test prompt")
        
        self.assertFalse(response.success)
        self.assertIn("not initialized", response.error)
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_description_cobol(self, mock_openai_class):
        """Test COBOL entity description generation"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "This is a COBOL banking function"
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        entity = {
            "type": "program",
            "name": "CALCULATE_INTEREST",
            "code_snippet": "PROGRAM-ID. CALCULATE_INTEREST."
        }
        
        description = self.provider.generate_description(entity, "Banking context")
        
        self.assertEqual(description, "This is a COBOL banking function")
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_description_generic(self, mock_openai_class):
        """Test generic entity description generation"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "This is a Python function"
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        entity = {
            "type": "function",
            "name": "calculate_sum",
            "properties": {"parameters": ["a", "b"]}
        }
        
        description = self.provider.generate_description(entity)
        
        self.assertEqual(description, "This is a Python function")
    
    @patch('providers.openai_provider.OpenAI')
    def test_generate_description_failure(self, mock_openai_class):
        """Test description generation failure"""
        mock_client = Mock()
        mock_client.chat.completions.create.side_effect = Exception("API error")
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        entity = {"type": "function", "name": "test"}
        description = self.provider.generate_description(entity)
        
        self.assertIn("Error generating description", description)
    
    @patch('providers.openai_provider.OpenAI')
    def test_extract_relationships_success(self, mock_openai_class):
        """Test successful relationship extraction"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = '''[
            {
                "source_entity": "func1",
                "target_entity": "func2",
                "relationship_type": "CALLS",
                "confidence": 0.9,
                "context": "func1 calls func2"
            }
        ]'''
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        relationships = self.provider.extract_relationships(
            "def func1(): func2()", 
            "def func2(): pass"
        )
        
        self.assertEqual(len(relationships), 1)
        self.assertEqual(relationships[0]['source_entity'], 'func1')
        self.assertEqual(relationships[0]['target_entity'], 'func2')
        self.assertEqual(relationships[0]['relationship_type'], 'CALLS')
        self.assertEqual(relationships[0]['confidence'], 0.9)
    
    @patch('providers.openai_provider.OpenAI')
    def test_extract_relationships_with_filter(self, mock_openai_class):
        """Test relationship extraction with type filter"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = '''[
            {
                "source_entity": "Class1",
                "target_entity": "Class2",
                "relationship_type": "INHERITS",
                "confidence": 0.8,
                "context": "Class1 inherits from Class2"
            },
            {
                "source_entity": "func1",
                "target_entity": "func2",
                "relationship_type": "CALLS",
                "confidence": 0.9,
                "context": "func1 calls func2"
            }
        ]'''
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        relationships = self.provider.extract_relationships(
            "class Class1(Class2): pass", 
            "class Class2: pass",
            relationship_types=["CALLS"]
        )
        
        self.assertEqual(len(relationships), 1)
        self.assertEqual(relationships[0]['relationship_type'], 'CALLS')
    
    @patch('providers.openai_provider.OpenAI')
    def test_extract_relationships_json_parse_error(self, mock_openai_class):
        """Test relationship extraction with JSON parse error"""
        mock_client = Mock()
        mock_response = Mock()
        mock_response.choices = [Mock()]
        mock_response.choices[0].message.content = "Invalid JSON response"
        mock_response.model = "gpt-4o-mini"
        mock_response.usage = None
        mock_response.choices[0].finish_reason = "stop"
        mock_client.chat.completions.create.return_value = mock_response
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        relationships = self.provider.extract_relationships(
            "def func1(): pass", 
            "def func2(): pass"
        )
        
        self.assertEqual(len(relationships), 0)
    
    @patch('providers.openai_provider.OpenAI')
    def test_extract_relationships_failure(self, mock_openai_class):
        """Test relationship extraction failure"""
        mock_client = Mock()
        mock_client.chat.completions.create.side_effect = Exception("API error")
        mock_openai_class.return_value = mock_client
        
        self.provider._initialize_client()
        
        relationships = self.provider.extract_relationships(
            "def func1(): pass", 
            "def func2(): pass"
        )
        
        self.assertEqual(len(relationships), 0)
    
    def test_get_provider_name(self):
        """Test provider name retrieval"""
        name = self.provider.get_provider_name()
        self.assertEqual(name, "OpenAI")
    
    def test_get_model_info(self):
        """Test model info retrieval"""
        info = self.provider.get_model_info()
        
        self.assertIsInstance(info, dict)
        self.assertEqual(info['provider'], 'OpenAI')
        self.assertEqual(info['model'], 'gpt-4o-mini')
        self.assertTrue(info['has_api_key'])
        self.assertEqual(info['base_url'], 'https://api.openai.com/v1')
        self.assertFalse(info['initialized'])
    
    def test_get_model_info_no_base_url(self):
        """Test model info without base_url"""
        config = ProviderConfig(
            provider_type=AIProviderType.OPENAI,
            api_key="test-key",
            model_name="gpt-4"
        )
        provider = OpenAIProvider(config, self.mock_logger)
        
        info = provider.get_model_info()
        
        self.assertEqual(info['base_url'], 'https://api.openai.com/v1')
    
    def test_format_context(self):
        """Test context formatting"""
        context = {
            "string": "value",
            "number": 42,
            "boolean": True,
            "list": [1, 2, 3],
            "dict": {"nested": "value"}
        }
        
        formatted = self.provider._format_context(context)
        
        self.assertIn("string: value", formatted)
        self.assertIn("number: 42", formatted)
        self.assertIn("boolean: True", formatted)
        self.assertIn("list:", formatted)
        self.assertIn("dict:", formatted)
    
    def test_format_context_empty(self):
        """Test context formatting with empty context"""
        formatted = self.provider._format_context({})
        self.assertEqual(formatted, "")
    
    def test_extract_json_from_response(self):
        """Test JSON extraction from response"""
        content = "Here is the JSON: [{\"key\": \"value\"}] and some extra text"
        
        result = self.provider._extract_json_from_response(content)
        
        self.assertEqual(len(result), 1)
        self.assertEqual(result[0]['key'], 'value')
    
    def test_extract_json_from_response_no_json(self):
        """Test JSON extraction with no JSON"""
        content = "No JSON here"
        
        result = self.provider._extract_json_from_response(content)
        
        self.assertEqual(len(result), 0)
    
    def test_shutdown(self):
        """Test provider shutdown"""
        # Should not raise exception
        self.provider.shutdown()
        self.assertTrue(True)  # If we get here, shutdown succeeded


if __name__ == '__main__':
    unittest.main()

