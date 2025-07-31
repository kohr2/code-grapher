"""
Gemini provider implementation that wraps the existing gemini_client.py
"""
import time
from typing import Any, Dict, List, Optional
from base_provider import BaseAIProvider
from provider_models import AIResponse, ProviderConfig, AIProviderType
from shared.interfaces.logger_interface import LoggerInterface


class GeminiProvider(BaseAIProvider):
    """Gemini AI provider that wraps existing gemini_client.py"""
    
    def __init__(self, config: Optional[ProviderConfig] = None, logger: Optional[LoggerInterface] = None):
        """Initialize Gemini provider"""
        if config is None:
            config = ProviderConfig(
                provider_type=AIProviderType.GEMINI,
                model_name="gemini-1.5-flash"
            )
        
        super().__init__(config, logger)
    
    def _initialize_client(self) -> None:
        """Initialize the Gemini client"""
        try:
            # Import and initialize the existing GeminiClient
            from gemini_client import GeminiClient
            
            # Pass configuration to the client
            self._client = GeminiClient(
                api_key=self.config.api_key,
                model_name=self.config.model_name or "gemini-1.5-flash"
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize Gemini client: {e}")
            raise
    
    def _test_connection(self) -> bool:
        """Test connection to Gemini API"""
        try:
            if self._client and hasattr(self._client, 'test_connection'):
                return self._client.test_connection()
            return True
        except Exception:
            return False
    
    def generate_text(self, prompt: str, context: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Generate text using Gemini"""
        start_time = time.time()
        operation = "generate_text"
        
        self._log_request(operation, {"prompt_length": len(prompt), "has_context": context is not None})
        
        try:
            if not self._client:
                raise RuntimeError("Gemini client not initialized")
            
            # Use the existing client's generate method
            if hasattr(self._client, 'generate'):
                result = self._client.generate(prompt)
            elif hasattr(self._client, 'query'):
                result = self._client.query(prompt)
            else:
                raise RuntimeError("Gemini client doesn't have expected methods")
            
            # Handle different response formats
            if isinstance(result, dict):
                content = result.get('content', str(result))
            else:
                content = str(result)
            
            response = self._create_response(
                content=content,
                success=True,
                metadata={"response_length": len(content)}
            )
            
        except Exception as e:
            response = self._handle_error(operation, e)
        
        duration = time.time() - start_time
        self._log_response(operation, response, duration)
        return response
    
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """Generate description for a code entity using Gemini"""
        try:
            entity_type = entity.get('type', 'unknown')
            entity_name = entity.get('name', 'unnamed')
            
            # Create specialized prompt for entity description
            prompt = f"Generate a concise technical description for this {entity_type}:\n"
            prompt += f"Name: {entity_name}\n"
            
            if 'properties' in entity:
                prompt += f"Properties: {entity['properties']}\n"
            
            if context:
                prompt += f"Business Context: {context}\n"
            
            prompt += f"\nProvide a clear, technical description in 1-2 sentences."
            
            response = self.generate_text(prompt)
            return response.content if response.success else f"Description generation failed: {response.error}"
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to generate entity description: {e}")
            return f"Error generating description: {e}"
    
    def extract_relationships(self, source_code: str, target_code: str, 
                            relationship_types: Optional[List[str]] = None) -> List[Dict[str, Any]]:
        """Extract relationships using Gemini"""
        try:
            # Create prompt for relationship extraction
            prompt = f"""Analyze the following code files and extract relationships between entities.

Source Code:
```
{source_code}
```

Target Code:
```
{target_code}
```

Find relationships like: CALLS, INHERITS, USES, IMPLEMENTS, DEPENDS_ON.
Return as JSON list with fields: source_entity, target_entity, relationship_type, confidence (0-1).
"""
            
            response = self.generate_text(prompt)
            
            if response.success:
                # Parse JSON response (simplified - would need robust JSON parsing)
                import json
                try:
                    relationships = json.loads(response.content)
                    if isinstance(relationships, list):
                        return relationships
                except json.JSONDecodeError:
                    if self.logger:
                        self.logger.log_warning("Failed to parse relationship extraction JSON")
            
            return []
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to extract relationships: {e}")
            return []
    
    def get_provider_name(self) -> str:
        """Get the name of this provider"""
        return "Gemini"
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the Gemini model"""
        return {
            "provider": "Gemini",
            "model": self.config.model_name or "gemini-1.5-flash",
            "has_api_key": bool(self.config.api_key),
            "initialized": self._initialized
        }