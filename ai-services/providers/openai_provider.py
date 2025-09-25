"""
OpenAI provider implementation using the official OpenAI Python client
"""
import time
import json
from typing import Any, Dict, List, Optional
from .base_provider import BaseAIProvider
from ..models.provider_models import AIResponse, ProviderConfig, AIProviderType
from shared.interfaces.logger_interface import LoggerInterface


class OpenAIProvider(BaseAIProvider):
    """OpenAI provider using the official OpenAI Python client"""
    
    def __init__(self, config: Optional[ProviderConfig] = None, logger: Optional[LoggerInterface] = None):
        """Initialize OpenAI provider"""
        if config is None:
            config = ProviderConfig(
                provider_type=AIProviderType.OPENAI,
                model_name="gpt-4o-mini"
            )
        
        super().__init__(config, logger)
    
    def _initialize_client(self) -> None:
        """Initialize the OpenAI client"""
        try:
            # Import OpenAI client
            from openai import OpenAI
            
            # Initialize client with configuration
            client_kwargs = {}
            
            if self.config.api_key:
                client_kwargs['api_key'] = self.config.api_key
            
            if self.config.base_url:
                client_kwargs['base_url'] = self.config.base_url
            
            self._client = OpenAI(**client_kwargs)
            
        except ImportError as e:
            if self.logger:
                self.logger.log_error(f"OpenAI client not installed: {e}")
            raise RuntimeError("OpenAI client not installed. Install with: pip install openai")
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize OpenAI client: {e}")
            raise
    
    def _test_connection(self) -> bool:
        """Test connection to OpenAI API"""
        try:
            if not self._client:
                return False
            
            # Test with a simple completion request
            response = self._client.chat.completions.create(
                model=self.config.model_name or "gpt-4o-mini",
                messages=[{"role": "user", "content": "Hello"}],
                max_tokens=5,
                timeout=10
            )
            
            return response.choices[0].message.content is not None
            
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"OpenAI connection test failed: {e}")
            return False
    
    def generate_text(self, prompt: str, context: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Generate text using OpenAI"""
        start_time = time.time()
        operation = "generate_text"
        
        self._log_request(operation, {"prompt_length": len(prompt), "has_context": context is not None})
        
        try:
            if not self._client:
                raise RuntimeError("OpenAI client not initialized")
            
            # Prepare messages
            messages = [{"role": "user", "content": prompt}]
            
            # Add context if provided
            if context:
                context_str = self._format_context(context)
                if context_str:
                    messages.insert(0, {"role": "system", "content": f"Context: {context_str}"})
            
            # Make API call
            response = self._client.chat.completions.create(
                model=self.config.model_name or "gpt-4o-mini",
                messages=messages,
                max_tokens=4000,
                temperature=0.7,
                timeout=self.config.timeout
            )
            
            content = response.choices[0].message.content or ""
            
            response_obj = self._create_response(
                content=content,
                success=True,
                metadata={
                    "response_length": len(content),
                    "model": response.model,
                    "usage": response.usage.dict() if response.usage else None,
                    "finish_reason": response.choices[0].finish_reason
                }
            )
            
        except Exception as e:
            response_obj = self._handle_error(operation, e)
        
        duration = time.time() - start_time
        self._log_response(operation, response_obj, duration)
        return response_obj
    
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """Generate description for a code entity using OpenAI"""
        try:
            entity_type = entity.get('type', 'unknown')
            entity_name = entity.get('name', 'unnamed')
            code_snippet = entity.get('code_snippet', '')
            
            # Create specialized prompt for entity description
            if entity_type in ['program', 'compilation_unit'] or 'COBOL' in str(context or ''):
                prompt = f"""Analyze this COBOL banking system component and describe its specific business logic:

Entity: {entity_name}
Type: {entity_type}

Code Context:
{code_snippet[:500] if code_snippet else 'No code snippet available'}

Business Context: {context or 'Banking system for account management, interest calculation, and reporting'}

Based on the entity name and context, describe the SPECIFIC business function this component performs in the banking system. Focus on:
- What specific banking operation it handles
- What data it processes
- What business rules it implements
- What output it produces

Provide a concise, specific description in 1-2 sentences that explains the actual business purpose, not generic technical details."""
            else:
                # Generic prompt for non-COBOL entities
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
        """Extract relationships using OpenAI"""
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

Find relationships like: CALLS, INHERITS, USES, IMPLEMENTS, DEPENDS_ON, IMPORTS, EXPORTS.
Return as JSON list with fields: source_entity, target_entity, relationship_type, confidence (0-1), context.

Example format:
[
  {{
    "source_entity": "function_name",
    "target_entity": "called_function",
    "relationship_type": "CALLS",
    "confidence": 0.9,
    "context": "function_name calls called_function on line X"
  }}
]

Only return valid JSON, no additional text."""
            
            response = self.generate_text(prompt)
            
            if response.success:
                # Parse JSON response
                try:
                    relationships = json.loads(response.content)
                    if isinstance(relationships, list):
                        # Filter by relationship types if specified
                        if relationship_types:
                            relationships = [
                                rel for rel in relationships 
                                if rel.get('relationship_type') in relationship_types
                            ]
                        return relationships
                except json.JSONDecodeError as e:
                    if self.logger:
                        self.logger.log_warning(f"Failed to parse relationship extraction JSON: {e}")
                    # Try to extract JSON from response
                    return self._extract_json_from_response(response.content)
            
            return []
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to extract relationships: {e}")
            return []
    
    def get_provider_name(self) -> str:
        """Get the name of this provider"""
        return "OpenAI"
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the OpenAI model"""
        return {
            "provider": "OpenAI",
            "model": self.config.model_name or "gpt-4o-mini",
            "has_api_key": bool(self.config.api_key),
            "base_url": self.config.base_url or "https://api.openai.com/v1",
            "initialized": self._initialized
        }
    
    def _format_context(self, context: Dict[str, Any]) -> str:
        """Format context dictionary into a readable string"""
        if not context:
            return ""
        
        context_parts = []
        for key, value in context.items():
            if isinstance(value, (str, int, float, bool)):
                context_parts.append(f"{key}: {value}")
            elif isinstance(value, (list, dict)):
                context_parts.append(f"{key}: {json.dumps(value, indent=2)}")
        
        return "\n".join(context_parts)
    
    def _extract_json_from_response(self, content: str) -> List[Dict[str, Any]]:
        """Extract JSON from response content that might have extra text"""
        try:
            # Look for JSON array in the content
            start_idx = content.find('[')
            end_idx = content.rfind(']') + 1
            
            if start_idx != -1 and end_idx > start_idx:
                json_str = content[start_idx:end_idx]
                return json.loads(json_str)
        except Exception:
            pass
        
        return []
