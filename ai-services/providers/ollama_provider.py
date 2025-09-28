"""
Ollama provider implementation that wraps the existing ollama_client.py
"""
import time
from typing import Any, Dict, List, Optional
from .base_provider import BaseAIProvider
from ..models.provider_models import AIResponse, ProviderConfig, AIProviderType
from shared.interfaces.logger_interface import LoggerInterface


class OllamaProvider(BaseAIProvider):
    """Ollama AI provider that wraps existing ollama_client.py"""
    
    def __init__(self, config: Optional[ProviderConfig] = None, logger: Optional[LoggerInterface] = None):
        """Initialize Ollama provider"""
        if config is None:
            config = ProviderConfig(
                provider_type=AIProviderType.OLLAMA,
                base_url="http://localhost:11434",
                model_name="gemma3:4b"
            )
        
        super().__init__(config, logger)
    
    def _initialize_client(self) -> None:
        """Initialize the Ollama client"""
        try:
            # Import and initialize the existing OllamaClient
            from ollama_client import OllamaClient
            
            # Pass configuration to the client
            self._client = OllamaClient(
                model_name=self.config.model_name or "gemma3:4b",
                base_url=self.config.base_url or "http://localhost:11434"
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize Ollama client: {e}")
            raise
    
    def _test_connection(self) -> bool:
        """Test connection to Ollama server"""
        try:
            if self._client and hasattr(self._client, 'test_connection'):
                return self._client.test_connection()
            return True
        except Exception:
            return False
    
    def generate_text(self, prompt: str, context: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Generate text using Ollama"""
        start_time = time.time()
        operation = "generate_text"
        
        self._log_request(operation, {"prompt_length": len(prompt), "has_context": context is not None})
        
        try:
            if not self._client:
                raise RuntimeError("Ollama client not initialized")
            
            # Use the existing client's generate method
            if hasattr(self._client, 'generate'):
                result = self._client.generate(prompt)
            elif hasattr(self._client, 'query'):
                result = self._client.query(prompt)
            else:
                raise RuntimeError("Ollama client doesn't have expected methods")
            
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
        """Generate description for a code entity using Ollama"""
        try:
            entity_type = entity.get('type', 'unknown')
            entity_name = entity.get('name', 'unnamed')
            code_snippet = entity.get('code_snippet', '')
            
            # Create business-focused prompt for COBOL entities
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
        """Extract relationships using Ollama (wrapper around existing AIRelationshipExtractor)"""
        try:
            # Import and use the existing AI relationship extractor
            from ai_relationship_extractor import AIRelationshipExtractor
            
            # Create extractor instance (it uses Ollama internally)
            extractor = AIRelationshipExtractor()
            
            # Extract relationships using the existing method
            relationships = extractor.extract_relationships(
                source_file="source.py",  # Placeholder
                target_file="target.py",  # Placeholder  
                source_code=source_code,
                target_code=target_code,
                relationship_types=relationship_types
            )
            
            # Convert to our format
            converted_relationships = []
            for rel in relationships:
                if hasattr(rel, '__dict__'):
                    # Convert RelationshipExtraction object to dict
                    rel_dict = {
                        'source_entity': getattr(rel, 'source_entity', ''),
                        'target_entity': getattr(rel, 'target_entity', ''),
                        'relationship_type': str(getattr(rel, 'relationship_type', '')),
                        'confidence': getattr(rel, 'confidence', 0.0),
                        'context': getattr(rel, 'context', '')
                    }
                    converted_relationships.append(rel_dict)
                elif isinstance(rel, dict):
                    converted_relationships.append(rel)
            
            return converted_relationships
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to extract relationships: {e}")
            return []
    
    def get_provider_name(self) -> str:
        """Get the name of this provider"""
        return "Ollama"
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the Ollama model"""
        return {
            "provider": "Ollama",
            "model": self.config.model_name or "gemma3:4b",
            "base_url": self.config.base_url or "http://localhost:11434",
            "initialized": self._initialized
        }