"""
Mock AI provider for testing and development
"""
import time
import random
from typing import Any, Dict, List, Optional
from .base_provider import BaseAIProvider
from ..models.provider_models import AIResponse, ProviderConfig, AIProviderType
from shared.interfaces.logger_interface import LoggerInterface


class MockProvider(BaseAIProvider):
    """Mock AI provider for testing"""
    
    def __init__(self, config: Optional[ProviderConfig] = None, logger: Optional[LoggerInterface] = None):
        """Initialize Mock provider"""
        if config is None:
            config = ProviderConfig(
                provider_type=AIProviderType.MOCK,
                model_name="mock-model-v1"
            )
        
        super().__init__(config, logger)
        self.responses = {
            "description": "This is a mock AI-generated description for {entity_type} '{entity_name}'.",
            "relationship": [
                {"source_entity": "Function1", "target_entity": "Function2", "relationship_type": "CALLS", "confidence": 0.9},
                {"source_entity": "Class1", "target_entity": "Class2", "relationship_type": "INHERITS", "confidence": 0.8}
            ]
        }
    
    def _initialize_client(self) -> None:
        """Initialize mock client"""
        self._client = "mock_client_initialized"
    
    def _test_connection(self) -> bool:
        """Mock connection test"""
        return True
    
    def generate_text(self, prompt: str, context: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Generate mock text response"""
        start_time = time.time()
        
        # Simulate processing time
        time.sleep(0.1)
        
        mock_content = f"Mock AI response to prompt: '{prompt[:50]}...'"
        if context:
            mock_content += f" (with context: {list(context.keys())})"
        
        response = self._create_response(
            content=mock_content,
            success=True,
            metadata={"mock": True, "prompt_length": len(prompt)}
        )
        
        duration = time.time() - start_time
        self._log_response("generate_text", response, duration)
        return response
    
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """Generate mock entity description"""
        entity_type = entity.get('type', 'unknown')
        entity_name = entity.get('name', 'unnamed')
        
        description = self.responses["description"].format(
            entity_type=entity_type,
            entity_name=entity_name
        )
        
        if context:
            description += f" Business context: {context[:100]}..."
        
        return description
    
    def extract_relationships(self, source_code: str, target_code: str, 
                            relationship_types: Optional[List[str]] = None) -> List[Dict[str, Any]]:
        """Extract mock relationships"""
        # Return mock relationships with some randomization
        mock_relationships = []
        
        for i, base_rel in enumerate(self.responses["relationship"]):
            rel = base_rel.copy()
            rel["confidence"] = random.uniform(0.7, 0.95)
            rel["source_entity"] = f"MockEntity{i+1}"
            rel["target_entity"] = f"MockEntity{i+2}"
            
            if relationship_types and rel["relationship_type"] not in relationship_types:
                continue
                
            mock_relationships.append(rel)
        
        return mock_relationships
    
    def get_provider_name(self) -> str:
        """Get provider name"""
        return "Mock"
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get mock model info"""
        return {
            "provider": "Mock",
            "model": "mock-model-v1",
            "purpose": "testing_and_development",
            "initialized": self._initialized
        }