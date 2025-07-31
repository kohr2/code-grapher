"""
AI Provider interface for abstraction over different AI services
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional
from ..models.provider_models import AIResponse, AIRequest


class AIProviderInterface(ABC):
    """Abstract interface for AI providers (Ollama, Gemini, etc.)"""
    
    @abstractmethod
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the AI provider with configuration"""
        pass
    
    @abstractmethod
    def is_available(self) -> bool:
        """Check if the AI provider is available and responsive"""
        pass
    
    @abstractmethod
    def generate_text(self, prompt: str, context: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Generate text using the AI provider"""
        pass
    
    @abstractmethod
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """Generate description for a code entity"""
        pass
    
    @abstractmethod
    def extract_relationships(self, source_code: str, target_code: str, 
                            relationship_types: Optional[List[str]] = None) -> List[Dict[str, Any]]:
        """Extract relationships between code files"""
        pass
    
    @abstractmethod
    def get_provider_name(self) -> str:
        """Get the name of this provider"""
        pass
    
    @abstractmethod
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the underlying model"""
        pass
    
    @abstractmethod
    def shutdown(self) -> None:
        """Clean shutdown of the provider"""
        pass