"""
AI service configuration
"""
from dataclasses import dataclass
from typing import Dict, Any, Optional
from provider_models import AIProviderType


@dataclass
class AIServiceConfig:
    """Configuration for AI services"""
    default_provider: AIProviderType = AIProviderType.OLLAMA
    ollama_url: str = "http://localhost:11434"
    ollama_model: str = "gemma3:4b"
    gemini_api_key: Optional[str] = None
    gemini_model: str = "gemini-1.5-flash"
    enable_evaluation_tracking: bool = True
    enable_caching: bool = True
    cache_ttl_seconds: int = 3600
    max_retries: int = 3
    timeout_seconds: int = 30
    confidence_threshold: float = 0.3
    
    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> 'AIServiceConfig':
        """Create config from dictionary"""
        provider_str = config.get('default_provider', 'ollama')
        
        # Map string to enum
        if isinstance(provider_str, str):
            try:
                default_provider = AIProviderType(provider_str.lower())
            except ValueError:
                default_provider = AIProviderType.OLLAMA
        else:
            default_provider = provider_str
        
        return cls(
            default_provider=default_provider,
            ollama_url=config.get('ollama_url', "http://localhost:11434"),
            ollama_model=config.get('ollama_model', "gemma3:4b"),
            gemini_api_key=config.get('gemini_api_key'),
            gemini_model=config.get('gemini_model', "gemini-1.5-flash"),
            enable_evaluation_tracking=config.get('enable_evaluation_tracking', True),
            enable_caching=config.get('enable_caching', True),
            cache_ttl_seconds=config.get('cache_ttl_seconds', 3600),
            max_retries=config.get('max_retries', 3),
            timeout_seconds=config.get('timeout_seconds', 30),
            confidence_threshold=config.get('confidence_threshold', 0.3)
        )