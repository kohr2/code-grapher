"""
AI service configuration
"""
import os
from dataclasses import dataclass
from typing import Dict, Any, Optional
from models.provider_models import AIProviderType


@dataclass
class AIServiceConfig:
    """Configuration for AI services"""
    default_provider: AIProviderType = None  # Will be set from env or config
    ollama_url: str = "http://localhost:11434"
    ollama_model: str = "tinyllama:latest"
    gemini_api_key: Optional[str] = None
    gemini_model: str = "gemini-1.5-flash"
    openai_api_key: Optional[str] = None
    openai_model: str = "gpt-4o-mini"
    openai_base_url: Optional[str] = None  # For custom OpenAI-compatible endpoints
    enable_evaluation_tracking: bool = True
    enable_caching: bool = True
    cache_ttl_seconds: int = 3600
    max_retries: int = 3
    timeout_seconds: int = 120
    confidence_threshold: float = 0.3
    
    def __post_init__(self):
        """Load configuration from environment variables after initialization"""
        # Load OpenAI API key from environment
        if not self.openai_api_key:
            self.openai_api_key = os.getenv('OPENAI_API_KEY')
        
        # Load Gemini API key from environment
        if not self.gemini_api_key:
            self.gemini_api_key = os.getenv('GEMINI_API_KEY')
        
        # Set default provider from environment or use OpenAI if key is available
        if self.default_provider is None:
            provider_str = os.getenv('AI_PROVIDER', 'openai' if self.openai_api_key else 'ollama')
            try:
                self.default_provider = AIProviderType(provider_str.lower())
            except ValueError:
                # Fallback to OpenAI if available, otherwise Ollama
                self.default_provider = AIProviderType.OPENAI if self.openai_api_key else AIProviderType.OLLAMA
    
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
            openai_api_key=config.get('openai_api_key'),
            openai_model=config.get('openai_model', "gpt-4o-mini"),
            openai_base_url=config.get('openai_base_url'),
            enable_evaluation_tracking=config.get('enable_evaluation_tracking', True),
            enable_caching=config.get('enable_caching', True),
            cache_ttl_seconds=config.get('cache_ttl_seconds', 3600),
            max_retries=config.get('max_retries', 3),
            timeout_seconds=config.get('timeout_seconds', 30),
            confidence_threshold=config.get('confidence_threshold', 0.3)
        )