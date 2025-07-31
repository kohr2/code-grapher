"""
Data models for AI providers
"""
from dataclasses import dataclass
from typing import Any, Dict, Optional
from enum import Enum


class AIProviderType(Enum):
    """Supported AI provider types"""
    OLLAMA = "ollama"
    GEMINI = "gemini"
    MOCK = "mock"


@dataclass
class AIRequest:
    """Request to AI provider"""
    prompt: str
    context: Optional[Dict[str, Any]] = None
    max_tokens: Optional[int] = None
    temperature: Optional[float] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class AIResponse:
    """Response from AI provider"""
    content: str
    provider: str
    model: str
    success: bool
    error: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None
    
    def is_success(self) -> bool:
        """Check if response was successful"""
        return self.success and self.error is None


@dataclass
class ProviderConfig:
    """Configuration for AI provider"""
    provider_type: AIProviderType
    base_url: Optional[str] = None
    api_key: Optional[str] = None
    model_name: Optional[str] = None
    timeout: int = 30
    max_retries: int = 3
    config: Optional[Dict[str, Any]] = None