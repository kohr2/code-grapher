"""
Base provider implementation with common functionality
"""
from abc import ABC
from typing import Any, Dict, List, Optional
from ..interfaces.ai_provider_interface import AIProviderInterface
from ..models.provider_models import AIResponse, ProviderConfig
from shared.interfaces.logger_interface import LoggerInterface


class BaseAIProvider(AIProviderInterface, ABC):
    """Base implementation for AI providers"""
    
    def __init__(self, config: ProviderConfig, logger: Optional[LoggerInterface] = None):
        """Initialize base provider"""
        self.config = config
        self.logger = logger
        self._initialized = False
        self._client = None
    
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the provider with configuration"""
        # Update config with runtime parameters
        for key, value in config.items():
            if hasattr(self.config, key):
                setattr(self.config, key, value)
        
        self._initialize_client()
        self._initialized = True
        
        if self.logger:
            self.logger.log_info(f"Initialized {self.get_provider_name()} provider")
    
    def _initialize_client(self) -> None:
        """Initialize the underlying client - implemented by subclasses"""
        pass
    
    def is_available(self) -> bool:
        """Check if the provider is available"""
        try:
            return self._initialized and self._client is not None and self._test_connection()
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Provider availability check failed: {e}")
            return False
    
    def _test_connection(self) -> bool:
        """Test connection to provider - implemented by subclasses"""
        return True
    
    def _create_response(self, content: str, success: bool = True, 
                        error: Optional[str] = None, metadata: Optional[Dict[str, Any]] = None) -> AIResponse:
        """Create standardized AI response"""
        return AIResponse(
            content=content,
            provider=self.get_provider_name(),
            model=self.config.model_name or "unknown",
            success=success,
            error=error,
            metadata=metadata or {}
        )
    
    def _log_request(self, operation: str, details: Dict[str, Any]) -> None:
        """Log AI request for tracking"""
        if self.logger:
            self.logger.log_debug(f"AI Request - {operation}: {details}")
    
    def _log_response(self, operation: str, response: AIResponse, duration: float) -> None:
        """Log AI response for tracking"""
        if self.logger:
            status = "SUCCESS" if response.success else "FAILED"
            self.logger.log_info(f"AI Response - {operation}: {status} in {duration:.2f}s")
    
    def _handle_error(self, operation: str, error: Exception) -> AIResponse:
        """Handle and log errors consistently"""
        error_msg = str(error)
        if self.logger:
            self.logger.log_error(f"AI Provider error in {operation}: {error_msg}")
        
        return self._create_response(
            content="",
            success=False,
            error=error_msg
        )
    
    def shutdown(self) -> None:
        """Clean shutdown of the provider"""
        try:
            if self._client and hasattr(self._client, 'close'):
                self._client.close()
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Error during provider shutdown: {e}")
        finally:
            self._initialized = False
            self._client = None