"""
Logger interface for dependency injection
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional


class LoggerInterface(ABC):
    """Abstract interface for logging services"""
    
    @abstractmethod
    def log_info(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log info level message"""
        pass
    
    @abstractmethod
    def log_warning(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log warning level message"""
        pass
    
    @abstractmethod
    def log_error(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log error level message"""
        pass
    
    @abstractmethod
    def log_debug(self, message: str, extra: Optional[Dict[str, Any]] = None) -> None:
        """Log debug level message"""
        pass
    
    @abstractmethod
    def create_session_logger(self, session_name: str) -> 'LoggerInterface':
        """Create a session-specific logger"""
        pass
    
    @abstractmethod
    def log_operation_start(self, operation: str, context: Dict[str, Any]) -> None:
        """Log operation start"""
        pass
    
    @abstractmethod
    def log_operation_end(self, operation: str, context: Dict[str, Any]) -> None:
        """Log operation end"""
        pass
    
    @abstractmethod
    def log_performance(self, operation: str, duration: float, context: Optional[Dict[str, Any]] = None) -> None:
        """Log performance metrics"""
        pass