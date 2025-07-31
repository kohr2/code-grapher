"""
Configuration management interface
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional


class ConfigInterface(ABC):
    """Interface for configuration management"""
    
    @abstractmethod
    def get(self, key: str, default: Any = None) -> Any:
        """Get configuration value by key"""
        pass
    
    @abstractmethod
    def set(self, key: str, value: Any) -> None:
        """Set configuration value"""
        pass
    
    @abstractmethod
    def get_section(self, section: str) -> Dict[str, Any]:
        """Get entire configuration section"""
        pass
    
    @abstractmethod
    def load_from_file(self, file_path: str) -> None:
        """Load configuration from file"""
        pass
    
    @abstractmethod
    def load_from_env(self) -> None:
        """Load configuration from environment variables"""
        pass