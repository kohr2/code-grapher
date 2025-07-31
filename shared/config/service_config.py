"""
Service-specific configuration classes
"""
from dataclasses import dataclass
from typing import Any, Dict, Optional


@dataclass
class DatabaseConfig:
    """Database configuration"""
    url: str
    username: str
    password: str
    connection_pool_size: int = 10
    timeout: int = 30
    
    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> 'DatabaseConfig':
        """Create from configuration dictionary"""
        return cls(
            url=config.get('url', 'bolt://localhost:7687'),
            username=config.get('username', 'neo4j'),
            password=config.get('password', 'password'),
            connection_pool_size=config.get('connection_pool_size', 10),
            timeout=config.get('timeout', 30)
        )


@dataclass
class AIConfig:
    """AI service configuration"""
    default_provider: str
    ollama_url: str
    ollama_model: str
    gemini_api_key: Optional[str]
    gemini_model: str
    primer_file_path: Optional[str] = None
    
    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> 'AIConfig':
        """Create from configuration dictionary"""
        return cls(
            default_provider=config.get('default_provider', 'ollama'),
            ollama_url=config.get('ollama', {}).get('url', 'http://localhost:11434'),
            ollama_model=config.get('ollama', {}).get('model', 'gemma3:4b'),
            gemini_api_key=config.get('gemini', {}).get('api_key'),
            gemini_model=config.get('gemini', {}).get('model', 'gemini-1.5-flash'),
            primer_file_path=config.get('primer_file_path')
        )


@dataclass
class LoggingConfig:
    """Logging configuration"""
    level: str
    file: Optional[str]
    format: str
    
    @classmethod
    def from_dict(cls, config: Dict[str, Any]) -> 'LoggingConfig':
        """Create from configuration dictionary"""
        return cls(
            level=config.get('level', 'INFO'),
            file=config.get('file'),
            format=config.get('format', '%(asctime)s - %(name)s - %(levelname)s - %(message)s')
        )


@dataclass
class ServiceConfig:
    """Generic service configuration"""
    name: str
    config: Dict[str, Any]
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get configuration value"""
        return self.config.get(key, default)
    
    def set(self, key: str, value: Any) -> None:
        """Set configuration value"""
        self.config[key] = value