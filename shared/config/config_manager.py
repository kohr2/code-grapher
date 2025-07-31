"""
Configuration manager implementation
"""
import os
import json
from pathlib import Path
from typing import Any, Dict, Optional
from dotenv import load_dotenv

from shared.interfaces.config_interface import ConfigInterface
from shared.exceptions.infrastructure_exceptions import ConfigurationError


class ConfigManager(ConfigInterface):
    """Configuration manager that handles environment and file-based configuration"""
    
    def __init__(self, config_file: Optional[str] = None):
        """
        Initialize configuration manager
        
        Args:
            config_file: Optional path to configuration file
        """
        self._config: Dict[str, Any] = {}
        self._env_loaded = False
        
        # Load environment variables
        self.load_from_env()
        
        # Load from file if provided
        if config_file:
            self.load_from_file(config_file)
            
        # Set up default configurations
        self._setup_defaults()
    
    def get(self, key: str, default: Any = None) -> Any:
        """Get configuration value by key, supports dot notation"""
        keys = key.split('.')
        value = self._config
        
        try:
            for k in keys:
                value = value[k]
            return value
        except (KeyError, TypeError):
            return default
    
    def set(self, key: str, value: Any) -> None:
        """Set configuration value, supports dot notation"""
        keys = key.split('.')
        config = self._config
        
        # Navigate to the correct nested dict
        for k in keys[:-1]:
            if k not in config:
                config[k] = {}
            config = config[k]
        
        # Set the final value
        config[keys[-1]] = value
    
    def get_section(self, section: str) -> Dict[str, Any]:
        """Get entire configuration section"""
        return self.get(section, {})
    
    def load_from_file(self, file_path: str) -> None:
        """Load configuration from JSON file"""
        try:
            path = Path(file_path)
            if not path.exists():
                raise ConfigurationError(f"Configuration file not found: {file_path}")
            
            with open(path, 'r') as f:
                file_config = json.load(f)
            
            # Merge with existing config
            self._deep_merge(self._config, file_config)
            
        except json.JSONDecodeError as e:
            raise ConfigurationError(f"Invalid JSON in configuration file {file_path}: {e}")
        except Exception as e:
            raise ConfigurationError(f"Failed to load configuration from {file_path}: {e}")
    
    def load_from_env(self) -> None:
        """Load configuration from environment variables"""
        if self._env_loaded:
            return
            
        # Load .env file if it exists
        load_dotenv()
        
        # Map environment variables to configuration
        env_mappings = {
            'NEO4J_URL': 'database.neo4j.url',
            'NEO4J_USERNAME': 'database.neo4j.username', 
            'NEO4J_PASSWORD': 'database.neo4j.password',
            'OLLAMA_URL': 'ai.ollama.url',
            'GEMINI_API_KEY': 'ai.gemini.api_key',
            'AI_PROVIDER': 'ai.default_provider',
            'PRIMER_FILE_PATH': 'ai.primer_file_path',
            'LOG_LEVEL': 'logging.level',
            'LOG_FILE': 'logging.file',
        }
        
        for env_var, config_key in env_mappings.items():
            value = os.getenv(env_var)
            if value is not None:
                self.set(config_key, value)
        
        self._env_loaded = True
    
    def _setup_defaults(self) -> None:
        """Set up default configuration values"""
        defaults = {
            'database': {
                'neo4j': {
                    'url': 'bolt://localhost:7687',
                    'username': 'neo4j',
                    'password': 'password'
                }
            },
            'ai': {
                'default_provider': 'ollama',
                'ollama': {
                    'url': 'http://localhost:11434',
                    'model': 'gemma3:4b'
                },
                'gemini': {
                    'model': 'gemini-1.5-flash'
                },
                'primer_file_path': None
            },
            'logging': {
                'level': 'INFO',
                'file': None,
                'format': '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
            },
            'services': {
                'graph_manager': {
                    'connection_pool_size': 10,
                    'timeout': 30
                },
                'rag_pipeline': {
                    'embedding_model': 'all-MiniLM-L6-v2',
                    'max_results': 10
                }
            }
        }
        
        # Only set defaults for keys that don't exist
        for key, value in defaults.items():
            if key not in self._config:
                self._config[key] = value
            elif isinstance(value, dict) and isinstance(self._config[key], dict):
                self._deep_merge_defaults(self._config[key], value)
    
    def _deep_merge(self, target: Dict[str, Any], source: Dict[str, Any]) -> None:
        """Deep merge source dict into target dict"""
        for key, value in source.items():
            if key in target and isinstance(target[key], dict) and isinstance(value, dict):
                self._deep_merge(target[key], value)
            else:
                target[key] = value
    
    def _deep_merge_defaults(self, target: Dict[str, Any], defaults: Dict[str, Any]) -> None:
        """Deep merge defaults, only setting values that don't exist"""
        for key, value in defaults.items():
            if key not in target:
                target[key] = value
            elif isinstance(target[key], dict) and isinstance(value, dict):
                self._deep_merge_defaults(target[key], value)
    
    def get_database_config(self) -> Dict[str, Any]:
        """Get database configuration"""
        return self.get_section('database')
    
    def get_ai_config(self) -> Dict[str, Any]:
        """Get AI configuration"""
        return self.get_section('ai') 
    
    def get_logging_config(self) -> Dict[str, Any]:
        """Get logging configuration"""
        return self.get_section('logging')
    
    def get_service_config(self, service_name: str) -> Dict[str, Any]:
        """Get configuration for a specific service"""
        return self.get(f'services.{service_name}', {})
    
    def to_dict(self) -> Dict[str, Any]:
        """Get all configuration as dictionary"""
        return self._config.copy()
    
    def __str__(self) -> str:
        """String representation of configuration (excluding sensitive data)"""
        safe_config = self._config.copy()
        
        # Remove sensitive fields
        sensitive_keys = ['password', 'api_key', 'secret', 'token']
        self._redact_sensitive_data(safe_config, sensitive_keys)
        
        return json.dumps(safe_config, indent=2)
    
    def _redact_sensitive_data(self, config: Dict[str, Any], sensitive_keys: list) -> None:
        """Recursively redact sensitive data from configuration"""
        for key, value in config.items():
            if isinstance(value, dict):
                self._redact_sensitive_data(value, sensitive_keys)
            elif any(sensitive_key in key.lower() for sensitive_key in sensitive_keys):
                config[key] = '***REDACTED***'