"""
Service locator pattern for dependency management during transition
"""
from typing import Dict, Any, Optional, Type
from shared.interfaces.logger_interface import LoggerInterface
from shared.services.logger_facade import LoggerFactory
from shared.services.service_registry import ServiceRegistry


class ServiceLocator:
    """Service locator for managing dependencies during verticalisation transition"""
    
    _services: Dict[str, Any] = {}
    _logger: Optional[LoggerInterface] = None
    _registry: Optional[ServiceRegistry] = None
    
    @classmethod
    def get_registry(cls) -> ServiceRegistry:
        """Get service registry instance"""
        if cls._registry is None:
            cls._registry = ServiceRegistry()
        return cls._registry
    
    @classmethod
    def get_logger(cls, name: str = "default") -> LoggerInterface:
        """Get logger instance"""
        # Try to get from registry first
        try:
            registry = cls.get_registry()
            if registry.is_registered(LoggerInterface):
                return registry.get(LoggerInterface)
        except Exception:
            pass
        
        # Fall back to factory
        if cls._logger is None:
            cls._logger = LoggerFactory.get_logger(name)
        return cls._logger
    
    @classmethod
    def get_graph_manager(cls):
        """Get graph manager instance - interim solution during transition"""
        # Try to get from registry first (facade pattern)
        try:
            from shared.interfaces.graph_operations_interface import GraphOperationsInterface
            registry = cls.get_registry()
            if registry.is_registered(GraphOperationsInterface):
                return registry.get(GraphOperationsInterface)
        except Exception:
            pass
        
        # Fall back to direct instantiation
        if "graph_manager" not in cls._services:
            # Import here to avoid circular dependencies
            try:
                # Try to use new GraphManagerV2 first
                from graph_manager_v2 import CodeGraphManagerV2
                cls._services["graph_manager"] = CodeGraphManagerV2()
                cls.get_logger("service_locator").log_info("Using GraphManagerV2 with graph-ops services")
            except Exception as e:
                # Fallback to legacy if V2 fails
                cls.get_logger("service_locator").log_warning(f"GraphManagerV2 failed, using legacy: {e}")
                from graph_manager import CodeGraphManager
                cls._services["graph_manager"] = CodeGraphManager()
        return cls._services["graph_manager"]
    
    @classmethod
    def register_service(cls, service_name: str, service_instance: Any) -> None:
        """Register a service instance"""
        cls._services[service_name] = service_instance
    
    @classmethod
    def get_service(cls, service_name: str) -> Any:
        """Get a registered service"""
        if service_name not in cls._services:
            raise ValueError(f"Service {service_name} not registered")
        return cls._services[service_name]
    
    @classmethod
    def clear_services(cls) -> None:
        """Clear all registered services - for testing"""
        cls._services.clear()
        cls._logger = None
        if cls._registry:
            cls._registry.clear_all()
            cls._registry = None
    
    @classmethod
    def register_ai_services(cls, ai_config: Optional[Dict[str, Any]] = None) -> None:
        """Register AI services in the service registry"""
        try:
            registry = cls.get_registry()
            
            # Import AI services here to avoid circular imports
            import sys
            from pathlib import Path
            
            # Add ai-services to Python path for imports
            ai_services_path = str(Path(__file__).parent.parent.parent / "ai-services")
            if ai_services_path not in sys.path:
                sys.path.insert(0, ai_services_path)
            
            # Import using absolute paths from ai-services directory
            sys.path.insert(0, str(Path(ai_services_path) / "interfaces"))
            sys.path.insert(0, str(Path(ai_services_path) / "services"))
            sys.path.insert(0, str(Path(ai_services_path) / "config"))
            sys.path.insert(0, str(Path(ai_services_path) / "models"))
            sys.path.insert(0, str(Path(ai_services_path) / "providers"))
            
            from ai_services_interface import AIServicesInterface
            from ai_service import AIService
            from ai_config import AIServiceConfig
            
            # Create AI service configuration
            if ai_config:
                config = AIServiceConfig.from_dict(ai_config)
            else:
                # Use environment-based configuration
                config = AIServiceConfig()
                
                # Override with environment variables
                import os
                if os.getenv("AI_PROVIDER"):
                    from provider_models import AIProviderType
                    try:
                        config.default_provider = AIProviderType(os.getenv("AI_PROVIDER").lower())
                    except ValueError:
                        pass
                
                if os.getenv("OLLAMA_URL"):
                    config.ollama_url = os.getenv("OLLAMA_URL")
                
                if os.getenv("GEMINI_API_KEY"):
                    config.gemini_api_key = os.getenv("GEMINI_API_KEY")
            
            # Create and register AI service
            logger = cls.get_logger("ai_service")
            ai_service = AIService(config, logger)
            
            registry.register(
                AIServicesInterface,
                ai_service,
                metadata={
                    "version": "1.0",
                    "description": "AI Services vertical slice",
                    "default_provider": config.default_provider.value
                }
            )
            
            # Initialize the service
            ai_service.initialize({})
            
            logger.log_info(f"AI Services registered with default provider: {config.default_provider.value}")
            
        except Exception as e:
            logger = cls.get_logger("service_locator")
            logger.log_error(f"Failed to register AI services: {e}")
            raise