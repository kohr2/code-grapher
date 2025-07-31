"""
Service registry implementation for dependency injection and service discovery
"""
from typing import Dict, Type, Any, Optional, List
from shared.exceptions.service_exceptions import ServiceNotRegistered, ServiceInitializationError
from shared.interfaces.service_interface import ServiceInterface
from shared.interfaces.logger_interface import LoggerInterface


class ServiceRegistry:
    """Registry for managing service instances and dependencies"""
    
    _instance: Optional['ServiceRegistry'] = None
    _services: Dict[Type, Any] = {}
    _service_metadata: Dict[Type, Dict[str, Any]] = {}
    _initialization_order: List[Type] = []
    
    def __new__(cls) -> 'ServiceRegistry':
        """Singleton pattern implementation"""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
    
    @classmethod
    def register(cls, interface: Type, implementation: Any, 
                metadata: Optional[Dict[str, Any]] = None) -> None:
        """
        Register a service implementation for an interface
        
        Args:
            interface: The interface type (e.g., LoggerInterface)
            implementation: The concrete implementation or factory function
            metadata: Optional metadata about the service
        """
        if cls._instance is None:
            cls._instance = cls()
            
        cls._services[interface] = implementation
        cls._service_metadata[interface] = metadata or {}
        
        # Track initialization order
        if interface not in cls._initialization_order:
            cls._initialization_order.append(interface)
    
    @classmethod
    def get(cls, interface: Type) -> Any:
        """
        Get a service instance by interface
        
        Args:
            interface: The interface type to retrieve
            
        Returns:
            The service instance
            
        Raises:
            ServiceNotRegistered: If the service is not registered
        """
        if cls._instance is None:
            cls._instance = cls()
            
        if interface not in cls._services:
            raise ServiceNotRegistered(f"Service {interface.__name__} not registered")
        
        service = cls._services[interface]
        
        # If it's a factory function, call it
        if callable(service) and not isinstance(service, type):
            try:
                return service()
            except Exception as e:
                raise ServiceInitializationError(f"Failed to initialize {interface.__name__}: {e}")
        
        # If it's a class, instantiate it
        if isinstance(service, type):
            try:
                instance = service()
                # Cache the instance for future use
                cls._services[interface] = instance
                return instance
            except Exception as e:
                raise ServiceInitializationError(f"Failed to instantiate {interface.__name__}: {e}")
        
        # Already an instance
        return service
    
    @classmethod
    def is_registered(cls, interface: Type) -> bool:
        """Check if a service is registered"""
        if cls._instance is None:
            return False
        return interface in cls._services
    
    @classmethod
    def get_registered_services(cls) -> List[Type]:
        """Get list of all registered service interfaces"""
        if cls._instance is None:
            return []
        return list(cls._services.keys())
    
    @classmethod
    def get_all_registered(cls) -> Dict[Type, Any]:
        """Get all registered services as interface -> service mapping"""
        if cls._instance is None:
            return {}
        return cls._services.copy()
    
    @classmethod
    def get_service_metadata(cls, interface: Type) -> Dict[str, Any]:
        """Get metadata for a service"""
        if cls._instance is None:
            return {}
        return cls._service_metadata.get(interface, {})
    
    @classmethod
    def unregister(cls, interface: Type) -> bool:
        """
        Unregister a service
        
        Args:
            interface: The interface type to unregister
            
        Returns:
            True if service was unregistered, False if it wasn't registered
        """
        if cls._instance is None or interface not in cls._services:
            return False
        
        # Shutdown service if it implements ServiceInterface
        service = cls._services[interface]
        if isinstance(service, ServiceInterface):
            try:
                service.shutdown()
            except Exception:
                pass  # Ignore shutdown errors during unregistration
        
        del cls._services[interface]
        if interface in cls._service_metadata:
            del cls._service_metadata[interface]
        if interface in cls._initialization_order:
            cls._initialization_order.remove(interface)
        
        return True
    
    @classmethod
    def clear_all(cls) -> None:
        """Clear all registered services - mainly for testing"""
        if cls._instance is None:
            return
        
        # Shutdown all services that implement ServiceInterface
        for service in cls._services.values():
            if isinstance(service, ServiceInterface):
                try:
                    service.shutdown()
                except Exception:
                    pass  # Ignore shutdown errors
        
        cls._services.clear()
        cls._service_metadata.clear()
        cls._initialization_order.clear()
    
    @classmethod
    def initialize_all_services(cls, config: Dict[str, Any]) -> None:
        """Initialize all registered services in order"""
        if cls._instance is None:
            return
        
        for interface in cls._initialization_order:
            try:
                service = cls.get(interface)
                if isinstance(service, ServiceInterface):
                    service_config = config.get(interface.__name__, {})
                    service.initialize(service_config)
            except Exception as e:
                raise ServiceInitializationError(f"Failed to initialize {interface.__name__}: {e}")
    
    @classmethod
    def health_check_all(cls) -> Dict[str, Dict[str, Any]]:
        """Run health checks on all services"""
        if cls._instance is None:
            return {}
        
        health_status = {}
        for interface in cls._services:
            try:
                service = cls.get(interface)
                if isinstance(service, ServiceInterface):
                    health_status[interface.__name__] = service.health_check()
                else:
                    health_status[interface.__name__] = {"status": "unknown", "message": "Service doesn't implement ServiceInterface"}
            except Exception as e:
                health_status[interface.__name__] = {"status": "error", "message": str(e)}
        
        return health_status