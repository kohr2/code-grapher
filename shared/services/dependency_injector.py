"""
Dependency injection container implementation
"""
from typing import Any, Callable, Dict, List, Type, TypeVar, Optional
from functools import wraps
from inspect import signature, Parameter

from shared.services.service_registry import ServiceRegistry
from shared.exceptions.service_exceptions import ServiceDependencyError, ServiceNotRegistered

T = TypeVar('T')


class DependencyInjector:
    """Dependency injection container that automatically resolves dependencies"""
    
    def __init__(self, service_registry: Optional[ServiceRegistry] = None):
        """
        Initialize dependency injector
        
        Args:
            service_registry: Optional service registry, uses singleton if not provided
        """
        self.service_registry = service_registry or ServiceRegistry()
        self._dependency_cache: Dict[Type, Any] = {}
        self._resolution_stack: List[Type] = []
    
    def inject(self, target_class: Type[T]) -> Callable[..., T]:
        """
        Decorator that automatically injects dependencies into constructor
        
        Usage:
            @inject
            class MyService:
                def __init__(self, logger: LoggerInterface, config: ConfigInterface):
                    self.logger = logger
                    self.config = config
        """
        original_init = target_class.__init__
        
        @wraps(original_init)
        def injected_init(self, *args, **kwargs):
            # Get the signature of the original __init__
            sig = signature(original_init)
            bound_args = sig.bind_partial(self, *args, **kwargs)
            
            # Inject missing dependencies
            for param_name, param in sig.parameters.items():
                if param_name == 'self':
                    continue
                    
                # Skip if already provided
                if param_name in bound_args.arguments:
                    continue
                
                # Try to resolve dependency
                if param.annotation != Parameter.empty:
                    try:
                        dependency = self.resolve(param.annotation)
                        bound_args.arguments[param_name] = dependency
                    except ServiceNotRegistered:
                        # If dependency not registered and no default, raise error
                        if param.default == Parameter.empty:
                            raise ServiceDependencyError(
                                f"Cannot resolve dependency {param.annotation.__name__} "
                                f"for {target_class.__name__}.{param_name}"
                            )
            
            # Call original __init__ with resolved dependencies
            original_init(**bound_args.arguments)
        
        # Replace __init__ with injected version
        target_class.__init__ = injected_init
        return target_class
    
    def resolve(self, interface: Type[T]) -> T:
        """
        Resolve a dependency by interface type
        
        Args:
            interface: The interface type to resolve
            
        Returns:
            Instance of the service implementing the interface
            
        Raises:
            ServiceNotRegistered: If service is not registered
            ServiceDependencyError: If circular dependency detected
        """
        # Check for circular dependencies
        if interface in self._resolution_stack:
            cycle = ' -> '.join([t.__name__ for t in self._resolution_stack] + [interface.__name__])
            raise ServiceDependencyError(f"Circular dependency detected: {cycle}")
        
        # Check cache first
        if interface in self._dependency_cache:
            return self._dependency_cache[interface]
        
        # Add to resolution stack
        self._resolution_stack.append(interface)
        
        try:
            # Resolve from service registry
            service = self.service_registry.get(interface)
            
            # Cache the resolved service
            self._dependency_cache[interface] = service
            
            return service
            
        finally:
            # Remove from resolution stack
            self._resolution_stack.remove(interface)
    
    def create_instance(self, target_class: Type[T], **override_dependencies) -> T:
        """
        Create an instance with automatic dependency injection
        
        Args:
            target_class: The class to instantiate
            **override_dependencies: Manual dependency overrides
            
        Returns:
            Instance of target_class with dependencies injected
        """
        # Get constructor signature
        sig = signature(target_class.__init__)
        dependencies = {}
        
        for param_name, param in sig.parameters.items():
            if param_name == 'self':
                continue
            
            # Use override if provided
            if param_name in override_dependencies:
                dependencies[param_name] = override_dependencies[param_name]
                continue
            
            # Try to resolve dependency
            if param.annotation != Parameter.empty:
                try:
                    dependency = self.resolve(param.annotation)
                    dependencies[param_name] = dependency
                except ServiceNotRegistered:
                    # If dependency not registered and no default, raise error
                    if param.default == Parameter.empty:
                        raise ServiceDependencyError(
                            f"Cannot resolve dependency {param.annotation.__name__} "
                            f"for {target_class.__name__}.{param_name}"
                        )
        
        return target_class(**dependencies)
    
    def clear_cache(self) -> None:
        """Clear dependency resolution cache"""
        self._dependency_cache.clear()
    
    def get_dependency_graph(self) -> Dict[str, List[str]]:
        """Get dependency graph for debugging"""
        graph = {}
        
        for interface in self.service_registry.get_registered_services():
            dependencies = []
            
            try:
                service = self.service_registry.get(interface)
                if hasattr(service, '__init__'):
                    sig = signature(service.__init__)
                    for param_name, param in sig.parameters.items():
                        if param_name != 'self' and param.annotation != Parameter.empty:
                            dependencies.append(param.annotation.__name__)
            except Exception:
                pass  # Ignore errors during graph generation
            
            graph[interface.__name__] = dependencies
        
        return graph


# Global dependency injector instance
_global_injector: Optional[DependencyInjector] = None


def get_injector() -> DependencyInjector:
    """Get global dependency injector instance"""
    global _global_injector
    if _global_injector is None:
        _global_injector = DependencyInjector()
    return _global_injector


def inject(target_class: Type[T]) -> Type[T]:
    """Decorator for automatic dependency injection"""
    return get_injector().inject(target_class)


def resolve(interface: Type[T]) -> T:
    """Resolve a dependency using global injector"""
    return get_injector().resolve(interface)