"""
Dependency Resolver

Handles complex service dependency resolution and initialization ordering.
"""

from typing import Dict, List, Type, Any, Set, Tuple
from dataclasses import dataclass
import asyncio
from enum import Enum

from shared.interfaces.logger_interface import LoggerInterface
from shared.services.service_registry import ServiceRegistry
from shared.exceptions.service_exceptions import CircularDependencyError, ServiceInitializationError


class DependencyStatus(Enum):
    """Status of dependency resolution"""
    PENDING = "pending"
    RESOLVING = "resolving"
    RESOLVED = "resolved"
    FAILED = "failed"


@dataclass
class ServiceDependencyInfo:
    """Information about a service and its dependencies"""
    interface: Type
    implementation: Any
    dependencies: List[Type]
    optional_dependencies: List[Type]
    status: DependencyStatus = DependencyStatus.PENDING
    error: str = None


class DependencyResolver:
    """
    Resolves service dependencies and determines initialization order
    """
    
    def __init__(self, logger: LoggerInterface):
        self.logger = logger
        self._service_info: Dict[Type, ServiceDependencyInfo] = {}
        self._resolution_order: List[Type] = []
        
    def register_service_with_dependencies(self, 
                                         interface: Type, 
                                         implementation: Any,
                                         dependencies: List[Type] = None,
                                         optional_dependencies: List[Type] = None) -> None:
        """
        Register a service with its dependencies
        
        Args:
            interface: Service interface type
            implementation: Service implementation
            dependencies: Required dependencies
            optional_dependencies: Optional dependencies
        """
        self._service_info[interface] = ServiceDependencyInfo(
            interface=interface,
            implementation=implementation,
            dependencies=dependencies or [],
            optional_dependencies=optional_dependencies or []
        )
        
        self.logger.log_info(f"Registered service {interface.__name__} with {len(dependencies or [])} dependencies")
    
    def resolve_dependencies(self) -> List[Type]:
        """
        Resolve all service dependencies and return initialization order
        
        Returns:
            List of service interfaces in dependency-resolved order
            
        Raises:
            CircularDependencyError: If circular dependencies are detected
            ServiceInitializationError: If required dependencies are missing
        """
        self.logger.log_info("Resolving service dependencies...")
        
        try:
            # Check for circular dependencies
            self._detect_circular_dependencies()
            
            # Resolve initialization order using topological sort
            self._resolution_order = self._topological_sort()
            
            self.logger.log_info(f"Dependency resolution complete. Order: {[s.__name__ for s in self._resolution_order]}")
            
            return self._resolution_order
            
        except Exception as e:
            self.logger.log_error(f"Dependency resolution failed: {e}")
            raise
    
    def get_service_dependencies(self, interface: Type) -> Tuple[List[Type], List[Type]]:
        """
        Get dependencies for a specific service
        
        Args:
            interface: Service interface type
            
        Returns:
            Tuple of (required_dependencies, optional_dependencies)
        """
        if interface not in self._service_info:
            return [], []
        
        service_info = self._service_info[interface]
        return service_info.dependencies, service_info.optional_dependencies
    
    def validate_dependencies(self) -> Dict[Type, List[str]]:
        """
        Validate that all dependencies can be satisfied
        
        Returns:
            Dictionary mapping service interfaces to list of validation errors
        """
        validation_errors = {}
        
        for interface, service_info in self._service_info.items():
            errors = []
            
            # Check required dependencies
            for dep in service_info.dependencies:
                if dep not in self._service_info:
                    errors.append(f"Required dependency {dep.__name__} is not registered")
            
            # Optional dependencies don't cause validation failures
            # but we can warn about them
            for dep in service_info.optional_dependencies:
                if dep not in self._service_info:
                    self.logger.log_warning(f"Optional dependency {dep.__name__} for {interface.__name__} is not available")
            
            if errors:
                validation_errors[interface] = errors
        
        return validation_errors
    
    def get_initialization_batches(self) -> List[List[Type]]:
        """
        Get services grouped into initialization batches for parallel initialization
        
        Returns:
            List of batches, where each batch contains services that can be initialized in parallel
        """
        if not self._resolution_order:
            self.resolve_dependencies()
        
        batches = []
        remaining_services = set(self._resolution_order)
        
        while remaining_services:
            # Find services with no unresolved dependencies
            current_batch = []
            
            for service in remaining_services:
                service_info = self._service_info[service]
                unresolved_deps = set(service_info.dependencies) & remaining_services
                
                if not unresolved_deps:
                    current_batch.append(service)
            
            if not current_batch:
                # This shouldn't happen if circular dependencies were properly detected
                raise ServiceInitializationError("Unable to resolve remaining dependencies")
            
            batches.append(current_batch)
            remaining_services -= set(current_batch)
        
        self.logger.log_info(f"Created {len(batches)} initialization batches")
        return batches
    
    def _detect_circular_dependencies(self) -> None:
        """
        Detect circular dependencies using depth-first search
        
        Raises:
            CircularDependencyError: If circular dependencies are found
        """
        visited = set()
        recursion_stack = set()
        
        def dfs(service: Type, path: List[Type]) -> bool:
            if service in recursion_stack:
                cycle_start = path.index(service)
                cycle = path[cycle_start:] + [service]
                cycle_names = [s.__name__ for s in cycle]
                raise CircularDependencyError(f"Circular dependency detected: {' -> '.join(cycle_names)}")
            
            if service in visited:
                return False
            
            visited.add(service)
            recursion_stack.add(service)
            
            if service in self._service_info:
                service_info = self._service_info[service]
                for dep in service_info.dependencies:
                    if dfs(dep, path + [service]):
                        return True
            
            recursion_stack.remove(service)
            return False
        
        for service in self._service_info:
            if service not in visited:
                dfs(service, [])
    
    def _topological_sort(self) -> List[Type]:
        """
        Perform topological sort to determine initialization order
        
        Returns:
            List of services in dependency-resolved order
        """
        # Calculate in-degrees (number of dependencies)
        in_degree = {}
        for service in self._service_info:
            in_degree[service] = 0
        
        for service_info in self._service_info.values():
            for dep in service_info.dependencies:
                if dep in in_degree:
                    in_degree[dep] += 1
        
        # Find services with no dependencies
        queue = [service for service, degree in in_degree.items() if degree == 0]
        result = []
        
        while queue:
            # Sort queue for deterministic ordering
            queue.sort(key=lambda x: x.__name__)
            current = queue.pop(0)
            result.append(current)
            
            # Update in-degrees for dependent services
            if current in self._service_info:
                service_info = self._service_info[current]
                for dep in service_info.dependencies:
                    if dep in in_degree:
                        in_degree[dep] -= 1
                        if in_degree[dep] == 0:
                            queue.append(dep)
        
        # Check if all services were processed
        if len(result) != len(self._service_info):
            unresolved = set(self._service_info.keys()) - set(result)
            unresolved_names = [s.__name__ for s in unresolved]
            raise ServiceInitializationError(f"Unable to resolve dependencies for: {unresolved_names}")
        
        return result
    
    def get_dependency_graph(self) -> Dict[str, Dict[str, Any]]:
        """
        Get a representation of the dependency graph for visualization
        
        Returns:
            Dictionary representing the dependency graph
        """
        graph = {}
        
        for interface, service_info in self._service_info.items():
            graph[interface.__name__] = {
                'dependencies': [dep.__name__ for dep in service_info.dependencies],
                'optional_dependencies': [dep.__name__ for dep in service_info.optional_dependencies],
                'status': service_info.status.value
            }
        
        return graph
    
    def update_service_status(self, interface: Type, status: DependencyStatus, error: str = None) -> None:
        """
        Update the status of a service during resolution
        
        Args:
            interface: Service interface type
            status: New status
            error: Error message if status is FAILED
        """
        if interface in self._service_info:
            self._service_info[interface].status = status
            if error:
                self._service_info[interface].error = error
            
            self.logger.log_info(f"Service {interface.__name__} status updated to {status.value}")
        else:
            self.logger.log_warning(f"Attempted to update status for unregistered service: {interface.__name__}")