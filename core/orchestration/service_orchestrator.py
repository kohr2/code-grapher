"""
Service Orchestrator

Handles service coordination and dependency management for the core orchestration layer.
"""

import asyncio
from typing import Dict, Any, List, Type, Optional
from dataclasses import dataclass

from core.interfaces.orchestrator_interface import OrchestratorInterface, OrchestrationConfig, ServiceDependency
from shared.interfaces.logger_interface import LoggerInterface
from shared.services.service_registry import ServiceRegistry
from shared.exceptions.service_exceptions import ServiceNotRegistered, ServiceInitializationError


@dataclass
class ServiceHealthCheck:
    """Service health check result"""
    service_name: str
    is_healthy: bool
    error_message: Optional[str] = None
    response_time: Optional[float] = None


class ServiceOrchestrator(OrchestratorInterface):
    """
    Orchestrates service initialization and coordination
    """
    
    def __init__(self, 
                 service_registry: ServiceRegistry,
                 logger: LoggerInterface):
        self.service_registry = service_registry
        self.logger = logger
        self._service_health: Dict[str, ServiceHealthCheck] = {}
        
    async def initialize_services(self, config: OrchestrationConfig) -> Dict[str, Any]:
        """
        Initialize all required services based on configuration
        """
        self.logger.log_info("Initializing services...")
        
        try:
            initialization_results = {
                'initialized_services': [],
                'failed_services': [],
                'health_checks': {}
            }
            
            # Initialize services based on dependencies
            if config.service_dependencies:
                for dependency in config.service_dependencies:
                    try:
                        service = await self._initialize_service_dependency(dependency)
                        initialization_results['initialized_services'].append({
                            'interface': dependency.interface.__name__,
                            'required': dependency.required,
                            'status': 'success'
                        })
                        
                    except Exception as e:
                        error_info = {
                            'interface': dependency.interface.__name__,
                            'required': dependency.required,
                            'error': str(e),
                            'status': 'failed'
                        }
                        
                        initialization_results['failed_services'].append(error_info)
                        
                        if dependency.required:
                            raise ServiceInitializationError(
                                f"Required service {dependency.interface.__name__} failed to initialize: {e}"
                            )
                        else:
                            self.logger.log_warning(f"Optional service {dependency.interface.__name__} failed: {e}")
            
            # Perform health checks
            health_results = await self._perform_health_checks()
            initialization_results['health_checks'] = health_results
            
            self.logger.log_info(f"Service initialization complete. {len(initialization_results['initialized_services'])} services initialized.")
            
            return initialization_results
            
        except Exception as e:
            self.logger.log_error(f"Service initialization failed: {e}")
            raise
    
    async def coordinate_execution(self, operation: str, **kwargs) -> Dict[str, Any]:
        """
        Coordinate execution across multiple services
        """
        self.logger.log_info(f"Coordinating operation: {operation}")
        
        start_time = asyncio.get_event_loop().time()
        
        try:
            result = {
                'operation': operation,
                'status': 'success',
                'execution_time': 0.0,
                'services_involved': [],
                'results': {}
            }
            
            # Route operation to appropriate coordination method
            if operation == 'pipeline_execution':
                coordination_result = await self._coordinate_pipeline_execution(**kwargs)
            elif operation == 'graph_update':
                coordination_result = await self._coordinate_graph_update(**kwargs)
            elif operation == 'rag_query':
                coordination_result = await self._coordinate_rag_query(**kwargs)
            else:
                raise ValueError(f"Unknown operation: {operation}")
            
            result['results'] = coordination_result
            result['execution_time'] = asyncio.get_event_loop().time() - start_time
            
            return result
            
        except Exception as e:
            execution_time = asyncio.get_event_loop().time() - start_time
            self.logger.log_error(f"Operation coordination failed: {e}")
            
            return {
                'operation': operation,
                'status': 'error',
                'execution_time': execution_time,
                'error': str(e)
            }
    
    def register_service(self, interface: Type, implementation: Any) -> None:
        """Register a service implementation"""
        try:
            self.service_registry.register(interface, implementation)
            self.logger.log_info(f"Registered service: {interface.__name__}")
        except Exception as e:
            self.logger.log_error(f"Failed to register service {interface.__name__}: {e}")
            raise
    
    def get_service(self, interface: Type) -> Any:
        """Get a registered service"""
        try:
            return self.service_registry.get(interface)
        except ServiceNotRegistered as e:
            self.logger.log_error(f"Service not registered: {interface.__name__}")
            raise
    
    async def _initialize_service_dependency(self, dependency: ServiceDependency) -> Any:
        """Initialize a single service dependency"""
        try:
            # Check if service is already registered
            if self.service_registry.is_registered(dependency.interface):
                service = self.service_registry.get(dependency.interface)
                
                # Initialize service if it has an initialize method
                if hasattr(service, 'initialize'):
                    config = dependency.config_key or {}
                    await service.initialize(config)
                
                return service
            else:
                raise ServiceNotRegistered(f"Service {dependency.interface.__name__} is not registered")
                
        except Exception as e:
            self.logger.log_error(f"Failed to initialize service {dependency.interface.__name__}: {e}")
            raise
    
    async def _perform_health_checks(self) -> Dict[str, ServiceHealthCheck]:
        """Perform health checks on all registered services"""
        health_results = {}
        
        try:
            # Get all registered services
            registered_services = self.service_registry.get_all_registered()
            
            # Perform health checks concurrently
            health_check_tasks = []
            for interface, service in registered_services.items():
                task = self._check_service_health(interface.__name__, service)
                health_check_tasks.append(task)
            
            if health_check_tasks:
                health_checks = await asyncio.gather(*health_check_tasks, return_exceptions=True)
                
                for health_check in health_checks:
                    if isinstance(health_check, ServiceHealthCheck):
                        health_results[health_check.service_name] = health_check
                    elif isinstance(health_check, Exception):
                        self.logger.log_error(f"Health check failed: {health_check}")
            
            return health_results
            
        except Exception as e:
            self.logger.log_error(f"Health check coordination failed: {e}")
            return {}
    
    async def _check_service_health(self, service_name: str, service: Any) -> ServiceHealthCheck:
        """Check health of a single service"""
        start_time = asyncio.get_event_loop().time()
        
        try:
            # Check if service has a health check method
            if hasattr(service, 'health_check'):
                is_healthy = await service.health_check()
            elif hasattr(service, 'is_healthy'):
                is_healthy = service.is_healthy()
            else:
                # Basic check - service exists and is not None
                is_healthy = service is not None
            
            response_time = asyncio.get_event_loop().time() - start_time
            
            return ServiceHealthCheck(
                service_name=service_name,
                is_healthy=is_healthy,
                response_time=response_time
            )
            
        except Exception as e:
            response_time = asyncio.get_event_loop().time() - start_time
            
            return ServiceHealthCheck(
                service_name=service_name,
                is_healthy=False,
                error_message=str(e),
                response_time=response_time
            )
    
    async def _coordinate_pipeline_execution(self, **kwargs) -> Dict[str, Any]:
        """Coordinate pipeline execution across services"""
        try:
            # This would coordinate between AI service, Graph service, and RAG service
            # for pipeline execution
            return {
                'coordination_type': 'pipeline_execution',
                'services_coordinated': ['ai_service', 'graph_service', 'rag_service'],
                'status': 'coordinated'
            }
        except Exception as e:
            self.logger.log_error(f"Pipeline coordination failed: {e}")
            raise
    
    async def _coordinate_graph_update(self, **kwargs) -> Dict[str, Any]:
        """Coordinate graph update operations"""
        try:
            # This would coordinate between Update service and Graph service
            return {
                'coordination_type': 'graph_update',
                'services_coordinated': ['update_service', 'graph_service'],
                'status': 'coordinated'
            }
        except Exception as e:
            self.logger.log_error(f"Graph update coordination failed: {e}")
            raise
    
    async def _coordinate_rag_query(self, **kwargs) -> Dict[str, Any]:
        """Coordinate RAG query operations"""
        try:
            # This would coordinate between RAG service and Graph service
            return {
                'coordination_type': 'rag_query',
                'services_coordinated': ['rag_service', 'graph_service'],
                'status': 'coordinated'
            }
        except Exception as e:
            self.logger.log_error(f"RAG query coordination failed: {e}")
            raise