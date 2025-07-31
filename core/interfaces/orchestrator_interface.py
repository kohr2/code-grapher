"""
Orchestrator Interface

Defines the contract for service orchestration patterns.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Type
from dataclasses import dataclass


@dataclass
class ServiceDependency:
    """Represents a service dependency"""
    interface: Type
    required: bool = True
    config_key: str = None


@dataclass
class OrchestrationConfig:
    """Configuration for service orchestration"""
    max_retries: int = 3
    timeout_seconds: int = 300
    enable_parallel_execution: bool = True
    service_dependencies: List[ServiceDependency] = None


class OrchestratorInterface(ABC):
    """Interface for service orchestration"""
    
    @abstractmethod
    async def initialize_services(self, config: OrchestrationConfig) -> Dict[str, Any]:
        """
        Initialize all required services
        
        Args:
            config: Orchestration configuration
            
        Returns:
            Dictionary of initialized services
        """
        pass
    
    @abstractmethod
    async def coordinate_execution(self, operation: str, **kwargs) -> Dict[str, Any]:
        """
        Coordinate execution across multiple services
        
        Args:
            operation: Operation to coordinate
            **kwargs: Operation parameters
            
        Returns:
            Execution results
        """
        pass
    
    @abstractmethod
    def register_service(self, interface: Type, implementation: Any) -> None:
        """
        Register a service implementation
        
        Args:
            interface: Service interface type
            implementation: Service implementation instance
        """
        pass
    
    @abstractmethod
    def get_service(self, interface: Type) -> Any:
        """
        Get a registered service
        
        Args:
            interface: Service interface type
            
        Returns:
            Service implementation instance
        """
        pass