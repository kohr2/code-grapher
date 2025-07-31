"""
Base service interface for all services
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional


class ServiceInterface(ABC):
    """Base interface for all services in the system"""
    
    @abstractmethod
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the service with configuration"""
        pass
    
    @abstractmethod
    def shutdown(self) -> None:
        """Clean shutdown of the service"""
        pass
    
    @abstractmethod
    def health_check(self) -> Dict[str, Any]:
        """Return health status of the service"""
        pass
    
    @abstractmethod
    def get_service_name(self) -> str:
        """Return the name of this service"""
        pass