"""
Repository pattern interfaces for data access
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional


class RepositoryInterface(ABC):
    """Base interface for repository pattern"""
    
    @abstractmethod
    def find_by_id(self, entity_id: str) -> Optional[Any]:
        """Find entity by ID"""
        pass
    
    @abstractmethod
    def find_by_criteria(self, criteria: Dict[str, Any]) -> List[Any]:
        """Find entities by criteria"""
        pass
    
    @abstractmethod
    def save(self, entity: Any) -> Any:
        """Save entity"""
        pass
    
    @abstractmethod
    def delete(self, entity_id: str) -> bool:
        """Delete entity by ID"""
        pass
    
    @abstractmethod
    def exists(self, entity_id: str) -> bool:
        """Check if entity exists"""
        pass


class GraphRepositoryInterface(RepositoryInterface):
    """Interface for graph-specific repository operations"""
    
    @abstractmethod
    def create_node(self, node_type: str, properties: Dict[str, Any]) -> Any:
        """Create a graph node"""
        pass
    
    @abstractmethod
    def create_relationship(self, source_id: str, target_id: str, 
                          relationship_type: str, properties: Dict[str, Any] = None) -> Any:
        """Create a relationship between nodes"""
        pass
    
    @abstractmethod
    def query_graph(self, query: str, parameters: Dict[str, Any] = None) -> List[Any]:
        """Execute graph query"""
        pass
    
    @abstractmethod
    def get_neighbors(self, node_id: str, relationship_types: List[str] = None, 
                     max_depth: int = 1) -> List[Any]:
        """Get neighboring nodes"""
        pass