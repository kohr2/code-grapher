"""
Graph Repository Interface - Repository pattern for graph operations
"""
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from dataclasses import dataclass


@dataclass
class GraphNode:
    """Represents a graph node"""
    id: str
    type: str
    name: str
    properties: Dict[str, Any]


@dataclass
class GraphRelationship:
    """Represents a graph relationship"""
    source_id: str
    target_id: str
    relationship_type: str
    properties: Dict[str, Any] = None


class GraphRepositoryInterface(ABC):
    """Repository interface for graph database operations"""
    
    @abstractmethod
    def create_entity(self, entity_type: str, name: str, properties: Dict[str, Any]) -> GraphNode:
        """Create a new entity node in the graph"""
        pass
    
    @abstractmethod
    def create_relationship(self, source_id: str, target_id: str, 
                          relationship_type: str, properties: Dict[str, Any] = None) -> GraphRelationship:
        """Create a relationship between two nodes"""
        pass
    
    @abstractmethod
    def find_entity(self, entity_type: str, name: str) -> Optional[GraphNode]:
        """Find an entity by type and name"""
        pass
    
    @abstractmethod
    def find_entities_by_type(self, entity_type: str, limit: int = 100) -> List[GraphNode]:
        """Find all entities of a specific type"""
        pass
    
    @abstractmethod
    def query_entities(self, query: str, parameters: Dict[str, Any] = None) -> List[GraphNode]:
        """Execute a custom query and return entities"""
        pass
    
    @abstractmethod
    def get_entity_relationships(self, node_id: str, depth: int = 1) -> List[GraphRelationship]:
        """Get relationships for an entity with specified depth"""
        pass
    
    @abstractmethod
    def clear_graph(self) -> None:
        """Clear all nodes and relationships from the graph"""
        pass
    
    @abstractmethod
    def get_graph_stats(self) -> Dict[str, Any]:
        """Get statistics about the graph"""
        pass
    
    @abstractmethod
    def close(self) -> None:
        """Close database connections"""
        pass