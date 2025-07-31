"""
Graph Service Interface - Business operations layer
"""
from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from .graph_repository_interface import GraphNode, GraphRelationship


class GraphOperationsInterface(ABC):
    """Business operations interface for graph management"""
    
    @abstractmethod
    def build_graph(self, entities: List[Dict], relationships: List[Dict]) -> Dict[str, Any]:
        """Build graph from entities and relationships"""
        pass
    
    @abstractmethod
    def add_entity(self, entity_type: str, name: str, properties: Dict[str, Any]) -> GraphNode:
        """Add an entity to the graph with business logic"""
        pass
    
    @abstractmethod
    def connect_entities(self, source_name: str, target_name: str, 
                        relationship_type: str, properties: Dict[str, Any] = None) -> GraphRelationship:
        """Connect two entities with a relationship"""
        pass
    
    @abstractmethod
    def find_related_entities(self, entity_name: str, depth: int = 2) -> List[GraphNode]:
        """Find entities related to the given entity"""
        pass
    
    @abstractmethod
    def search_entities(self, search_term: str, entity_types: List[str] = None) -> List[GraphNode]:
        """Search for entities by name or properties"""
        pass
    
    @abstractmethod
    def get_entity_context(self, entity_name: str, context_depth: int = 1) -> Dict[str, Any]:
        """Get contextual information about an entity"""
        pass
    
    @abstractmethod
    def validate_graph_integrity(self) -> Dict[str, Any]:
        """Validate graph integrity and return report"""
        pass
    
    @abstractmethod
    def get_graph_summary(self) -> Dict[str, Any]:
        """Get summary statistics of the graph"""
        pass