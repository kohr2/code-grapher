"""
Graph operations interface for dependency abstraction
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional, Tuple


class GraphOperationsInterface(ABC):
    """Interface for graph database operations"""
    
    @abstractmethod
    def create_entity_node(self, entity_type: str, name: str, properties: Dict[str, Any]) -> Any:
        """Create an entity node in the graph"""
        pass
    
    @abstractmethod
    def add_relationship(self, source_entity: str, target_entity: str, 
                        relationship_type: str, properties: Dict[str, Any] = None) -> Any:
        """Add a relationship between entities"""
        pass
    
    @abstractmethod
    def query_entities(self, criteria: Dict[str, Any], limit: int = 10) -> List[Any]:
        """Query entities by criteria"""
        pass
    
    @abstractmethod
    def get_entity_relationships(self, entity_name: str, max_depth: int = 2) -> List[Any]:
        """Get relationships for an entity"""
        pass
    
    @abstractmethod
    def remove_entity_relationships(self, entity_identifier: str) -> int:
        """Remove all relationships for an entity"""
        pass
    
    @abstractmethod
    def clear_database(self) -> bool:
        """Clear all data from the database"""
        pass
    
    @abstractmethod
    def get_database_stats(self) -> Dict[str, Any]:
        """Get database statistics"""
        pass
    
    @abstractmethod
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph in D3 format"""
        pass
    
    @abstractmethod
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple format"""
        pass
    
    @abstractmethod
    def close_connection(self) -> None:
        """Close database connection"""
        pass