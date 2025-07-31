"""
Migration Adapter - Bridges legacy CodeGraphManager interface with new GraphService

This adapter allows core_pipeline.py to use the new graph-ops services 
while maintaining the same interface as the legacy CodeGraphManager.
"""
import sys
import os
from typing import Dict, Any, List, Optional
from datetime import datetime

# Add project root to path for imports
sys.path.append(os.path.dirname(os.path.dirname(__file__)))

from graph_ops.config.database_config import DatabaseConfig
from graph_ops.repositories.connection_manager import ConnectionManager
from graph_ops.repositories.neo4j_repository import Neo4jRepository
from graph_ops.services.graph_service import GraphService
from graph_ops.services.export_service import ExportService
from graph_ops.interfaces.graph_repository_interface import GraphNode
from shared.services.service_locator import ServiceLocator


class GraphManagerAdapter:
    """
    Adapter that provides the legacy CodeGraphManager interface
    while using the new graph-ops services underneath
    """
    
    def __init__(self):
        self.logger = ServiceLocator.get_logger("graph_manager_adapter")
        
        # Initialize new services
        self.config = DatabaseConfig.from_environment()
        self.connection_manager = ConnectionManager(
            self.config.uri, 
            self.config.username, 
            self.config.password
        )
        self.repository = Neo4jRepository(self.connection_manager)
        self.graph_service = GraphService(self.repository)
        self.export_service = ExportService(self.connection_manager)
        
        # Initialize connection
        self.connection_manager.initialize()
        
        self.logger.log_info("GraphManagerAdapter initialized with new graph-ops services")
    
    def create_code_entity(self, entity_type: str, name: str, properties: Dict[str, Any]) -> GraphNode:
        """
        Legacy interface: Create a code entity
        Maps to new GraphService.add_entity()
        """
        try:
            # Add legacy metadata for compatibility
            enhanced_properties = properties.copy()
            enhanced_properties.update({
                "name": name,
                "created_at": datetime.now().isoformat(),
                "entity_type": entity_type
            })
            
            node = self.graph_service.add_entity(entity_type, name, enhanced_properties)
            
            # Return object that matches legacy interface expectations
            return LegacyNodeAdapter(node)
            
        except Exception as e:
            self.logger.log_error(f"Failed to create entity {entity_type}:{name}: {e}")
            raise
    
    def create_relationship(self, from_node, to_node, rel_type: str, properties: Optional[Dict] = None):
        """
        Legacy interface: Create a relationship between nodes
        Maps to new GraphService.connect_entities()
        """
        try:
            # Extract node information
            source_name = from_node.name if hasattr(from_node, 'name') else from_node.get('name')
            target_name = to_node.name if hasattr(to_node, 'name') else to_node.get('name')
            
            relationship = self.graph_service.connect_entities(
                source_name, target_name, rel_type, properties
            )
            
            return LegacyRelationshipAdapter(relationship)
            
        except Exception as e:
            self.logger.log_error(f"Failed to create relationship {rel_type}: {e}")
            raise
    
    def find_entity(self, entity_type: str, name: str):
        """
        Legacy interface: Find an entity by type and name
        Maps to new GraphRepository.find_entity()
        """
        try:
            node = self.repository.find_entity(entity_type, name)
            return LegacyNodeAdapter(node) if node else None
            
        except Exception as e:
            self.logger.log_error(f"Failed to find entity {entity_type}:{name}: {e}")
            return None
    
    def get_graph_stats(self) -> Dict[str, Any]:
        """
        Legacy interface: Get graph statistics
        Maps to new GraphRepository.get_graph_stats()
        """
        try:
            return self.repository.get_graph_stats()
            
        except Exception as e:
            self.logger.log_error(f"Failed to get graph stats: {e}")
            return {"error": str(e)}
    
    def clear_graph(self) -> None:
        """
        Legacy interface: Clear the graph
        Maps to new GraphRepository.clear_graph()
        """
        try:
            self.repository.clear_graph()
            self.logger.log_info("Graph cleared via adapter")
            
        except Exception as e:
            self.logger.log_error(f"Failed to clear graph: {e}")
            raise
    
    def export_d3_format(self) -> Dict[str, Any]:
        """
        Legacy interface: Export D3 format
        Maps to new ExportService.export_d3_format()
        """
        try:
            return self.export_service.export_d3_format()
            
        except Exception as e:
            self.logger.log_error(f"Failed to export D3 format: {e}")
            raise
    
    def export_mermaid_format(self) -> str:
        """
        Legacy interface: Export Mermaid format
        Maps to new ExportService.export_mermaid_format()
        """
        try:
            return self.export_service.export_mermaid_format()
            
        except Exception as e:
            self.logger.log_error(f"Failed to export Mermaid format: {e}")
            raise
    
    def export_simple_format(self) -> Dict[str, Any]:
        """
        Legacy interface: Export simple format
        Maps to new ExportService.export_simple_format()
        """
        try:
            return self.export_service.export_simple_format()
            
        except Exception as e:
            self.logger.log_error(f"Failed to export simple format: {e}")
            raise
    
    @property
    def graph(self):
        """
        Legacy interface: Direct graph access
        This is used for raw Cypher queries in core_pipeline.py
        """
        # Return a wrapper that provides the .run() method
        return GraphQueryAdapter(self.connection_manager)
    
    def close(self):
        """Close connections"""
        try:
            self.repository.close()
            self.logger.log_info("GraphManagerAdapter closed")
        except Exception as e:
            self.logger.log_error(f"Error closing adapter: {e}")


class LegacyNodeAdapter:
    """
    Adapter that makes GraphNode look like the legacy py2neo Node
    """
    
    def __init__(self, graph_node: Optional[GraphNode]):
        self._node = graph_node
        
        if graph_node:
            self.identity = int(graph_node.id)
            self._properties = graph_node.properties
            self.name = graph_node.name
        else:
            self.identity = None
            self._properties = {}
            self.name = None
    
    def get(self, key: str, default=None):
        """Get property value (legacy interface)"""
        return self._properties.get(key, default)
    
    def __getitem__(self, key: str):
        """Get property by key (legacy interface)"""
        return self._properties[key]
    
    def __setitem__(self, key: str, value):
        """Set property by key (legacy interface)"""
        self._properties[key] = value
    
    def __contains__(self, key: str):
        """Check if property exists (legacy interface)"""
        return key in self._properties
    
    def __iter__(self):
        """Iterate over properties (legacy interface)"""
        return iter(self._properties)
    
    def items(self):
        """Get property items (legacy interface)"""
        return self._properties.items()


class LegacyRelationshipAdapter:
    """
    Adapter that makes GraphRelationship look like the legacy py2neo Relationship
    """
    
    def __init__(self, graph_relationship):
        self._relationship = graph_relationship
        self._properties = graph_relationship.properties if graph_relationship else {}
    
    def type(self):
        """Get relationship type (legacy interface)"""
        return self._relationship.relationship_type if self._relationship else None
    
    def __getitem__(self, key: str):
        """Get property by key (legacy interface)"""
        return self._properties[key]
    
    def __setitem__(self, key: str, value):
        """Set property by key (legacy interface)"""
        self._properties[key] = value


class GraphQueryAdapter:
    """
    Adapter that provides the legacy .graph.run() interface for raw Cypher queries
    """
    
    def __init__(self, connection_manager: ConnectionManager):
        self.connection_manager = connection_manager
        self.logger = ServiceLocator.get_logger("graph_query_adapter")
    
    def run(self, query: str, **parameters):
        """
        Execute a Cypher query (legacy interface)
        Returns a result that can be converted to list or iterated
        """
        try:
            with self.connection_manager.get_session() as session:
                result = session.run(query, parameters)
                # Convert to list to match legacy behavior
                return QueryResultAdapter(list(result))
                
        except Exception as e:
            self.logger.log_error(f"Query execution failed: {e}")
            raise


class QueryResultAdapter:
    """
    Adapter for query results to match legacy interface
    """
    
    def __init__(self, records: List):
        self.records = records
        self._index = 0
    
    def __iter__(self):
        """Iterate over records"""
        return iter(self.records)
    
    def __getitem__(self, index):
        """Get record by index"""
        return self.records[index]
    
    def data(self):
        """Convert to data format (legacy interface)"""
        return [dict(record) for record in self.records]
    
    def single(self):
        """Get single record (legacy interface)"""
        return self.records[0] if self.records else None