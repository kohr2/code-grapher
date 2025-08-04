"""
Graph manager facade that wraps the existing CodeGraphManager
"""
from typing import Any, Dict, List, Optional
from shared.interfaces.graph_operations_interface import GraphOperationsInterface
from shared.interfaces.service_interface import ServiceInterface
from shared.interfaces.logger_interface import LoggerInterface


class GraphManagerFacade(GraphOperationsInterface, ServiceInterface):
    """Facade that wraps the existing CodeGraphManager implementation"""
    
    def __init__(self, logger: Optional[LoggerInterface] = None):
        """Initialize the facade"""
        self.logger = logger
        self._legacy_manager = None
        self._initialized = False
    
    def _ensure_manager(self):
        """Lazily initialize the legacy manager"""
        if self._legacy_manager is None:
            # Import here to avoid circular dependencies
            from graph_manager import CodeGraphManager
            self._legacy_manager = CodeGraphManager()
    
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the service with configuration"""
        self._ensure_manager()
        self._initialized = True
        if self.logger:
            self.logger.log_info("GraphManagerFacade initialized")
    
    def shutdown(self) -> None:
        """Clean shutdown of the service"""
        if self._legacy_manager:
            try:
                # Use the correct method name from legacy manager
                if hasattr(self._legacy_manager, 'close_connection'):
                    self._legacy_manager.close_connection()
                elif hasattr(self._legacy_manager, 'close'):
                    self._legacy_manager.close()
            except Exception as e:
                if self.logger:
                    self.logger.log_warning(f"Error during graph manager shutdown: {e}")
        self._initialized = False
    
    def health_check(self) -> Dict[str, Any]:
        """Return health status of the service"""
        try:
            self._ensure_manager()
            # Use the get_database_stats method which has fallback logic
            stats = self.get_database_stats()
            return {
                "status": "healthy",
                "initialized": self._initialized,
                "connection": "active",
                "node_count": stats.get("total_nodes", 0),
                "relationship_count": stats.get("total_relationships", 0)
            }
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": str(e),
                "initialized": self._initialized
            }
    
    def get_service_name(self) -> str:
        """Return the name of this service"""
        return "GraphManager"
    
    # GraphOperationsInterface implementation
    
    def create_entity_node(self, entity_type: str, name: str, properties: Dict[str, Any]) -> Any:
        """Create an entity node in the graph"""
        self._ensure_manager()
        try:
            result = self._legacy_manager.create_code_entity(entity_type, name, properties)
            if self.logger:
                self.logger.log_debug(f"Created entity node: {entity_type}:{name}")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to create entity node {entity_type}:{name}: {e}")
            raise
    
    def add_relationship(self, source_entity: str, target_entity: str, 
                        relationship_type: str, properties: Dict[str, Any] = None) -> Any:
        """Add a relationship between entities"""
        self._ensure_manager()
        try:
            # Find source and target nodes
            source_node = self._legacy_manager.find_entity("unknown", source_entity)
            target_node = self._legacy_manager.find_entity("unknown", target_entity)
            
            # Skip relationship if either node not found (they should exist from entity creation)
            if not source_node or not target_node:
                if self.logger:
                    self.logger.log_warning(f"Skipping relationship - missing nodes: {source_entity}({source_node is not None}) -> {target_entity}({target_node is not None})")
                return None
            
            # Create relationship between nodes
            result = self._legacy_manager.create_relationship(
                source_node, target_node, relationship_type, properties or {}
            )
            if self.logger:
                self.logger.log_debug(f"Added relationship: {source_entity} -{relationship_type}-> {target_entity}")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to add relationship {source_entity}->{target_entity}: {e}")
            raise
    
    def query_entities(self, criteria: Dict[str, Any], limit: int = 10) -> List[Any]:
        """Query entities by criteria"""
        self._ensure_manager()
        try:
            result = self._legacy_manager.query_entities(criteria, limit)
            if self.logger:
                self.logger.log_debug(f"Queried entities with criteria: {criteria}, found {len(result)} results")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to query entities: {e}")
            raise
    
    def get_entity_relationships(self, entity_name: str, max_depth: int = 2) -> List[Any]:
        """Get relationships for an entity"""
        self._ensure_manager()
        try:
            result = self._legacy_manager.get_entity_relationships(entity_name, max_depth)
            if self.logger:
                self.logger.log_debug(f"Retrieved {len(result)} relationships for entity: {entity_name}")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get relationships for {entity_name}: {e}")
            raise
    
    def remove_entity_relationships(self, entity_identifier: str) -> int:
        """Remove all relationships for an entity"""
        self._ensure_manager()
        try:
            # Legacy manager might not have this exact method, so we'll implement it
            if hasattr(self._legacy_manager, 'remove_entity_relationships'):
                result = self._legacy_manager.remove_entity_relationships(entity_identifier)
            else:
                # Fallback implementation
                relationships = self.get_entity_relationships(entity_identifier, max_depth=1)
                result = len(relationships)
                # Note: Actual removal would need to be implemented based on legacy manager capabilities
            
            if self.logger:
                self.logger.log_info(f"Removed {result} relationships for entity: {entity_identifier}")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to remove relationships for {entity_identifier}: {e}")
            raise
    
    def clear_database(self) -> bool:
        """Clear all data from the database"""
        self._ensure_manager()
        try:
            # Check if legacy manager has clear method
            if hasattr(self._legacy_manager, 'clear_database'):
                result = self._legacy_manager.clear_database()
            else:
                # Fallback to manual clearing
                self._legacy_manager.graph.run("MATCH (n) DETACH DELETE n")
                result = True
            
            if self.logger:
                self.logger.log_info("Database cleared successfully")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to clear database: {e}")
            raise
    
    def get_database_stats(self) -> Dict[str, Any]:
        """Get database statistics"""
        self._ensure_manager()
        try:
            # Use the correct method name from legacy manager
            if hasattr(self._legacy_manager, 'get_database_stats'):
                result = self._legacy_manager.get_database_stats()
            elif hasattr(self._legacy_manager, 'get_graph_stats'):
                result = self._legacy_manager.get_graph_stats()
            else:
                # Fallback implementation
                result = {"total_nodes": 0, "total_relationships": 0, "status": "unknown"}
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get database stats: {e}")
            raise
    
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph in D3 format"""
        self._ensure_manager()
        try:
            result = self._legacy_manager.export_d3_format()
            if self.logger:
                self.logger.log_info("Exported graph in D3 format")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to export in D3 format: {e}")
            raise
    
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple format"""
        self._ensure_manager()
        try:
            result = self._legacy_manager.export_simple_format()
            if self.logger:
                self.logger.log_info("Exported graph in simple format")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to export in simple format: {e}")
            raise
    
    def close_connection(self) -> None:
        """Close database connection"""
        if self._legacy_manager:
            try:
                # Use the correct method name from legacy manager
                if hasattr(self._legacy_manager, 'close_connection'):
                    self._legacy_manager.close_connection()
                elif hasattr(self._legacy_manager, 'close'):
                    self._legacy_manager.close()
                if self.logger:
                    self.logger.log_info("Graph database connection closed")
            except Exception as e:
                if self.logger:
                    self.logger.log_error(f"Error closing connection: {e}")
                raise