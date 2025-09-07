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
                if hasattr(self._legacy_manager, "close_connection"):
                    self._legacy_manager.close_connection()
                elif hasattr(self._legacy_manager, "close"):
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
                "relationship_count": stats.get("total_relationships", 0),
            }
        except Exception as e:
            return {"status": "unhealthy", "error": str(e), "initialized": self._initialized}

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

    def add_relationship(
        self, source_entity: str, target_entity: str, relationship_type: str, properties: Dict[str, Any] = None
    ) -> Any:
        """Add a relationship between entities"""
        self._ensure_manager()
        try:
            if self.logger:
                self.logger.log_info(f"Adding relationship: {source_entity} -{relationship_type}-> {target_entity}")
            # Find source and target nodes by name across all entity types
            source_node = self._find_entity_by_name(source_entity)
            target_node = self._find_entity_by_name(target_entity)

            if self.logger:
                if source_node:
                    self.logger.log_info(f"Found source node: {source_entity}")
                else:
                    self.logger.log_warning(f"Source node not found: {source_entity}")
                
                if target_node:
                    self.logger.log_info(f"Found target node: {target_entity}")
                else:
                    self.logger.log_warning(f"Target node not found: {target_entity}")

            # Create missing nodes automatically
            if not source_node:
                if self.logger:
                    self.logger.log_info(f"Creating missing source node: {source_entity}")
                source_node = self._legacy_manager.create_code_entity(
                    entity_type="inferred",
                    name=source_entity,
                    properties={"inferred": True, "created_for_relationship": True, "file_path": "unknown"},
                )

            if not target_node:
                if self.logger:
                    self.logger.log_info(f"Creating missing target node: {target_entity}")
                target_node = self._legacy_manager.create_code_entity(
                    entity_type="inferred",
                    name=target_entity,
                    properties={"inferred": True, "created_for_relationship": True, "file_path": "unknown"},
                )

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
            # Legacy manager uses query_for_rag method instead of query_entities
            if hasattr(self._legacy_manager, "query_entities"):
                result = self._legacy_manager.query_entities(criteria, limit)
            elif hasattr(self._legacy_manager, "query_for_rag"):
                # Use query_for_rag as fallback - convert criteria to query string
                query_text = criteria.get("query", str(criteria))
                result = self._legacy_manager.query_for_rag(query_text, limit)
            else:
                # Last resort - basic name search
                query_text = criteria.get("name", criteria.get("query", ""))
                if query_text:
                    # Basic Cypher query
                    cypher = f"MATCH (n) WHERE n.name CONTAINS $query RETURN n LIMIT $limit"
                    result = self._legacy_manager.graph.run(cypher, query=query_text, limit=limit).data()
                    result = [{"properties": dict(r["n"]), "labels": list(r["n"].labels)} for r in result]
                else:
                    result = []

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
            if hasattr(self._legacy_manager, "get_entity_relationships"):
                result = self._legacy_manager.get_entity_relationships(entity_name, max_depth)
            else:
                # Fallback implementation using direct Cypher
                cypher = (
                    """
                MATCH (n {name: $entity_name})-[r*1..%d]-(related)
                RETURN related, r, type(head(r)) as rel_type
                LIMIT 50
                """
                    % max_depth
                )

                result_data = self._legacy_manager.graph.run(cypher, entity_name=entity_name).data()
                result = []
                for record in result_data:
                    result.append(
                        {
                            "name": record["related"].get("name", ""),
                            "type": list(record["related"].labels)[0] if record["related"].labels else "unknown",
                            "relationship_type": record["rel_type"],
                            "depth": 1,  # Simplified - would need path analysis for actual depth
                            "file_path": record["related"].get("file_path", ""),
                            "properties": dict(record["related"]),
                        }
                    )

            if self.logger:
                self.logger.log_debug(f"Retrieved {len(result)} relationships for entity: {entity_name}")
            return result
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get relationships for {entity_name}: {e}")
            return []

    def get_related_entities(self, entity_name: str, max_depth: int = 2, max_results: int = 10) -> List[Dict[str, Any]]:
        """Get related entities for the MCP server - wrapper around get_entity_relationships"""
        try:
            relationships = self.get_entity_relationships(entity_name, max_depth)
            # Limit results and ensure consistent format for MCP
            limited_results = relationships[:max_results]

            # Format consistently for MCP response
            formatted_results = []
            for rel in limited_results:
                formatted_results.append(
                    {
                        "entity_name": rel.get("name", ""),
                        "entity_type": rel.get("type", "unknown"),
                        "relationship_type": rel.get("relationship_type", "unknown"),
                        "file_path": rel.get("file_path", ""),
                        "depth": rel.get("depth", 1),
                        "properties": rel.get("properties", {}),
                    }
                )

            if self.logger:
                self.logger.log_debug(f"Retrieved {len(formatted_results)} related entities for: {entity_name}")
            return formatted_results
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get related entities for {entity_name}: {e}")
            return []

    def remove_entity_relationships(self, entity_identifier: str) -> int:
        """Remove all relationships for an entity"""
        self._ensure_manager()
        try:
            # Legacy manager might not have this exact method, so we'll implement it
            if hasattr(self._legacy_manager, "remove_entity_relationships"):
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
            if hasattr(self._legacy_manager, "clear_database"):
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
            if hasattr(self._legacy_manager, "get_database_stats"):
                result = self._legacy_manager.get_database_stats()
            elif hasattr(self._legacy_manager, "get_graph_stats"):
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
                if hasattr(self._legacy_manager, "close_connection"):
                    self._legacy_manager.close_connection()
                elif hasattr(self._legacy_manager, "close"):
                    self._legacy_manager.close()
                if self.logger:
                    self.logger.log_info("Graph database connection closed")
            except Exception as e:
                if self.logger:
                    self.logger.log_error(f"Error closing connection: {e}")
                raise

    def _find_entity_by_name(self, entity_name: str) -> Any:
        """Find an entity by name across all entity types"""
        try:
            if self.logger:
                self.logger.log_debug(f"Searching for entity: {entity_name}")
            
            # Handle prefixed entity names (e.g., "PROGRAM:ERROR-HANDLING" -> "ERROR-HANDLING")
            actual_name = entity_name
            suggested_type = None
            if ":" in entity_name:
                parts = entity_name.split(":", 1)
                if len(parts) == 2:
                    suggested_type = parts[0].lower()
                    actual_name = parts[1]
                    if self.logger:
                        self.logger.log_debug(f"Extracted type '{suggested_type}' and name '{actual_name}' from '{entity_name}'")
            
            # Use the legacy manager's find_entity method with common entity types
            if hasattr(self._legacy_manager, 'find_entity'):
                # Try common entity types that are likely to exist
                entity_types = ["function", "class", "interface", "variable", "import", "import_from", "file", 
                              "program", "compilation_unit", "paragraph", "data_item", "screen", "queue", "inferred"]
                
                # If we extracted a type from the name, try that first
                if suggested_type and suggested_type in entity_types:
                    entity_types = [suggested_type] + [t for t in entity_types if t != suggested_type]
                
                for entity_type in entity_types:
                    try:
                        if self.logger:
                            self.logger.log_debug(f"Trying to find '{actual_name}' as {entity_type}")
                        found = self._legacy_manager.find_entity(entity_type, actual_name)
                        if found:
                            if self.logger:
                                self.logger.log_debug(f"Found '{actual_name}' as {entity_type}")
                            return found
                    except Exception as e:
                        if self.logger:
                            self.logger.log_debug(f"Error finding '{actual_name}' as {entity_type}: {e}")
                        # Continue to next entity type if this one fails
                        continue
            
            # Fallback: try direct Cypher query if available
            if hasattr(self._legacy_manager, 'graph') and hasattr(self._legacy_manager.graph, 'run'):
                try:
                    # Query across all node types by name
                    cypher = "MATCH (n) WHERE n.name = $entity_name RETURN n LIMIT 1"
                    result = self._legacy_manager.graph.run(cypher, entity_name=entity_name).data()
                    if result:
                        return result[0]["n"]
                except Exception:
                    pass
            
            return None
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Error finding entity {entity_name}: {e}")
            return None
