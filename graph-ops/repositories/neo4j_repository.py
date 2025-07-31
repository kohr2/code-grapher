"""
Neo4j Repository - Wraps existing CodeGraphManager with repository pattern
"""
import time
from datetime import datetime
from typing import Dict, List, Any, Optional
from py2neo import Graph, Node, Relationship

from ..interfaces.graph_repository_interface import (
    GraphRepositoryInterface, 
    GraphNode, 
    GraphRelationship
)
from .connection_manager import ConnectionManager
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(__file__))))
from graph_manager import CodeGraphManager
from shared.services.service_locator import ServiceLocator


class GraphOperationError(Exception):
    """Exception raised for graph operation errors"""
    pass


class Neo4jRepository(GraphRepositoryInterface):
    """
    Repository implementation wrapping existing CodeGraphManager
    Provides connection pooling and enhanced error handling
    """
    
    def __init__(self, connection_manager: ConnectionManager):
        self.connection_manager = connection_manager
        self.logger = ServiceLocator.get_logger("neo4j_repository")
        
        # Initialize legacy manager for gradual migration
        self._legacy_manager = CodeGraphManager()
        
        # Initialize connection pool
        self.connection_manager.initialize()
        
        self.logger.log_info("Neo4jRepository initialized with legacy wrapper")
    
    def create_entity(self, entity_type: str, name: str, properties: Dict[str, Any]) -> GraphNode:
        """Create a new entity node in the graph"""
        start_time = time.time()
        
        try:
            # Use legacy manager for now while maintaining new interface
            node = self._legacy_manager.create_code_entity(entity_type, name, properties)
            
            # Convert py2neo Node to our GraphNode
            graph_node = GraphNode(
                id=str(node.identity),
                type=entity_type,
                name=name,
                properties=dict(node)
            )
            
            duration = time.time() - start_time
            self.logger.log_debug(f"Created entity {entity_type}:{name} in {duration:.3f}s")
            
            return graph_node
            
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Failed to create entity {entity_type}:{name} after {duration:.3f}s: {e}")
            raise GraphOperationError(f"Entity creation failed: {e}")
    
    def create_relationship(self, source_id: str, target_id: str, 
                          relationship_type: str, properties: Dict[str, Any] = None) -> GraphRelationship:
        """Create a relationship between two nodes"""
        start_time = time.time()
        
        try:
            # For now, we need to work with the legacy interface
            # This is a limitation we'll address when fully migrating away from py2neo
            
            # Find source and target nodes
            with self.connection_manager.get_session() as session:
                source_result = session.run(
                    "MATCH (n) WHERE ID(n) = $id RETURN n", 
                    id=int(source_id)
                ).single()
                
                target_result = session.run(
                    "MATCH (n) WHERE ID(n) = $id RETURN n", 
                    id=int(target_id)
                ).single()
                
                if not source_result or not target_result:
                    raise GraphOperationError("Source or target node not found")
                
                # Create relationship using Cypher
                rel_query = f"""
                MATCH (source) WHERE ID(source) = $source_id
                MATCH (target) WHERE ID(target) = $target_id
                CREATE (source)-[r:{relationship_type}]->(target)
                """
                
                if properties:
                    rel_query += " SET r += $properties"
                    session.run(rel_query, 
                              source_id=int(source_id), 
                              target_id=int(target_id),
                              properties=properties)
                else:
                    session.run(rel_query, 
                              source_id=int(source_id), 
                              target_id=int(target_id))
                
                # Return our GraphRelationship
                graph_relationship = GraphRelationship(
                    source_id=source_id,
                    target_id=target_id,
                    relationship_type=relationship_type,
                    properties=properties or {}
                )
                
                duration = time.time() - start_time
                self.logger.log_debug(f"Created relationship {relationship_type} in {duration:.3f}s")
                
                return graph_relationship
                
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Failed to create relationship {relationship_type} after {duration:.3f}s: {e}")
            raise GraphOperationError(f"Relationship creation failed: {e}")
    
    def find_entity(self, entity_type: str, name: str) -> Optional[GraphNode]:
        """Find an entity by type and name"""
        try:
            # Use legacy manager
            node = self._legacy_manager.find_entity(entity_type, name)
            
            if node is None:
                return None
                
            return GraphNode(
                id=str(node.identity),
                type=entity_type,
                name=name,
                properties=dict(node)
            )
            
        except Exception as e:
            self.logger.log_error(f"Failed to find entity {entity_type}:{name}: {e}")
            raise GraphOperationError(f"Entity search failed: {e}")
    
    def find_entities_by_type(self, entity_type: str, limit: int = 100) -> List[GraphNode]:
        """Find all entities of a specific type"""
        try:
            with self.connection_manager.get_session() as session:
                result = session.run(
                    f"MATCH (n:{entity_type}) RETURN n LIMIT $limit",
                    limit=limit
                )
                
                entities = []
                for record in result:
                    node = record['n']
                    entities.append(GraphNode(
                        id=str(node.id),
                        type=entity_type,
                        name=node.get('name', ''),
                        properties=dict(node)
                    ))
                
                return entities
                
        except Exception as e:
            self.logger.log_error(f"Failed to find entities of type {entity_type}: {e}")
            raise GraphOperationError(f"Entity type search failed: {e}")
    
    def query_entities(self, query: str, parameters: Dict[str, Any] = None) -> List[GraphNode]:
        """Execute a custom query and return entities"""
        try:
            with self.connection_manager.get_session() as session:
                result = session.run(query, parameters or {})
                
                entities = []
                for record in result:
                    # Assume the query returns nodes in a field called 'n'
                    if 'n' in record:
                        node = record['n']
                        entities.append(GraphNode(
                            id=str(node.id),
                            type=list(node.labels)[0] if node.labels else 'Unknown',
                            name=node.get('name', ''),
                            properties=dict(node)
                        ))
                
                return entities
                
        except Exception as e:
            self.logger.log_error(f"Failed to execute query: {e}")
            raise GraphOperationError(f"Query execution failed: {e}")
    
    def get_entity_relationships(self, node_id: str, depth: int = 1) -> List[GraphRelationship]:
        """Get relationships for an entity with specified depth"""
        try:
            with self.connection_manager.get_session() as session:
                query = f"""
                MATCH (n)-[r*1..{depth}]-(m)
                WHERE ID(n) = $node_id
                RETURN r
                """
                
                result = session.run(query, node_id=int(node_id))
                
                relationships = []
                for record in result:
                    # Handle relationship paths
                    rel_path = record['r']
                    if isinstance(rel_path, list):
                        for rel in rel_path:
                            relationships.append(GraphRelationship(
                                source_id=str(rel.start_node.id),
                                target_id=str(rel.end_node.id),
                                relationship_type=rel.type,
                                properties=dict(rel)
                            ))
                    else:
                        relationships.append(GraphRelationship(
                            source_id=str(rel_path.start_node.id),
                            target_id=str(rel_path.end_node.id),
                            relationship_type=rel_path.type,
                            properties=dict(rel_path)
                        ))
                
                return relationships
                
        except Exception as e:
            self.logger.log_error(f"Failed to get relationships for node {node_id}: {e}")
            raise GraphOperationError(f"Relationship query failed: {e}")
    
    def clear_graph(self) -> None:
        """Clear all nodes and relationships from the graph"""
        try:
            # Use legacy manager method
            self._legacy_manager.clear_graph()
            self.logger.log_info("Graph cleared successfully")
            
        except Exception as e:
            self.logger.log_error(f"Failed to clear graph: {e}")
            raise GraphOperationError(f"Graph clearing failed: {e}")
    
    def get_graph_stats(self) -> Dict[str, Any]:
        """Get statistics about the graph"""
        try:
            # Use legacy manager method
            return self._legacy_manager.get_graph_stats()
            
        except Exception as e:
            self.logger.log_error(f"Failed to get graph stats: {e}")
            raise GraphOperationError(f"Graph stats retrieval failed: {e}")
    
    def close(self) -> None:
        """Close database connections"""
        try:
            if hasattr(self._legacy_manager, 'close'):
                self._legacy_manager.close()
            self.connection_manager.close()
            self.logger.log_info("Repository connections closed")
            
        except Exception as e:
            self.logger.log_error(f"Error closing repository: {e}")