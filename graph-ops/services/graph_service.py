"""
Graph Service - Main business operations for graph management
"""
import time
from typing import Dict, List, Any, Optional

from ..interfaces.graph_service_interface import GraphOperationsInterface
from ..interfaces.graph_repository_interface import GraphRepositoryInterface, GraphNode, GraphRelationship
from shared.services.service_locator import ServiceLocator


class GraphService(GraphOperationsInterface):
    """
    Main graph service implementing business operations
    Uses repository pattern for data access
    """
    
    def __init__(self, repository: GraphRepositoryInterface):
        self.repository = repository
        self.logger = ServiceLocator.get_logger("graph_service")
        
    def build_graph(self, entities: List[Dict], relationships: List[Dict]) -> Dict[str, Any]:
        """Build graph from entities and relationships"""
        start_time = time.time()
        
        try:
            # Track created entities for relationship building
            entity_map = {}
            created_nodes = []
            created_relationships = []
            
            # Create entities first
            for entity_data in entities:
                entity_type = entity_data.get('type', 'Unknown')
                entity_name = entity_data.get('name', 'Unnamed')
                properties = entity_data.get('properties', {})
                
                node = self.repository.create_entity(entity_type, entity_name, properties)
                entity_map[entity_name] = node
                created_nodes.append(node)
            
            # Create relationships
            for rel_data in relationships:
                source_name = rel_data.get('source')
                target_name = rel_data.get('target')
                rel_type = rel_data.get('type', 'RELATED_TO')
                properties = rel_data.get('properties', {})
                
                if source_name in entity_map and target_name in entity_map:
                    source_node = entity_map[source_name]
                    target_node = entity_map[target_name]
                    
                    relationship = self.repository.create_relationship(
                        source_node.id, target_node.id, rel_type, properties
                    )
                    created_relationships.append(relationship)
                else:
                    self.logger.log_warning(f"Skipping relationship {source_name} -> {target_name}: entities not found")
            
            duration = time.time() - start_time
            
            result = {
                "success": True,
                "nodes_created": len(created_nodes),
                "relationships_created": len(created_relationships),
                "duration": duration,
                "entities": created_nodes,
                "relationships": created_relationships
            }
            
            self.logger.log_info(f"Built graph with {len(created_nodes)} nodes and {len(created_relationships)} relationships in {duration:.2f}s")
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Graph building failed after {duration:.3f}s: {e}")
            return {
                "success": False,
                "error": str(e),
                "duration": duration,
                "nodes_created": 0,
                "relationships_created": 0
            }
    
    def add_entity(self, entity_type: str, name: str, properties: Dict[str, Any]) -> GraphNode:
        """Add an entity to the graph with business logic"""
        try:
            # Check if entity already exists
            existing = self.repository.find_entity(entity_type, name)
            if existing:
                self.logger.log_debug(f"Entity {entity_type}:{name} already exists, returning existing")
                return existing
            
            # Create new entity
            node = self.repository.create_entity(entity_type, name, properties)
            self.logger.log_debug(f"Created new entity {entity_type}:{name}")
            
            return node
            
        except Exception as e:
            self.logger.log_error(f"Failed to add entity {entity_type}:{name}: {e}")
            raise
    
    def connect_entities(self, source_name: str, target_name: str, 
                        relationship_type: str, properties: Dict[str, Any] = None) -> GraphRelationship:
        """Connect two entities with a relationship"""
        try:
            # Find source and target entities by name
            # Note: This assumes we can find by name across all types
            # In a real implementation, you might need type information too
            
            with self.repository.connection_manager.get_session() as session:
                # Find source entity
                source_query = "MATCH (n {name: $name}) RETURN n LIMIT 1"
                source_result = session.run(source_query, name=source_name).single()
                
                # Find target entity
                target_result = session.run(source_query, name=target_name).single()
                
                if not source_result or not target_result:
                    raise ValueError(f"Could not find entities: {source_name} or {target_name}")
                
                source_id = str(source_result['n'].id)
                target_id = str(target_result['n'].id)
                
                # Create relationship
                relationship = self.repository.create_relationship(
                    source_id, target_id, relationship_type, properties
                )
                
                self.logger.log_debug(f"Connected {source_name} -> {target_name} with {relationship_type}")
                
                return relationship
                
        except Exception as e:
            self.logger.log_error(f"Failed to connect entities {source_name} -> {target_name}: {e}")
            raise
    
    def find_related_entities(self, entity_name: str, depth: int = 2) -> List[GraphNode]:
        """Find entities related to the given entity"""
        try:
            # Find the entity first
            with self.repository.connection_manager.get_session() as session:
                entity_query = "MATCH (n {name: $name}) RETURN n LIMIT 1"
                entity_result = session.run(entity_query, name=entity_name).single()
                
                if not entity_result:
                    return []
                
                entity_id = str(entity_result['n'].id)
                
                # Find related entities
                related_query = f"""
                MATCH (start {{name: $name}})
                MATCH (start)-[*1..{depth}]-(related)
                WHERE related <> start
                RETURN DISTINCT related
                """
                
                related_results = session.run(related_query, name=entity_name)
                
                related_entities = []
                for record in related_results:
                    node = record['related']
                    related_entities.append(GraphNode(
                        id=str(node.id),
                        type=list(node.labels)[0] if node.labels else 'Unknown',
                        name=node.get('name', ''),
                        properties=dict(node)
                    ))
                
                self.logger.log_debug(f"Found {len(related_entities)} entities related to {entity_name}")
                
                return related_entities
                
        except Exception as e:
            self.logger.log_error(f"Failed to find related entities for {entity_name}: {e}")
            return []
    
    def search_entities(self, search_term: str, entity_types: List[str] = None) -> List[GraphNode]:
        """Search for entities by name or properties"""
        try:
            with self.repository.connection_manager.get_session() as session:
                if entity_types:
                    # Search within specific types
                    type_labels = ':'.join(entity_types)
                    query = f"""
                    MATCH (n:{type_labels})
                    WHERE n.name CONTAINS $search_term
                    RETURN n
                    LIMIT 50
                    """
                else:
                    # Search all entities
                    query = """
                    MATCH (n)
                    WHERE n.name CONTAINS $search_term
                    RETURN n
                    LIMIT 50
                    """
                
                results = session.run(query, search_term=search_term)
                
                entities = []
                for record in results:
                    node = record['n']
                    entities.append(GraphNode(
                        id=str(node.id),
                        type=list(node.labels)[0] if node.labels else 'Unknown',
                        name=node.get('name', ''),
                        properties=dict(node)
                    ))
                
                self.logger.log_debug(f"Found {len(entities)} entities matching '{search_term}'")
                
                return entities
                
        except Exception as e:
            self.logger.log_error(f"Failed to search entities for '{search_term}': {e}")
            return []
    
    def get_entity_context(self, entity_name: str, context_depth: int = 1) -> Dict[str, Any]:
        """Get contextual information about an entity"""
        try:
            # Find the entity
            with self.repository.connection_manager.get_session() as session:
                entity_query = "MATCH (n {name: $name}) RETURN n LIMIT 1"
                entity_result = session.run(entity_query, name=entity_name).single()
                
                if not entity_result:
                    return {"error": f"Entity '{entity_name}' not found"}
                
                entity = entity_result['n']
                entity_id = str(entity.id)
                
                # Get relationships within context depth
                relationships = self.repository.get_entity_relationships(entity_id, context_depth)
                
                # Get related entities
                related_entities = self.find_related_entities(entity_name, context_depth)
                
                context = {
                    "entity": GraphNode(
                        id=entity_id,
                        type=list(entity.labels)[0] if entity.labels else 'Unknown',
                        name=entity.get('name', ''),
                        properties=dict(entity)
                    ),
                    "relationships": relationships,
                    "related_entities": related_entities,
                    "context_depth": context_depth,
                    "total_relationships": len(relationships),
                    "total_related": len(related_entities)
                }
                
                return context
                
        except Exception as e:
            self.logger.log_error(f"Failed to get context for {entity_name}: {e}")
            return {"error": str(e)}
    
    def validate_graph_integrity(self) -> Dict[str, Any]:
        """Validate graph integrity and return report"""
        try:
            with self.repository.connection_manager.get_session() as session:
                # Check for orphaned relationships
                orphaned_rels_query = """
                MATCH ()-[r]->()
                WHERE NOT EXISTS(()-[r]->())
                RETURN count(r) as orphaned_relationships
                """
                
                # Check for duplicate entities
                duplicate_entities_query = """
                MATCH (n)
                WITH n.name as name, labels(n) as types, count(n) as count
                WHERE count > 1
                RETURN name, types, count
                """
                
                # Get basic stats
                stats = self.repository.get_graph_stats()
                
                # Run integrity checks
                orphaned_result = session.run(orphaned_rels_query).single()
                duplicate_results = list(session.run(duplicate_entities_query))
                
                integrity_report = {
                    "valid": True,
                    "stats": stats,
                    "orphaned_relationships": orphaned_result['orphaned_relationships'] if orphaned_result else 0,
                    "duplicate_entities": len(duplicate_results),
                    "duplicate_details": [dict(record) for record in duplicate_results],
                    "issues": []
                }
                
                # Add issues if found
                if integrity_report["orphaned_relationships"] > 0:
                    integrity_report["issues"].append(f"Found {integrity_report['orphaned_relationships']} orphaned relationships")
                    integrity_report["valid"] = False
                
                if integrity_report["duplicate_entities"] > 0:
                    integrity_report["issues"].append(f"Found {integrity_report['duplicate_entities']} duplicate entities")
                    integrity_report["valid"] = False
                
                return integrity_report
                
        except Exception as e:
            self.logger.log_error(f"Graph integrity validation failed: {e}")
            return {
                "valid": False,
                "error": str(e),
                "issues": ["Integrity check failed due to error"]
            }
    
    def get_graph_summary(self) -> Dict[str, Any]:
        """Get summary statistics of the graph"""
        try:
            stats = self.repository.get_graph_stats()
            return {
                "summary": stats,
                "status": "healthy" if stats.get("total_nodes", 0) > 0 else "empty",
                "last_updated": stats.get("last_updated", "unknown")
            }
        except Exception as e:
            self.logger.log_error(f"Failed to get graph summary: {e}")
            return {
                "summary": {},
                "status": "error",
                "error": str(e)
            }