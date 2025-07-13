import time
import os
from typing import Dict, Any, List, Optional, Set
import json
from collections import defaultdict
from dotenv import load_dotenv

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError

# Load environment variables
load_dotenv()

try:
    from neo4j import GraphDatabase, basic_auth
    NEO4J_AVAILABLE = True
except ImportError:
    NEO4J_AVAILABLE = False


class GraphBuilderAgent(BaseAgent):
    """
    Agent for constructing knowledge graphs from entities and relationships
    Integrates with Neo4j database for graph storage and querying
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        # Agent-specific configuration - set before super().__init__() to allow validation
        agent_config = config.get("config", {})
        self.graph_database_config = agent_config.get("graphDatabase", {})
        self.node_labels = agent_config.get("nodeLabels", {})
        self.relationship_types = agent_config.get("relationshipTypes", {})
        self.indexing_config = agent_config.get("indexing", {})
        self.batch_size = agent_config.get("batchSize", 1000)
        
        # Neo4j connection setup - before super().__init__() for validation
        self.driver = None
        self.connection_config = self._get_connection_config()
        
        super().__init__(agent_id, config, shared_state)
        
        # Graph statistics
        self.nodes_created = 0
        self.relationships_created = 0
        self.indices_created = 0
        
        # Initialize database connection
        if NEO4J_AVAILABLE:
            self._initialize_database_connection()
        else:
            self.session_logger.log_error(
                Exception("Neo4j driver not available"),
                {"workaround": "Install neo4j driver: pip install neo4j"}
            )
        
        self.session_logger.log_info(
            f"GraphBuilderAgent initialized with batch size: {self.batch_size}"
        )
    
    def _get_connection_config(self) -> Dict[str, Any]:
        """Get Neo4j connection configuration from environment variables or config"""
        # First try from explicit config
        if self.graph_database_config.get("connection"):
            return self.graph_database_config["connection"]
        
        # Fall back to environment variables
        return {
            "uri": os.getenv("NEO4J_URL", "bolt://localhost:7687"),
            "auth": {
                "username": os.getenv("NEO4J_USERNAME", "neo4j"),
                "password": os.getenv("NEO4J_PASSWORD", "password")
            }
        }
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        if not NEO4J_AVAILABLE:
            raise AgentConfigurationError("Neo4j driver not available. Install with: pip install neo4j")
        
        if not self.graph_database_config:
            raise AgentConfigurationError("graphDatabase configuration is required")
        
        db_type = self.graph_database_config.get("type")
        if db_type != "neo4j":
            raise AgentConfigurationError(f"Unsupported database type: {db_type}")
        
        connection_config = self.connection_config
        if not connection_config.get("uri"):
            raise AgentConfigurationError("Database URI is required")
        
        auth_config = connection_config.get("auth", {})
        if not auth_config.get("username") or not auth_config.get("password"):
            raise AgentConfigurationError("Database username and password are required")
        
        if self.batch_size < 1 or self.batch_size > 10000:
            raise AgentConfigurationError("batchSize must be between 1 and 10000")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return ["graph-construction", "neo4j-integration", "schema-validation"]
    
    def _initialize_database_connection(self) -> None:
        """Initialize connection to Neo4j database"""
        try:
            uri = self.connection_config["uri"]
            auth_config = self.connection_config["auth"]
            username = auth_config["username"]
            password = auth_config["password"]
            
            self.driver = GraphDatabase.driver(
                uri,
                auth=basic_auth(username, password)
            )
            
            # Test connection
            with self.driver.session() as session:
                result = session.run("RETURN 1 as test")
                test_value = result.single()["test"]
                
            self.session_logger.log_decision(
                decision="Successfully connected to Neo4j database",
                reasoning="Database connection test passed",
                alternatives=["Use local file storage", "Switch to different graph database"]
            )
            
        except Exception as e:
            self.session_logger.log_error(e, {"connection_config": self.connection_config})
            raise AgentConfigurationError(f"Failed to connect to Neo4j: {str(e)}") from e
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute graph construction from entities and relationships
        
        Expected input format:
        {
            "entities": [...],      # From EntityExtractionAgent
            "relationships": [...]  # From RelationshipAnalysisAgent
        }
        """
        start_time = time.time()
        
        self.session_logger.log_operation_start(
            "GraphBuilder.execute",
            {
                "entities_count": len(input_data.get("entities", [])),
                "relationships_count": len(input_data.get("relationships", [])),
                "batch_size": self.batch_size
            }
        )
        
        try:
            # Extract input data
            entities = input_data.get("entities", [])
            relationships = input_data.get("relationships", [])
            
            if not entities and not relationships:
                raise AgentExecutionError("No entities or relationships provided for graph construction")
            
            # Reset counters
            self.nodes_created = 0
            self.relationships_created = 0
            self.indices_created = 0
            
            # Setup database schema
            if self.indexing_config.get("createIndices", True):
                self._create_database_indices()
                self._create_database_constraints()
            
            # Create nodes from entities
            if entities:
                self._create_nodes_from_entities(entities)
            
            # Create relationships
            if relationships:
                self._create_relationships_from_data(relationships)
            
            # Generate graph statistics
            graph_stats = self._generate_graph_statistics()
            
            # Create result summary
            indexing_results = {
                "indices_created": self.indices_created,
                "constraints_created": len(self.indexing_config.get("uniqueConstraints", [])),
                "full_text_indices": len(self.indexing_config.get("fullTextIndices", []))
            }
            
            duration = time.time() - start_time
            
            result = {
                "nodesCreated": self.nodes_created,
                "relationshipsCreated": self.relationships_created,
                "graphStats": graph_stats,
                "indexingResults": indexing_results,
                "metadata": {
                    "database_type": "neo4j",
                    "batch_size": self.batch_size,
                    "processing_time": duration,
                    "node_labels_used": list(self.node_labels.values()),
                    "relationship_types_used": list(self.relationship_types.values())
                }
            }
            
            self.session_logger.log_operation_end(
                "GraphBuilder.execute",
                duration=duration,
                success=True,
                details={
                    "nodes_created": self.nodes_created,
                    "relationships_created": self.relationships_created,
                    "graph_nodes_total": graph_stats.get("total_nodes", 0)
                }
            )
            
            # Log performance metrics
            self.log_agent_specific_metrics({
                "nodes_per_second": self.nodes_created / duration if duration > 0 else 0,
                "relationships_per_second": self.relationships_created / duration if duration > 0 else 0,
                "total_graph_operations": self.nodes_created + self.relationships_created
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "GraphBuilder.execute",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            raise AgentExecutionError(f"Graph construction failed: {str(e)}") from e
    
    def _create_database_indices(self) -> None:
        """Create database indices for performance optimization"""
        if not self.driver:
            return
        
        self.session_logger.log_operation_start("create_database_indices", {})
        
        try:
            with self.driver.session() as session:
                # Create unique constraints
                unique_constraints = self.indexing_config.get("uniqueConstraints", [])
                for constraint in unique_constraints:
                    if "." in constraint:
                        label, property_name = constraint.split(".", 1)
                        cypher = f"CREATE CONSTRAINT IF NOT EXISTS FOR (n:{label}) REQUIRE n.{property_name} IS UNIQUE"
                        session.run(cypher)
                        self.indices_created += 1
                        
                        self.session_logger.log_decision(
                            decision=f"Created unique constraint on {label}.{property_name}",
                            reasoning="Unique constraints improve query performance and data integrity",
                            alternatives=["Use regular indices", "Skip indexing"]
                        )
                
                # Create full-text indices
                full_text_indices = self.indexing_config.get("fullTextIndices", [])
                for index_spec in full_text_indices:
                    if "." in index_spec:
                        label, property_name = index_spec.split(".", 1)
                        index_name = f"full_text_{label}_{property_name}".replace(".", "_")
                        cypher = f"""
                        CALL db.index.fulltext.createNodeIndex(
                            '{index_name}', 
                            ['{label}'], 
                            ['{property_name}']
                        )
                        """
                        try:
                            session.run(cypher)
                            self.indices_created += 1
                        except Exception as e:
                            # Index might already exist
                            self.session_logger.log_info(f"Full-text index {index_name} might already exist: {str(e)}")
            
            self.session_logger.log_operation_end("create_database_indices", duration=0.5, success=True)
            
        except Exception as e:
            self.session_logger.log_operation_end("create_database_indices", duration=0.5, success=False)
            self.session_logger.log_error(e, {"indexing_config": self.indexing_config})
            # Don't fail the entire operation for index creation errors
    
    def _create_database_constraints(self) -> None:
        """Create database constraints"""
        if not self.driver:
            return
        
        # Additional constraints beyond unique constraints
        # Could include NOT NULL constraints, property existence constraints, etc.
        pass
    
    def _create_nodes_from_entities(self, entities: List[Dict[str, Any]]) -> None:
        """Create graph nodes from entity data"""
        if not self.driver:
            return
        
        self.session_logger.log_operation_start("create_nodes", {
            "entity_count": len(entities),
            "batch_size": self.batch_size
        })
        
        try:
            # Process entities in batches
            for i in range(0, len(entities), self.batch_size):
                batch = entities[i:i + self.batch_size]
                self._create_node_batch(batch)
            
            self.session_logger.log_operation_end("create_nodes", duration=1.0, success=True, details={
                "nodes_created": self.nodes_created
            })
            
        except Exception as e:
            self.session_logger.log_operation_end("create_nodes", duration=1.0, success=False)
            raise AgentExecutionError(f"Failed to create nodes: {str(e)}") from e
    
    def _create_node_batch(self, entities: List[Dict[str, Any]]) -> None:
        """Create a batch of nodes"""
        if not self.driver:
            return
        
        with self.driver.session() as session:
            # Group entities by type for more efficient batch creation
            entities_by_type = defaultdict(list)
            for entity in entities:
                entity_type = entity.get("type", "Unknown")
                entities_by_type[entity_type].append(entity)
            
            # Create nodes for each entity type
            for entity_type, type_entities in entities_by_type.items():
                self._create_nodes_for_type(session, entity_type, type_entities)
    
    def _create_nodes_for_type(self, session, entity_type: str, entities: List[Dict[str, Any]]) -> None:
        """Create nodes for a specific entity type"""
        # Map entity type to Neo4j label
        node_label = self.node_labels.get(entity_type, entity_type.capitalize())
        
        # Prepare node data
        node_data_list = []
        for entity in entities:
            node_data = self._prepare_node_data(entity)
            node_data_list.append(node_data)
        
        # Create nodes using UNWIND for batch efficiency
        cypher = f"""
        UNWIND $nodes AS nodeData
        MERGE (n:{node_label} {{id: nodeData.id}})
        SET n += nodeData.properties
        """
        
        session.run(cypher, nodes=node_data_list)
        self.nodes_created += len(entities)
        
        self.session_logger.log_decision(
            decision=f"Created {len(entities)} {node_label} nodes",
            reasoning="Batch creation improves performance for large datasets",
            alternatives=["Create nodes individually", "Use different batch sizes"]
        )
    
    def _prepare_node_data(self, entity: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare entity data for node creation"""
        node_data = {
            "id": entity.get("id"),
            "properties": {
                "name": entity.get("name"),
                "type": entity.get("type"),
                "scope": entity.get("scope"),
                "visibility": entity.get("visibility", "public"),
                "docstring": entity.get("docstring"),
                "created_at": time.time()
            }
        }
        
        # Add location information
        location = entity.get("location", {})
        if location:
            node_data["properties"]["file_path"] = location.get("file")
            node_data["properties"]["line_number"] = location.get("line")
            node_data["properties"]["column_number"] = location.get("column")
        
        # Add entity-specific attributes
        attributes = entity.get("attributes", {})
        if attributes:
            # Flatten attributes into properties (be careful with nested objects)
            for key, value in attributes.items():
                if isinstance(value, (str, int, float, bool)):
                    node_data["properties"][f"attr_{key}"] = value
                elif isinstance(value, list) and all(isinstance(v, str) for v in value):
                    node_data["properties"][f"attr_{key}"] = value
                # Skip complex nested objects that Neo4j can't store directly
        
        # Remove None values
        node_data["properties"] = {k: v for k, v in node_data["properties"].items() if v is not None}
        
        return node_data
    
    def _create_relationships_from_data(self, relationships: List[Dict[str, Any]]) -> None:
        """Create graph relationships from relationship data"""
        if not self.driver:
            return
        
        self.session_logger.log_operation_start("create_relationships", {
            "relationship_count": len(relationships),
            "batch_size": self.batch_size
        })
        
        try:
            # Process relationships in batches
            for i in range(0, len(relationships), self.batch_size):
                batch = relationships[i:i + self.batch_size]
                self._create_relationship_batch(batch)
            
            self.session_logger.log_operation_end("create_relationships", duration=1.0, success=True, details={
                "relationships_created": self.relationships_created
            })
            
        except Exception as e:
            self.session_logger.log_operation_end("create_relationships", duration=1.0, success=False)
            raise AgentExecutionError(f"Failed to create relationships: {str(e)}") from e
    
    def _create_relationship_batch(self, relationships: List[Dict[str, Any]]) -> None:
        """Create a batch of relationships"""
        if not self.driver:
            return
        
        with self.driver.session() as session:
            # Group relationships by type for efficiency
            relationships_by_type = defaultdict(list)
            for relationship in relationships:
                rel_type = relationship.get("type", "RELATED_TO")
                relationships_by_type[rel_type].append(relationship)
            
            # Create relationships for each type
            for rel_type, type_relationships in relationships_by_type.items():
                self._create_relationships_for_type(session, rel_type, type_relationships)
    
    def _create_relationships_for_type(self, session, rel_type: str, relationships: List[Dict[str, Any]]) -> None:
        """Create relationships for a specific type"""
        # Map relationship type to Neo4j relationship type
        neo4j_rel_type = self.relationship_types.get(rel_type, rel_type.upper().replace("-", "_"))
        
        # Prepare relationship data
        rel_data_list = []
        for relationship in relationships:
            rel_data = self._prepare_relationship_data(relationship)
            rel_data_list.append(rel_data)
        
        # Create relationships using UNWIND
        cypher = f"""
        UNWIND $relationships AS relData
        MATCH (source {{id: relData.source_id}})
        MATCH (target {{id: relData.target_id}})
        MERGE (source)-[r:{neo4j_rel_type}]->(target)
        SET r += relData.properties
        """
        
        session.run(cypher, relationships=rel_data_list)
        self.relationships_created += len(relationships)
        
        self.session_logger.log_decision(
            decision=f"Created {len(relationships)} {neo4j_rel_type} relationships",
            reasoning="Batch relationship creation with type grouping optimizes performance",
            alternatives=["Create all relationships together", "Create individually"]
        )
    
    def _prepare_relationship_data(self, relationship: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare relationship data for creation"""
        rel_data = {
            "source_id": relationship.get("source"),
            "target_id": relationship.get("target"),
            "properties": {
                "strength": relationship.get("strength", 1.0),
                "confidence": relationship.get("confidence", 1.0),
                "created_at": relationship.get("created_at", time.time())
            }
        }
        
        # Add metadata
        metadata = relationship.get("metadata", {})
        if metadata:
            for key, value in metadata.items():
                if isinstance(value, (str, int, float, bool)):
                    rel_data["properties"][f"meta_{key}"] = value
        
        # Remove None values
        rel_data["properties"] = {k: v for k, v in rel_data["properties"].items() if v is not None}
        
        return rel_data
    
    def _generate_graph_statistics(self) -> Dict[str, Any]:
        """Generate statistics about the constructed graph"""
        if not self.driver:
            return {}
        
        stats = {}
        
        try:
            with self.driver.session() as session:
                # Count nodes by label
                node_counts = {}
                for entity_type, label in self.node_labels.items():
                    result = session.run(f"MATCH (n:{label}) RETURN count(n) as count")
                    count = result.single()["count"]
                    node_counts[label] = count
                
                # Count relationships by type
                relationship_counts = {}
                for rel_type, neo4j_type in self.relationship_types.items():
                    result = session.run(f"MATCH ()-[r:{neo4j_type}]->() RETURN count(r) as count")
                    count = result.single()["count"]
                    relationship_counts[neo4j_type] = count
                
                # Overall statistics
                total_nodes_result = session.run("MATCH (n) RETURN count(n) as count")
                total_nodes = total_nodes_result.single()["count"]
                
                total_rels_result = session.run("MATCH ()-[r]->() RETURN count(r) as count")
                total_relationships = total_rels_result.single()["count"]
                
                stats = {
                    "total_nodes": total_nodes,
                    "total_relationships": total_relationships,
                    "nodes_by_label": node_counts,
                    "relationships_by_type": relationship_counts,
                    "graph_density": total_relationships / (total_nodes * (total_nodes - 1)) if total_nodes > 1 else 0.0,
                    "average_degree": (2 * total_relationships) / total_nodes if total_nodes > 0 else 0.0
                }
                
                self.session_logger.log_performance(
                    metric="graph_density",
                    value=stats["graph_density"],
                    unit="ratio",
                    context={"total_nodes": total_nodes, "total_relationships": total_relationships}
                )
                
        except Exception as e:
            self.session_logger.log_error(e, {"context": "graph_statistics"})
            stats = {
                "error": "Failed to generate statistics",
                "nodes_created_in_session": self.nodes_created,
                "relationships_created_in_session": self.relationships_created
            }
        
        return stats
    
    def close_connection(self) -> None:
        """Close database connection"""
        if self.driver:
            self.driver.close()
            self.session_logger.log_info("Neo4j database connection closed")
    
    def __del__(self):
        """Ensure connection is closed when agent is destroyed"""
        self.close_connection()