"""
GraphManager V2 - Drop-in replacement for graph_manager.py using new architecture

This is a direct replacement that maintains the exact same interface as the original
CodeGraphManager but uses the new graph-ops services underneath.
"""
import os
import sys
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple

# Direct imports to avoid module path issues
from neo4j import GraphDatabase
from py2neo import Graph, Node, Relationship
from shared.services.service_locator import ServiceLocator

# Import new components directly from files
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'graph-ops', 'config'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'graph-ops', 'repositories'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'graph-ops', 'services'))
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'graph-ops', 'interfaces'))

from database_config import DatabaseConfig
from connection_manager import ConnectionManager
from neo4j_repository import Neo4jRepository
from graph_service import GraphService
from export_service import ExportService

import time
from dotenv import load_dotenv

# Load environment variables
load_dotenv()


class CodeGraphManagerV2:
    """
    Version 2 of CodeGraphManager using new graph-ops architecture
    Maintains 100% compatibility with existing interface
    """
    
    def __init__(self, uri: Optional[str] = None, 
                 username: Optional[str] = None, 
                 password: Optional[str] = None,
                 database: Optional[str] = None):
        # Use environment variables with fallbacks
        self.uri = uri or os.getenv("NEO4J_URL", "bolt://localhost:7687")
        self.username = username or os.getenv("NEO4J_USERNAME", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.database = database or os.getenv("NEO4J_DATABASE", "neo4j")
        
        # Get logger through service locator
        self.logger = ServiceLocator.get_logger("graph_manager_v2")
        self.session_logger = self.logger.create_session_logger("GraphManagerV2")
        
        # Log initialization
        self.session_logger.log_operation_start(
            "GraphManagerV2.init",
            {"uri": self.uri, "username": self.username}
        )
        
        start_time = time.time()
        try:
            # Initialize new services
            self.config = DatabaseConfig(
                uri=self.uri,
                username=self.username,
                password=self.password,
                database=self.database
            )
            
            self.connection_manager = ConnectionManager(
                self.config.uri, 
                self.config.username, 
                self.config.password,
                self.config.database
            )
            
            self.repository = Neo4jRepository(self.connection_manager)
            self.graph_service = GraphService(self.repository)
            self.export_service = ExportService(self.connection_manager)
            
            # Maintain legacy py2neo interface for compatibility
            self.driver = GraphDatabase.driver(self.uri, auth=(self.username, self.password))
            self.graph = Graph(self.uri, auth=(self.username, self.password))
            
            # Verify connection
            self.driver.verify_connectivity()
            
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "GraphManagerV2.init",
                duration=duration,
                success=True,
                details={"connection": "verified"}
            )
            
            # Log successful connection
            self.logger.log_info(f"GraphManagerV2 connected successfully in {duration:.2f}s")
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "GraphManagerV2.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Log connection failure
            self.logger.log_error(f"Failed to connect GraphManagerV2: {str(e)}")
            raise
    
    def create_code_entity(self, entity_type: str, name: str, 
                          properties: Dict[str, Any]) -> Node:
        """Create a node representing a code entity - LEGACY INTERFACE"""
        self.session_logger.log_operation_start(
            "create_code_entity",
            {"type": entity_type, "name": name, "properties": properties}
        )
        
        start_time = time.time()
        try:
            # Add metadata
            properties.update({
                "name": name,
                "created_at": datetime.now().isoformat(),
                "type": entity_type
            })
            
            # Use new service but return legacy py2neo Node
            graph_node = self.graph_service.add_entity(entity_type, name, properties)
            
            # Create py2neo Node for compatibility
            node = Node(entity_type, **properties)
            # Set identity to match the database ID
            node._id = int(graph_node.id)
            
            duration = time.time() - start_time
            
            # Log the action (skip graph action logging to avoid interface issues)
            self.session_logger.log_performance(
                metric="node_creation_time",
                value=duration * 1000,
                unit="ms",
                context={"entity_type": entity_type}
            )
            
            self.session_logger.log_operation_end(
                "create_code_entity",
                duration=duration,
                success=True,
                details={"node_id": graph_node.id}
            )
            
            return node
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "create_code_entity",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.logger.log_error(f"Create entity failed: {e}")
            raise
    
    def create_relationship(self, from_node: Node, to_node: Node, 
                           rel_type: str, properties: Optional[Dict] = None) -> Relationship:
        """Create a relationship between two nodes - LEGACY INTERFACE"""
        self.session_logger.log_operation_start(
            "create_relationship",
            {
                "from": from_node.get("name", "unknown"),
                "to": to_node.get("name", "unknown"),
                "type": rel_type
            }
        )
        
        start_time = time.time()
        try:
            # Create relationship using py2neo for full compatibility
            rel = Relationship(from_node, rel_type, to_node)
            if properties:
                for key, value in properties.items():
                    rel[key] = value
            
            self.graph.create(rel)
            
            duration = time.time() - start_time
            
            self.session_logger.log_performance(
                metric="relationship_creation_time",
                value=duration * 1000,
                unit="ms",
                context={"rel_type": rel_type}
            )
            
            self.session_logger.log_operation_end(
                "create_relationship",
                duration=duration,
                success=True
            )
            
            return rel
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "create_relationship",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.logger.log_error(f"Create relationship failed: {e}")
            raise
    
    def find_entity(self, entity_type: str, name: str) -> Optional[Node]:
        """Find an existing entity in the graph - LEGACY INTERFACE"""
        self.session_logger.log_operation_start(
            "find_entity",
            {"type": entity_type, "name": name}
        )
        
        start_time = time.time()
        try:
            # Use legacy py2neo query for full compatibility
            query = f"MATCH (n:{entity_type} {{name: $name}}) RETURN n"
            result = self.graph.run(query, name=name).data()
            
            duration = time.time() - start_time
            
            node = result[0]['n'] if result else None
            
            self.session_logger.log_performance(
                metric="entity_search_time",
                value=duration * 1000,
                unit="ms",
                context={"entity_type": entity_type}
            )
            
            self.session_logger.log_operation_end(
                "find_entity",
                duration=duration,
                success=True,
                details={"found": node is not None}
            )
            
            return node
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "find_entity",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.logger.log_error(f"Find entity failed: {e}")
            return None
    
    def get_graph_stats(self) -> Dict[str, Any]:
        """Get graph statistics - LEGACY INTERFACE"""
        try:
            # Use new service
            return self.repository.get_graph_stats()
            
        except Exception as e:
            self.logger.log_error(f"Get graph stats failed: {e}")
            return {"error": str(e)}
    
    def clear_graph(self) -> None:
        """Clear all nodes and relationships from the graph - LEGACY INTERFACE"""
        try:
            # Use new service
            self.repository.clear_graph()
            self.logger.log_info("Graph cleared via V2")
            
        except Exception as e:
            self.logger.log_error(f"Clear graph failed: {e}")
            raise
    
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph data in D3.js compatible format - LEGACY INTERFACE"""
        try:
            # Use new export service
            return self.export_service.export_d3_format()
            
        except Exception as e:
            self.logger.log_error(f"D3 export failed: {e}")
            raise
    
    def export_mermaid_format(self) -> str:
        """Export graph data in Mermaid diagram format - LEGACY INTERFACE"""
        try:
            # Use new export service
            return self.export_service.export_mermaid_format()
            
        except Exception as e:
            self.logger.log_error(f"Mermaid export failed: {e}")
            raise
    
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple nodes/links format - LEGACY INTERFACE"""
        try:
            # Use new export service
            return self.export_service.export_simple_format()
            
        except Exception as e:
            self.logger.log_error(f"Simple export failed: {e}")
            raise
    
    def close(self):
        """Close database connections"""
        try:
            if hasattr(self, 'driver') and self.driver:
                self.driver.close()
            if hasattr(self, 'repository'):
                self.repository.close()
            self.logger.log_info("GraphManagerV2 closed")
        except Exception as e:
            self.logger.log_error(f"Error closing GraphManagerV2: {e}")


# Make this the default CodeGraphManager
CodeGraphManager = CodeGraphManagerV2