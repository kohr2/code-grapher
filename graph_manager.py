import os
from datetime import datetime
from typing import Dict, List, Optional, Any, Tuple
from neo4j import GraphDatabase
from py2neo import Graph, Node, Relationship
from shared.services.service_locator import ServiceLocator

# Get logger through service locator
logger = ServiceLocator.get_logger("graph_manager")
# Removed ai_evaluation_tracker dependency
import time
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

class CodeGraphManager:
    """Manages the graph database for code analysis and RAG"""
    
    def __init__(self, uri: Optional[str] = None, 
                 username: Optional[str] = None, 
                 password: Optional[str] = None):
        # Use environment variables with fallbacks
        self.uri = uri or os.getenv("NEO4J_URL", "bolt://localhost:7687")
        self.username = username or os.getenv("NEO4J_USERNAME", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.session_logger = logger.create_session_logger("GraphManager")
        
        # Log initialization
        self.session_logger.log_operation_start(
            "GraphManager.init",
            {"uri": self.uri, "username": self.username}
        )
        
        start_time = time.time()
        try:
            # Initialize connection
            self.driver = GraphDatabase.driver(self.uri, auth=(self.username, self.password))
            self.graph = Graph(self.uri, auth=(self.username, self.password))
            
            # Verify connection
            self.driver.verify_connectivity()
            
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "GraphManager.init",
                duration=duration,
                success=True,
                details={"connection": "verified"}
            )
            
            # Log successful connection
            logger.log_info(f"Successfully connected to Neo4j graph database in {duration:.2f}s")
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "GraphManager.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Log connection failure
            logger.log_error(f"Failed to connect to Neo4j: {str(e)}")
            raise
    
    def create_code_entity(self, entity_type: str, name: str, 
                          properties: Dict[str, Any]) -> Node:
        """Create a node representing a code entity"""
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
                "entity_type": entity_type
            })
            
            # Create node
            node = Node(entity_type, **properties)
            self.graph.create(node)
            
            duration = time.time() - start_time
            
            # Log the action
            self.session_logger.log_graph_action(
                action="CREATE_NODE",
                entity_type=entity_type,
                entity_name=name
            )
            
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
                details={"node_id": node.identity}
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
            
            self.session_logger.log_error(e, {"entity_type": entity_type, "name": name})
            raise
    
    def create_relationship(self, from_node: Node, to_node: Node, 
                           rel_type: str, properties: Optional[Dict] = None) -> Relationship:
        """Create a relationship between two nodes"""
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
            # Create relationship
            rel = Relationship(from_node, rel_type, to_node)
            if properties:
                for key, value in properties.items():
                    rel[key] = value
            
            self.graph.create(rel)
            
            duration = time.time() - start_time
            
            # Log the action
            self.session_logger.log_graph_action(
                action="CREATE_RELATIONSHIP",
                entity_type=rel_type,
                entity_name=f"{from_node.get('name')} -> {to_node.get('name')}",
                relationships=[rel_type]
            )
            
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
            
            self.session_logger.log_error(e, {"rel_type": rel_type})
            raise
    
    def find_entity(self, entity_type: str, name: str) -> Optional[Node]:
        """Find an existing entity in the graph"""
        self.session_logger.log_operation_start(
            "find_entity",
            {"type": entity_type, "name": name}
        )
        
        start_time = time.time()
        try:
            query = f"MATCH (n:{entity_type} {{name: $name}}) RETURN n"
            result = self.graph.run(query, name=name).data()
            
            duration = time.time() - start_time
            
            node = result[0]['n'] if result else None
            
            self.session_logger.log_operation_end(
                "find_entity",
                duration=duration,
                success=True,
                details={"found": node is not None}
            )
            
            self.session_logger.log_performance(
                metric="entity_lookup_time",
                value=duration * 1000,
                unit="ms",
                context={"entity_type": entity_type, "found": node is not None}
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
            
            self.session_logger.log_error(e, {"entity_type": entity_type, "name": name})
            return None
    
    def analyze_code_structure(self, file_path: str, ast_data: Dict[str, Any]):
        """Analyze code structure and populate graph"""
        self.session_logger.log_operation_start(
            "analyze_code_structure",
            {"file": file_path, "ast_nodes": len(ast_data.get("nodes", []))}
        )
        
        start_time = time.time()
        nodes_created = 0
        relationships_created = 0
        
        try:
            # Create file node
            file_node = self.create_code_entity(
                "File",
                file_path,
                {
                    "path": file_path,
                    "language": ast_data.get("language", "unknown"),
                    "size": ast_data.get("size", 0),
                    "last_modified": ast_data.get("last_modified", "")
                }
            )
            nodes_created += 1
            
            # Process AST nodes
            for node_data in ast_data.get("nodes", []):
                node_type = node_data.get("type")
                node_name = node_data.get("name")
                
                if not node_type or not node_name:
                    continue
                
                # Log decision
                self.session_logger.log_decision(
                    decision=f"Creating {node_type} node",
                    reasoning=f"Found {node_type} '{node_name}' in AST",
                    alternatives=["Skip node", "Merge with existing"]
                )
                
                # Create entity node
                entity_node = self.create_code_entity(
                    node_type,
                    node_name,
                    node_data.get("properties", {})
                )
                nodes_created += 1
                
                # Create relationship to file
                self.create_relationship(
                    file_node,
                    entity_node,
                    "CONTAINS"
                )
                relationships_created += 1
                
                # Process dependencies
                for dep in node_data.get("dependencies", []):
                    dep_node = self.find_entity(dep["type"], dep["name"])
                    if dep_node:
                        self.create_relationship(
                            entity_node,
                            dep_node,
                            dep["relationship"]
                        )
                        relationships_created += 1
            
            duration = time.time() - start_time
            
            # Log analysis results
            self.session_logger.log_code_analysis(
                file_path,
                {
                    "nodes_created": nodes_created,
                    "relationships_created": relationships_created,
                    "duration_seconds": duration
                }
            )
            
            self.session_logger.log_operation_end(
                "analyze_code_structure",
                duration=duration,
                success=True,
                details={
                    "nodes": nodes_created,
                    "relationships": relationships_created
                }
            )
            
            # Log performance
            logger.log_info(f"Analyzed {file_path} creating {nodes_created} nodes in {duration:.2f}s")
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "analyze_code_structure",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {"file": file_path})
            
            # Log failure
            logger.log_error(f"Failed to analyze {file_path}: {str(e)}")
            raise
    
    def query_for_rag(self, query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """Query the graph for RAG retrieval"""
        self.session_logger.log_operation_start(
            "query_for_rag",
            {"query": query, "limit": limit}
        )
        
        start_time = time.time()
        try:
            # Simple text search across node properties
            # In production, this would use vector embeddings
            cypher_query = """
            MATCH (n)
            WHERE any(prop in keys(n) WHERE toString(n[prop]) CONTAINS $query)
            RETURN n, labels(n) as labels
            LIMIT $limit
            """
            
            results = self.graph.run(cypher_query, query=query, limit=limit).data()
            
            duration = time.time() - start_time
            
            # Process results
            processed_results = []
            for result in results:
                node = result['n']
                processed_results.append({
                    "labels": result['labels'],
                    "properties": dict(node),
                    "relevance_score": 0.8  # Placeholder - would calculate actual relevance
                })
            
            # Log RAG operation
            relevance_scores = [r["relevance_score"] for r in processed_results]
            self.session_logger.log_rag_operation(
                operation="graph_query",
                query=query,
                results_count=len(processed_results),
                relevance_scores=relevance_scores
            )
            
            self.session_logger.log_operation_end(
                "query_for_rag",
                duration=duration,
                success=True,
                details={"results": len(processed_results)}
            )
            
            self.session_logger.log_performance(
                metric="rag_query_time",
                value=duration * 1000,
                unit="ms",
                context={"query_length": len(query), "results": len(processed_results)}
            )
            
            return processed_results
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "query_for_rag",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {"query": query})
            return []
    
    def get_graph_stats(self) -> Dict[str, Any]:
        """Get statistics about the graph"""
        self.session_logger.log_info("Retrieving graph statistics")
        
        try:
            stats = {
                "total_nodes": self.graph.run("MATCH (n) RETURN count(n) as count").data()[0]['count'],
                "total_relationships": self.graph.run("MATCH ()-[r]->() RETURN count(r) as count").data()[0]['count'],
                "node_types": {},
                "relationship_types": {}
            }
            
            # Count by node type
            node_types = self.graph.run("MATCH (n) RETURN labels(n) as labels, count(n) as count").data()
            for nt in node_types:
                if nt['labels']:
                    stats["node_types"][nt['labels'][0]] = nt['count']
            
            # Count by relationship type
            rel_types = self.graph.run("MATCH ()-[r]->() RETURN type(r) as type, count(r) as count").data()
            for rt in rel_types:
                stats["relationship_types"][rt['type']] = rt['count']
            
            self.session_logger.log_info(f"Graph stats: {stats}")
            
            return stats
            
        except Exception as e:
            self.session_logger.log_error(e)
            return {}
    
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph data in D3.js compatible format"""
        self.session_logger.log_operation_start("export_d3_format")
        
        start_time = time.time()
        try:
            # Get all nodes
            nodes_query = """
            MATCH (n)
            RETURN id(n) as id, labels(n) as labels, properties(n) as properties
            """
            nodes_result = self.graph.run(nodes_query).data()
            
            # Get all relationships  
            rels_query = """
            MATCH (a)-[r]->(b)
            RETURN id(a) as source, id(b) as target, type(r) as type, properties(r) as properties
            """
            rels_result = self.graph.run(rels_query).data()
            
            # Format nodes for D3
            nodes = []
            for node in nodes_result:
                # Convert properties to JSON-serializable format
                properties = {}
                for key, value in (node['properties'] or {}).items():
                    if hasattr(value, 'isoformat'):  # DateTime object
                        properties[key] = value.isoformat()
                    elif hasattr(value, '__str__'):  # Other non-serializable objects
                        properties[key] = str(value)
                    else:
                        properties[key] = value
                
                node_data = {
                    "id": node['id'],
                    "labels": node['labels'],
                    "type": node['labels'][0] if node['labels'] else "Unknown",
                    "name": properties.get('name', f"Node_{node['id']}"),
                    "properties": properties
                }
                nodes.append(node_data)
            
            # Format relationships for D3
            links = []
            for rel in rels_result:
                # Convert properties to JSON-serializable format
                rel_properties = {}
                for key, value in (rel['properties'] or {}).items():
                    if hasattr(value, 'isoformat'):  # DateTime object
                        rel_properties[key] = value.isoformat()
                    elif hasattr(value, '__str__'):  # Other non-serializable objects
                        rel_properties[key] = str(value)
                    else:
                        rel_properties[key] = value
                
                link_data = {
                    "source": rel['source'],
                    "target": rel['target'],
                    "type": rel['type'],
                    "properties": rel_properties
                }
                links.append(link_data)
            
            export_data = {
                "nodes": nodes,
                "links": links,
                "metadata": {
                    "exported_at": datetime.now().isoformat(),
                    "total_nodes": len(nodes),
                    "total_links": len(links),
                    "node_types": list(set(n['type'] for n in nodes)),
                    "link_types": list(set(l['type'] for l in links))
                }
            }
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "export_d3_format",
                duration=duration,
                success=True,
                details={"nodes": len(nodes), "links": len(links)}
            )
            
            self.session_logger.log_performance(
                metric="d3_export_time",
                value=duration * 1000,
                unit="ms",
                context={"nodes": len(nodes), "links": len(links)}
            )
            
            # Log export success
            logger.log_info(f"Exported {len(nodes)} nodes and {len(links)} links to D3 format in {duration:.2f}s")
            
            return export_data
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "export_d3_format",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e)
            
            # Log export failure
            logger.log_error(f"Failed to export graph to D3 format: {str(e)}")
            raise
    
    def export_mermaid_format(self) -> str:
        """Export graph data in Mermaid diagram format"""
        self.session_logger.log_operation_start("export_mermaid_format")
        
        start_time = time.time()
        try:
            # Get nodes and relationships
            query = """
            MATCH (a)-[r]->(b)
            RETURN a.name as source_name, labels(a) as source_labels, 
                   b.name as target_name, labels(b) as target_labels, 
                   type(r) as rel_type
            """
            result = self.graph.run(query).data()
            
            mermaid_lines = ["graph TD"]
            
            # Track nodes we've seen to avoid duplicates
            seen_nodes = set()
            
            for row in result:
                source_name = row['source_name'] or "Unknown"
                target_name = row['target_name'] or "Unknown"
                source_type = row['source_labels'][0] if row['source_labels'] else "Node"
                target_type = row['target_labels'][0] if row['target_labels'] else "Node"
                rel_type = row['rel_type']
                
                # Clean names for Mermaid (remove spaces, special chars)
                source_id = f"{source_type}_{source_name}".replace(" ", "_").replace(".", "_")
                target_id = f"{target_type}_{target_name}".replace(" ", "_").replace(".", "_")
                
                # Add node definitions if not seen before
                if source_id not in seen_nodes:
                    mermaid_lines.append(f'    {source_id}["{source_name}<br/>({source_type})"]')
                    seen_nodes.add(source_id)
                    
                if target_id not in seen_nodes:
                    mermaid_lines.append(f'    {target_id}["{target_name}<br/>({target_type})"]')
                    seen_nodes.add(target_id)
                
                # Add relationship
                mermaid_lines.append(f'    {source_id} -->|{rel_type}| {target_id}')
            
            # Add styling for different node types
            mermaid_lines.extend([
                "",
                "    classDef memory fill:#e1f5fe,stroke:#0277bd,stroke-width:2px",
                "    classDef entity fill:#f3e5f5,stroke:#7b1fa2,stroke-width:2px", 
                "    classDef world fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px",
                "    classDef file fill:#fff3e0,stroke:#ef6c00,stroke-width:2px",
                "    classDef class fill:#fce4ec,stroke:#c2185b,stroke-width:2px",
                "    classDef function fill:#f1f8e9,stroke:#558b2f,stroke-width:2px"
            ])
            
            mermaid_content = "\n".join(mermaid_lines)
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "export_mermaid_format",
                duration=duration,
                success=True,
                details={"relationships": len(result), "unique_nodes": len(seen_nodes)}
            )
            
            return mermaid_content
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "export_mermaid_format",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e)
            raise
    
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple nodes/links format for basic visualization"""
        self.session_logger.log_operation_start("export_simple_format")
        
        start_time = time.time()
        try:
            stats = self.get_graph_stats()
            d3_data = self.export_d3_format()
            
            # Simplify the D3 data
            simple_data = {
                "nodes": [
                    {
                        "id": node["id"],
                        "name": node["name"],
                        "type": node["type"],
                        "label": f"{node['name']} ({node['type']})"
                    }
                    for node in d3_data["nodes"]
                ],
                "edges": [
                    {
                        "from": link["source"],
                        "to": link["target"],
                        "label": link["type"]
                    }
                    for link in d3_data["links"]
                ],
                "summary": stats
            }
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "export_simple_format",
                duration=duration,
                success=True,
                details={"nodes": len(simple_data["nodes"]), "edges": len(simple_data["edges"])}
            )
            
            return simple_data
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "export_simple_format",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e)
            raise

    def close(self):
        """Close database connections"""
        self.session_logger.log_info("Closing graph database connections")
        if hasattr(self, 'driver'):
            self.driver.close()