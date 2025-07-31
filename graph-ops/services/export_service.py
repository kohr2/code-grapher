"""
Export Service - Extracted export functionality from graph_manager.py
"""
import time
from datetime import datetime
from typing import Dict, List, Any

from ..interfaces.export_service_interface import ExportServiceInterface
from ..interfaces.graph_repository_interface import GraphRepositoryInterface
from ..repositories.connection_manager import ConnectionManager
from shared.services.service_locator import ServiceLocator


class ExportService(ExportServiceInterface):
    """
    Service for exporting graph data in various formats
    Extracted from CodeGraphManager to separate concerns
    """
    
    def __init__(self, connection_manager: ConnectionManager):
        self.connection_manager = connection_manager
        self.logger = ServiceLocator.get_logger("export_service")
        
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph data in D3.js compatible format"""
        start_time = time.time()
        
        try:
            with self.connection_manager.get_session() as session:
                # Get all nodes
                nodes_query = """
                MATCH (n)
                RETURN id(n) as id, labels(n) as labels, properties(n) as properties
                """
                nodes_result = session.run(nodes_query)
                
                # Get all relationships  
                rels_query = """
                MATCH (a)-[r]->(b)
                RETURN id(a) as source, id(b) as target, type(r) as type, properties(r) as properties
                """
                rels_result = session.run(rels_query)
                
                # Format nodes for D3
                nodes = []
                for record in nodes_result:
                    # Convert properties to JSON-serializable format
                    properties = {}
                    for key, value in (record['properties'] or {}).items():
                        if hasattr(value, 'isoformat'):  # DateTime object
                            properties[key] = value.isoformat()
                        elif hasattr(value, '__str__'):  # Other non-serializable objects
                            properties[key] = str(value)
                        else:
                            properties[key] = value
                    
                    node_data = {
                        "id": record['id'],
                        "labels": record['labels'],
                        "type": record['labels'][0] if record['labels'] else "Unknown",
                        "name": properties.get('name', f"Node_{record['id']}"),
                        "properties": properties
                    }
                    nodes.append(node_data)
                
                # Format relationships for D3
                links = []
                for record in rels_result:
                    # Convert properties to JSON-serializable format
                    rel_properties = {}
                    for key, value in (record['properties'] or {}).items():
                        if hasattr(value, 'isoformat'):  # DateTime object
                            rel_properties[key] = value.isoformat()
                        elif hasattr(value, '__str__'):  # Other non-serializable objects
                            rel_properties[key] = str(value)
                        else:
                            rel_properties[key] = value
                    
                    link_data = {
                        "source": record['source'],
                        "target": record['target'],
                        "type": record['type'],
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
                self.logger.log_info(f"Exported {len(nodes)} nodes and {len(links)} links to D3 format in {duration:.2f}s")
                
                return export_data
                
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"D3 export failed after {duration:.3f}s: {e}")
            raise
    
    def export_mermaid_format(self) -> str:
        """Export graph data in Mermaid diagram format"""
        start_time = time.time()
        
        try:
            with self.connection_manager.get_session() as session:
                # Get nodes and relationships
                query = """
                MATCH (a)-[r]->(b)
                RETURN a.name as source_name, labels(a) as source_labels, 
                       b.name as target_name, labels(b) as target_labels, 
                       type(r) as rel_type
                """
                result = session.run(query)
                
                mermaid_lines = ["graph TD"]
                
                # Track nodes we've seen to avoid duplicates
                seen_nodes = set()
                
                for record in result:
                    source_name = record['source_name'] or "Unknown"
                    target_name = record['target_name'] or "Unknown"
                    source_type = record['source_labels'][0] if record['source_labels'] else "Node"
                    target_type = record['target_labels'][0] if record['target_labels'] else "Node"
                    rel_type = record['rel_type']
                    
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
                self.logger.log_debug(f"Exported Mermaid format in {duration:.3f}s")
                
                return mermaid_content
                
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Mermaid export failed after {duration:.3f}s: {e}")
            raise
    
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple nodes/links format for basic visualization"""
        start_time = time.time()
        
        try:
            # Get D3 data and simplify it
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
                "summary": {
                    "total_nodes": len(d3_data["nodes"]),
                    "total_edges": len(d3_data["links"]),
                    "exported_at": datetime.now().isoformat()
                }
            }
            
            duration = time.time() - start_time
            self.logger.log_debug(f"Exported simple format in {duration:.3f}s")
            
            return simple_data
            
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Simple export failed after {duration:.3f}s: {e}")
            raise
    
    def export_graphml_format(self) -> str:
        """Export graph in GraphML format"""
        start_time = time.time()
        
        try:
            with self.connection_manager.get_session() as session:
                # Get all nodes
                nodes_query = "MATCH (n) RETURN id(n) as id, labels(n) as labels, properties(n) as properties"
                nodes_result = session.run(nodes_query)
                
                # Get all relationships
                rels_query = "MATCH (a)-[r]->(b) RETURN id(a) as source, id(b) as target, type(r) as type, properties(r) as properties"
                rels_result = session.run(rels_query)
                
                # Build GraphML content
                graphml_lines = [
                    '<?xml version="1.0" encoding="UTF-8"?>',
                    '<graphml xmlns="http://graphml.graphdrawing.org/xmlns" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://graphml.graphdrawing.org/xmlns http://graphml.graphdrawing.org/xmlns/1.0/graphml.xsd">',
                    '  <key id="name" for="node" attr.name="name" attr.type="string"/>',
                    '  <key id="type" for="node" attr.name="type" attr.type="string"/>',
                    '  <key id="rel_type" for="edge" attr.name="type" attr.type="string"/>',
                    '  <graph id="G" edgedefault="directed">'
                ]
                
                # Add nodes
                for record in nodes_result:
                    node_id = record['id']
                    properties = record['properties'] or {}
                    name = properties.get('name', f'Node_{node_id}')
                    node_type = record['labels'][0] if record['labels'] else 'Unknown'
                    
                    graphml_lines.append(f'    <node id="{node_id}">')
                    graphml_lines.append(f'      <data key="name">{name}</data>')
                    graphml_lines.append(f'      <data key="type">{node_type}</data>')
                    graphml_lines.append('    </node>')
                
                # Add edges
                edge_id = 0
                for record in rels_result:
                    source = record['source']
                    target = record['target']
                    rel_type = record['type']
                    
                    graphml_lines.append(f'    <edge id="e{edge_id}" source="{source}" target="{target}">')
                    graphml_lines.append(f'      <data key="rel_type">{rel_type}</data>')
                    graphml_lines.append('    </edge>')
                    edge_id += 1
                
                graphml_lines.extend(['  </graph>', '</graphml>'])
                
                graphml_content = '\n'.join(graphml_lines)
                
                duration = time.time() - start_time
                self.logger.log_debug(f"Exported GraphML format in {duration:.3f}s")
                
                return graphml_content
                
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"GraphML export failed after {duration:.3f}s: {e}")
            raise
    
    def export_cypher_queries(self) -> List[str]:
        """Export graph as Cypher CREATE statements"""
        start_time = time.time()
        
        try:
            with self.connection_manager.get_session() as session:
                # Get all nodes
                nodes_query = "MATCH (n) RETURN id(n) as id, labels(n) as labels, properties(n) as properties"
                nodes_result = session.run(nodes_query)
                
                # Get all relationships
                rels_query = """
                MATCH (a)-[r]->(b) 
                RETURN id(a) as source_id, id(b) as target_id, 
                       a.name as source_name, b.name as target_name,
                       labels(a) as source_labels, labels(b) as target_labels,
                       type(r) as type, properties(r) as properties
                """
                rels_result = session.run(rels_query)
                
                queries = []
                
                # Generate node creation queries
                for record in nodes_result:
                    node_id = record['id']
                    labels = ':'.join(record['labels']) if record['labels'] else 'Node'
                    properties = record['properties'] or {}
                    
                    # Format properties
                    prop_str = ', '.join([f"{k}: {repr(v)}" for k, v in properties.items()])
                    
                    query = f"CREATE (n{node_id}:{labels} {{{prop_str}}})"
                    queries.append(query)
                
                # Generate relationship creation queries
                for record in rels_result:
                    source_id = record['source_id']
                    target_id = record['target_id']
                    rel_type = record['type']
                    properties = record['properties'] or {}
                    
                    # Format properties
                    prop_str = ', '.join([f"{k}: {repr(v)}" for k, v in properties.items()])
                    prop_part = f" {{{prop_str}}}" if prop_str else ""
                    
                    query = f"MATCH (a) WHERE id(a) = {source_id} MATCH (b) WHERE id(b) = {target_id} CREATE (a)-[:{rel_type}{prop_part}]->(b)"
                    queries.append(query)
                
                duration = time.time() - start_time
                self.logger.log_debug(f"Generated {len(queries)} Cypher queries in {duration:.3f}s")
                
                return queries
                
        except Exception as e:
            duration = time.time() - start_time
            self.logger.log_error(f"Cypher export failed after {duration:.3f}s: {e}")
            raise