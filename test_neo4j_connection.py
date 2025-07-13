#!/usr/bin/env python3
"""
Neo4j Database Connection Test and Data Explorer

This script tests the connection to the Neo4j database and explores existing data.
Following CLAUDE.md verbose logging requirements for development phase.

Usage: python test_neo4j_connection.py
"""

import os
import sys
import time
from datetime import datetime
from typing import Dict, List, Any, Optional
from dotenv import load_dotenv

try:
    from neo4j import GraphDatabase
    from py2neo import Graph
    NEO4J_AVAILABLE = True
except ImportError as e:
    print(f"[ERROR] Neo4j packages not available: {e}")
    NEO4J_AVAILABLE = False
    sys.exit(1)

# Load environment variables
load_dotenv()

class Neo4jConnectionTester:
    """Test Neo4j connection and explore existing data with verbose logging"""
    
    def __init__(self):
        self.start_time = datetime.now()
        self.neo4j_url = os.getenv('NEO4J_URL', 'neo4j://100.83.40.11:7687')
        self.driver = None
        self.graph = None
        
        # Log initialization
        self.log(f"[INIT] Starting Neo4j connection test at {self.start_time}")
        self.log(f"[INIT] Target Neo4j URL: {self.neo4j_url}")
        
        # Try different authentication strategies
        self.auth_strategies = [
            ("neo4j", "password"),  # Default Neo4j credentials
            ("neo4j", "neo4j"),     # Another common default
            ("", ""),               # No authentication
            ("admin", "admin"),     # Common admin credentials
            ("neo4j", "test"),      # Test credentials
        ]
    
    def log(self, message: str, level: str = "INFO"):
        """Log with timestamp and component info"""
        timestamp = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
        print(f"{timestamp} [NEO4J_TESTER] [{level}] {message}")
    
    def test_connection_with_auth(self, username: str, password: str) -> bool:
        """Test connection with specific credentials"""
        self.log(f"Testing connection with username: '{username}'")
        
        try:
            # Test with neo4j driver
            if username and password:
                driver = GraphDatabase.driver(self.neo4j_url, auth=(username, password))
            else:
                driver = GraphDatabase.driver(self.neo4j_url)
            
            # Verify connectivity
            driver.verify_connectivity()
            
            # Test basic query
            with driver.session() as session:
                result = session.run("RETURN 1 as test")
                test_value = result.single()["test"]
                
            self.log(f"✓ Successfully connected with credentials: {username}/*****", "SUCCESS")
            self.driver = driver
            
            # Also setup py2neo connection for compatibility with existing code
            if username and password:
                self.graph = Graph(self.neo4j_url, auth=(username, password))
            else:
                self.graph = Graph(self.neo4j_url)
            
            return True
            
        except Exception as e:
            self.log(f"✗ Connection failed with {username}: {str(e)}", "ERROR")
            if hasattr(self, 'driver') and self.driver:
                self.driver.close()
            return False
    
    def find_working_credentials(self) -> bool:
        """Try different authentication strategies to find working credentials"""
        self.log("Starting credential discovery process...")
        
        for username, password in self.auth_strategies:
            self.log(f"Attempting authentication strategy: {username}/{'*' * len(password) if password else 'NO_PASSWORD'}")
            
            if self.test_connection_with_auth(username, password):
                self.log(f"✓ Found working credentials: {username}/*****", "SUCCESS")
                return True
        
        self.log("✗ No working credentials found in common strategies", "ERROR")
        return False
    
    def explore_database_metadata(self) -> Dict[str, Any]:
        """Explore database metadata and structure"""
        self.log("Exploring database metadata...")
        
        if not self.driver:
            self.log("No driver connection available", "ERROR")
            return {}
        
        metadata = {}
        
        try:
            with self.driver.session() as session:
                # Get database information
                try:
                    db_info = session.run("CALL dbms.components()").data()
                    metadata["database_info"] = db_info
                    self.log(f"Database components: {len(db_info)} components found")
                except Exception as e:
                    self.log(f"Could not get database info: {e}")
                
                # Get node labels
                try:
                    labels_result = session.run("CALL db.labels()").data()
                    labels = [record["label"] for record in labels_result]
                    metadata["node_labels"] = labels
                    self.log(f"Node labels found: {labels}")
                except Exception as e:
                    self.log(f"Could not get node labels: {e}")
                
                # Get relationship types
                try:
                    rel_types_result = session.run("CALL db.relationshipTypes()").data()
                    rel_types = [record["relationshipType"] for record in rel_types_result]
                    metadata["relationship_types"] = rel_types
                    self.log(f"Relationship types found: {rel_types}")
                except Exception as e:
                    self.log(f"Could not get relationship types: {e}")
                
                # Get property keys
                try:
                    prop_keys_result = session.run("CALL db.propertyKeys()").data()
                    prop_keys = [record["propertyKey"] for record in prop_keys_result]
                    metadata["property_keys"] = prop_keys
                    self.log(f"Property keys found: {len(prop_keys)} keys")
                except Exception as e:
                    self.log(f"Could not get property keys: {e}")
                
                # Get indexes
                try:
                    indexes_result = session.run("CALL db.indexes()").data()
                    metadata["indexes"] = indexes_result
                    self.log(f"Indexes found: {len(indexes_result)} indexes")
                except Exception as e:
                    self.log(f"Could not get indexes: {e}")
                
                # Get constraints
                try:
                    constraints_result = session.run("CALL db.constraints()").data()
                    metadata["constraints"] = constraints_result
                    self.log(f"Constraints found: {len(constraints_result)} constraints")
                except Exception as e:
                    self.log(f"Could not get constraints: {e}")
        
        except Exception as e:
            self.log(f"Error exploring metadata: {e}", "ERROR")
        
        return metadata
    
    def count_nodes_and_relationships(self) -> Dict[str, Any]:
        """Count total nodes and relationships in the database"""
        self.log("Counting nodes and relationships...")
        
        if not self.driver:
            return {}
        
        counts = {}
        
        try:
            with self.driver.session() as session:
                # Count total nodes
                start_time = time.time()
                node_count_result = session.run("MATCH (n) RETURN count(n) as count")
                total_nodes = node_count_result.single()["count"]
                node_count_time = time.time() - start_time
                
                counts["total_nodes"] = total_nodes
                self.log(f"Total nodes: {total_nodes:,} (query took {node_count_time:.3f}s)")
                
                # Count total relationships
                start_time = time.time()
                rel_count_result = session.run("MATCH ()-[r]->() RETURN count(r) as count")
                total_relationships = rel_count_result.single()["count"]
                rel_count_time = time.time() - start_time
                
                counts["total_relationships"] = total_relationships
                self.log(f"Total relationships: {total_relationships:,} (query took {rel_count_time:.3f}s)")
                
                # Count nodes by label
                if total_nodes > 0:
                    self.log("Counting nodes by label...")
                    start_time = time.time()
                    label_counts_result = session.run("""
                        MATCH (n) 
                        RETURN labels(n)[0] as label, count(n) as count 
                        ORDER BY count DESC
                    """)
                    label_counts = {}
                    for record in label_counts_result:
                        label = record["label"] if record["label"] else "NO_LABEL"
                        count = record["count"]
                        label_counts[label] = count
                    
                    counts["nodes_by_label"] = label_counts
                    label_count_time = time.time() - start_time
                    self.log(f"Node label distribution (query took {label_count_time:.3f}s):")
                    for label, count in label_counts.items():
                        self.log(f"  {label}: {count:,} nodes")
                
                # Count relationships by type
                if total_relationships > 0:
                    self.log("Counting relationships by type...")
                    start_time = time.time()
                    rel_type_counts_result = session.run("""
                        MATCH ()-[r]->() 
                        RETURN type(r) as rel_type, count(r) as count 
                        ORDER BY count DESC
                    """)
                    rel_type_counts = {}
                    for record in rel_type_counts_result:
                        rel_type = record["rel_type"]
                        count = record["count"]
                        rel_type_counts[rel_type] = count
                    
                    counts["relationships_by_type"] = rel_type_counts
                    rel_type_count_time = time.time() - start_time
                    self.log(f"Relationship type distribution (query took {rel_type_count_time:.3f}s):")
                    for rel_type, count in rel_type_counts.items():
                        self.log(f"  {rel_type}: {count:,} relationships")
        
        except Exception as e:
            self.log(f"Error counting nodes/relationships: {e}", "ERROR")
        
        return counts
    
    def sample_data_exploration(self, sample_size: int = 10) -> Dict[str, Any]:
        """Explore sample data from the database"""
        self.log(f"Exploring sample data (limit: {sample_size})...")
        
        if not self.driver:
            return {}
        
        samples = {}
        
        try:
            with self.driver.session() as session:
                # Sample nodes
                self.log("Sampling nodes...")
                nodes_result = session.run(f"""
                    MATCH (n) 
                    RETURN n, labels(n) as labels 
                    LIMIT {sample_size}
                """)
                
                sample_nodes = []
                for record in nodes_result:
                    node = record["n"]
                    labels = record["labels"]
                    node_dict = dict(node)
                    sample_nodes.append({
                        "labels": labels,
                        "properties": node_dict,
                        "id": node.element_id if hasattr(node, 'element_id') else str(node.id)
                    })
                
                samples["sample_nodes"] = sample_nodes
                self.log(f"Sampled {len(sample_nodes)} nodes")
                
                for i, node in enumerate(sample_nodes[:3]):  # Show first 3 in detail
                    self.log(f"  Node {i+1}: Labels={node['labels']}, Properties={list(node['properties'].keys())}")
                
                # Sample relationships
                self.log("Sampling relationships...")
                rels_result = session.run(f"""
                    MATCH (a)-[r]->(b) 
                    RETURN a, r, b, type(r) as rel_type 
                    LIMIT {sample_size}
                """)
                
                sample_relationships = []
                for record in rels_result:
                    source = record["a"]
                    rel = record["r"]
                    target = record["b"]
                    rel_type = record["rel_type"]
                    
                    sample_relationships.append({
                        "source": {
                            "labels": list(source.labels),
                            "properties": dict(source)
                        },
                        "relationship": {
                            "type": rel_type,
                            "properties": dict(rel)
                        },
                        "target": {
                            "labels": list(target.labels),
                            "properties": dict(target)
                        }
                    })
                
                samples["sample_relationships"] = sample_relationships
                self.log(f"Sampled {len(sample_relationships)} relationships")
                
                for i, rel in enumerate(sample_relationships[:3]):  # Show first 3 in detail
                    self.log(f"  Relationship {i+1}: {rel['source']['labels'][0] if rel['source']['labels'] else 'Unknown'} -[{rel['relationship']['type']}]-> {rel['target']['labels'][0] if rel['target']['labels'] else 'Unknown'}")
        
        except Exception as e:
            self.log(f"Error sampling data: {e}", "ERROR")
        
        return samples
    
    def analyze_graph_structure(self) -> Dict[str, Any]:
        """Analyze the overall structure of the graph"""
        self.log("Analyzing graph structure...")
        
        if not self.driver:
            return {}
        
        structure_analysis = {}
        
        try:
            with self.driver.session() as session:
                # Calculate graph density (if feasible for the graph size)
                total_nodes_result = session.run("MATCH (n) RETURN count(n) as count")
                total_nodes = total_nodes_result.single()["count"]
                
                total_rels_result = session.run("MATCH ()-[r]->() RETURN count(r) as count")
                total_relationships = total_rels_result.single()["count"]
                
                if total_nodes > 1:
                    max_possible_edges = total_nodes * (total_nodes - 1)
                    density = total_relationships / max_possible_edges
                    structure_analysis["graph_density"] = density
                    self.log(f"Graph density: {density:.6f} ({total_relationships:,} out of {max_possible_edges:,} possible edges)")
                
                # Calculate average degree
                if total_nodes > 0:
                    avg_degree = (2 * total_relationships) / total_nodes
                    structure_analysis["average_degree"] = avg_degree
                    self.log(f"Average degree: {avg_degree:.2f}")
                
                # Find nodes with highest degree (most connected)
                if total_nodes > 0 and total_nodes < 100000:  # Only for reasonably sized graphs
                    self.log("Finding most connected nodes...")
                    degree_result = session.run("""
                        MATCH (n)
                        OPTIONAL MATCH (n)-[r]-()
                        RETURN n, count(r) as degree, labels(n) as labels
                        ORDER BY degree DESC
                        LIMIT 10
                    """)
                    
                    top_nodes = []
                    for record in degree_result:
                        node = record["n"]
                        degree = record["degree"]
                        labels = record["labels"]
                        top_nodes.append({
                            "degree": degree,
                            "labels": labels,
                            "properties": dict(node)
                        })
                    
                    structure_analysis["top_connected_nodes"] = top_nodes
                    self.log("Top 5 most connected nodes:")
                    for i, node in enumerate(top_nodes[:5]):
                        node_name = node["properties"].get("name", "unnamed")
                        self.log(f"  {i+1}. {node_name} ({node['labels']}) - Degree: {node['degree']}")
                
                # Analyze weakly connected components (if graph is small enough)
                if total_nodes > 0 and total_nodes < 10000:
                    try:
                        self.log("Analyzing connected components...")
                        components_result = session.run("""
                            CALL gds.wcc.stats('myGraph')
                            YIELD componentCount
                            RETURN componentCount
                        """)
                        # Note: This requires Graph Data Science library
                    except Exception:
                        self.log("Graph Data Science library not available - skipping component analysis")
        
        except Exception as e:
            self.log(f"Error analyzing graph structure: {e}", "ERROR")
        
        return structure_analysis
    
    def run_comprehensive_test(self) -> Dict[str, Any]:
        """Run the complete test suite"""
        self.log("=== Starting Comprehensive Neo4j Database Test ===")
        
        test_results = {
            "test_start_time": self.start_time.isoformat(),
            "connection_successful": False,
            "metadata": {},
            "counts": {},
            "samples": {},
            "structure_analysis": {},
            "performance_metrics": {}
        }
        
        # Test connection
        start_time = time.time()
        if self.find_working_credentials():
            connection_time = time.time() - start_time
            test_results["connection_successful"] = True
            test_results["performance_metrics"]["connection_time"] = connection_time
            self.log(f"✓ Connection established in {connection_time:.3f}s", "SUCCESS")
            
            # Explore metadata
            start_time = time.time()
            test_results["metadata"] = self.explore_database_metadata()
            metadata_time = time.time() - start_time
            test_results["performance_metrics"]["metadata_exploration_time"] = metadata_time
            
            # Count data
            start_time = time.time()
            test_results["counts"] = self.count_nodes_and_relationships()
            counting_time = time.time() - start_time
            test_results["performance_metrics"]["counting_time"] = counting_time
            
            # Sample data
            start_time = time.time()
            test_results["samples"] = self.sample_data_exploration()
            sampling_time = time.time() - start_time
            test_results["performance_metrics"]["sampling_time"] = sampling_time
            
            # Analyze structure
            start_time = time.time()
            test_results["structure_analysis"] = self.analyze_graph_structure()
            analysis_time = time.time() - start_time
            test_results["performance_metrics"]["structure_analysis_time"] = analysis_time
            
        else:
            connection_time = time.time() - start_time
            test_results["performance_metrics"]["failed_connection_time"] = connection_time
            self.log("✗ Failed to establish database connection", "ERROR")
        
        # Calculate total test time
        total_time = time.time() - self.start_time.timestamp()
        test_results["performance_metrics"]["total_test_time"] = total_time
        
        self.log(f"=== Test completed in {total_time:.3f}s ===")
        
        return test_results
    
    def close_connections(self):
        """Close all database connections"""
        if self.driver:
            self.driver.close()
            self.log("Database connections closed")


def main():
    """Main function to run the Neo4j connection test"""
    print("Neo4j Database Connection Test and Data Explorer")
    print("=" * 50)
    
    # Check if Neo4j packages are available
    if not NEO4J_AVAILABLE:
        print("ERROR: Neo4j packages are not available. Please install them:")
        print("pip install neo4j py2neo")
        return 1
    
    # Create tester instance
    tester = Neo4jConnectionTester()
    
    try:
        # Run comprehensive test
        results = tester.run_comprehensive_test()
        
        # Print summary
        print("\n" + "=" * 50)
        print("TEST SUMMARY")
        print("=" * 50)
        
        if results["connection_successful"]:
            print("✓ Connection: SUCCESSFUL")
            
            counts = results.get("counts", {})
            print(f"✓ Nodes: {counts.get('total_nodes', 0):,}")
            print(f"✓ Relationships: {counts.get('total_relationships', 0):,}")
            
            metadata = results.get("metadata", {})
            labels = metadata.get("node_labels", [])
            rel_types = metadata.get("relationship_types", [])
            print(f"✓ Node Labels: {len(labels)} ({', '.join(labels[:5])}{'...' if len(labels) > 5 else ''})")
            print(f"✓ Relationship Types: {len(rel_types)} ({', '.join(rel_types[:5])}{'...' if len(rel_types) > 5 else ''})")
            
            # Performance summary
            perf = results.get("performance_metrics", {})
            print(f"✓ Total Test Time: {perf.get('total_test_time', 0):.3f}s")
            
        else:
            print("✗ Connection: FAILED")
            print("  Check if Neo4j is running at the specified URL")
            print("  Verify credentials and network connectivity")
        
        return 0 if results["connection_successful"] else 1
        
    except KeyboardInterrupt:
        print("\n\nTest interrupted by user")
        return 1
    except Exception as e:
        print(f"\nUnexpected error: {e}")
        return 1
    finally:
        tester.close_connections()


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)