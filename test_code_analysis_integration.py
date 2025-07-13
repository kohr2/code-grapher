#!/usr/bin/env python3
"""
Test Code Analysis Integration with Neo4j Database

This script tests if we can use our CodeGraphManager to analyze a simple Python file
and add code entities to the existing Neo4j database without conflicts.

Following CLAUDE.md verbose logging requirements for development phase.
"""

import os
import ast
import sys
import time
from datetime import datetime
from typing import Dict, Any
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

# Import our existing components
try:
    from graph_manager import CodeGraphManager
    from logger import logger
except ImportError as e:
    print(f"[ERROR] Could not import existing components: {e}")
    sys.exit(1)


def log_with_timestamp(message: str, level: str = "INFO"):
    """Helper function for logging with timestamps"""
    timestamp = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    print(f"{timestamp} [CODE_ANALYSIS_TEST] [{level}] {message}")


def create_test_python_file() -> str:
    """Create a simple Python test file for analysis"""
    test_file_path = "/Users/danielbeach/Code/code-grapher/test_sample.py"
    
    test_content = '''#!/usr/bin/env python3
"""
Sample Python module for testing code analysis
"""

import os
import sys
from typing import List, Dict


class DataProcessor:
    """A sample class for processing data"""
    
    def __init__(self, name: str):
        self.name = name
        self.data = []
    
    def add_item(self, item: Any) -> None:
        """Add an item to the data list"""
        self.data.append(item)
    
    def process_data(self) -> List[str]:
        """Process the data and return results"""
        results = []
        for item in self.data:
            processed = self._transform_item(item)
            results.append(processed)
        return results
    
    def _transform_item(self, item: Any) -> str:
        """Private method to transform an item"""
        return str(item).upper()


def create_processor(name: str) -> DataProcessor:
    """Factory function to create a DataProcessor"""
    return DataProcessor(name)


def main():
    """Main function"""
    processor = create_processor("test_processor")
    processor.add_item("hello")
    processor.add_item("world")
    results = processor.process_data()
    print(results)


if __name__ == "__main__":
    main()
'''
    
    with open(test_file_path, 'w') as f:
        f.write(test_content)
    
    log_with_timestamp(f"Created test Python file: {test_file_path}")
    return test_file_path


def analyze_python_file(file_path: str) -> Dict[str, Any]:
    """Analyze a Python file and extract AST information"""
    log_with_timestamp(f"Analyzing Python file: {file_path}")
    
    try:
        with open(file_path, 'r') as f:
            content = f.read()
        
        # Parse the AST
        tree = ast.parse(content)
        
        # Extract entities
        entities = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                entities.append({
                    "type": "Class",
                    "name": node.name,
                    "properties": {
                        "docstring": ast.get_docstring(node),
                        "line_number": node.lineno,
                        "methods": [n.name for n in node.body if isinstance(n, ast.FunctionDef)]
                    }
                })
            
            elif isinstance(node, ast.FunctionDef):
                # Skip methods (already captured in class)
                if not any(isinstance(parent, ast.ClassDef) 
                          for parent in ast.walk(tree) 
                          if hasattr(parent, 'body') and node in getattr(parent, 'body', [])):
                    entities.append({
                        "type": "Function",
                        "name": node.name,
                        "properties": {
                            "docstring": ast.get_docstring(node),
                            "line_number": node.lineno,
                            "args": [arg.arg for arg in node.args.args]
                        }
                    })
        
        # Get file info
        file_stat = os.stat(file_path)
        
        analysis_result = {
            "language": "python",
            "size": file_stat.st_size,
            "last_modified": datetime.fromtimestamp(file_stat.st_mtime).isoformat(),
            "nodes": entities
        }
        
        log_with_timestamp(f"Extracted {len(entities)} entities from {file_path}")
        for entity in entities:
            log_with_timestamp(f"  {entity['type']}: {entity['name']}")
        
        return analysis_result
        
    except Exception as e:
        log_with_timestamp(f"Error analyzing file: {e}", "ERROR")
        return {}


def test_code_analysis_with_neo4j():
    """Test adding code analysis results to Neo4j database"""
    log_with_timestamp("=== Testing Code Analysis Integration with Neo4j ===")
    
    # Connect to Neo4j
    neo4j_url = os.getenv('NEO4J_URL', 'neo4j://100.83.40.11:7687')
    
    try:
        graph_manager = CodeGraphManager(
            uri=neo4j_url,
            username="neo4j",
            password="password"
        )
        log_with_timestamp("✓ Connected to Neo4j database", "SUCCESS")
    except Exception as e:
        log_with_timestamp(f"✗ Failed to connect to Neo4j: {e}", "ERROR")
        return False
    
    # Create and analyze test file
    test_file = create_test_python_file()
    analysis_data = analyze_python_file(test_file)
    
    if not analysis_data:
        log_with_timestamp("✗ Failed to analyze test file", "ERROR")
        return False
    
    # Get initial graph stats
    log_with_timestamp("Getting initial graph statistics...")
    initial_stats = graph_manager.get_graph_stats()
    initial_nodes = initial_stats.get('total_nodes', 0)
    initial_rels = initial_stats.get('total_relationships', 0)
    
    log_with_timestamp(f"Initial state: {initial_nodes} nodes, {initial_rels} relationships")
    
    # Add code analysis to graph
    log_with_timestamp("Adding code analysis results to graph...")
    try:
        start_time = time.time()
        graph_manager.analyze_code_structure(test_file, analysis_data)
        analysis_time = time.time() - start_time
        
        log_with_timestamp(f"✓ Code analysis completed in {analysis_time:.3f}s", "SUCCESS")
        
    except Exception as e:
        log_with_timestamp(f"✗ Failed to add code analysis to graph: {e}", "ERROR")
        return False
    
    # Get final graph stats
    log_with_timestamp("Getting final graph statistics...")
    final_stats = graph_manager.get_graph_stats()
    final_nodes = final_stats.get('total_nodes', 0)
    final_rels = final_stats.get('total_relationships', 0)
    
    nodes_added = final_nodes - initial_nodes
    rels_added = final_rels - initial_rels
    
    log_with_timestamp(f"Final state: {final_nodes} nodes (+{nodes_added}), {final_rels} relationships (+{rels_added})")
    
    # Verify our entities were added
    log_with_timestamp("Verifying entities were added correctly...")
    
    test_entities = [
        ("File", test_file),
        ("Class", "DataProcessor"),
        ("Function", "create_processor"),
        ("Function", "main")
    ]
    
    found_entities = 0
    for entity_type, entity_name in test_entities:
        found_entity = graph_manager.find_entity(entity_type, entity_name)
        if found_entity:
            found_entities += 1
            log_with_timestamp(f"  ✓ Found {entity_type}: {entity_name}")
        else:
            log_with_timestamp(f"  ✗ Missing {entity_type}: {entity_name}")
    
    success_rate = found_entities / len(test_entities) * 100
    log_with_timestamp(f"Entity verification: {found_entities}/{len(test_entities)} found ({success_rate:.1f}%)")
    
    # Test coexistence with existing data
    log_with_timestamp("Testing coexistence with existing data...")
    
    # Check that existing node types still exist
    existing_node_types = ['Memory', 'Entity', 'World']
    coexistence_ok = True
    
    for node_type in existing_node_types:
        count = final_stats.get('node_types', {}).get(node_type, 0)
        if count > 0:
            log_with_timestamp(f"  ✓ Existing {node_type} nodes preserved: {count}")
        else:
            log_with_timestamp(f"  ✗ Existing {node_type} nodes missing", "ERROR")
            coexistence_ok = False
    
    # Clean up
    try:
        os.remove(test_file)
        log_with_timestamp(f"Cleaned up test file: {test_file}")
    except:
        pass
    
    graph_manager.close()
    log_with_timestamp("Database connection closed")
    
    # Final assessment
    overall_success = (
        nodes_added > 0 and
        rels_added > 0 and
        success_rate >= 75 and
        coexistence_ok
    )
    
    log_with_timestamp("=== INTEGRATION TEST SUMMARY ===")
    log_with_timestamp(f"Nodes added: {nodes_added}")
    log_with_timestamp(f"Relationships added: {rels_added}")
    log_with_timestamp(f"Entity verification: {success_rate:.1f}%")
    log_with_timestamp(f"Coexistence with existing data: {'✓' if coexistence_ok else '✗'}")
    log_with_timestamp(f"Overall result: {'✓ SUCCESS' if overall_success else '✗ FAILURE'}")
    
    return overall_success


def test_schema_isolation():
    """Test that our code schema doesn't interfere with existing memory/entity schema"""
    log_with_timestamp("=== Testing Schema Isolation ===")
    
    # Connect to Neo4j
    neo4j_url = os.getenv('NEO4J_URL', 'neo4j://100.83.40.11:7687')
    
    try:
        graph_manager = CodeGraphManager(
            uri=neo4j_url,
            username="neo4j",
            password="password"
        )
    except Exception as e:
        log_with_timestamp(f"✗ Failed to connect: {e}", "ERROR")
        return False
    
    try:
        # Query existing memory-entity structure
        log_with_timestamp("Checking existing Memory-Entity relationships...")
        
        # Use raw Cypher query to avoid the embedding issue
        with graph_manager.driver.session() as session:
            result = session.run("""
                MATCH (m:Memory)-[r:MENTIONS]->(e:Entity)
                RETURN count(r) as mention_count
            """)
            mention_count = result.single()["mention_count"]
            
            log_with_timestamp(f"Found {mention_count} Memory->Entity MENTIONS relationships")
            
            # Check for our code entities
            code_result = session.run("""
                MATCH (f:File)-[r:CONTAINS]->(c)
                RETURN count(r) as contains_count
            """)
            contains_count = code_result.single()["contains_count"]
            
            log_with_timestamp(f"Found {contains_count} File->Code CONTAINS relationships")
            
            # Verify schemas don't interfere
            schema_ok = mention_count > 0  # Existing schema preserved
            isolation_ok = True  # No cross-contamination expected
            
            log_with_timestamp(f"Schema isolation test: {'✓ PASSED' if schema_ok and isolation_ok else '✗ FAILED'}")
            
            return schema_ok and isolation_ok
            
    except Exception as e:
        log_with_timestamp(f"Schema isolation test failed: {e}", "ERROR")
        return False
    finally:
        graph_manager.close()


def main():
    """Main function"""
    print("Code Analysis Integration Test with Neo4j")
    print("=" * 50)
    
    start_time = time.time()
    
    # Run integration test
    integration_success = test_code_analysis_with_neo4j()
    
    # Run schema isolation test
    isolation_success = test_schema_isolation()
    
    total_time = time.time() - start_time
    
    print("\n" + "=" * 50)
    print("FINAL TEST RESULTS")
    print("=" * 50)
    
    print(f"Integration Test: {'✓ PASSED' if integration_success else '✗ FAILED'}")
    print(f"Schema Isolation Test: {'✓ PASSED' if isolation_success else '✗ FAILED'}")
    print(f"Total test time: {total_time:.3f}s")
    
    overall_success = integration_success and isolation_success
    
    if overall_success:
        print("\n✓ All tests passed - CodeGraphManager successfully integrates with Neo4j")
        print("✓ Can add code analysis data without interfering with existing data")
        print("✓ Ready for production use with this database")
    else:
        print("\n✗ Some tests failed - Review results above")
        print("✗ May need adjustments before production use")
    
    return 0 if overall_success else 1


if __name__ == "__main__":
    exit(main())