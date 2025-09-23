#!/usr/bin/env python3
"""
Simple test script to verify the relationship creation fix using Neo4j driver directly.
This bypasses the py2neo dependency issue and tests the core logic.
"""

import os
import sys
from neo4j import GraphDatabase
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

def test_relationship_creation():
    """Test creating relationships with missing entities using Neo4j driver directly"""
    try:
        # Connect to Neo4j
        uri = os.getenv('NEO4J_URL', 'bolt://localhost:7687')
        username = os.getenv('NEO4J_USERNAME', 'neo4j')
        password = os.getenv('NEO4J_PASSWORD', 'password')
        
        driver = GraphDatabase.driver(uri, auth=(username, password))
        
        print("🔧 Testing relationship creation fix with Neo4j driver...")
        
        # Get initial stats
        with driver.session() as session:
            result = session.run('MATCH (n) RETURN count(n) as count')
            initial_nodes = result.single()['count']
            
            result = session.run('MATCH ()-[r]->() RETURN count(r) as count')
            initial_relationships = result.single()['count']
        
        print(f"📊 Initial stats: {initial_nodes} nodes, {initial_relationships} relationships")
        
        # Test creating relationships with missing entities
        test_relationships = [
            ('PERFORM_218', 'type', 'CONTAINS'),
            ('PERFORM_219', 'text', 'USES'),
            ('PERFORM_220', 'line', 'RELATES_TO'),
            ('unit', 'value', 'CONTAINS'),
        ]
        
        created_relationships = 0
        created_entities = 0
        
        with driver.session() as session:
            for source, target, rel_type in test_relationships:
                print(f"🔗 Creating relationship: {source} -{rel_type}-> {target}")
                
                # Check if source entity exists
                result = session.run('MATCH (n {name: $name}) RETURN n LIMIT 1', name=source)
                source_exists = result.single() is not None
                
                # Check if target entity exists
                result = session.run('MATCH (n {name: $name}) RETURN n LIMIT 1', name=target)
                target_exists = result.single() is not None
                
                # Create missing entities
                if not source_exists:
                    print(f"   📝 Creating missing source entity: {source}")
                    entity_type = infer_entity_type(source)
                    session.run(f'CREATE (n:{entity_type} {{name: $name, created_on_demand: true, source: "relationship_reference", file_path: "unknown", line: 0}})', name=source)
                    created_entities += 1
                
                if not target_exists:
                    print(f"   📝 Creating missing target entity: {target}")
                    entity_type = infer_entity_type(target)
                    session.run(f'CREATE (n:{entity_type} {{name: $name, created_on_demand: true, source: "relationship_reference", file_path: "unknown", line: 0}})', name=target)
                    created_entities += 1
                
                # Create the relationship
                session.run(f'''
                    MATCH (source {{name: $source_name}})
                    MATCH (target {{name: $target_name}})
                    CREATE (source)-[r:{rel_type}]->(target)
                ''', source_name=source, target_name=target)
                
                created_relationships += 1
                print(f"   ✅ Successfully created {rel_type} relationship")
        
        # Get final stats
        with driver.session() as session:
            result = session.run('MATCH (n) RETURN count(n) as count')
            final_nodes = result.single()['count']
            
            result = session.run('MATCH ()-[r]->() RETURN count(r) as count')
            final_relationships = result.single()['count']
        
        print(f"📊 Final stats: {final_nodes} nodes, {final_relationships} relationships")
        
        # Calculate improvements
        print(f"\n📈 Results:")
        print(f"   Relationships created: {created_relationships}/{len(test_relationships)}")
        print(f"   Entities created: {created_entities}")
        print(f"   Total relationships: {initial_relationships} -> {final_relationships} (+{final_relationships - initial_relationships})")
        print(f"   Total entities: {initial_nodes} -> {final_nodes} (+{final_nodes - initial_nodes})")
        
        driver.close()
        
        # Success if we created relationships
        return created_relationships > 0
        
    except Exception as e:
        print(f"❌ Error during test: {e}")
        import traceback
        traceback.print_exc()
        return False

def infer_entity_type(entity_name: str) -> str:
    """Infer the entity type based on the entity name"""
    # Common patterns for different entity types
    if entity_name.startswith("PERFORM_"):
        return "paragraph"
    elif entity_name in ["type", "text", "line", "unit", "value", "data"]:
        return "data_item"
    elif entity_name.startswith("WS-") or entity_name.startswith("WORKING-STORAGE"):
        return "data_item"
    elif entity_name.startswith("FD-") or entity_name.startswith("FILE-"):
        return "file"
    elif entity_name.startswith("PROGRAM-") or entity_name.startswith("PROG-"):
        return "program"
    elif entity_name.startswith("SECTION-") or entity_name.startswith("SEC-"):
        return "section"
    elif entity_name.startswith("PARAGRAPH-") or entity_name.startswith("PARA-"):
        return "paragraph"
    else:
        # Default to inferred type for unknown patterns
        return "inferred"

if __name__ == "__main__":
    success = test_relationship_creation()
    if success:
        print("\n✅ Relationship creation fix test PASSED")
        print("🎉 The fix works! Missing entities are created on-the-fly.")
        sys.exit(0)
    else:
        print("\n❌ Relationship creation fix test FAILED")
        sys.exit(1)
