#!/usr/bin/env python3
"""
Direct test of Neo4j connection and simple retrieval
"""

import os
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).parent))

# Load environment variables
from dotenv import load_dotenv
load_dotenv()

from neo4j import GraphDatabase

def test_direct_neo4j():
    """Test direct Neo4j connection"""
    print("🔍 Testing Direct Neo4j Connection")
    print("=" * 50)
    
    # Connection details
    neo4j_url = os.getenv("NEO4J_URL", "neo4j://100.83.40.11:7687")
    neo4j_username = os.getenv("NEO4J_USERNAME", "neo4j")
    neo4j_password = os.getenv("NEO4J_PASSWORD", "password")
    
    print(f"Connecting to: {neo4j_url}")
    print(f"Username: {neo4j_username}")
    
    try:
        # Create driver
        driver = GraphDatabase.driver(neo4j_url, auth=(neo4j_username, neo4j_password))
        
        # Test connection
        with driver.session() as session:
            # Basic test
            result = session.run("RETURN 1 as test")
            test_value = result.single()["test"]
            print(f"✅ Connection successful! Test value: {test_value}")
            
            # Get node counts
            result = session.run("MATCH (n) RETURN labels(n) as labels, count(n) as count")
            node_counts = {}
            total_nodes = 0
            for record in result:
                labels = record["labels"]
                count = record["count"]
                if labels:
                    label = labels[0]  # Primary label
                    node_counts[label] = count
                    total_nodes += count
            
            print(f"\n📊 Database Contents:")
            print(f"   Total nodes: {total_nodes}")
            for label, count in sorted(node_counts.items(), key=lambda x: x[1], reverse=True):
                print(f"   • {label}: {count}")
            
            # Get relationship counts
            result = session.run("MATCH ()-[r]->() RETURN type(r) as rel_type, count(r) as count")
            rel_counts = {}
            total_rels = 0
            for record in result:
                rel_type = record["rel_type"]
                count = record["count"]
                rel_counts[rel_type] = count
                total_rels += count
            
            print(f"\n🔗 Relationships:")
            print(f"   Total relationships: {total_rels}")
            for rel_type, count in sorted(rel_counts.items(), key=lambda x: x[1], reverse=True):
                print(f"   • {rel_type}: {count}")
            
            # Sample some entities
            print(f"\n📝 Sample Entities:")
            result = session.run("""
                MATCH (n) 
                WHERE labels(n)[0] IN ['Memory', 'Entity', 'File', 'Class', 'Function']
                RETURN labels(n)[0] as type,
                       COALESCE(n.name, n.title, n.content, 'Unknown')[..50] as name,
                       id(n) as node_id
                LIMIT 10
            """)
            
            for record in result:
                node_type = record["type"]
                name = record["name"]
                node_id = record["node_id"]
                print(f"   • {node_type}: {name} (ID: {node_id})")
            
            # Look for specific entities that might be useful for retrieval
            print(f"\n🔍 Looking for entities that mention 'code' or 'analysis':")
            result = session.run("""
                MATCH (n)
                WHERE toLower(toString(n.name)) CONTAINS 'code' 
                   OR toLower(toString(n.content)) CONTAINS 'code'
                   OR toLower(toString(n.name)) CONTAINS 'analysis'
                   OR toLower(toString(n.content)) CONTAINS 'analysis'
                RETURN labels(n)[0] as type,
                       COALESCE(n.name, n.title, n.content, 'Unknown')[..100] as description,
                       id(n) as node_id
                LIMIT 5
            """)
            
            found_relevant = False
            for record in result:
                found_relevant = True
                node_type = record["type"]
                description = record["description"]
                node_id = record["node_id"]
                print(f"   • {node_type}: {description} (ID: {node_id})")
            
            if not found_relevant:
                print("   ❌ No entities found containing 'code' or 'analysis'")
            
            # Check for recent nodes (if timestamp exists)
            print(f"\n🕒 Checking for recent activity:")
            result = session.run("""
                MATCH (n)
                WHERE n.timestamp IS NOT NULL OR n.created_at IS NOT NULL
                RETURN labels(n)[0] as type,
                       COALESCE(n.timestamp, n.created_at) as timestamp,
                       COALESCE(n.name, n.title, 'Unknown')[..50] as name
                ORDER BY COALESCE(n.timestamp, n.created_at) DESC
                LIMIT 5
            """)
            
            recent_found = False
            for record in result:
                recent_found = True
                node_type = record["type"]
                timestamp = record["timestamp"]
                name = record["name"]
                print(f"   • {node_type}: {name} ({timestamp})")
            
            if not recent_found:
                print("   ℹ️  No timestamped entities found")
        
        driver.close()
        return True, node_counts, rel_counts
        
    except Exception as e:
        print(f"❌ Connection failed: {e}")
        return False, {}, {}

def test_simple_vector_search():
    """Test if ChromaDB has any content"""
    print(f"\n🤖 Testing ChromaDB Vector Store")
    print("=" * 50)
    
    try:
        import chromadb
        from chromadb.config import Settings
        
        # Initialize ChromaDB (same as RAG pipeline)
        client = chromadb.Client(Settings(
            chroma_db_impl="duckdb+parquet",
            persist_directory="./chroma_db"
        ))
        
        # List collections
        collections = client.list_collections()
        print(f"📚 ChromaDB Collections: {len(collections)}")
        
        if collections:
            for collection in collections:
                print(f"   • {collection.name}: {collection.count()} items")
                
                # Sample some items from the first collection
                if collection.count() > 0:
                    results = collection.query(
                        query_texts=["code"],
                        n_results=min(3, collection.count())
                    )
                    
                    print(f"     Sample items (query: 'code'):")
                    for i, doc in enumerate(results.get('documents', [[]])[0]):
                        content_preview = doc[:100].replace('\n', ' ')
                        distance = results.get('distances', [[]])[0][i] if results.get('distances') else 'N/A'
                        print(f"       {i+1}. Distance: {distance}")
                        print(f"          Content: {content_preview}...")
        else:
            print("   ❌ No collections found in ChromaDB")
            return False
            
        return True
        
    except Exception as e:
        print(f"❌ ChromaDB test failed: {e}")
        return False

def main():
    """Run all tests"""
    print("🚀 Direct Database Connection Test")
    print("=" * 60)
    
    # Test Neo4j
    neo4j_success, node_counts, rel_counts = test_direct_neo4j()
    
    # Test ChromaDB
    chroma_success = test_simple_vector_search()
    
    # Summary
    print(f"\n📊 Connection Test Summary")
    print("=" * 50)
    
    if neo4j_success:
        print("✅ Neo4j: Connected successfully")
        total_nodes = sum(node_counts.values())
        total_rels = sum(rel_counts.values())
        print(f"   • Nodes: {total_nodes}")
        print(f"   • Relationships: {total_rels}")
        
        # Check if this looks like it has code data
        code_indicators = ['File', 'Class', 'Function', 'Method']
        has_code_data = any(label in node_counts for label in code_indicators)
        
        if has_code_data:
            print("   • Contains code-related data ✅")
        else:
            print("   • No code entities found (Memory/Entity system only)")
    else:
        print("❌ Neo4j: Connection failed")
    
    if chroma_success:
        print("✅ ChromaDB: Content found")
    else:
        print("❌ ChromaDB: No content or connection failed")
    
    # Recommendations
    print(f"\n💡 Recommendations:")
    if neo4j_success and chroma_success:
        print("   • Try simple retrieval queries on existing data")
        print("   • Use MLflow dashboard to see agent execution history")
    elif neo4j_success and not chroma_success:
        print("   • Neo4j has data, but ChromaDB is empty")
        print("   • Try running RAG indexer to populate vector store")
    elif not neo4j_success:
        print("   • Fix Neo4j connection first")
        print("   • Check if remote database is accessible")
    
    return neo4j_success, chroma_success

if __name__ == "__main__":
    main()