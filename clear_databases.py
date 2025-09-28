#!/usr/bin/env python3
"""
Clear both Neo4j and ChromaDB databases and verify they're empty
"""

import os
import shutil
from pathlib import Path
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

def clear_neo4j_database():
    """Clear all data from Neo4j database"""
    print("🗑️  Clearing Neo4j database...")
    
    try:
        # Get database configuration from environment
        database_name = os.getenv("NEO4J_DATABASE", "neo4j")
        uri = os.getenv("NEO4J_URL", "bolt://localhost:7687")
        username = os.getenv("NEO4J_USERNAME", "neo4j")
        password = os.getenv("NEO4J_PASSWORD", "password")
        
        print(f"   📊 Using database: {database_name}")
        print(f"   🔗 Using URI: {uri}")
        
        # Use Neo4j driver directly with database parameter
        from neo4j import GraphDatabase
        driver = GraphDatabase.driver(uri, auth=(username, password))
        
        # Clear all nodes and relationships in the specific database
        with driver.session(database=database_name) as session:
            # Clear all nodes and relationships
            clear_query = "MATCH (n) DETACH DELETE n"
            result = session.run(clear_query)
            print(f"   🗑️  Cleared all nodes and relationships")
            
            # Clear any constraints and indexes (optional)
            try:
                # Drop all constraints
                constraints_query = "SHOW CONSTRAINTS"
                constraints = session.run(constraints_query)
                constraint_count = 0
                for constraint in constraints:
                    constraint_name = constraint.get('name', '')
                    if constraint_name:
                        drop_constraint_query = f"DROP CONSTRAINT {constraint_name}"
                        session.run(drop_constraint_query)
                        constraint_count += 1
                
                if constraint_count > 0:
                    print(f"   🗑️  Dropped {constraint_count} constraints")
                
                # Drop all indexes
                indexes_query = "SHOW INDEXES"
                indexes = session.run(indexes_query)
                index_count = 0
                for index in indexes:
                    index_name = index.get('name', '')
                    if index_name and not index_name.startswith('system'):
                        drop_index_query = f"DROP INDEX {index_name}"
                        session.run(drop_index_query)
                        index_count += 1
                
                if index_count > 0:
                    print(f"   🗑️  Dropped {index_count} indexes")
                    
            except Exception as e:
                print(f"   ⚠️  Warning: Could not clear constraints/indexes: {e}")
            
            # Verify it's empty
            count_query = "MATCH (n) RETURN count(n) as count"
            result = session.run(count_query)
            count = result.single()["count"]
            
            print(f"   ✅ Neo4j database '{database_name}' cleared. Remaining nodes: {count}")
        
        driver.close()
        return count == 0
        
    except Exception as e:
        print(f"   ❌ Failed to clear Neo4j database: {e}")
        return False


def clear_chroma_database():
    """Clear ChromaDB database"""
    print("🗑️  Clearing ChromaDB database...")
    
    try:
        # Default ChromaDB path from RAG indexer agent
        chroma_path = "./chroma_db"
        
        if os.path.exists(chroma_path):
            shutil.rmtree(chroma_path)
            print(f"   ✅ ChromaDB directory removed: {chroma_path}")
        else:
            print(f"   ℹ️  ChromaDB directory doesn't exist: {chroma_path}")
        
        # Verify it's gone
        exists = os.path.exists(chroma_path)
        print(f"   ✅ ChromaDB cleared. Directory exists: {exists}")
        
        return not exists
        
    except Exception as e:
        print(f"   ❌ Failed to clear ChromaDB: {e}")
        return False


def verify_databases_empty():
    """Verify both databases are empty"""
    print("\n📊 Verifying databases are empty...")
    
    # Check Neo4j
    try:
        database_name = os.getenv("NEO4J_DATABASE", "neo4j")
        uri = os.getenv("NEO4J_URL", "bolt://localhost:7687")
        username = os.getenv("NEO4J_USERNAME", "neo4j")
        password = os.getenv("NEO4J_PASSWORD", "password")
        
        print(f"   📊 Checking Neo4j database: {database_name}")
        
        # Use Neo4j driver directly with database parameter
        from neo4j import GraphDatabase
        driver = GraphDatabase.driver(uri, auth=(username, password))
        
        with driver.session(database=database_name) as session:
            # Get node count
            node_count_query = "MATCH (n) RETURN count(n) as count"
            node_result = session.run(node_count_query)
            node_count = node_result.single()["count"]
            
            # Get relationship count
            rel_count_query = "MATCH ()-[r]->() RETURN count(r) as count"
            rel_result = session.run(rel_count_query)
            rel_count = rel_result.single()["count"]
            
            print(f"   📈 Neo4j nodes: {node_count}")
            print(f"   🔗 Neo4j relationships: {rel_count}")
            
            neo4j_empty = node_count == 0 and rel_count == 0
        
        driver.close()
        
    except Exception as e:
        print(f"   ❌ Failed to check Neo4j: {e}")
        neo4j_empty = False
    
    # Check ChromaDB
    chroma_path = "./chroma_db"
    chroma_empty = not os.path.exists(chroma_path)
    print(f"   💾 ChromaDB directory exists: {not chroma_empty}")
    
    # Summary
    print(f"\n🎯 Database Status:")
    print(f"   • Neo4j empty: {'✅' if neo4j_empty else '❌'}")
    print(f"   • ChromaDB empty: {'✅' if chroma_empty else '❌'}")
    
    return neo4j_empty and chroma_empty


def main():
    """Main execution function"""
    print("🧹 Database Cleaner")
    print("=" * 30)
    
    # Show configuration
    neo4j_database = os.getenv("NEO4J_DATABASE", "neo4j")
    neo4j_url = os.getenv("NEO4J_URL", "bolt://localhost:7687")
    print(f"📊 Neo4j Database: {neo4j_database}")
    print(f"🔗 Neo4j URL: {neo4j_url}")
    print(f"💾 ChromaDB Path: ./chroma_db")
    print()
    
    # Clear databases
    neo4j_cleared = clear_neo4j_database()
    chroma_cleared = clear_chroma_database()
    
    # Verify they're empty
    all_empty = verify_databases_empty()
    
    # Final summary
    print(f"\n🏁 Cleanup Complete:")
    print(f"   • Neo4j database '{neo4j_database}' cleared: {'✅' if neo4j_cleared else '❌'}")
    print(f"   • ChromaDB cleared: {'✅' if chroma_cleared else '❌'}")
    print(f"   • All databases empty: {'✅' if all_empty else '❌'}")
    
    if all_empty:
        print("🎉 Both databases successfully cleared!")
    else:
        print("⚠️  Some databases may not be completely empty")


if __name__ == "__main__":
    main()