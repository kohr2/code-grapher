#!/usr/bin/env python3
"""
Clear both Neo4j and ChromaDB databases and verify they're empty
"""

import os
import shutil
from pathlib import Path
from graph_manager import CodeGraphManager

def clear_neo4j_database():
    """Clear all data from Neo4j database"""
    print("🗑️  Clearing Neo4j database...")
    
    try:
        graph_manager = CodeGraphManager()
        
        # Clear all nodes and relationships
        clear_query = "MATCH (n) DETACH DELETE n"
        result = graph_manager.graph.run(clear_query)
        
        # Verify it's empty
        count_query = "MATCH (n) RETURN count(n) as count"
        result = graph_manager.graph.run(count_query)
        count = list(result)[0]["count"]
        
        print(f"   ✅ Neo4j database cleared. Remaining nodes: {count}")
        
        graph_manager.close()
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
        graph_manager = CodeGraphManager()
        
        # Get node count
        node_count_query = "MATCH (n) RETURN count(n) as count"
        node_result = graph_manager.graph.run(node_count_query)
        node_count = list(node_result)[0]["count"]
        
        # Get relationship count
        rel_count_query = "MATCH ()-[r]->() RETURN count(r) as count"
        rel_result = graph_manager.graph.run(rel_count_query)
        rel_count = list(rel_result)[0]["count"]
        
        print(f"   📈 Neo4j nodes: {node_count}")
        print(f"   🔗 Neo4j relationships: {rel_count}")
        
        neo4j_empty = node_count == 0 and rel_count == 0
        
        graph_manager.close()
        
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
    
    # Clear databases
    neo4j_cleared = clear_neo4j_database()
    chroma_cleared = clear_chroma_database()
    
    # Verify they're empty
    all_empty = verify_databases_empty()
    
    # Final summary
    print(f"\n🏁 Cleanup Complete:")
    print(f"   • Neo4j cleared: {'✅' if neo4j_cleared else '❌'}")
    print(f"   • ChromaDB cleared: {'✅' if chroma_cleared else '❌'}")
    print(f"   • All databases empty: {'✅' if all_empty else '❌'}")
    
    if all_empty:
        print("🎉 Both databases successfully cleared!")
    else:
        print("⚠️  Some databases may not be completely empty")


if __name__ == "__main__":
    main()