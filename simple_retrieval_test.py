#!/usr/bin/env python3
"""
Simple test to check what's in the knowledge graph and test retrieval
"""

import sys
import json
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from graph_manager import CodeGraphManager
from rag_pipeline import CodeRAGPipeline

def check_graph_contents():
    """Check what's currently in the Neo4j graph"""
    print("🔍 Checking Neo4j Graph Contents")
    print("=" * 50)
    
    try:
        graph_manager = CodeGraphManager()
        
        # Get basic stats
        stats = graph_manager.get_graph_statistics()
        print(f"📊 Graph Statistics:")
        print(f"   • Total nodes: {stats.get('total_nodes', 0)}")
        print(f"   • Total relationships: {stats.get('total_relationships', 0)}")
        print(f"   • Node types: {stats.get('node_types', {})}")
        print(f"   • Relationship types: {stats.get('relationship_types', {})}")
        
        # Check for code-related nodes
        print(f"\n🔍 Looking for code entities...")
        code_query = """
        MATCH (n)
        WHERE labels(n)[0] IN ['File', 'Class', 'Function', 'Method', 'Variable']
        RETURN labels(n)[0] as type, count(n) as count
        ORDER BY count DESC
        """
        
        code_results = list(graph_manager.execute_query(code_query))
        if code_results:
            print("   ✅ Found code entities:")
            for result in code_results:
                print(f"     • {result['type']}: {result['count']}")
        else:
            print("   ❌ No code entities found")
        
        # Sample some nodes
        print(f"\n📝 Sample entities:")
        sample_query = """
        MATCH (n)
        RETURN labels(n)[0] as type, 
               CASE 
                   WHEN n.name IS NOT NULL THEN n.name
                   WHEN n.title IS NOT NULL THEN n.title
                   ELSE 'Unknown'
               END as name,
               id(n) as node_id
        LIMIT 10
        """
        
        sample_results = list(graph_manager.execute_query(sample_query))
        for result in sample_results:
            print(f"     • {result['type']}: {result['name']} (ID: {result['node_id']})")
        
        graph_manager.close()
        return stats, code_results, sample_results
        
    except Exception as e:
        print(f"   ❌ Error accessing graph: {e}")
        return None, None, None

def test_rag_retrieval():
    """Test RAG retrieval on whatever content exists"""
    print(f"\n🤖 Testing RAG Retrieval")
    print("=" * 50)
    
    try:
        rag_pipeline = CodeRAGPipeline()
        
        # Test queries that might find existing content
        test_queries = [
            "memory",
            "entity", 
            "analysis",
            "code",
            "function",
            "class",
            "graph",
            "database"
        ]
        
        results = {}
        
        for query in test_queries:
            print(f"\n🔍 Testing query: '{query}'")
            
            try:
                # Try retrieval
                retrieved_content = rag_pipeline.retrieve_relevant_content(query, top_k=3)
                
                if retrieved_content:
                    print(f"   ✅ Found {len(retrieved_content)} items")
                    for i, item in enumerate(retrieved_content[:2]):  # Show first 2
                        content_preview = item["content"][:100].replace('\n', ' ')
                        print(f"     {i+1}. Relevance: {item.get('relevance', 0):.3f}")
                        print(f"        Content: {content_preview}...")
                        if item.get('metadata'):
                            print(f"        Metadata: {item['metadata']}")
                    
                    results[query] = {
                        "found": len(retrieved_content),
                        "top_relevance": retrieved_content[0].get('relevance', 0) if retrieved_content else 0
                    }
                else:
                    print(f"   ❌ No results found")
                    results[query] = {"found": 0, "top_relevance": 0}
                    
            except Exception as e:
                print(f"   ❌ Error: {e}")
                results[query] = {"error": str(e)}
        
        return results
        
    except Exception as e:
        print(f"   ❌ Error initializing RAG pipeline: {e}")
        return {"error": str(e)}

def test_hybrid_retrieval():
    """Test if hybrid retrieval (graph + vector) works"""
    print(f"\n🔄 Testing Hybrid Retrieval")
    print("=" * 50)
    
    try:
        rag_pipeline = CodeRAGPipeline()
        
        # Test a query that should trigger both graph and vector search
        query = "What are the different types of entities in the system?"
        
        print(f"Query: '{query}'")
        
        # Try the full answer generation if available
        if hasattr(rag_pipeline, 'answer_question'):
            try:
                answer = rag_pipeline.answer_question(query)
                if answer and answer.get('success'):
                    print(f"   ✅ Generated Answer:")
                    print(f"     {answer.get('answer', 'No answer')}")
                    print(f"   📊 Sources used: {len(answer.get('sources', []))}")
                else:
                    print(f"   ❌ Failed to generate answer: {answer.get('error', 'Unknown error')}")
                return answer
            except Exception as e:
                print(f"   ❌ Error in answer generation: {e}")
        else:
            print(f"   ℹ️  Answer generation not available (LLM not configured)")
            
        # Fall back to just retrieval
        retrieved = rag_pipeline.retrieve_relevant_content(query, top_k=5)
        print(f"   📋 Retrieved {len(retrieved)} items via content search")
        
        return {"retrieval_only": len(retrieved)}
        
    except Exception as e:
        print(f"   ❌ Error in hybrid retrieval: {e}")
        return {"error": str(e)}

def main():
    """Run the complete retrieval test"""
    print("🚀 Code Grapher Simple Retrieval Test")
    print("=" * 60)
    
    # Step 1: Check what's in the graph
    graph_stats, code_entities, sample_entities = check_graph_contents()
    
    # Step 2: Test basic RAG retrieval
    rag_results = test_rag_retrieval()
    
    # Step 3: Test hybrid retrieval
    hybrid_results = test_hybrid_retrieval()
    
    # Summary
    print(f"\n📊 Test Summary")
    print("=" * 50)
    
    if graph_stats:
        print(f"✅ Graph connection: Working")
        print(f"   • Nodes: {graph_stats.get('total_nodes', 0)}")
        print(f"   • Relationships: {graph_stats.get('total_relationships', 0)}")
        
        if code_entities:
            print(f"   • Code entities: {sum(r['count'] for r in code_entities)}")
        else:
            print(f"   • Code entities: 0 (only Memory/Entity nodes found)")
    else:
        print(f"❌ Graph connection: Failed")
    
    if rag_results and "error" not in rag_results:
        successful_queries = len([r for r in rag_results.values() if isinstance(r, dict) and r.get("found", 0) > 0])
        print(f"✅ RAG retrieval: Working")
        print(f"   • Successful queries: {successful_queries}/{len(rag_results)}")
    else:
        print(f"❌ RAG retrieval: Failed")
    
    if hybrid_results and "error" not in hybrid_results:
        print(f"✅ Hybrid retrieval: Working")
    else:
        print(f"❌ Hybrid retrieval: Failed")
    
    # Save results
    results = {
        "graph_stats": graph_stats,
        "code_entities": code_entities,
        "sample_entities": sample_entities,
        "rag_results": rag_results,
        "hybrid_results": hybrid_results
    }
    
    with open("simple_retrieval_test_results.json", "w") as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 Full results saved to: simple_retrieval_test_results.json")
    
    return results

if __name__ == "__main__":
    main()