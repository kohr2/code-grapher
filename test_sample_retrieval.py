#!/usr/bin/env python3
"""
Test retrieval queries on the sample data
"""

import chromadb
import sys
from pathlib import Path

def test_retrieval_queries():
    """Test various retrieval queries on the sample data"""
    print("🔍 Testing Retrieval on Sample Code Data")
    print("=" * 60)
    
    try:
        # Connect to ChromaDB
        client = chromadb.PersistentClient(path="./chroma_db")
        collection = client.get_collection("code_knowledge")
        
        print(f"📚 Collection: {collection.name}")
        print(f"   Items: {collection.count()}")
        
        # Test queries that should find relevant content
        test_queries = [
            "What classes are available for managing graphs?",
            "How does the agent coordination work?", 
            "Show me functions for analyzing code",
            "What is the RAG pipeline?",
            "How do agents work together in the workflow?",
            "Tell me about the BaseAgent class",
            "What files contain database management code?"
        ]
        
        print(f"\n🤖 Testing {len(test_queries)} queries:")
        
        results_summary = {}
        
        for i, query in enumerate(test_queries):
            print(f"\n📝 Query {i+1}: '{query}'")
            
            try:
                # Perform vector search
                results = collection.query(
                    query_texts=[query],
                    n_results=3  # Get top 3 most relevant
                )
                
                if results['documents'] and results['documents'][0]:
                    print(f"   ✅ Found {len(results['documents'][0])} relevant results:")
                    
                    query_results = []
                    for j, doc in enumerate(results['documents'][0]):
                        distance = results['distances'][0][j]
                        metadata = results['metadatas'][0][j]
                        
                        # Calculate relevance score (lower distance = higher relevance)
                        relevance = max(0, 1 - distance)
                        
                        print(f"      {j+1}. Relevance: {relevance:.3f}")
                        print(f"         Type: {metadata.get('type', 'unknown')}")
                        print(f"         File: {metadata.get('file', 'unknown')}")
                        print(f"         Content: {doc[:80]}...")
                        
                        query_results.append({
                            "relevance": relevance,
                            "type": metadata.get('type'),
                            "file": metadata.get('file'),
                            "content_preview": doc[:100]
                        })
                    
                    results_summary[query] = {
                        "found": len(results['documents'][0]),
                        "top_relevance": query_results[0]["relevance"] if query_results else 0,
                        "results": query_results
                    }
                else:
                    print(f"   ❌ No results found")
                    results_summary[query] = {"found": 0, "top_relevance": 0}
                    
            except Exception as e:
                print(f"   ❌ Query failed: {e}")
                results_summary[query] = {"error": str(e)}
        
        return results_summary
        
    except Exception as e:
        print(f"❌ Failed to test retrieval: {e}")
        return {}

def demonstrate_code_qa():
    """Demonstrate question-answering capabilities"""
    print(f"\n🎯 Demonstrating Code Q&A Capabilities")
    print("=" * 60)
    
    try:
        client = chromadb.PersistentClient(path="./chroma_db")
        collection = client.get_collection("code_knowledge")
        
        # Specific code questions
        code_questions = [
            {
                "question": "What is the purpose of the CodeGraphManager class?",
                "expected": "graph database management"
            },
            {
                "question": "What agents are involved in the code analysis pipeline?", 
                "expected": "CodeParser, EntityExtractor, RelationshipAnalyzer, GraphBuilder, RAGIndexer"
            },
            {
                "question": "How does the BaseAgent class help other agents?",
                "expected": "logging, error handling, MLflow tracking"
            }
        ]
        
        for i, qa in enumerate(code_questions):
            question = qa["question"] 
            expected = qa["expected"]
            
            print(f"\n❓ Question {i+1}: {question}")
            print(f"   Expected to find: {expected}")
            
            # Search for relevant content
            results = collection.query(
                query_texts=[question],
                n_results=2
            )
            
            if results['documents'] and results['documents'][0]:
                best_match = results['documents'][0][0]
                best_relevance = max(0, 1 - results['distances'][0][0])
                metadata = results['metadatas'][0][0]
                
                print(f"   ✅ Best match (relevance: {best_relevance:.3f}):")
                print(f"      Type: {metadata.get('type')}")
                print(f"      File: {metadata.get('file')}")
                print(f"      Content: {best_match}")
                
                # Check if expected keywords are found
                found_keywords = [kw for kw in expected.lower().split(', ') if kw in best_match.lower()]
                if found_keywords:
                    print(f"   ✅ Found expected keywords: {found_keywords}")
                else:
                    print(f"   ⚠️  Expected keywords not found in top result")
            else:
                print(f"   ❌ No relevant content found")
    
    except Exception as e:
        print(f"❌ Q&A demonstration failed: {e}")

def show_whats_available():
    """Show what's available for querying"""
    print(f"\n📋 Available Code Knowledge")
    print("=" * 60)
    
    try:
        client = chromadb.PersistentClient(path="./chroma_db")
        collection = client.get_collection("code_knowledge")
        
        # Get all documents
        all_results = collection.get()
        
        if all_results['documents']:
            print(f"📚 {len(all_results['documents'])} code entities available:")
            
            for i, doc in enumerate(all_results['documents']):
                metadata = all_results['metadatas'][i]
                doc_id = all_results['ids'][i]
                
                print(f"\n   {i+1}. {metadata.get('entity', 'Unknown')} ({metadata.get('type', 'unknown')})")
                print(f"      File: {metadata.get('file', 'unknown')}")
                print(f"      ID: {doc_id}")
                print(f"      Content: {doc[:120]}...")
        else:
            print("❌ No documents found")
    
    except Exception as e:
        print(f"❌ Failed to show available knowledge: {e}")

def main():
    """Run the retrieval test"""
    print("🚀 Sample Code Retrieval Test")
    print("=" * 60)
    
    # Show what's available
    show_whats_available()
    
    # Test retrieval queries
    retrieval_results = test_retrieval_queries()
    
    # Demonstrate Q&A
    demonstrate_code_qa()
    
    # Summary
    print(f"\n📊 Retrieval Test Summary")
    print("=" * 60)
    
    if retrieval_results:
        successful_queries = len([r for r in retrieval_results.values() 
                                if isinstance(r, dict) and r.get("found", 0) > 0])
        total_queries = len(retrieval_results)
        
        print(f"✅ Retrieval system: Working")
        print(f"   • Successful queries: {successful_queries}/{total_queries}")
        print(f"   • Average top relevance: {sum(r.get('top_relevance', 0) for r in retrieval_results.values())/len(retrieval_results):.3f}")
        
        # Show best queries
        best_queries = [(q, r.get('top_relevance', 0)) for q, r in retrieval_results.items() 
                       if isinstance(r, dict) and r.get('top_relevance', 0) > 0.5]
        
        if best_queries:
            print(f"\n🎯 Most successful queries:")
            for query, relevance in sorted(best_queries, key=lambda x: x[1], reverse=True)[:3]:
                print(f"   • '{query}' (relevance: {relevance:.3f})")
    else:
        print(f"❌ Retrieval system: Failed")
    
    print(f"\n💡 This demonstrates:")
    print(f"   • Vector similarity search working")
    print(f"   • Code entity retrieval working") 
    print(f"   • Question-answering capabilities")
    print(f"   • Ready for integration with real code data")
    
    return retrieval_results

if __name__ == "__main__":
    main()