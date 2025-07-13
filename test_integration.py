#!/usr/bin/env python3
"""
Integration Test Script for Gemini + Neo4j RAG Pipeline

This script tests the complete integration of:
1. Remote Neo4j connection using environment variables
2. Gemini Flash LLM integration 
3. End-to-end RAG pipeline functionality

Following CLAUDE.md verbose logging requirements for development phase.
"""

import os
import sys
import time
from datetime import datetime
from typing import Dict, Any, Optional
from dotenv import load_dotenv

# Load environment variables
load_dotenv()


def log_with_timestamp(message: str, level: str = "INFO"):
    """Helper function for logging with timestamps"""
    timestamp = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
    print(f"{timestamp} [INTEGRATION_TEST] [{level}] {message}")


def test_neo4j_connection() -> bool:
    """Test Neo4j connection with environment variables"""
    log_with_timestamp("Testing Neo4j connection with environment variables...")
    
    try:
        from graph_manager import CodeGraphManager
        
        # Test with environment variables (no explicit parameters)
        log_with_timestamp("Creating CodeGraphManager with env vars...")
        start_time = time.time()
        
        graph_manager = CodeGraphManager()
        
        duration = time.time() - start_time
        log_with_timestamp(f"✓ Neo4j connection successful in {duration:.3f}s", "SUCCESS")
        log_with_timestamp(f"Connected to: {graph_manager.uri}")
        
        # Test basic functionality
        stats = graph_manager.get_graph_stats()
        log_with_timestamp(f"Graph stats: {stats}")
        
        graph_manager.close()
        return True
        
    except Exception as e:
        log_with_timestamp(f"✗ Neo4j connection failed: {str(e)}", "ERROR")
        return False


def test_gemini_client() -> bool:
    """Test Gemini client initialization and basic functionality"""
    log_with_timestamp("Testing Gemini client...")
    
    try:
        from gemini_client import GeminiClient
        
        # Check if API key is configured
        api_key = os.getenv("GOOGLE_API_KEY")
        if not api_key or api_key == "your_google_api_key_here":
            log_with_timestamp("✗ GOOGLE_API_KEY not configured properly", "ERROR")
            log_with_timestamp("Please set a valid API key in .env file", "ERROR")
            return False
        
        log_with_timestamp("Creating GeminiClient...")
        start_time = time.time()
        
        client = GeminiClient()
        
        duration = time.time() - start_time
        log_with_timestamp(f"✓ Gemini client initialized in {duration:.3f}s", "SUCCESS")
        
        # Test basic generation
        log_with_timestamp("Testing basic text generation...")
        start_time = time.time()
        
        response = client.generate_response(
            "What is the purpose of a code graph database? Keep it brief.",
            max_tokens=200
        )
        
        duration = time.time() - start_time
        
        if response.get("response"):
            log_with_timestamp(f"✓ Generated response in {duration:.3f}s", "SUCCESS")
            log_with_timestamp(f"Response length: {len(response['response'])} chars")
            log_with_timestamp(f"Tokens used: {response.get('total_tokens', 'unknown')}")
            log_with_timestamp(f"Sample response: {response['response'][:100]}...")
            return True
        else:
            log_with_timestamp(f"✗ Failed to generate response: {response.get('error', 'unknown')}", "ERROR")
            return False
            
    except Exception as e:
        log_with_timestamp(f"✗ Gemini client test failed: {str(e)}", "ERROR")
        return False


def test_rag_pipeline() -> bool:
    """Test the complete RAG pipeline integration"""
    log_with_timestamp("Testing RAG pipeline integration...")
    
    try:
        from graph_manager import CodeGraphManager
        from rag_pipeline import CodeRAGPipeline
        
        # Initialize components
        log_with_timestamp("Initializing RAG pipeline components...")
        start_time = time.time()
        
        graph_manager = CodeGraphManager()
        rag_pipeline = CodeRAGPipeline(graph_manager)
        
        duration = time.time() - start_time
        log_with_timestamp(f"✓ RAG pipeline initialized in {duration:.3f}s", "SUCCESS")
        log_with_timestamp(f"LLM available: {rag_pipeline.llm_available}")
        
        # Test indexing some sample content
        log_with_timestamp("Testing content indexing...")
        sample_code = '''
def calculate_fibonacci(n):
    """Calculate the nth Fibonacci number using dynamic programming."""
    if n <= 1:
        return n
    
    fib = [0, 1]
    for i in range(2, n + 1):
        fib.append(fib[i-1] + fib[i-2])
    
    return fib[n]

class DataProcessor:
    """A class for processing and analyzing data."""
    
    def __init__(self, data_source):
        self.data_source = data_source
        self.processed_data = []
    
    def process_data(self):
        """Process the raw data."""
        # Implementation here
        pass
'''
        
        metadata = {
            "file_path": "test_sample.py",
            "language": "python",
            "chunk_index": 0
        }
        
        start_time = time.time()
        indexing_success = rag_pipeline.index_code_content(sample_code, metadata)
        duration = time.time() - start_time
        
        if indexing_success:
            log_with_timestamp(f"✓ Content indexed successfully in {duration:.3f}s", "SUCCESS")
        else:
            log_with_timestamp(f"✗ Content indexing failed", "ERROR")
            return False
        
        # Test retrieval
        log_with_timestamp("Testing content retrieval...")
        start_time = time.time()
        
        query = "How do you calculate Fibonacci numbers?"
        retrieved_content = rag_pipeline.retrieve_relevant_content(query, k=3)
        
        duration = time.time() - start_time
        log_with_timestamp(f"✓ Retrieved {len(retrieved_content)} items in {duration:.3f}s", "SUCCESS")
        
        for i, item in enumerate(retrieved_content):
            relevance = item.get("relevance_score", 0)
            content_preview = item.get("content", "")[:100]
            log_with_timestamp(f"  Result {i+1}: relevance={relevance:.3f}, preview={content_preview}...")
        
        # Test end-to-end question answering if LLM is available
        if rag_pipeline.llm_available:
            log_with_timestamp("Testing end-to-end question answering...")
            start_time = time.time()
            
            answer_result = rag_pipeline.answer_question(query)
            
            duration = time.time() - start_time
            
            if answer_result.get("success"):
                log_with_timestamp(f"✓ Generated answer in {duration:.3f}s", "SUCCESS")
                log_with_timestamp(f"Answer length: {len(answer_result.get('answer', ''))}")
                log_with_timestamp(f"Sources used: {len(answer_result.get('sources', []))}")
                log_with_timestamp(f"Answer preview: {answer_result.get('answer', '')[:150]}...")
            else:
                log_with_timestamp(f"✗ Question answering failed: {answer_result.get('llm_metadata', {}).get('error', 'unknown')}", "ERROR")
                return False
        else:
            log_with_timestamp("Skipping question answering test (LLM not available)", "WARNING")
        
        # Cleanup
        graph_manager.close()
        
        return True
        
    except Exception as e:
        log_with_timestamp(f"✗ RAG pipeline test failed: {str(e)}", "ERROR")
        return False


def test_agent_configuration() -> bool:
    """Test that agents can use environment configuration"""
    log_with_timestamp("Testing agent configuration with environment variables...")
    
    try:
        from agents.graph_builder_agent import GraphBuilderAgent
        
        # Create minimal config for testing
        test_config = {
            "config": {
                "graphDatabase": {
                    "type": "neo4j"
                },
                "nodeLabels": {
                    "function": "Function",
                    "class": "Class"
                },
                "relationshipTypes": {
                    "calls": "CALLS",
                    "contains": "CONTAINS"
                },
                "indexing": {
                    "createIndices": True,
                    "uniqueConstraints": ["Function.name", "Class.name"]
                }
            }
        }
        
        log_with_timestamp("Creating GraphBuilderAgent with minimal config...")
        start_time = time.time()
        
        agent = GraphBuilderAgent("test_agent", test_config)
        
        duration = time.time() - start_time
        log_with_timestamp(f"✓ Agent initialized successfully in {duration:.3f}s", "SUCCESS")
        log_with_timestamp(f"Agent capabilities: {agent.get_capabilities()}")
        log_with_timestamp(f"Neo4j URI from agent: {agent.connection_config.get('uri')}")
        
        # Test that it uses environment variables
        expected_uri = os.getenv("NEO4J_URL", "bolt://localhost:7687")
        actual_uri = agent.connection_config.get('uri')
        
        if actual_uri == expected_uri:
            log_with_timestamp("✓ Agent correctly uses environment variables", "SUCCESS")
        else:
            log_with_timestamp(f"✗ Agent URI mismatch. Expected: {expected_uri}, Got: {actual_uri}", "ERROR")
            return False
        
        # Cleanup
        agent.close_connection()
        
        return True
        
    except Exception as e:
        log_with_timestamp(f"✗ Agent configuration test failed: {str(e)}", "ERROR")
        return False


def run_integration_tests() -> Dict[str, bool]:
    """Run all integration tests"""
    log_with_timestamp("=== Starting Integration Tests ===")
    
    test_results = {}
    
    # Test 1: Neo4j Connection
    log_with_timestamp("--- Test 1: Neo4j Connection ---")
    test_results["neo4j_connection"] = test_neo4j_connection()
    
    # Test 2: Gemini Client
    log_with_timestamp("--- Test 2: Gemini Client ---")
    test_results["gemini_client"] = test_gemini_client()
    
    # Test 3: RAG Pipeline
    log_with_timestamp("--- Test 3: RAG Pipeline ---")
    test_results["rag_pipeline"] = test_rag_pipeline()
    
    # Test 4: Agent Configuration
    log_with_timestamp("--- Test 4: Agent Configuration ---")
    test_results["agent_configuration"] = test_agent_configuration()
    
    return test_results


def main():
    """Main function to run integration tests"""
    print("Code Grapher Integration Test Suite")
    print("Gemini Flash + Remote Neo4j Integration")
    print("=" * 60)
    
    # Check environment configuration
    neo4j_url = os.getenv("NEO4J_URL")
    google_api_key = os.getenv("GOOGLE_API_KEY")
    
    log_with_timestamp(f"NEO4J_URL: {neo4j_url}")
    log_with_timestamp(f"GOOGLE_API_KEY configured: {bool(google_api_key and google_api_key != 'your_google_api_key_here')}")
    
    # Run tests
    start_time = time.time()
    results = run_integration_tests()
    total_time = time.time() - start_time
    
    # Print summary
    print("\n" + "=" * 60)
    print("INTEGRATION TEST SUMMARY")
    print("=" * 60)
    
    passed = 0
    total = len(results)
    
    for test_name, success in results.items():
        status = "✓ PASS" if success else "✗ FAIL"
        print(f"{status}: {test_name.replace('_', ' ').title()}")
        if success:
            passed += 1
    
    print(f"\nResults: {passed}/{total} tests passed")
    print(f"Total time: {total_time:.3f}s")
    
    if passed == total:
        print("\n🎉 All integration tests PASSED!")
        print("✓ Remote Neo4j connection working")
        print("✓ Gemini Flash LLM integration working") 
        print("✓ End-to-end RAG pipeline functional")
        print("✓ Agent configuration using environment variables")
        return 0
    else:
        print(f"\n❌ {total - passed} test(s) FAILED")
        print("Check the logs above for error details")
        return 1


if __name__ == "__main__":
    try:
        exit_code = main()
        sys.exit(exit_code)
    except KeyboardInterrupt:
        print("\n\nTests interrupted by user")
        sys.exit(1)
    except Exception as e:
        print(f"\nUnexpected error: {e}")
        sys.exit(1)