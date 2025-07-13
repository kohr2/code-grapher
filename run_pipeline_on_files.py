#!/usr/bin/env python3
"""
Run Code Grapher pipeline on actual Python files and test retrieval
"""

import sys
import os
import json
import time
from pathlib import Path
from typing import Dict, Any, List

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from coordinator.agent_coordinator import AgentCoordinator
from rag_pipeline import CodeRAGPipeline
from graph_manager import CodeGraphManager
from logger import logger
from ai_evaluation_tracker import ai_tracker

class CodeGrapherRunner:
    """Run pipeline on actual Python files and test retrieval"""
    
    def __init__(self):
        self.session_logger = logger.create_session_logger("CodeGrapherRunner")
        self.session_logger.log_operation_start("CodeGrapherRunner.init")
        
    def select_test_files(self) -> List[str]:
        """Select a few Python files to process"""
        project_root = Path("/Users/danielbeach/Code/code-grapher")
        
        # Select diverse files for testing
        test_files = [
            str(project_root / "graph_manager.py"),  # Core functionality
            str(project_root / "agents" / "base_agent.py"),  # Agent framework
            str(project_root / "gemini_client.py"),  # LLM integration
        ]
        
        # Verify files exist
        existing_files = [f for f in test_files if Path(f).exists()]
        
        self.session_logger.logger.info(
            f"Selected {len(existing_files)} test files for processing (from {len(test_files)} candidates)"
        )
        
        return existing_files
    
    def run_pipeline_on_files(self, files: List[str]) -> Dict[str, Any]:
        """Run the agent pipeline on selected files"""
        self.session_logger.log_operation_start(
            "run_pipeline_on_files",
            {"file_count": len(files), "files": [Path(f).name for f in files]}
        )
        
        start_time = time.time()
        
        try:
            # Initialize coordinator
            self.session_logger.logger.info("Initializing AgentCoordinator...")
            config_path = "/Users/danielbeach/Code/code-grapher/config/agent_pipeline_config.json"
            coordinator = AgentCoordinator(config_path)
            
            # Prepare workflow input
            workflow_input = {
                "project_path": "/Users/danielbeach/Code/code-grapher",
                "files_to_analyze": files,
                "analysis_type": "full_analysis",
                "output_format": "knowledge_graph"
            }
            
            self.session_logger.logger.info(f"Starting pipeline execution on {len(files)} files...")
            
            # Execute workflow
            result = coordinator.execute_workflow("code_analysis", workflow_input)
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "run_pipeline_on_files",
                duration=duration,
                success=result.get("success", False),
                details={
                    "files_processed": result.get("files_processed", 0),
                    "entities_extracted": result.get("entities_extracted", 0),
                    "relationships_found": result.get("relationships_found", 0)
                }
            )
            
            ai_tracker.record_success(
                component="pipeline-execution",
                description=f"Successfully processed {len(files)} files",
                impact=9
            )
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "run_pipeline_on_files",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            ai_tracker.record_failure(
                component="pipeline-execution",
                description=f"Pipeline failed: {str(e)}",
                error_type=type(e).__name__
            )
            
            self.session_logger.logger.error(f"Pipeline execution failed: {e}")
            return {"success": False, "error": str(e)}
    
    def check_knowledge_graph_contents(self) -> Dict[str, Any]:
        """Check what was created in the knowledge graph"""
        self.session_logger.log_operation_start("check_knowledge_graph_contents")
        
        try:
            # Initialize graph manager
            graph_manager = CodeGraphManager()
            
            # Get graph statistics
            stats = graph_manager.get_graph_statistics()
            
            self.session_logger.logger.info(f"Graph Statistics: {stats}")
            
            # Query for code-related nodes
            code_nodes_query = """
            MATCH (n)
            WHERE labels(n)[0] IN ['File', 'Class', 'Function', 'Method', 'Variable']
            RETURN labels(n)[0] as node_type, count(n) as count
            ORDER BY count DESC
            """
            
            code_nodes = graph_manager.execute_query(code_nodes_query)
            
            self.session_logger.logger.info(f"Code nodes found: {list(code_nodes)}")
            
            # Query for recent relationships
            relationships_query = """
            MATCH (a)-[r]->(b)
            WHERE labels(a)[0] IN ['File', 'Class', 'Function', 'Method']
            RETURN type(r) as relationship_type, count(r) as count
            ORDER BY count DESC
            LIMIT 10
            """
            
            relationships = graph_manager.execute_query(relationships_query)
            
            self.session_logger.logger.info(f"Relationships found: {list(relationships)}")
            
            # Sample some actual nodes
            sample_nodes_query = """
            MATCH (n)
            WHERE labels(n)[0] IN ['Class', 'Function']
            RETURN n.name as name, labels(n)[0] as type, n.file_path as file
            LIMIT 5
            """
            
            sample_nodes = graph_manager.execute_query(sample_nodes_query)
            
            result = {
                "graph_stats": stats,
                "code_node_counts": list(code_nodes),
                "relationship_counts": list(relationships),
                "sample_nodes": list(sample_nodes)
            }
            
            self.session_logger.log_operation_end(
                "check_knowledge_graph_contents",
                success=True,
                details=result
            )
            
            graph_manager.close()
            return result
            
        except Exception as e:
            self.session_logger.logger.error(f"Failed to check graph contents: {e}")
            return {"error": str(e)}
    
    def test_retrieval_queries(self) -> Dict[str, Any]:
        """Test various retrieval queries on the processed data"""
        self.session_logger.log_operation_start("test_retrieval_queries")
        
        try:
            # Initialize RAG pipeline
            rag_pipeline = CodeRAGPipeline()
            
            # Test queries that should find content
            test_queries = [
                "What classes are defined in the code?",
                "Show me functions related to graph management",
                "What agents are available in the system?",
                "How does the logging system work?",
                "What database connections are used?"
            ]
            
            results = {}
            
            for query in test_queries:
                self.session_logger.logger.info(f"Testing query: {query}")
                
                try:
                    # Try retrieval
                    retrieved_content = rag_pipeline.retrieve_relevant_content(query, top_k=3)
                    
                    # Try full Q&A if LLM is available
                    if hasattr(rag_pipeline, 'answer_question'):
                        answer = rag_pipeline.answer_question(query)
                        results[query] = {
                            "retrieved_items": len(retrieved_content),
                            "sample_content": retrieved_content[0]["content"][:200] if retrieved_content else None,
                            "answer": answer.get("answer", "No answer generated") if answer else "LLM not available"
                        }
                    else:
                        results[query] = {
                            "retrieved_items": len(retrieved_content),
                            "sample_content": retrieved_content[0]["content"][:200] if retrieved_content else None,
                            "answer": "LLM integration not available"
                        }
                        
                except Exception as e:
                    results[query] = {"error": str(e)}
            
            self.session_logger.log_operation_end(
                "test_retrieval_queries",
                success=True,
                details={"queries_tested": len(test_queries), "successful_queries": len([r for r in results.values() if "error" not in r])}
            )
            
            return results
            
        except Exception as e:
            self.session_logger.logger.error(f"Failed to test retrieval: {e}")
            return {"error": str(e)}
    
    def run_complete_test(self) -> Dict[str, Any]:
        """Run the complete end-to-end test"""
        print("🚀 Starting Code Grapher End-to-End Test")
        print("=" * 60)
        
        # Step 1: Select files
        print("\n📁 Step 1: Selecting test files...")
        test_files = self.select_test_files()
        print(f"   Selected {len(test_files)} files:")
        for f in test_files:
            print(f"   • {Path(f).name}")
        
        # Step 2: Run pipeline
        print("\n🤖 Step 2: Running agent pipeline...")
        pipeline_result = self.run_pipeline_on_files(test_files)
        
        if pipeline_result.get("success"):
            print("   ✅ Pipeline completed successfully!")
            print(f"   • Files processed: {pipeline_result.get('files_processed', 0)}")
            print(f"   • Entities extracted: {pipeline_result.get('entities_extracted', 0)}")
        else:
            print(f"   ❌ Pipeline failed: {pipeline_result.get('error', 'Unknown error')}")
            return pipeline_result
        
        # Step 3: Check graph contents
        print("\n📊 Step 3: Checking knowledge graph contents...")
        graph_contents = self.check_knowledge_graph_contents()
        
        if "error" not in graph_contents:
            print("   ✅ Knowledge graph populated!")
            print(f"   • Total nodes: {graph_contents['graph_stats'].get('total_nodes', 0)}")
            print(f"   • Total relationships: {graph_contents['graph_stats'].get('total_relationships', 0)}")
            
            if graph_contents.get('sample_nodes'):
                print("   • Sample entities found:")
                for node in graph_contents['sample_nodes'][:3]:
                    print(f"     - {node.get('type', 'Unknown')}: {node.get('name', 'Unknown')} ({Path(node.get('file', 'Unknown')).name})")
        else:
            print(f"   ❌ Failed to check graph: {graph_contents['error']}")
        
        # Step 4: Test retrieval
        print("\n🔍 Step 4: Testing retrieval queries...")
        retrieval_results = self.test_retrieval_queries()
        
        successful_queries = 0
        for query, result in retrieval_results.items():
            if "error" not in result:
                successful_queries += 1
                print(f"   ✅ '{query}' -> {result.get('retrieved_items', 0)} items found")
                if result.get('answer') and result['answer'] != "LLM not available":
                    print(f"      Answer: {result['answer'][:100]}...")
            else:
                print(f"   ❌ '{query}' -> Error: {result['error']}")
        
        print(f"\n📈 Results Summary:")
        print(f"   • Queries successful: {successful_queries}/{len(retrieval_results)}")
        
        # Combined results
        return {
            "pipeline_result": pipeline_result,
            "graph_contents": graph_contents,
            "retrieval_results": retrieval_results,
            "summary": {
                "pipeline_success": pipeline_result.get("success", False),
                "graph_populated": "error" not in graph_contents,
                "retrieval_working": successful_queries > 0,
                "files_processed": len(test_files)
            }
        }

if __name__ == "__main__":
    runner = CodeGrapherRunner()
    results = runner.run_complete_test()
    
    # Save results
    with open("end_to_end_test_results.json", "w") as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 Full results saved to: end_to_end_test_results.json")
    print("\n🎯 Test completed!")