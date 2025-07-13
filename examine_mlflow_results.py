#!/usr/bin/env python3
"""
Examine MLflow results to see what the pipeline actually processed
"""

import mlflow
import json
from datetime import datetime

def examine_mlflow_data():
    """Examine what was tracked in MLflow"""
    print("🔍 Examining MLflow Tracking Data")
    print("=" * 50)
    
    # Set tracking URI
    mlflow.set_tracking_uri('sqlite:///mlflow.db')
    client = mlflow.tracking.MlflowClient()
    
    # Get experiment
    experiment = client.get_experiment_by_name('CodeGrapher-Agent-Pipeline')
    if not experiment:
        print("❌ No CodeGrapher experiment found")
        return
    
    print(f"✅ Found experiment: {experiment.name}")
    print(f"   Experiment ID: {experiment.experiment_id}")
    
    # Get all runs
    runs = client.search_runs([experiment.experiment_id])
    print(f"   Total runs: {len(runs)}")
    
    # Categorize runs
    pipeline_runs = [run for run in runs if "pipeline_" in run.info.run_name]
    agent_runs = [run for run in runs if "agent_" in run.info.run_name]
    
    print(f"   • Pipeline runs: {len(pipeline_runs)}")
    print(f"   • Agent runs: {len(agent_runs)}")
    
    # Examine pipeline runs
    print(f"\n📊 Pipeline Run Analysis:")
    for i, run in enumerate(pipeline_runs[:3]):  # Look at top 3
        print(f"\n   Pipeline Run {i+1}: {run.info.run_name}")
        print(f"   • Status: {run.info.status}")
        print(f"   • Start: {datetime.fromtimestamp(run.info.start_time/1000)}")
        if run.info.end_time:
            duration = (run.info.end_time - run.info.start_time) / 1000
            print(f"   • Duration: {duration:.2f} seconds")
        
        # Check parameters
        if run.data.params:
            print(f"   • Parameters:")
            for key, value in list(run.data.params.items())[:5]:
                print(f"     - {key}: {value}")
        
        # Check metrics
        if run.data.metrics:
            print(f"   • Metrics:")
            for key, value in list(run.data.metrics.items())[:5]:
                print(f"     - {key}: {value}")
        
        # Check for artifacts
        artifacts = client.list_artifacts(run.info.run_id)
        if artifacts:
            print(f"   • Artifacts: {len(artifacts)} items")
            for artifact in artifacts[:3]:
                print(f"     - {artifact.path}")
    
    # Examine agent runs by type
    print(f"\n🤖 Agent Run Analysis:")
    agent_types = {}
    for run in agent_runs:
        agent_name = run.info.run_name.split('_')[1] if '_' in run.info.run_name else 'unknown'
        if agent_name not in agent_types:
            agent_types[agent_name] = []
        agent_types[agent_name].append(run)
    
    for agent_name, runs in agent_types.items():
        print(f"\n   {agent_name.title()} Agent:")
        print(f"   • Total runs: {len(runs)}")
        
        # Look at most recent run
        if runs:
            latest_run = sorted(runs, key=lambda r: r.info.start_time, reverse=True)[0]
            print(f"   • Latest run: {latest_run.info.run_name}")
            print(f"   • Status: {latest_run.info.status}")
            
            # Show some metrics
            if latest_run.data.metrics:
                print(f"   • Sample metrics:")
                for key, value in list(latest_run.data.metrics.items())[:3]:
                    print(f"     - {key}: {value}")
    
    return {
        "pipeline_runs": len(pipeline_runs),
        "agent_runs": len(agent_runs),
        "agent_types": list(agent_types.keys()),
        "latest_pipeline": pipeline_runs[0].info.run_name if pipeline_runs else None
    }

def test_demo_artifacts():
    """Check if we have any demo artifacts or test data"""
    print(f"\n📁 Checking for Demo Artifacts")
    print("=" * 50)
    
    import os
    from pathlib import Path
    
    # Check for generated files
    demo_files = [
        "mlflow_demo_report.html",
        "end_to_end_test_results.json", 
        "simple_retrieval_test_results.json",
        "agent_flow_diagram.md",
        "agent_flow_interactive.html"
    ]
    
    found_files = []
    for file in demo_files:
        if Path(file).exists():
            size = Path(file).stat().st_size
            print(f"   ✅ {file} ({size:,} bytes)")
            found_files.append(file)
        else:
            print(f"   ❌ {file} (not found)")
    
    # Try to load and examine JSON results
    if "end_to_end_test_results.json" in found_files:
        try:
            with open("end_to_end_test_results.json", "r") as f:
                results = json.load(f)
            
            print(f"\n📊 End-to-End Test Results:")
            print(f"   • Success: {results.get('success', False)}")
            if 'error' in results:
                print(f"   • Error: {results['error']}")
            
            if 'summary' in results:
                summary = results['summary']
                print(f"   • Files processed: {summary.get('files_processed', 0)}")
                print(f"   • Pipeline success: {summary.get('pipeline_success', False)}")
                print(f"   • Graph populated: {summary.get('graph_populated', False)}")
                print(f"   • Retrieval working: {summary.get('retrieval_working', False)}")
                
        except Exception as e:
            print(f"   ❌ Could not parse test results: {e}")
    
    return found_files

def create_sample_data_for_retrieval():
    """Create some sample data to test retrieval without needing the full pipeline"""
    print(f"\n🛠️  Creating Sample Data for Retrieval Test")
    print("=" * 50)
    
    try:
        # Create a simple ChromaDB collection with sample code data
        import chromadb
        
        # Use the new client initialization
        client = chromadb.PersistentClient(path="./chroma_db")
        
        # Create or get collection
        collection_name = "code_knowledge"
        try:
            collection = client.get_collection(collection_name)
            print(f"   ✅ Found existing collection: {collection_name}")
        except:
            collection = client.create_collection(collection_name)
            print(f"   ✅ Created new collection: {collection_name}")
        
        # Add some sample code content for testing
        sample_documents = [
            {
                "id": "sample_1",
                "document": "class CodeGraphManager: def __init__(self, uri, username, password): self.uri = uri. This class manages connections to Neo4j graph database for storing code entities and relationships.",
                "metadata": {"type": "class", "file": "graph_manager.py", "entity": "CodeGraphManager"}
            },
            {
                "id": "sample_2", 
                "document": "def analyze_code_entities(self, file_path): Parse Python files and extract classes, functions, and variables. Returns a list of entities with their relationships.",
                "metadata": {"type": "function", "file": "entity_analyzer.py", "entity": "analyze_code_entities"}
            },
            {
                "id": "sample_3",
                "document": "The RAG pipeline combines graph database queries with vector similarity search to provide comprehensive code analysis and question answering capabilities.",
                "metadata": {"type": "documentation", "file": "rag_pipeline.py", "entity": "RAG pipeline overview"}
            },
            {
                "id": "sample_4",
                "document": "class BaseAgent: Abstract base class for all code analysis agents. Provides logging, error handling, and MLflow tracking integration.",
                "metadata": {"type": "class", "file": "base_agent.py", "entity": "BaseAgent"}
            },
            {
                "id": "sample_5", 
                "document": "Agent coordination workflow: CodeParser -> EntityExtractor -> RelationshipAnalyzer -> GraphBuilder -> RAGIndexer",
                "metadata": {"type": "workflow", "file": "agent_coordinator.py", "entity": "pipeline workflow"}
            }
        ]
        
        # Check if collection already has data
        existing_count = collection.count()
        print(f"   • Existing items: {existing_count}")
        
        if existing_count == 0:
            # Add the sample documents
            documents = [doc["document"] for doc in sample_documents]
            metadatas = [doc["metadata"] for doc in sample_documents]
            ids = [doc["id"] for doc in sample_documents]
            
            collection.add(
                documents=documents,
                metadatas=metadatas,
                ids=ids
            )
            
            print(f"   ✅ Added {len(sample_documents)} sample documents")
        else:
            print(f"   ℹ️  Collection already contains data, skipping sample insertion")
        
        # Test a simple query
        results = collection.query(
            query_texts=["What classes are available for graph management?"],
            n_results=3
        )
        
        print(f"   ✅ Test query successful:")
        print(f"   • Found {len(results['documents'][0])} relevant documents")
        for i, doc in enumerate(results['documents'][0]):
            distance = results['distances'][0][i]
            metadata = results['metadatas'][0][i]
            print(f"     {i+1}. Distance: {distance:.3f} | Type: {metadata.get('type', 'unknown')}")
            print(f"        Preview: {doc[:80]}...")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Failed to create sample data: {e}")
        return False

def main():
    """Run the MLflow examination"""
    print("🚀 MLflow Results Examination")
    print("=" * 60)
    
    # Examine MLflow data
    mlflow_summary = examine_mlflow_data()
    
    # Check for artifacts
    demo_files = test_demo_artifacts()
    
    # Create sample data for retrieval testing
    sample_success = create_sample_data_for_retrieval()
    
    # Summary
    print(f"\n📊 Overall Status")
    print("=" * 50)
    
    if mlflow_summary:
        print("✅ MLflow tracking: Contains execution data")
        print(f"   • Pipeline runs: {mlflow_summary['pipeline_runs']}")
        print(f"   • Agent runs: {mlflow_summary['agent_runs']}")
        print(f"   • Agents tracked: {', '.join(mlflow_summary['agent_types'])}")
    else:
        print("❌ MLflow tracking: No data found")
    
    if demo_files:
        print(f"✅ Demo artifacts: {len(demo_files)} files found")
    else:
        print("❌ Demo artifacts: No files found")
    
    if sample_success:
        print("✅ Sample retrieval data: Created successfully")
        print("   • Ready for retrieval testing!")
    else:
        print("❌ Sample retrieval data: Failed to create")
    
    print(f"\n💡 What You Can Do Now:")
    print("   • View MLflow dashboard: http://localhost:5002")
    print("   • Test retrieval with sample data")
    print("   • View agent flow diagram: agent_flow_interactive.html")
    
    return mlflow_summary

if __name__ == "__main__":
    main()