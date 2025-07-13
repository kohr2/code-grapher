#!/usr/bin/env python3
"""
MLflow Integration Demo for Code Grapher Pipeline

[2025-07-12 11:05:00] [DEMO] [INFO] Demonstration of MLflow integration features
with Code Grapher agent pipeline.

This demo shows:
1. Pipeline tracking setup
2. Agent execution with monitoring
3. Dashboard access
4. Report generation
"""

import sys
import time
from pathlib import Path
from datetime import datetime

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from mlflow_config import setup_mlflow_environment, get_dashboard_url
from mlflow_agent_tracker import mlflow_tracker, cleanup_mlflow_state
from mlflow_dashboard_setup import start_dashboard_server, create_pipeline_report, export_pipeline_report
from logger import logger


def main():
    """Run MLflow integration demonstration"""
    
    print("🚀 Code Grapher MLflow Integration Demo")
    print("=" * 50)
    
    # Initialize session logger
    session_logger = logger.create_session_logger("MLflowDemo")
    session_logger.log_info("Starting MLflow integration demonstration")
    
    try:
        # Step 1: Setup MLflow Environment
        print("\n📋 Step 1: Setting up MLflow environment...")
        setup_result = setup_mlflow_environment()
        
        if setup_result["success"]:
            print(f"✅ MLflow setup successful!")
            experiment_info = mlflow_tracker.get_experiment_info()
            print(f"   Tracking URI: {experiment_info['tracking_uri']}")
            print(f"   Experiment: {experiment_info['experiment_name']}")
        else:
            print(f"❌ MLflow setup failed")
            return 1
        
        # Step 2: Start Dashboard Server (optional)
        print("\n🖥️  Step 2: Starting MLflow dashboard server...")
        server_info = start_dashboard_server()
        
        if server_info.get("started", False):
            dashboard_url = server_info.get("dashboard_url")
            print(f"✅ Dashboard server started at: {dashboard_url}")
            print(f"   You can open this URL to view the MLflow dashboard")
        else:
            dashboard_url = get_dashboard_url()
            if dashboard_url:
                print(f"✅ Dashboard available at: {dashboard_url}")
            else:
                print("⚠️  Dashboard server not started (using file-based tracking)")
        
        # Step 3: Demonstrate Pipeline Tracking
        print("\n📊 Step 3: Demonstrating pipeline tracking...")
        
        # Start a demo pipeline
        demo_workflow = "demo_code_analysis"
        demo_input = {
            "project_path": "/demo/project",
            "files_to_analyze": ["file1.py", "file2.py", "file3.py"],
            "analysis_type": "full",
            "timestamp": datetime.now().isoformat()
        }
        
        pipeline_run_id = mlflow_tracker.start_pipeline_run(
            workflow_name=demo_workflow,
            input_data=demo_input,
            config={"type": "sequential", "timeout": 300}
        )
        
        print(f"✅ Started pipeline tracking: {pipeline_run_id}")
        
        # Step 4: Demonstrate Agent Tracking
        print("\n🤖 Step 4: Demonstrating agent tracking...")
        
        demo_agents = [
            ("code-parser", "CodeParserAgent", ["parse", "tokenize"]),
            ("entity-extractor", "EntityExtractionAgent", ["extract", "classify"]),
            ("graph-builder", "GraphBuilderAgent", ["build", "connect"])
        ]
        
        for agent_id, agent_type, capabilities in demo_agents:
            print(f"   Processing with {agent_id}...")
            
            with mlflow_tracker.agent_tracking_context(
                agent_id=agent_id,
                agent_type=agent_type,
                input_data={"files": ["file1.py", "file2.py"]},
                capabilities=capabilities
            ) as agent_run_id:
                
                # Simulate agent processing
                processing_time = 0.5 + (len(capabilities) * 0.3)  # Simulate variable processing time
                time.sleep(processing_time)
                
                # Log agent metrics
                demo_metrics = {
                    "processing_time_seconds": processing_time,
                    "files_processed": 2,
                    "items_extracted": 10 + (len(capabilities) * 5),
                    "success_rate": 0.95
                }
                
                mlflow_tracker.log_agent_metrics(agent_id, demo_metrics)
                
                # Log agent output as artifact
                agent_output = {
                    "agent_id": agent_id,
                    "processed_files": ["file1.py", "file2.py"],
                    "results": f"Processed with {agent_type}",
                    "metrics": demo_metrics,
                    "timestamp": datetime.now().isoformat()
                }
                
                mlflow_tracker.log_agent_artifact(agent_id, agent_output, "processing_result", "json")
                
                print(f"     ✅ {agent_id} completed in {processing_time:.2f}s")
        
        # Step 5: Complete Pipeline
        print("\n🏁 Step 5: Completing pipeline...")
        
        final_result = {
            "workflow": demo_workflow,
            "status": "completed",
            "agents_executed": len(demo_agents),
            "total_files_processed": 2,
            "total_entities_extracted": sum(10 + (len(caps) * 5) for _, _, caps in demo_agents),
            "completed_at": datetime.now().isoformat()
        }
        
        mlflow_tracker.end_pipeline_run(success=True, result=final_result)
        
        print(f"✅ Pipeline completed successfully!")
        print(f"   Total agents: {final_result['agents_executed']}")
        print(f"   Files processed: {final_result['total_files_processed']}")
        print(f"   Entities extracted: {final_result['total_entities_extracted']}")
        
        # Step 6: Generate and Display Report
        print("\n📈 Step 6: Generating pipeline report...")
        
        # Wait a moment for data to be written
        time.sleep(1)
        
        report = create_pipeline_report()
        
        if "error" not in report:
            stats = report["pipeline_statistics"]
            print(f"✅ Report generated successfully!")
            print(f"   Total runs: {stats.get('total_runs', 0)}")
            print(f"   Success rate: {stats.get('success_rate_percent', 0):.1f}%")
            print(f"   Average duration: {stats.get('avg_duration_seconds', 0):.2f}s")
            
            # Export report to HTML
            report_file = "mlflow_demo_report.html"
            if export_pipeline_report(report_file, "html"):
                print(f"   📄 HTML report saved to: {report_file}")
        else:
            print(f"⚠️  Report generation issue: {report.get('error', 'Unknown error')}")
        
        # Step 7: Provide Access Instructions
        print("\n🎯 Step 7: Next steps...")
        
        if dashboard_url:
            print(f"🖥️  View real-time dashboard: {dashboard_url}")
            print("   The dashboard shows:")
            print("   • Pipeline execution timeline")
            print("   • Agent performance metrics")
            print("   • Success/failure analysis")
            print("   • Detailed run information")
        
        print("\n📊 View detailed metrics:")
        print(f"   • Experiment: {experiment_info['experiment_name']}")
        print(f"   • Run ID: {pipeline_run_id}")
        print(f"   • Tracking URI: {experiment_info['tracking_uri']}")
        
        print("\n🔧 Additional commands:")
        print("   • Run tests: python test_mlflow_integration.py")
        print("   • Generate reports: python -c \"from mlflow_dashboard_setup import export_pipeline_report; export_pipeline_report('report.html', 'html')\"")
        print("   • Clean state: python -c \"from mlflow_agent_tracker import cleanup_mlflow_state; cleanup_mlflow_state()\"")
        
        session_logger.log_info("MLflow integration demonstration completed successfully")
        
        print("\n🎉 Demo completed successfully!")
        print("The MLflow integration is ready for production use.")
        
        return 0
        
    except Exception as e:
        print(f"\n❌ Demo failed: {str(e)}")
        session_logger.log_error(e, {"context": "mlflow_demo"})
        return 1
    
    finally:
        # Cleanup
        cleanup_mlflow_state()


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)