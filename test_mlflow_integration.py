#!/usr/bin/env python3
"""
MLflow Integration Test for Code Grapher Pipeline

[2025-07-12 10:50:15] [MLFLOW_TEST] [INFO] Testing MLflow integration with existing
Code Grapher agent pipeline to verify tracking functionality.

This test:
1. Sets up MLflow environment
2. Initializes AgentCoordinator with MLflow tracking
3. Executes a sample workflow with agent tracking
4. Verifies MLflow data logging and retrieval
5. Tests dashboard functionality
"""

import os
import sys
import time
import json
from pathlib import Path
from datetime import datetime
from typing import Dict, Any

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker
from mlflow_config import mlflow_config, setup_mlflow_environment
from mlflow_agent_tracker import mlflow_tracker, cleanup_mlflow_state
from mlflow_dashboard_setup import mlflow_dashboard
from coordinator.agent_coordinator import AgentCoordinator


class MLflowIntegrationTest:
    """
    Comprehensive test suite for MLflow integration with Code Grapher pipeline.
    """
    
    def __init__(self):
        self.session_logger = logger.create_session_logger("MLflowIntegrationTest")
        self.test_results = {
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "start_time": datetime.now().isoformat(),
            "test_details": {}
        }
        
        self.session_logger.log_info("Starting MLflow integration tests")
    
    def run_all_tests(self) -> Dict[str, Any]:
        """
        Run comprehensive MLflow integration test suite.
        
        Returns:
            Test results summary
        """
        self.session_logger.log_operation_start(
            "MLflowIntegrationTest.run_all_tests",
            {"test_environment": "development"}
        )
        
        start_time = time.time()
        
        try:
            # Test 1: MLflow Configuration and Setup
            self._test_mlflow_setup()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 2: MLflow Tracker Initialization
            self._test_tracker_initialization()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 3: Pipeline Run Tracking
            self._test_pipeline_tracking()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 4: Agent Tracking Integration
            self._test_agent_tracking()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 5: Dashboard Functionality
            self._test_dashboard_functionality()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 6: Data Retrieval and Analysis
            self._test_data_retrieval()
            cleanup_mlflow_state()  # Clean up between tests
            
            # Test 7: Error Handling and Recovery
            self._test_error_handling()
            cleanup_mlflow_state()  # Final cleanup
            
            duration = time.time() - start_time
            
            # Calculate success rate
            success_rate = (self.test_results["tests_passed"] / self.test_results["tests_run"]) * 100 if self.test_results["tests_run"] > 0 else 0
            
            self.test_results.update({
                "end_time": datetime.now().isoformat(),
                "total_duration_seconds": duration,
                "success_rate_percent": success_rate,
                "overall_status": "PASSED" if self.test_results["tests_failed"] == 0 else "FAILED"
            })
            
            self.session_logger.log_operation_end(
                "MLflowIntegrationTest.run_all_tests",
                duration=duration,
                success=self.test_results["tests_failed"] == 0,
                details={
                    "tests_passed": self.test_results["tests_passed"],
                    "tests_failed": self.test_results["tests_failed"],
                    "success_rate": success_rate
                }
            )
            
            # Track overall test results
            if self.test_results["tests_failed"] == 0:
                ai_tracker.record_success(
                    component="mlflow-integration-test",
                    description=f"All MLflow integration tests passed ({self.test_results['tests_passed']}/{self.test_results['tests_run']})",
                    time_saved=duration,
                    accuracy=success_rate
                )
            else:
                ai_tracker.record_failure(
                    component="mlflow-integration-test",
                    description=f"MLflow integration tests failed ({self.test_results['tests_failed']} failures)",
                    error_type="TestFailure"
                )
            
            return self.test_results
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.test_results.update({
                "end_time": datetime.now().isoformat(),
                "total_duration_seconds": duration,
                "overall_status": "ERROR",
                "error": str(e)
            })
            
            self.session_logger.log_error(e, {"context": "integration_test_suite"})
            
            ai_tracker.record_failure(
                component="mlflow-integration-test",
                description=f"MLflow integration test suite crashed: {str(e)}",
                error_type=type(e).__name__
            )
            
            return self.test_results
    
    def _test_mlflow_setup(self) -> None:
        """Test MLflow environment setup and configuration"""
        test_name = "mlflow_setup"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Test MLflow setup
            setup_result = setup_mlflow_environment()
            
            # Validate setup results
            assert setup_result.get("success", False), "MLflow setup failed"
            assert setup_result.get("tracking_uri_set", False), "Tracking URI not set"
            
            # Test configuration validation
            validation_result = mlflow_config.validate_setup()
            assert validation_result["overall_status"] in ["healthy", "partial"], f"MLflow validation failed: {validation_result['overall_status']}"
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "setup_result": setup_result,
                "validation_result": validation_result
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_tracker_initialization(self) -> None:
        """Test MLflow tracker initialization and basic functionality"""
        test_name = "tracker_initialization"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Test tracker initialization
            assert mlflow_tracker is not None, "MLflow tracker not initialized"
            
            # Test experiment info
            experiment_info = mlflow_tracker.get_experiment_info()
            assert experiment_info["experiment_name"] is not None, "Experiment name not set"
            assert experiment_info["experiment_id"] is not None, "Experiment ID not set"
            
            # Test current run info (should be empty initially)
            run_info = mlflow_tracker.get_current_run_info()
            assert isinstance(run_info, dict), "Run info not returned as dict"
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "experiment_info": experiment_info,
                "run_info": run_info
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_pipeline_tracking(self) -> None:
        """Test pipeline-level MLflow tracking"""
        test_name = "pipeline_tracking"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Create test workflow data
            test_workflow_name = "test_workflow_mlflow"
            test_input_data = {
                "test_data": "sample input for MLflow testing",
                "timestamp": datetime.now().isoformat()
            }
            test_config = {
                "type": "sequential",
                "steps": []
            }
            
            # Start pipeline tracking
            pipeline_run_id = mlflow_tracker.start_pipeline_run(
                workflow_name=test_workflow_name,
                input_data=test_input_data,
                config=test_config
            )
            
            assert pipeline_run_id is not None, "Pipeline run ID not returned"
            
            # Verify run is active
            run_info = mlflow_tracker.get_current_run_info()
            assert run_info["pipeline_run_active"], "Pipeline run not marked as active"
            assert run_info["pipeline_run_id"] == pipeline_run_id, "Pipeline run ID mismatch"
            
            # Log some pipeline artifacts
            test_result = {
                "test_metric": 42,
                "test_string": "MLflow test result",
                "timestamp": datetime.now().isoformat()
            }
            
            mlflow_tracker.log_pipeline_artifact(test_result, "test_result", "json")
            
            # End pipeline tracking
            mlflow_tracker.end_pipeline_run(success=True, result=test_result)
            
            # Verify run is no longer active
            run_info = mlflow_tracker.get_current_run_info()
            assert not run_info["pipeline_run_active"], "Pipeline run still marked as active after ending"
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "pipeline_run_id": pipeline_run_id,
                "test_result": test_result
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_agent_tracking(self) -> None:
        """Test agent-level MLflow tracking"""
        test_name = "agent_tracking"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Start a pipeline run first
            pipeline_run_id = mlflow_tracker.start_pipeline_run(
                workflow_name="test_agent_workflow",
                input_data={"test": "agent tracking"},
                config={"type": "sequential"}
            )
            
            # Test agent tracking with context manager
            agent_id = "test-agent"
            agent_type = "TestAgent"
            input_data = {"test_input": "agent test data"}
            capabilities = ["test", "tracking"]
            
            with mlflow_tracker.agent_tracking_context(agent_id, agent_type, input_data, capabilities) as agent_run_id:
                assert agent_run_id is not None, "Agent run ID not returned"
                
                # Log agent metrics
                test_metrics = {
                    "processing_time": 1.5,
                    "items_processed": 10,
                    "accuracy": 0.95
                }
                
                mlflow_tracker.log_agent_metrics(agent_id, test_metrics)
                
                # Log agent artifact
                agent_output = {
                    "processed_items": ["item1", "item2", "item3"],
                    "processing_time": 1.5,
                    "status": "completed"
                }
                
                mlflow_tracker.log_agent_artifact(agent_id, agent_output, "agent_output", "json")
                
                # Verify agent run is tracked
                run_info = mlflow_tracker.get_current_run_info()
                assert agent_id in run_info["active_agent_runs"], f"Agent {agent_id} not in active runs"
            
            # End pipeline run
            mlflow_tracker.end_pipeline_run(success=True, result={"test": "completed"})
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "agent_run_id": agent_run_id,
                "metrics": test_metrics,
                "output": agent_output
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_dashboard_functionality(self) -> None:
        """Test MLflow dashboard functionality"""
        test_name = "dashboard_functionality"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Test dashboard URL creation
            dashboard_url = mlflow_dashboard.create_dashboard_url()
            assert dashboard_url is not None, "Dashboard URL not created"
            assert "experiments" in dashboard_url, "Dashboard URL does not contain experiments path"
            
            # Test pipeline overview report creation
            report = mlflow_dashboard.create_pipeline_overview_report()
            assert isinstance(report, dict), "Pipeline report not returned as dict"
            assert "generated_at" in report, "Report missing generation timestamp"
            assert "pipeline_statistics" in report, "Report missing pipeline statistics"
            
            # Test export functionality
            test_export_file = Path("test_mlflow_report.json")
            export_success = mlflow_dashboard.export_pipeline_report(str(test_export_file), "json")
            assert export_success, "Report export failed"
            assert test_export_file.exists(), "Export file not created"
            
            # Clean up test file
            test_export_file.unlink()
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "dashboard_url": dashboard_url,
                "report_keys": list(report.keys()),
                "export_success": export_success
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_data_retrieval(self) -> None:
        """Test MLflow data retrieval and analysis"""
        test_name = "data_retrieval"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Get experiment info
            experiment_info = mlflow_tracker.get_experiment_info()
            assert experiment_info["experiment_id"] is not None, "Experiment ID not available"
            
            # Test data retrieval through dashboard
            report = mlflow_dashboard.create_pipeline_overview_report()
            
            # Verify report structure
            required_sections = ["pipeline_statistics", "agent_performance", "recent_executions"]
            for section in required_sections:
                assert section in report, f"Report missing required section: {section}"
            
            # Verify statistics are calculated
            stats = report["pipeline_statistics"]
            assert isinstance(stats.get("total_runs", 0), int), "Total runs not calculated"
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "experiment_id": experiment_info["experiment_id"],
                "total_runs": stats.get("total_runs", 0),
                "report_sections": list(report.keys())
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def _test_error_handling(self) -> None:
        """Test MLflow error handling and recovery"""
        test_name = "error_handling"
        self.session_logger.log_info(f"Running test: {test_name}")
        
        try:
            self.test_results["tests_run"] += 1
            
            # Test pipeline failure tracking
            pipeline_run_id = mlflow_tracker.start_pipeline_run(
                workflow_name="test_error_workflow",
                input_data={"test": "error handling"},
                config={"type": "sequential"}
            )
            
            # Simulate error
            error_info = {
                "error_type": "TestError",
                "error_message": "Simulated error for testing"
            }
            
            # End pipeline with error
            mlflow_tracker.end_pipeline_run(success=False, error_info=error_info)
            
            # Verify error tracking
            run_info = mlflow_tracker.get_current_run_info()
            assert not run_info["pipeline_run_active"], "Pipeline run still active after error"
            
            # Test that system continues to work after error
            new_run_id = mlflow_tracker.start_pipeline_run(
                workflow_name="test_recovery_workflow",
                input_data={"test": "recovery"},
                config={"type": "sequential"}
            )
            
            assert new_run_id != pipeline_run_id, "New run ID same as failed run"
            
            mlflow_tracker.end_pipeline_run(success=True, result={"status": "recovered"})
            
            self.test_results["tests_passed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "PASSED",
                "failed_run_id": pipeline_run_id,
                "recovery_run_id": new_run_id,
                "error_info": error_info
            }
            
            self.session_logger.log_info(f"Test {test_name}: PASSED")
            
        except Exception as e:
            self.test_results["tests_failed"] += 1
            self.test_results["test_details"][test_name] = {
                "status": "FAILED",
                "error": str(e)
            }
            
            self.session_logger.log_error(e, {"test": test_name})
    
    def print_test_summary(self) -> None:
        """Print a summary of test results"""
        print("\n" + "="*60)
        print("MLflow Integration Test Summary")
        print("="*60)
        print(f"Total Tests: {self.test_results['tests_run']}")
        print(f"Passed: {self.test_results['tests_passed']}")
        print(f"Failed: {self.test_results['tests_failed']}")
        print(f"Success Rate: {self.test_results.get('success_rate_percent', 0):.1f}%")
        print(f"Total Duration: {self.test_results.get('total_duration_seconds', 0):.2f}s")
        print(f"Overall Status: {self.test_results.get('overall_status', 'UNKNOWN')}")
        
        print("\nTest Details:")
        print("-" * 40)
        for test_name, details in self.test_results["test_details"].items():
            status = details["status"]
            status_symbol = "✓" if status == "PASSED" else "✗"
            print(f"{status_symbol} {test_name}: {status}")
            if status == "FAILED" and "error" in details:
                print(f"    Error: {details['error']}")
        
        print("\n" + "="*60)


def main():
    """Run MLflow integration tests"""
    print("Starting MLflow Integration Tests for Code Grapher")
    print(f"Test started at: {datetime.now().isoformat()}")
    
    # Initialize test suite
    test_suite = MLflowIntegrationTest()
    
    try:
        # Run all tests
        results = test_suite.run_all_tests()
        
        # Print summary
        test_suite.print_test_summary()
        
        # Save results to file
        results_file = Path("mlflow_integration_test_results.json")
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2, default=str)
        
        print(f"\nDetailed results saved to: {results_file}")
        
        # Return exit code based on test results
        if results.get("tests_failed", 0) == 0:
            print("\n🎉 All tests passed! MLflow integration is working correctly.")
            return 0
        else:
            print(f"\n❌ {results['tests_failed']} test(s) failed. Check the logs for details.")
            return 1
    
    except Exception as e:
        print(f"\n💥 Test suite crashed: {str(e)}")
        return 2


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)