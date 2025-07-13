"""
MLflow Agent Tracker for Code Grapher Pipeline

[2025-07-12 10:30:45] [MLFLOW_TRACKER] [INFO] Creating comprehensive MLflow integration 
for agent pipeline monitoring and visualization.

This module provides:
1. Pipeline-level MLflow run management
2. Agent-level nested run tracking  
3. Performance metrics and artifact logging
4. Real-time pipeline visualization
5. Integration with existing logging and AI evaluation systems
"""

import os
import time
import json
import traceback
from typing import Dict, Any, Optional, List, Union
from datetime import datetime
from pathlib import Path
from contextlib import contextmanager

import mlflow
import mlflow.sklearn
from mlflow.tracking import MlflowClient
import pandas as pd

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment


class MLflowAgentTracker:
    """
    Comprehensive MLflow tracking system for Code Grapher agent pipeline.
    
    Features:
    - Pipeline-level experiment management
    - Nested runs for individual agents
    - Performance metrics tracking
    - Artifact logging and management
    - Real-time dashboard integration
    - Error tracking and debugging support
    """
    
    def __init__(self, experiment_name: str = "CodeGrapher-Agent-Pipeline"):
        self.experiment_name = experiment_name
        self.client = MlflowClient()
        
        # Initialize session logger
        self.session_logger = logger.create_session_logger("MLflowTracker")
        
        self.session_logger.log_operation_start(
            "MLflowTracker.init",
            {
                "experiment_name": experiment_name,
                "mlflow_tracking_uri": mlflow.get_tracking_uri()
            }
        )
        
        start_time = time.time()
        
        try:
            # Setup MLflow experiment
            self._setup_experiment()
            
            # Initialize tracking state
            self.current_pipeline_run = None
            self.agent_runs = {}
            self.pipeline_metrics = {}
            self.agent_metrics = {}
            
            # Create artifacts directory
            self.artifacts_dir = Path("mlflow_artifacts")
            self.artifacts_dir.mkdir(exist_ok=True)
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "MLflowTracker.init",
                duration=duration,
                success=True,
                details={
                    "experiment_id": self.experiment.experiment_id,
                    "tracking_uri": mlflow.get_tracking_uri()
                }
            )
            
            # Track successful initialization
            ai_tracker.record_success(
                component="mlflow-tracker",
                description=f"Successfully initialized MLflow tracking for experiment {experiment_name}",
                time_saved=duration
            )
            
            self.session_logger.log_info(f"MLflow Agent Tracker initialized for experiment: {experiment_name}")
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "MLflowTracker.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Track initialization failure
            ai_tracker.record_failure(
                component="mlflow-tracker",
                description=f"Failed to initialize MLflow tracker: {str(e)}",
                error_type=type(e).__name__,
                workaround="Check MLflow installation and tracking URI configuration"
            )
            raise
    
    def _setup_experiment(self) -> None:
        """Setup or get existing MLflow experiment"""
        try:
            self.experiment = mlflow.get_experiment_by_name(self.experiment_name)
            if self.experiment is None:
                experiment_id = mlflow.create_experiment(
                    name=self.experiment_name,
                    tags={
                        "project": "code-grapher",
                        "purpose": "agent-pipeline-tracking", 
                        "created_by": "mlflow-agent-tracker",
                        "created_at": datetime.now().isoformat()
                    }
                )
                self.experiment = mlflow.get_experiment(experiment_id)
                
                self.session_logger.log_decision(
                    decision=f"Created new MLflow experiment: {self.experiment_name}",
                    reasoning="No existing experiment found with this name",
                    alternatives=["Use default experiment", "Generate unique experiment name"]
                )
            else:
                self.session_logger.log_decision(
                    decision=f"Using existing MLflow experiment: {self.experiment_name}",
                    reasoning="Found existing experiment with matching name",
                    alternatives=["Create new experiment with timestamp", "Archive old experiment"]
                )
                
            mlflow.set_experiment(self.experiment_name)
            
        except Exception as e:
            self.session_logger.log_error(e, {"context": "experiment_setup"})
            raise
    
    def start_pipeline_run(self, 
                          workflow_name: str, 
                          input_data: Dict[str, Any],
                          config: Optional[Dict[str, Any]] = None) -> str:
        """
        Start a new MLflow run for the entire pipeline execution.
        
        Args:
            workflow_name: Name of the workflow being executed
            input_data: Input data for the pipeline
            config: Pipeline configuration
            
        Returns:
            MLflow run ID for the pipeline
        """
        run_name = f"pipeline_{workflow_name}_{datetime.now().strftime('%Y%m%d_%H%M%S')}"
        
        self.session_logger.log_operation_start(
            "MLflowTracker.start_pipeline_run",
            {
                "workflow_name": workflow_name,
                "run_name": run_name,
                "input_keys": list(input_data.keys()) if input_data else []
            }
        )
        
        try:
            # End any existing active run first
            if mlflow.active_run():
                mlflow.end_run()
                self.session_logger.log_decision(
                    decision="Ended existing active MLflow run before starting new pipeline",
                    reasoning="MLflow only allows one active run at a time",
                    alternatives=["Use nested runs", "Skip pipeline tracking"]
                )
            
            # Start the main pipeline run
            self.current_pipeline_run = mlflow.start_run(
                run_name=run_name,
                tags={
                    "workflow_name": workflow_name,
                    "run_type": "pipeline",
                    "started_at": datetime.now().isoformat()
                }
            )
            
            # Log pipeline parameters
            if config:
                for key, value in config.items():
                    if isinstance(value, (str, int, float, bool)):
                        mlflow.log_param(f"config_{key}", value)
                    else:
                        mlflow.log_param(f"config_{key}", str(value))
            
            # Log input data summary
            input_summary = self._summarize_input_data(input_data)
            for key, value in input_summary.items():
                mlflow.log_metric(f"input_{key}", value)
            
            # Initialize pipeline metrics tracking
            self.pipeline_metrics = {
                "start_time": time.time(),
                "workflow_name": workflow_name,
                "agents_executed": 0,
                "total_processing_time": 0,
                "errors_count": 0
            }
            
            # Clear agent runs for new pipeline
            self.agent_runs.clear()
            
            run_id = self.current_pipeline_run.info.run_id
            
            self.session_logger.log_operation_end(
                "MLflowTracker.start_pipeline_run",
                duration=time.time() - self.pipeline_metrics["start_time"],
                success=True,
                details={
                    "run_id": run_id,
                    "run_name": run_name
                }
            )
            
            self.session_logger.log_info(f"Started MLflow pipeline run: {run_id}")
            return run_id
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "workflow_name": workflow_name,
                "run_name": run_name
            })
            
            ai_tracker.record_failure(
                component="mlflow-pipeline-start",
                description=f"Failed to start MLflow pipeline run: {str(e)}",
                error_type=type(e).__name__
            )
            raise
    
    def start_agent_run(self, 
                       agent_id: str, 
                       agent_type: str,
                       input_data: Dict[str, Any],
                       capabilities: List[str]) -> str:
        """
        Start a nested MLflow run for an individual agent execution.
        
        Args:
            agent_id: Unique identifier for the agent
            agent_type: Type/class of the agent
            input_data: Input data for the agent
            capabilities: List of agent capabilities
            
        Returns:
            MLflow run ID for the agent
        """
        if not self.current_pipeline_run:
            raise RuntimeError("No active pipeline run. Call start_pipeline_run() first.")
        
        run_name = f"agent_{agent_id}_{datetime.now().strftime('%H%M%S')}"
        
        self.session_logger.log_operation_start(
            f"MLflowTracker.start_agent_run.{agent_id}",
            {
                "agent_id": agent_id,
                "agent_type": agent_type,
                "run_name": run_name,
                "capabilities": capabilities
            }
        )
        
        try:
            # Start nested run for agent
            agent_run = mlflow.start_run(
                run_name=run_name,
                nested=True,
                tags={
                    "agent_id": agent_id,
                    "agent_type": agent_type,
                    "run_type": "agent",
                    "parent_run": self.current_pipeline_run.info.run_id,
                    "started_at": datetime.now().isoformat()
                }
            )
            
            # Log agent parameters
            mlflow.log_param("agent_id", agent_id)
            mlflow.log_param("agent_type", agent_type)
            mlflow.log_param("capabilities", json.dumps(capabilities))
            
            # Log input data summary
            input_summary = self._summarize_input_data(input_data)
            for key, value in input_summary.items():
                mlflow.log_metric(f"input_{key}", value)
            
            # Track agent run
            self.agent_runs[agent_id] = {
                "run": agent_run,
                "start_time": time.time(),
                "metrics": {},
                "agent_type": agent_type
            }
            
            run_id = agent_run.info.run_id
            
            self.session_logger.log_operation_end(
                f"MLflowTracker.start_agent_run.{agent_id}",
                duration=time.time() - self.agent_runs[agent_id]["start_time"],
                success=True,
                details={
                    "run_id": run_id,
                    "run_name": run_name
                }
            )
            
            self.session_logger.log_info(f"Started MLflow agent run for {agent_id}: {run_id}")
            return run_id
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "agent_id": agent_id,
                "agent_type": agent_type
            })
            
            ai_tracker.record_failure(
                component=f"mlflow-agent-start-{agent_id}",
                description=f"Failed to start MLflow agent run: {str(e)}",
                error_type=type(e).__name__
            )
            raise
    
    def log_agent_metrics(self, 
                         agent_id: str, 
                         metrics: Dict[str, Union[int, float]],
                         step: Optional[int] = None) -> None:
        """
        Log metrics for a specific agent.
        
        Args:
            agent_id: Agent identifier
            metrics: Dictionary of metric name -> value
            step: Optional step number for time series metrics
        """
        if agent_id not in self.agent_runs:
            self.session_logger.log_warning(f"No active run for agent {agent_id}")
            return
        
        self.session_logger.log_operation_start(
            f"MLflowTracker.log_agent_metrics.{agent_id}",
            {
                "metrics_count": len(metrics),
                "metrics_keys": list(metrics.keys()),
                "step": step
            }
        )
        
        try:
            # Set context to agent run
            with mlflow.start_run(run_id=self.agent_runs[agent_id]["run"].info.run_id):
                for metric_name, metric_value in metrics.items():
                    if isinstance(metric_value, (int, float)):
                        mlflow.log_metric(metric_name, metric_value, step=step)
                        
                        # Store in agent metrics for pipeline aggregation
                        if metric_name not in self.agent_runs[agent_id]["metrics"]:
                            self.agent_runs[agent_id]["metrics"][metric_name] = []
                        self.agent_runs[agent_id]["metrics"][metric_name].append(metric_value)
            
            # Log to session logger as well
            for metric_name, metric_value in metrics.items():
                self.session_logger.log_performance(
                    metric=f"agent_{agent_id}_{metric_name}",
                    value=metric_value,
                    unit="units",
                    context={"agent_id": agent_id, "step": step}
                )
            
            self.session_logger.log_operation_end(
                f"MLflowTracker.log_agent_metrics.{agent_id}",
                duration=0.1,
                success=True,
                details={"metrics_logged": len(metrics)}
            )
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "agent_id": agent_id,
                "metrics": metrics
            })
            
            ai_tracker.record_failure(
                component=f"mlflow-metrics-{agent_id}",
                description=f"Failed to log agent metrics: {str(e)}",
                error_type=type(e).__name__
            )
    
    def log_agent_artifact(self, 
                          agent_id: str, 
                          artifact_data: Any,
                          artifact_name: str,
                          artifact_type: str = "json") -> None:
        """
        Log an artifact for a specific agent.
        
        Args:
            agent_id: Agent identifier
            artifact_data: Data to store as artifact
            artifact_name: Name for the artifact
            artifact_type: Type of artifact (json, csv, txt, etc.)
        """
        if agent_id not in self.agent_runs:
            self.session_logger.log_warning(f"No active run for agent {agent_id}")
            return
        
        self.session_logger.log_operation_start(
            f"MLflowTracker.log_agent_artifact.{agent_id}",
            {
                "artifact_name": artifact_name,
                "artifact_type": artifact_type,
                "data_type": type(artifact_data).__name__
            }
        )
        
        try:
            # Create artifact file
            artifact_file = self.artifacts_dir / f"{agent_id}_{artifact_name}.{artifact_type}"
            
            if artifact_type == "json":
                with open(artifact_file, 'w') as f:
                    json.dump(artifact_data, f, indent=2, default=str)
            elif artifact_type == "csv" and isinstance(artifact_data, pd.DataFrame):
                artifact_data.to_csv(artifact_file, index=False)
            elif artifact_type == "txt":
                with open(artifact_file, 'w') as f:
                    f.write(str(artifact_data))
            else:
                # Default to JSON serialization
                with open(artifact_file, 'w') as f:
                    json.dump(artifact_data, f, indent=2, default=str)
            
            # Log artifact to MLflow
            with mlflow.start_run(run_id=self.agent_runs[agent_id]["run"].info.run_id):
                mlflow.log_artifact(str(artifact_file))
            
            self.session_logger.log_operation_end(
                f"MLflowTracker.log_agent_artifact.{agent_id}",
                duration=0.1,
                success=True,
                details={
                    "artifact_file": str(artifact_file),
                    "file_size": artifact_file.stat().st_size
                }
            )
            
            self.session_logger.log_info(f"Logged artifact {artifact_name} for agent {agent_id}")
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "agent_id": agent_id,
                "artifact_name": artifact_name
            })
            
            ai_tracker.record_failure(
                component=f"mlflow-artifact-{agent_id}",
                description=f"Failed to log agent artifact: {str(e)}",
                error_type=type(e).__name__
            )
    
    def end_agent_run(self, 
                     agent_id: str, 
                     success: bool = True,
                     output_data: Optional[Dict[str, Any]] = None,
                     error_info: Optional[Dict[str, Any]] = None) -> None:
        """
        End the MLflow run for a specific agent.
        
        Args:
            agent_id: Agent identifier
            success: Whether the agent execution was successful
            output_data: Output data from the agent
            error_info: Error information if agent failed
        """
        if agent_id not in self.agent_runs:
            self.session_logger.log_warning(f"No active run for agent {agent_id}")
            return
        
        self.session_logger.log_operation_start(
            f"MLflowTracker.end_agent_run.{agent_id}",
            {
                "success": success,
                "has_output": output_data is not None,
                "has_error": error_info is not None
            }
        )
        
        try:
            agent_run_info = self.agent_runs[agent_id]
            duration = time.time() - agent_run_info["start_time"]
            
            # Set context to agent run
            with mlflow.start_run(run_id=agent_run_info["run"].info.run_id):
                # Log final metrics
                mlflow.log_metric("execution_duration_seconds", duration)
                mlflow.log_metric("success", 1 if success else 0)
                
                # Log output data summary
                if output_data:
                    output_summary = self._summarize_output_data(output_data)
                    for key, value in output_summary.items():
                        mlflow.log_metric(f"output_{key}", value)
                
                # Log error information
                if error_info:
                    mlflow.log_param("error_type", error_info.get("error_type", "unknown"))
                    mlflow.log_param("error_message", str(error_info.get("error_message", "")))
                
                # Set run status
                status = "FINISHED" if success else "FAILED"
                mlflow.end_run(status=status)
            
            # Update pipeline metrics
            self.pipeline_metrics["agents_executed"] += 1
            self.pipeline_metrics["total_processing_time"] += duration
            if not success:
                self.pipeline_metrics["errors_count"] += 1
            
            # Clean up agent run tracking
            del self.agent_runs[agent_id]
            
            self.session_logger.log_operation_end(
                f"MLflowTracker.end_agent_run.{agent_id}",
                duration=duration,
                success=success,
                details={
                    "execution_duration": duration,
                    "pipeline_agents_completed": self.pipeline_metrics["agents_executed"]
                }
            )
            
            # Track agent completion in AI evaluation system
            if success:
                ai_tracker.record_success(
                    component=f"agent-{agent_id}",
                    description=f"Successfully completed agent {agent_id} execution",
                    time_saved=duration
                )
            else:
                ai_tracker.record_failure(
                    component=f"agent-{agent_id}",
                    description=f"Agent {agent_id} execution failed",
                    error_type=error_info.get("error_type") if error_info else "unknown"
                )
            
            self.session_logger.log_info(f"Ended MLflow agent run for {agent_id} - Success: {success}")
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "agent_id": agent_id,
                "success": success
            })
            
            ai_tracker.record_failure(
                component=f"mlflow-agent-end-{agent_id}",
                description=f"Failed to end MLflow agent run: {str(e)}",
                error_type=type(e).__name__
            )
    
    def end_pipeline_run(self, 
                        success: bool = True,
                        result: Optional[Dict[str, Any]] = None,
                        error_info: Optional[Dict[str, Any]] = None) -> None:
        """
        End the MLflow run for the entire pipeline.
        
        Args:
            success: Whether the pipeline execution was successful
            result: Final result data from the pipeline
            error_info: Error information if pipeline failed
        """
        if not self.current_pipeline_run:
            self.session_logger.log_warning("No active pipeline run to end")
            return
        
        self.session_logger.log_operation_start(
            "MLflowTracker.end_pipeline_run",
            {
                "success": success,
                "has_result": result is not None,
                "has_error": error_info is not None
            }
        )
        
        try:
            total_duration = time.time() - self.pipeline_metrics["start_time"]
            
            # Set context to pipeline run
            with mlflow.start_run(run_id=self.current_pipeline_run.info.run_id):
                # Log final pipeline metrics
                mlflow.log_metric("total_duration_seconds", total_duration)
                mlflow.log_metric("agents_executed", self.pipeline_metrics["agents_executed"])
                mlflow.log_metric("total_processing_time", self.pipeline_metrics["total_processing_time"])
                mlflow.log_metric("errors_count", self.pipeline_metrics["errors_count"])
                mlflow.log_metric("success", 1 if success else 0)
                
                # Calculate and log pipeline efficiency metrics
                if self.pipeline_metrics["agents_executed"] > 0:
                    avg_agent_time = self.pipeline_metrics["total_processing_time"] / self.pipeline_metrics["agents_executed"]
                    mlflow.log_metric("avg_agent_execution_time", avg_agent_time)
                    
                    efficiency = self.pipeline_metrics["total_processing_time"] / total_duration * 100
                    mlflow.log_metric("pipeline_efficiency_percent", efficiency)
                
                # Log result summary
                if result:
                    result_summary = self._summarize_pipeline_result(result)
                    for key, value in result_summary.items():
                        mlflow.log_metric(f"result_{key}", value)
                
                # Log error information
                if error_info:
                    mlflow.log_param("pipeline_error_type", error_info.get("error_type", "unknown"))
                    mlflow.log_param("pipeline_error_message", str(error_info.get("error_message", "")))
                
                # Create and log pipeline summary
                pipeline_summary = self._create_pipeline_summary(success, total_duration, result)
                self.log_pipeline_artifact(pipeline_summary, "pipeline_summary", "json")
                
                # Set run status
                status = "FINISHED" if success else "FAILED"
                mlflow.end_run(status=status)
            
            # Clean up tracking state
            self.current_pipeline_run = None
            self.pipeline_metrics.clear()
            self.agent_runs.clear()
            
            self.session_logger.log_operation_end(
                "MLflowTracker.end_pipeline_run",
                duration=total_duration,
                success=success,
                details={
                    "total_duration": total_duration,
                    "agents_executed": self.pipeline_metrics.get("agents_executed", 0)
                }
            )
            
            # Track pipeline completion in AI evaluation system
            if success:
                ai_tracker.record_success(
                    component="pipeline-execution",
                    description=f"Successfully completed pipeline execution in {total_duration:.3f}s",
                    time_saved=total_duration
                )
            else:
                ai_tracker.record_failure(
                    component="pipeline-execution",
                    description=f"Pipeline execution failed: {error_info.get('error_message') if error_info else 'unknown error'}",
                    error_type=error_info.get("error_type") if error_info else "unknown"
                )
            
            self.session_logger.log_info(f"Ended MLflow pipeline run - Success: {success}, Duration: {total_duration:.3f}s")
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "success": success,
                "pipeline_metrics": self.pipeline_metrics
            })
            
            ai_tracker.record_failure(
                component="mlflow-pipeline-end",
                description=f"Failed to end MLflow pipeline run: {str(e)}",
                error_type=type(e).__name__
            )
    
    def log_pipeline_artifact(self, 
                             artifact_data: Any,
                             artifact_name: str,
                             artifact_type: str = "json") -> None:
        """
        Log an artifact for the pipeline run.
        
        Args:
            artifact_data: Data to store as artifact
            artifact_name: Name for the artifact
            artifact_type: Type of artifact (json, csv, txt, etc.)
        """
        if not self.current_pipeline_run:
            self.session_logger.log_warning("No active pipeline run for artifact logging")
            return
        
        try:
            # Create artifact file
            artifact_file = self.artifacts_dir / f"pipeline_{artifact_name}.{artifact_type}"
            
            if artifact_type == "json":
                with open(artifact_file, 'w') as f:
                    json.dump(artifact_data, f, indent=2, default=str)
            elif artifact_type == "csv" and isinstance(artifact_data, pd.DataFrame):
                artifact_data.to_csv(artifact_file, index=False)
            elif artifact_type == "txt":
                with open(artifact_file, 'w') as f:
                    f.write(str(artifact_data))
            else:
                # Default to JSON serialization
                with open(artifact_file, 'w') as f:
                    json.dump(artifact_data, f, indent=2, default=str)
            
            # Log artifact to MLflow
            with mlflow.start_run(run_id=self.current_pipeline_run.info.run_id):
                mlflow.log_artifact(str(artifact_file))
            
            self.session_logger.log_info(f"Logged pipeline artifact: {artifact_name}")
            
        except Exception as e:
            self.session_logger.log_error(e, {
                "artifact_name": artifact_name,
                "artifact_type": artifact_type
            })
    
    @contextmanager
    def agent_tracking_context(self, 
                              agent_id: str, 
                              agent_type: str,
                              input_data: Dict[str, Any],
                              capabilities: List[str]):
        """
        Context manager for automatic agent run tracking.
        
        Usage:
            with tracker.agent_tracking_context("my-agent", "CodeParserAgent", input_data, capabilities):
                # Agent execution code here
                tracker.log_agent_metrics("my-agent", {"processed_files": 10})
                result = agent.execute(input_data)
                tracker.log_agent_artifact("my-agent", result, "output")
        """
        agent_run_id = None
        success = True
        error_info = None
        
        try:
            agent_run_id = self.start_agent_run(agent_id, agent_type, input_data, capabilities)
            yield agent_run_id
            
        except Exception as e:
            success = False
            error_info = {
                "error_type": type(e).__name__,
                "error_message": str(e),
                "traceback": traceback.format_exc()
            }
            
            # Log error to agent run
            if agent_run_id:
                self.log_agent_artifact(agent_id, error_info, "error_info", "json")
            
            raise
            
        finally:
            if agent_run_id:
                self.end_agent_run(agent_id, success=success, error_info=error_info)
    
    def _summarize_input_data(self, input_data: Dict[str, Any]) -> Dict[str, Union[int, float]]:
        """Create numerical summary of input data for metrics logging"""
        if not input_data:
            return {"input_empty": 1}
        
        summary = {
            "input_keys_count": len(input_data),
            "input_size_bytes": len(str(input_data))
        }
        
        # Count different data types
        for key, value in input_data.items():
            if isinstance(value, list):
                summary[f"input_{key}_list_length"] = len(value)
            elif isinstance(value, dict):
                summary[f"input_{key}_dict_keys"] = len(value)
            elif isinstance(value, str):
                summary[f"input_{key}_string_length"] = len(value)
        
        return summary
    
    def _summarize_output_data(self, output_data: Dict[str, Any]) -> Dict[str, Union[int, float]]:
        """Create numerical summary of output data for metrics logging"""
        if not output_data:
            return {"output_empty": 1}
        
        summary = {
            "output_keys_count": len(output_data),
            "output_size_bytes": len(str(output_data))
        }
        
        # Count different data types and common patterns
        for key, value in output_data.items():
            if isinstance(value, list):
                summary[f"output_{key}_list_length"] = len(value)
            elif isinstance(value, dict):
                summary[f"output_{key}_dict_keys"] = len(value)
                
                # Check for common code analysis patterns
                if "entities" in value:
                    summary["entities_count"] = len(value["entities"])
                if "relationships" in value:
                    summary["relationships_count"] = len(value["relationships"])
                if "nodes" in value:
                    summary["nodes_count"] = len(value["nodes"])
                if "errors" in value:
                    summary["errors_count"] = len(value["errors"])
        
        return summary
    
    def _summarize_pipeline_result(self, result: Dict[str, Any]) -> Dict[str, Union[int, float]]:
        """Create numerical summary of pipeline result for metrics logging"""
        if not result:
            return {"result_empty": 1}
        
        summary = {
            "result_keys_count": len(result),
            "result_size_bytes": len(str(result))
        }
        
        # Aggregate metrics from all pipeline steps
        total_entities = 0
        total_relationships = 0
        total_nodes = 0
        total_errors = 0
        
        for key, value in result.items():
            if isinstance(value, dict):
                if "entities" in value:
                    total_entities += len(value["entities"])
                if "relationships" in value:
                    total_relationships += len(value["relationships"])
                if "nodes" in value:
                    total_nodes += len(value["nodes"])
                if "errors" in value:
                    total_errors += len(value["errors"])
        
        summary.update({
            "total_entities": total_entities,
            "total_relationships": total_relationships,
            "total_nodes": total_nodes,
            "total_errors": total_errors
        })
        
        return summary
    
    def _create_pipeline_summary(self, 
                                success: bool, 
                                duration: float,
                                result: Optional[Dict[str, Any]]) -> Dict[str, Any]:
        """Create comprehensive pipeline execution summary"""
        summary = {
            "execution_summary": {
                "success": success,
                "total_duration_seconds": duration,
                "agents_executed": self.pipeline_metrics.get("agents_executed", 0),
                "total_processing_time": self.pipeline_metrics.get("total_processing_time", 0),
                "errors_count": self.pipeline_metrics.get("errors_count", 0),
                "completed_at": datetime.now().isoformat()
            },
            "performance_metrics": {
                "avg_agent_execution_time": (
                    self.pipeline_metrics.get("total_processing_time", 0) / 
                    max(self.pipeline_metrics.get("agents_executed", 1), 1)
                ),
                "pipeline_efficiency_percent": (
                    self.pipeline_metrics.get("total_processing_time", 0) / 
                    max(duration, 0.001) * 100
                )
            }
        }
        
        if result:
            summary["result_summary"] = self._summarize_pipeline_result(result)
        
        return summary
    
    def get_current_run_info(self) -> Dict[str, Any]:
        """Get information about current active runs"""
        return {
            "pipeline_run_active": self.current_pipeline_run is not None,
            "pipeline_run_id": self.current_pipeline_run.info.run_id if self.current_pipeline_run else None,
            "active_agent_runs": list(self.agent_runs.keys()),
            "agent_run_ids": {
                agent_id: run_info["run"].info.run_id 
                for agent_id, run_info in self.agent_runs.items()
            }
        }
    
    def get_experiment_info(self) -> Dict[str, Any]:
        """Get information about the MLflow experiment"""
        return {
            "experiment_name": self.experiment_name,
            "experiment_id": self.experiment.experiment_id,
            "tracking_uri": mlflow.get_tracking_uri(),
            "artifacts_location": self.experiment.artifact_location
        }
    
    def cleanup_active_runs(self) -> None:
        """Force cleanup of any active MLflow runs"""
        try:
            # End any active MLflow run
            if mlflow.active_run():
                mlflow.end_run()
                self.session_logger.log_decision(
                    decision="Force-ended active MLflow run during cleanup",
                    reasoning="Ensuring clean state for next pipeline execution",
                    alternatives=["Leave run active", "Mark run as failed"]
                )
            
            # Clear internal tracking state
            self.current_pipeline_run = None
            self.agent_runs.clear()
            self.pipeline_metrics.clear()
            
            self.session_logger.log_info("Cleaned up MLflow tracking state")
            
        except Exception as e:
            self.session_logger.log_error(e, {"context": "cleanup_active_runs"})


# Global MLflow tracker instance
mlflow_tracker = MLflowAgentTracker()

# Convenience functions
def start_pipeline_tracking(workflow_name: str, input_data: Dict[str, Any], config: Optional[Dict[str, Any]] = None) -> str:
    """Start MLflow tracking for a pipeline"""
    return mlflow_tracker.start_pipeline_run(workflow_name, input_data, config)

def start_agent_tracking(agent_id: str, agent_type: str, input_data: Dict[str, Any], capabilities: List[str]) -> str:
    """Start MLflow tracking for an agent"""
    return mlflow_tracker.start_agent_run(agent_id, agent_type, input_data, capabilities)

def log_agent_metrics(agent_id: str, metrics: Dict[str, Union[int, float]], step: Optional[int] = None) -> None:
    """Log metrics for an agent"""
    mlflow_tracker.log_agent_metrics(agent_id, metrics, step)

def end_agent_tracking(agent_id: str, success: bool = True, output_data: Optional[Dict[str, Any]] = None, error_info: Optional[Dict[str, Any]] = None) -> None:
    """End MLflow tracking for an agent"""
    mlflow_tracker.end_agent_run(agent_id, success, output_data, error_info)

def end_pipeline_tracking(success: bool = True, result: Optional[Dict[str, Any]] = None, error_info: Optional[Dict[str, Any]] = None) -> None:
    """End MLflow tracking for a pipeline"""
    mlflow_tracker.end_pipeline_run(success, result, error_info)

def cleanup_mlflow_state() -> None:
    """Force cleanup of MLflow tracking state"""
    mlflow_tracker.cleanup_active_runs()