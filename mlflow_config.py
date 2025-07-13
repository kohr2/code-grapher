"""
MLflow Configuration and Setup Utilities for Code Grapher

[2025-07-12 10:35:15] [MLFLOW_CONFIG] [INFO] Creating MLflow configuration management
and setup utilities for seamless integration with Code Grapher pipeline.

This module provides:
1. MLflow server setup and configuration
2. Environment-specific tracking URI management
3. Dashboard configuration and startup utilities
4. Experiment management and cleanup tools
5. Integration with existing logging system
"""

import os
import sys
import subprocess
import time
import requests
from typing import Dict, Any, Optional, List
from pathlib import Path
import mlflow
from mlflow.tracking import MlflowClient

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker


class MLflowConfig:
    """
    MLflow configuration management for Code Grapher.
    
    Handles environment setup, server management, and dashboard configuration.
    """
    
    def __init__(self, config_file: Optional[str] = None):
        self.session_logger = logger.create_session_logger("MLflowConfig")
        
        # Default configuration
        self.config = {
            "tracking_uri": "sqlite:///mlflow.db",
            "artifact_location": "./mlflow_artifacts",
            "server_host": "localhost",
            "server_port": 5000,
            "backend_store_uri": "sqlite:///mlflow.db",
            "default_experiment_name": "CodeGrapher-Agent-Pipeline",
            "auto_start_server": True,
            "dashboard_url": None
        }
        
        # Load custom configuration if provided
        if config_file and Path(config_file).exists():
            self._load_config_file(config_file)
        
        # Override with environment variables
        self._load_env_config()
        
        self.session_logger.log_info(f"MLflow config initialized with tracking URI: {self.config['tracking_uri']}")
    
    def _load_config_file(self, config_file: str) -> None:
        """Load configuration from JSON file"""
        try:
            import json
            with open(config_file, 'r') as f:
                file_config = json.load(f)
            
            self.config.update(file_config.get("mlflow", {}))
            
            self.session_logger.log_decision(
                decision=f"Loaded MLflow config from {config_file}",
                reasoning="Custom configuration file provided",
                alternatives=["Use default configuration", "Load from environment only"]
            )
            
        except Exception as e:
            self.session_logger.log_error(e, {"config_file": config_file})
            ai_tracker.record_failure(
                component="mlflow-config-load",
                description=f"Failed to load config file: {str(e)}",
                error_type=type(e).__name__
            )
    
    def _load_env_config(self) -> None:
        """Load configuration from environment variables"""
        env_mappings = {
            "MLFLOW_TRACKING_URI": "tracking_uri",
            "MLFLOW_ARTIFACT_LOCATION": "artifact_location",
            "MLFLOW_SERVER_HOST": "server_host",
            "MLFLOW_SERVER_PORT": "server_port",
            "MLFLOW_BACKEND_STORE_URI": "backend_store_uri",
            "MLFLOW_DEFAULT_EXPERIMENT": "default_experiment_name"
        }
        
        for env_var, config_key in env_mappings.items():
            if env_var in os.environ:
                # Handle port as integer
                if config_key == "server_port":
                    self.config[config_key] = int(os.environ[env_var])
                else:
                    self.config[config_key] = os.environ[env_var]
                
                self.session_logger.log_decision(
                    decision=f"Override config {config_key} from environment variable {env_var}",
                    reasoning="Environment variable takes precedence over default config",
                    alternatives=["Use config file value", "Use default value"]
                )
    
    def setup_mlflow(self) -> Dict[str, Any]:
        """
        Setup MLflow environment and configuration.
        
        Returns:
            Setup status and configuration information
        """
        self.session_logger.log_operation_start(
            "MLflowConfig.setup_mlflow",
            self.config
        )
        
        start_time = time.time()
        setup_status = {
            "success": False,
            "tracking_uri_set": False,
            "server_started": False,
            "dashboard_available": False,
            "experiment_created": False
        }
        
        try:
            # Create artifact directory
            artifact_path = Path(self.config["artifact_location"])
            artifact_path.mkdir(parents=True, exist_ok=True)
            
            # Set MLflow tracking URI
            mlflow.set_tracking_uri(self.config["tracking_uri"])
            setup_status["tracking_uri_set"] = True
            
            self.session_logger.log_info(f"Set MLflow tracking URI to: {self.config['tracking_uri']}")
            
            # Start MLflow server if configured
            if self.config.get("auto_start_server", False) and self._should_start_server():
                server_info = self.start_mlflow_server()
                setup_status["server_started"] = server_info["started"]
                setup_status["dashboard_available"] = server_info["dashboard_available"]
            
            # Create default experiment if it doesn't exist
            experiment_info = self.create_default_experiment()
            setup_status["experiment_created"] = experiment_info["created"]
            
            setup_status["success"] = True
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "MLflowConfig.setup_mlflow",
                duration=duration,
                success=True,
                details=setup_status
            )
            
            # Track successful setup
            ai_tracker.record_success(
                component="mlflow-setup",
                description="Successfully configured MLflow environment",
                time_saved=duration
            )
            
            return setup_status
            
        except Exception as e:
            duration = time.time() - start_time
            setup_status["error"] = str(e)
            
            self.session_logger.log_operation_end(
                "MLflowConfig.setup_mlflow",
                duration=duration,
                success=False,
                details=setup_status
            )
            
            ai_tracker.record_failure(
                component="mlflow-setup",
                description=f"Failed to setup MLflow: {str(e)}",
                error_type=type(e).__name__
            )
            
            raise
    
    def _should_start_server(self) -> bool:
        """Determine if MLflow server should be started"""
        # Don't start server if tracking URI is already a server URL
        if self.config["tracking_uri"].startswith("http"):
            return False
        
        # Check if server is already running
        try:
            dashboard_url = f"http://{self.config['server_host']}:{self.config['server_port']}"
            response = requests.get(dashboard_url, timeout=2)
            if response.status_code == 200:
                self.session_logger.log_decision(
                    decision="MLflow server already running, skipping startup",
                    reasoning=f"Server responded at {dashboard_url}",
                    alternatives=["Force restart server", "Use different port"]
                )
                return False
        except requests.RequestException:
            pass
        
        return True
    
    def start_mlflow_server(self) -> Dict[str, Any]:
        """
        Start MLflow tracking server.
        
        Returns:
            Server startup status and information
        """
        self.session_logger.log_operation_start(
            "MLflowConfig.start_mlflow_server",
            {
                "host": self.config["server_host"],
                "port": self.config["server_port"],
                "backend_store_uri": self.config["backend_store_uri"]
            }
        )
        
        server_info = {
            "started": False,
            "dashboard_available": False,
            "process_id": None,
            "dashboard_url": None
        }
        
        try:
            # Build MLflow server command
            cmd = [
                sys.executable, "-m", "mlflow", "server",
                "--backend-store-uri", self.config["backend_store_uri"],
                "--default-artifact-root", self.config["artifact_location"],
                "--host", self.config["server_host"],
                "--port", str(self.config["server_port"])
            ]
            
            self.session_logger.log_decision(
                decision=f"Starting MLflow server with command: {' '.join(cmd)}",
                reasoning="No existing server found, starting new instance",
                alternatives=["Use file-based tracking only", "Connect to remote server"]
            )
            
            # Start server process
            process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                start_new_session=True
            )
            
            server_info["process_id"] = process.pid
            
            # Wait for server to start and check availability
            dashboard_url = f"http://{self.config['server_host']}:{self.config['server_port']}"
            server_info["dashboard_url"] = dashboard_url
            
            max_wait_time = 30  # seconds
            wait_interval = 1
            elapsed_time = 0
            
            while elapsed_time < max_wait_time:
                try:
                    response = requests.get(dashboard_url, timeout=2)
                    if response.status_code == 200:
                        server_info["started"] = True
                        server_info["dashboard_available"] = True
                        break
                except requests.RequestException:
                    pass
                
                time.sleep(wait_interval)
                elapsed_time += wait_interval
            
            if server_info["started"]:
                # Update configuration with server URL
                self.config["tracking_uri"] = dashboard_url
                self.config["dashboard_url"] = dashboard_url
                mlflow.set_tracking_uri(dashboard_url)
                
                self.session_logger.log_operation_end(
                    "MLflowConfig.start_mlflow_server",
                    duration=elapsed_time,
                    success=True,
                    details=server_info
                )
                
                self.session_logger.log_info(f"MLflow server started successfully at {dashboard_url}")
                
                ai_tracker.record_success(
                    component="mlflow-server-start",
                    description=f"Successfully started MLflow server at {dashboard_url}",
                    time_saved=elapsed_time
                )
            else:
                server_info["error"] = "Server failed to start within timeout period"
                
                self.session_logger.log_operation_end(
                    "MLflowConfig.start_mlflow_server",
                    duration=elapsed_time,
                    success=False,
                    details=server_info
                )
                
                ai_tracker.record_failure(
                    component="mlflow-server-start",
                    description="MLflow server failed to start within timeout",
                    error_type="TimeoutError"
                )
            
            return server_info
            
        except Exception as e:
            server_info["error"] = str(e)
            
            self.session_logger.log_error(e, {
                "server_config": self.config,
                "cmd": cmd if 'cmd' in locals() else None
            })
            
            ai_tracker.record_failure(
                component="mlflow-server-start",
                description=f"Failed to start MLflow server: {str(e)}",
                error_type=type(e).__name__
            )
            
            return server_info
    
    def create_default_experiment(self) -> Dict[str, Any]:
        """
        Create the default experiment for Code Grapher.
        
        Returns:
            Experiment creation status and information
        """
        experiment_info = {
            "created": False,
            "experiment_id": None,
            "experiment_name": self.config["default_experiment_name"]
        }
        
        try:
            client = MlflowClient()
            
            # Check if experiment already exists
            experiment = client.get_experiment_by_name(self.config["default_experiment_name"])
            
            if experiment is None:
                # Create new experiment
                experiment_id = mlflow.create_experiment(
                    name=self.config["default_experiment_name"],
                    tags={
                        "project": "code-grapher",
                        "purpose": "agent-pipeline-tracking",
                        "created_by": "mlflow-config",
                        "created_at": time.strftime("%Y-%m-%d %H:%M:%S")
                    }
                )
                
                experiment_info["created"] = True
                experiment_info["experiment_id"] = experiment_id
                
                self.session_logger.log_decision(
                    decision=f"Created new MLflow experiment: {self.config['default_experiment_name']}",
                    reasoning="No existing experiment found with this name",
                    alternatives=["Use default experiment", "Generate unique name"]
                )
                
                ai_tracker.record_success(
                    component="mlflow-experiment-create",
                    description=f"Created new experiment: {self.config['default_experiment_name']}",
                    time_saved=0.1
                )
            else:
                experiment_info["experiment_id"] = experiment.experiment_id
                
                self.session_logger.log_decision(
                    decision=f"Using existing MLflow experiment: {self.config['default_experiment_name']}",
                    reasoning="Found existing experiment with matching name",
                    alternatives=["Create new with timestamp", "Archive old experiment"]
                )
            
            # Set as active experiment
            mlflow.set_experiment(self.config["default_experiment_name"])
            
            return experiment_info
            
        except Exception as e:
            experiment_info["error"] = str(e)
            
            self.session_logger.log_error(e, {
                "experiment_name": self.config["default_experiment_name"]
            })
            
            ai_tracker.record_failure(
                component="mlflow-experiment-create",
                description=f"Failed to create/access experiment: {str(e)}",
                error_type=type(e).__name__
            )
            
            return experiment_info
    
    def get_dashboard_url(self) -> Optional[str]:
        """Get the MLflow dashboard URL if available"""
        return self.config.get("dashboard_url")
    
    def get_tracking_uri(self) -> str:
        """Get the current MLflow tracking URI"""
        return self.config["tracking_uri"]
    
    def get_artifacts_location(self) -> str:
        """Get the artifacts storage location"""
        return self.config["artifact_location"]
    
    def validate_setup(self) -> Dict[str, Any]:
        """
        Validate MLflow setup and connectivity.
        
        Returns:
            Validation results and recommendations
        """
        self.session_logger.log_operation_start("MLflowConfig.validate_setup")
        
        validation_results = {
            "tracking_uri_accessible": False,
            "experiments_accessible": False,
            "artifacts_writable": False,
            "dashboard_accessible": False,
            "overall_status": "failed",
            "recommendations": []
        }
        
        try:
            # Test tracking URI accessibility
            client = MlflowClient()
            try:
                experiments = client.list_experiments()
                validation_results["tracking_uri_accessible"] = True
                validation_results["experiments_accessible"] = True
                
                self.session_logger.log_info(f"Successfully connected to MLflow tracking server, found {len(experiments)} experiments")
                
            except Exception as e:
                validation_results["recommendations"].append(
                    f"Cannot access tracking URI {self.config['tracking_uri']}: {str(e)}"
                )
            
            # Test artifacts directory writability
            try:
                artifact_path = Path(self.config["artifact_location"])
                artifact_path.mkdir(parents=True, exist_ok=True)
                
                test_file = artifact_path / "test_write.txt"
                test_file.write_text("test")
                test_file.unlink()
                
                validation_results["artifacts_writable"] = True
                
            except Exception as e:
                validation_results["recommendations"].append(
                    f"Cannot write to artifacts location {self.config['artifact_location']}: {str(e)}"
                )
            
            # Test dashboard accessibility
            if self.config.get("dashboard_url"):
                try:
                    response = requests.get(self.config["dashboard_url"], timeout=5)
                    if response.status_code == 200:
                        validation_results["dashboard_accessible"] = True
                except Exception as e:
                    validation_results["recommendations"].append(
                        f"Dashboard not accessible at {self.config['dashboard_url']}: {str(e)}"
                    )
            
            # Determine overall status
            critical_checks = [
                validation_results["tracking_uri_accessible"],
                validation_results["experiments_accessible"],
                validation_results["artifacts_writable"]
            ]
            
            if all(critical_checks):
                validation_results["overall_status"] = "healthy"
            elif any(critical_checks):
                validation_results["overall_status"] = "partial"
            else:
                validation_results["overall_status"] = "failed"
            
            self.session_logger.log_operation_end(
                "MLflowConfig.validate_setup",
                duration=1.0,
                success=validation_results["overall_status"] != "failed",
                details=validation_results
            )
            
            return validation_results
            
        except Exception as e:
            validation_results["error"] = str(e)
            validation_results["recommendations"].append(f"Validation failed: {str(e)}")
            
            self.session_logger.log_error(e, {"config": self.config})
            
            return validation_results
    
    def cleanup_experiments(self, keep_recent: int = 10) -> Dict[str, Any]:
        """
        Clean up old MLflow experiments and runs.
        
        Args:
            keep_recent: Number of recent experiments to keep
            
        Returns:
            Cleanup results and statistics
        """
        self.session_logger.log_operation_start(
            "MLflowConfig.cleanup_experiments",
            {"keep_recent": keep_recent}
        )
        
        cleanup_stats = {
            "experiments_deleted": 0,
            "runs_deleted": 0,
            "artifacts_cleaned": 0,
            "space_freed_mb": 0
        }
        
        try:
            client = MlflowClient()
            
            # Get all experiments sorted by creation time
            experiments = client.list_experiments()
            
            # Sort by creation time (most recent first)
            experiments.sort(
                key=lambda x: x.creation_time,
                reverse=True
            )
            
            # Keep only recent experiments, delete the rest
            for experiment in experiments[keep_recent:]:
                if experiment.name != self.config["default_experiment_name"]:
                    try:
                        # Delete all runs in the experiment first
                        runs = client.search_runs(experiment.experiment_id)
                        for run in runs:
                            client.delete_run(run.info.run_id)
                            cleanup_stats["runs_deleted"] += 1
                        
                        # Delete the experiment
                        client.delete_experiment(experiment.experiment_id)
                        cleanup_stats["experiments_deleted"] += 1
                        
                        self.session_logger.log_info(f"Deleted experiment: {experiment.name}")
                        
                    except Exception as e:
                        self.session_logger.log_error(e, {
                            "experiment_id": experiment.experiment_id,
                            "experiment_name": experiment.name
                        })
            
            self.session_logger.log_operation_end(
                "MLflowConfig.cleanup_experiments",
                duration=1.0,
                success=True,
                details=cleanup_stats
            )
            
            return cleanup_stats
            
        except Exception as e:
            cleanup_stats["error"] = str(e)
            
            self.session_logger.log_error(e, {"keep_recent": keep_recent})
            
            return cleanup_stats
    
    def export_config(self, output_file: str) -> None:
        """Export current configuration to JSON file"""
        try:
            import json
            with open(output_file, 'w') as f:
                json.dump(self.config, f, indent=2)
            
            self.session_logger.log_info(f"Exported MLflow configuration to {output_file}")
            
        except Exception as e:
            self.session_logger.log_error(e, {"output_file": output_file})


# Global configuration instance
mlflow_config = MLflowConfig()

# Convenience functions
def setup_mlflow_environment(config_file: Optional[str] = None) -> Dict[str, Any]:
    """Setup MLflow environment with optional config file"""
    if config_file:
        config = MLflowConfig(config_file)
    else:
        config = mlflow_config
    
    return config.setup_mlflow()

def get_dashboard_url() -> Optional[str]:
    """Get MLflow dashboard URL"""
    return mlflow_config.get_dashboard_url()

def validate_mlflow_setup() -> Dict[str, Any]:
    """Validate current MLflow setup"""
    return mlflow_config.validate_setup()

def cleanup_old_experiments(keep_recent: int = 10) -> Dict[str, Any]:
    """Clean up old MLflow experiments"""
    return mlflow_config.cleanup_experiments(keep_recent)