import json
import os
import time
from typing import Dict, Any, List, Optional, Union
from pathlib import Path
from datetime import datetime
import threading
from concurrent.futures import ThreadPoolExecutor, Future

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment
from mlflow_agent_tracker import mlflow_tracker, start_pipeline_tracking, end_pipeline_tracking
from mlflow_config import mlflow_config, setup_mlflow_environment


class WorkflowExecutionError(Exception):
    """Raised when workflow execution fails"""
    pass

class ConfigurationError(Exception):
    """Raised when configuration is invalid"""
    pass

class AgentCoordinator:
    """
    Main orchestration engine for AI Lego Brick agents
    Manages workflow execution, state coordination, and monitoring
    """
    
    def __init__(self, config_path: str):
        self.config_path = config_path
        
        # Initialize session logger first
        self.session_logger = logger.create_session_logger("AgentCoordinator")
        
        self.config = self._load_configuration()
        self.agents = {}
        self.shared_state = {}
        self.execution_history = []
        
        # Initialize MLflow tracking
        self.mlflow_run_id = None
        self._setup_mlflow_tracking()
        
        self.session_logger.log_operation_start(
            "AgentCoordinator.init",
            {
                "config_path": config_path,
                "pipeline_name": self.config.get("pipeline", {}).get("name", "unknown")
            }
        )
        
        start_time = time.time()
        
        try:
            # Validate configuration
            self._validate_configuration()
            
            # Initialize agents
            self._initialize_agents()
            
            # Setup monitoring
            self._setup_monitoring()
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "AgentCoordinator.init",
                duration=duration,
                success=True,
                details={
                    "agents_initialized": len(self.agents),
                    "workflows_available": len(self.config.get("workflows", {}))
                }
            )
            
            # Track successful initialization
            ai_tracker.record_success(
                component="agent-coordinator",
                description=f"Successfully initialized coordinator with {len(self.agents)} agents",
                time_saved=duration
            )
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "AgentCoordinator.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Track initialization failure
            ai_tracker.record_failure(
                component="agent-coordinator",
                description=f"Failed to initialize coordinator: {str(e)}",
                error_type=type(e).__name__,
                workaround="Check configuration file format and agent dependencies"
            )
            raise
    
    def _load_configuration(self) -> Dict[str, Any]:
        """Load and parse configuration file"""
        try:
            with open(self.config_path, 'r') as f:
                config = json.load(f)
            
            # Environment variable substitution
            config = self._substitute_environment_variables(config)
            
            self.session_logger.log_info(f"Loaded configuration from {self.config_path}")
            return config
            
        except FileNotFoundError:
            raise ConfigurationError(f"Configuration file not found: {self.config_path}")
        except json.JSONDecodeError as e:
            raise ConfigurationError(f"Invalid JSON in configuration file: {e}")
    
    def _substitute_environment_variables(self, obj: Any) -> Any:
        """Recursively substitute environment variables in configuration"""
        if isinstance(obj, str):
            # Handle ${VAR_NAME} and ${VAR_NAME:-default} patterns
            import re
            pattern = r'\$\{([^}]+)\}'
            
            def replace_var(match):
                var_expr = match.group(1)
                if ':-' in var_expr:
                    var_name, default = var_expr.split(':-', 1)
                    return os.getenv(var_name, default)
                else:
                    return os.getenv(var_expr, match.group(0))
            
            return re.sub(pattern, replace_var, obj)
        
        elif isinstance(obj, dict):
            return {k: self._substitute_environment_variables(v) for k, v in obj.items()}
        elif isinstance(obj, list):
            return [self._substitute_environment_variables(item) for item in obj]
        else:
            return obj
    
    def _validate_configuration(self) -> None:
        """Validate configuration structure and required fields"""
        required_sections = ["pipeline", "agents", "workflows"]
        
        for section in required_sections:
            if section not in self.config:
                raise ConfigurationError(f"Missing required configuration section: {section}")
        
        # Validate pipeline section
        pipeline = self.config["pipeline"]
        if "name" not in pipeline:
            raise ConfigurationError("Pipeline name is required")
        
        # Validate agents section
        agents = self.config["agents"]
        if not agents:
            raise ConfigurationError("At least one agent must be configured")
        
        for agent_id, agent_config in agents.items():
            self._validate_agent_config(agent_id, agent_config)
        
        # Validate workflows section
        workflows = self.config["workflows"]
        if not workflows:
            raise ConfigurationError("At least one workflow must be configured")
        
        for workflow_id, workflow_config in workflows.items():
            self._validate_workflow_config(workflow_id, workflow_config)
        
        self.session_logger.log_decision(
            decision="Configuration validation passed",
            reasoning="All required sections and fields are present and valid",
            alternatives=["Use default configuration", "Prompt for missing values"]
        )
    
    def _validate_agent_config(self, agent_id: str, config: Dict[str, Any]) -> None:
        """Validate individual agent configuration"""
        required_fields = ["type", "description", "capabilities"]
        
        for field in required_fields:
            if field not in config:
                raise ConfigurationError(f"Agent {agent_id} missing required field: {field}")
        
        if not isinstance(config["capabilities"], list):
            raise ConfigurationError(f"Agent {agent_id} capabilities must be a list")
    
    def _validate_workflow_config(self, workflow_id: str, config: Dict[str, Any]) -> None:
        """Validate individual workflow configuration"""
        required_fields = ["type", "steps"]
        
        for field in required_fields:
            if field not in config:
                raise ConfigurationError(f"Workflow {workflow_id} missing required field: {field}")
        
        if not isinstance(config["steps"], list):
            raise ConfigurationError(f"Workflow {workflow_id} steps must be a list")
        
        # Validate each step
        for i, step in enumerate(config["steps"]):
            if "agent" not in step:
                raise ConfigurationError(f"Workflow {workflow_id} step {i} missing agent specification")
            
            agent_id = step["agent"]
            if agent_id not in self.config["agents"]:
                raise ConfigurationError(f"Workflow {workflow_id} references unknown agent: {agent_id}")
    
    def _initialize_agents(self) -> None:
        """Initialize all configured agents"""
        agents_config = self.config["agents"]
        
        # Import agent classes dynamically
        agent_classes = self._discover_agent_classes()
        
        for agent_id, agent_config in agents_config.items():
            try:
                agent_type = agent_config["type"]
                
                # Skip supervisor agents for now (they're handled differently)
                if agent_type == "supervisor":
                    continue
                
                # Map agent ID to class name
                class_name = self._get_agent_class_name(agent_id)
                
                if class_name not in agent_classes:
                    self.session_logger.log_error(
                        Exception(f"Agent class not found: {class_name}"),
                        {"agent_id": agent_id, "agent_type": agent_type}
                    )
                    continue
                
                # Create agent instance
                agent_class = agent_classes[class_name]
                agent = agent_class(agent_id, agent_config, self.shared_state)
                
                self.agents[agent_id] = agent
                
                self.session_logger.log_info(f"Initialized agent: {agent_id} ({class_name})")
                
            except Exception as e:
                self.session_logger.log_error(e, {"agent_id": agent_id})
                ai_tracker.record_failure(
                    component=f"agent-init-{agent_id}",
                    description=f"Failed to initialize agent {agent_id}: {str(e)}",
                    error_type=type(e).__name__
                )
    
    def _discover_agent_classes(self) -> Dict[str, type]:
        """Dynamically discover available agent classes"""
        agent_classes = {}
        
        try:
            # Import known agent classes
            from agents.code_parser_agent import CodeParserAgent
            from agents.entity_extraction_agent import EntityExtractionAgent
            from agents.relationship_analysis_agent import RelationshipAnalysisAgent
            from agents.graph_builder_agent import GraphBuilderAgent
            from agents.rag_indexer_agent import RAGIndexerAgent
            
            agent_classes = {
                "CodeParserAgent": CodeParserAgent,
                "EntityExtractionAgent": EntityExtractionAgent,
                "RelationshipAnalysisAgent": RelationshipAnalysisAgent,
                "GraphBuilderAgent": GraphBuilderAgent,
                "RAGIndexerAgent": RAGIndexerAgent
            }
            
        except ImportError as e:
            self.session_logger.log_error(e, {"context": "agent_discovery"})
            ai_tracker.record_failure(
                component="agent-discovery",
                description=f"Failed to import agent classes: {str(e)}",
                error_type="ImportError",
                workaround="Ensure all agent modules are implemented and available"
            )
        
        return agent_classes
    
    def _get_agent_class_name(self, agent_id: str) -> str:
        """Map agent ID to class name"""
        id_mapping = {
            "code-parser": "CodeParserAgent",
            "entity-extractor": "EntityExtractionAgent", 
            "relationship-analyzer": "RelationshipAnalysisAgent",
            "graph-builder": "GraphBuilderAgent",
            "rag-indexer": "RAGIndexerAgent"
        }
        
        return id_mapping.get(agent_id, "BaseAgent")
    
    def _setup_monitoring(self) -> None:
        """Setup monitoring and metrics collection"""
        monitoring_config = self.config.get("monitoring", {})
        
        self.metrics_to_track = monitoring_config.get("metrics", [])
        self.performance_thresholds = monitoring_config.get("alerts", {}).get("performanceThresholds", {})
        
        self.session_logger.log_info(f"Monitoring setup with {len(self.metrics_to_track)} metrics")
    
    def _setup_mlflow_tracking(self) -> None:
        """Setup MLflow tracking for pipeline monitoring"""
        self.session_logger.log_operation_start(
            "AgentCoordinator._setup_mlflow_tracking",
            {"config_path": self.config_path}
        )
        
        start_time = time.time()
        
        try:
            # Setup MLflow environment if not already configured
            mlflow_setup_config = self.config.get("mlflow", {})
            
            if mlflow_setup_config.get("enabled", True):
                setup_result = setup_mlflow_environment()
                
                self.session_logger.log_decision(
                    decision="MLflow tracking enabled for pipeline",
                    reasoning="MLflow configuration found and setup successful",
                    alternatives=["Disable MLflow tracking", "Use file-based tracking only"]
                )
                
                # Track MLflow setup success
                ai_tracker.record_success(
                    component="coordinator-mlflow-setup",
                    description="Successfully initialized MLflow tracking for AgentCoordinator",
                    time_saved=time.time() - start_time
                )
                
                self.session_logger.log_info(f"MLflow tracking initialized - Dashboard: {mlflow_config.get_dashboard_url()}")
            else:
                self.session_logger.log_decision(
                    decision="MLflow tracking disabled",
                    reasoning="MLflow configuration disabled in config file",
                    alternatives=["Enable MLflow tracking", "Use alternative monitoring"]
                )
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "AgentCoordinator._setup_mlflow_tracking",
                duration=duration,
                success=True,
                details={"mlflow_enabled": mlflow_setup_config.get("enabled", True)}
            )
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "AgentCoordinator._setup_mlflow_tracking",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Track MLflow setup failure but don't fail initialization
            ai_tracker.record_failure(
                component="coordinator-mlflow-setup",
                description=f"Failed to setup MLflow tracking: {str(e)}",
                error_type=type(e).__name__,
                workaround="Pipeline will continue without MLflow tracking"
            )
            
            self.session_logger.log_error(e, {"context": "mlflow_setup"})
            # Don't raise - allow coordinator to work without MLflow
    
    def execute_workflow(self, workflow_name: str, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a named workflow with input data"""
        if workflow_name not in self.config["workflows"]:
            raise WorkflowExecutionError(f"Unknown workflow: {workflow_name}")
        
        workflow_config = self.config["workflows"][workflow_name]
        
        execution_id = f"{workflow_name}_{int(time.time())}"
        
        self.session_logger.log_operation_start(
            f"Workflow.{workflow_name}.execute",
            {
                "execution_id": execution_id,
                "workflow_type": workflow_config.get("type", "unknown"),
                "steps_count": len(workflow_config.get("steps", [])),
                "input_keys": list(input_data.keys())
            }
        )
        
        start_time = time.time()
        
        # Start MLflow pipeline tracking
        mlflow_run_id = None
        try:
            mlflow_run_id = start_pipeline_tracking(
                workflow_name=workflow_name,
                input_data=input_data,
                config=workflow_config
            )
            self.mlflow_run_id = mlflow_run_id
            
            self.session_logger.log_info(f"Started MLflow tracking for workflow {workflow_name}: {mlflow_run_id}")
            
        except Exception as e:
            # Log MLflow start failure but continue execution
            self.session_logger.log_error(e, {"context": "mlflow_pipeline_start"})
            ai_tracker.record_failure(
                component="workflow-mlflow-start",
                description=f"Failed to start MLflow tracking: {str(e)}",
                error_type=type(e).__name__,
                workaround="Workflow will continue without MLflow tracking"
            )
        
        try:
            # Initialize workflow state
            workflow_state = {
                "input": input_data,
                "execution_id": execution_id,
                "started_at": datetime.now().isoformat(),
                "mlflow_run_id": mlflow_run_id
            }
            
            # Execute workflow based on type
            workflow_type = workflow_config.get("type", "sequential")
            
            if workflow_type == "sequential":
                result = self._execute_sequential_workflow(workflow_config, workflow_state)
            elif workflow_type == "parallel":
                result = self._execute_parallel_workflow(workflow_config, workflow_state)
            elif workflow_type == "conditional":
                result = self._execute_conditional_workflow(workflow_config, workflow_state)
            else:
                raise WorkflowExecutionError(f"Unsupported workflow type: {workflow_type}")
            
            duration = time.time() - start_time
            
            # Add execution metadata to result
            result["execution_metadata"] = {
                "execution_id": execution_id,
                "workflow_name": workflow_name,
                "duration_seconds": duration,
                "completed_at": datetime.now().isoformat()
            }
            
            self.session_logger.log_operation_end(
                f"Workflow.{workflow_name}.execute",
                duration=duration,
                success=True,
                details={
                    "execution_id": execution_id,
                    "result_keys": list(result.keys())
                }
            )
            
            # Log workflow performance
            self.session_logger.log_performance(
                metric=f"workflow_{workflow_name}_execution_time",
                value=duration * 1000,
                unit="ms",
                context={
                    "workflow_type": workflow_type,
                    "steps_count": len(workflow_config.get("steps", [])),
                    "execution_id": execution_id
                }
            )
            
            # End MLflow pipeline tracking (success)
            try:
                if mlflow_run_id:
                    end_pipeline_tracking(success=True, result=result)
                    self.session_logger.log_info(f"Completed MLflow tracking for successful workflow {workflow_name}")
            except Exception as e:
                self.session_logger.log_error(e, {"context": "mlflow_pipeline_end_success"})
            
            # Track successful workflow execution
            ai_tracker.record_success(
                component=f"workflow-{workflow_name}",
                description=f"Successfully executed workflow {workflow_name} in {duration:.3f}s",
                time_saved=duration,
                accuracy=self._calculate_workflow_quality(result)
            )
            
            # Store execution in history
            self.execution_history.append({
                "execution_id": execution_id,
                "workflow_name": workflow_name,
                "started_at": workflow_state["started_at"],
                "completed_at": result["execution_metadata"]["completed_at"],
                "duration": duration,
                "success": True,
                "result_summary": self._summarize_result(result)
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                f"Workflow.{workflow_name}.execute",
                duration=duration,
                success=False,
                details={"error": str(e), "execution_id": execution_id}
            )
            
            self.session_logger.log_error(e, {
                "workflow_name": workflow_name,
                "execution_id": execution_id,
                "input_data": input_data
            })
            
            # End MLflow pipeline tracking (failure)
            try:
                if mlflow_run_id:
                    error_info = {
                        "error_type": type(e).__name__,
                        "error_message": str(e)
                    }
                    end_pipeline_tracking(success=False, error_info=error_info)
                    self.session_logger.log_info(f"Completed MLflow tracking for failed workflow {workflow_name}")
            except Exception as mlflow_error:
                self.session_logger.log_error(mlflow_error, {"context": "mlflow_pipeline_end_failure"})
            
            # Track workflow failure
            ai_tracker.record_failure(
                component=f"workflow-{workflow_name}",
                description=f"Failed to execute workflow {workflow_name}: {str(e)}",
                error_type=type(e).__name__,
                workaround=self._suggest_workflow_workaround(e, workflow_config)
            )
            
            # Store failed execution in history
            self.execution_history.append({
                "execution_id": execution_id,
                "workflow_name": workflow_name,
                "started_at": workflow_state["started_at"],
                "completed_at": datetime.now().isoformat(),
                "duration": duration,
                "success": False,
                "error": str(e)
            })
            
            raise WorkflowExecutionError(f"Workflow {workflow_name} failed: {str(e)}") from e
    
    def _execute_sequential_workflow(self, workflow_config: Dict[str, Any], workflow_state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute workflow steps sequentially"""
        steps = workflow_config["steps"]
        result = {"workflow_state": workflow_state.copy()}
        
        self.session_logger.log_decision(
            decision="Executing workflow sequentially",
            reasoning="Workflow type is sequential, steps must execute in order",
            alternatives=["Execute in parallel", "Execute conditionally"]
        )
        
        for i, step in enumerate(steps):
            step_name = step.get("name", f"step_{i}")
            agent_id = step["agent"]
            
            self.session_logger.log_operation_start(
                f"WorkflowStep.{step_name}",
                {
                    "step_index": i,
                    "agent_id": agent_id,
                    "step_config": step
                }
            )
            
            step_start_time = time.time()
            
            try:
                # Prepare step input
                step_input = self._prepare_step_input(step, result, workflow_state)
                
                # Execute agent
                agent = self.agents.get(agent_id)
                if not agent:
                    raise WorkflowExecutionError(f"Agent not found: {agent_id}")
                
                step_result = agent.execute_with_monitoring(step_input)
                
                # Store step result
                step_output_key = step.get("output", f"step_{i}_output")
                self._store_step_result(step_output_key, step_result, result)
                
                step_duration = time.time() - step_start_time
                
                self.session_logger.log_operation_end(
                    f"WorkflowStep.{step_name}",
                    duration=step_duration,
                    success=True,
                    details={
                        "step_index": i,
                        "agent_id": agent_id,
                        "output_key": step_output_key
                    }
                )
                
            except Exception as e:
                step_duration = time.time() - step_start_time
                
                self.session_logger.log_operation_end(
                    f"WorkflowStep.{step_name}",
                    duration=step_duration,
                    success=False,
                    details={"error": str(e), "step_index": i, "agent_id": agent_id}
                )
                
                # Handle step failure based on error handling strategy
                error_handling = workflow_config.get("errorHandling", {})
                if self._should_continue_on_error(error_handling, e):
                    self.session_logger.log_decision(
                        decision="Continuing workflow despite step failure",
                        reasoning="Error handling strategy allows continuation",
                        alternatives=["Stop workflow", "Retry step"]
                    )
                    continue
                else:
                    raise
        
        return result
    
    def _execute_parallel_workflow(self, workflow_config: Dict[str, Any], workflow_state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute workflow steps in parallel"""
        steps = workflow_config["steps"]
        parallelization = workflow_config.get("parallelization", {})
        max_workers = parallelization.get("maxConcurrentAgents", len(steps))
        
        self.session_logger.log_decision(
            decision=f"Executing workflow in parallel with {max_workers} workers",
            reasoning="Workflow type is parallel, steps can execute concurrently",
            alternatives=["Execute sequentially", "Execute in batches"]
        )
        
        result = {"workflow_state": workflow_state.copy()}
        
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            # Submit all steps
            future_to_step = {}
            for i, step in enumerate(steps):
                step_name = step.get("name", f"step_{i}")
                future = executor.submit(self._execute_parallel_step, step, i, result, workflow_state)
                future_to_step[future] = (step, i, step_name)
            
            # Collect results
            for future in future_to_step:
                step, step_index, step_name = future_to_step[future]
                try:
                    step_result, output_key = future.result()
                    self._store_step_result(output_key, step_result, result)
                    
                except Exception as e:
                    self.session_logger.log_error(e, {
                        "step_name": step_name,
                        "step_index": step_index,
                        "parallel_execution": True
                    })
                    
                    # Handle parallel step failure
                    error_handling = workflow_config.get("errorHandling", {})
                    if not self._should_continue_on_error(error_handling, e):
                        raise WorkflowExecutionError(f"Parallel step {step_name} failed: {str(e)}") from e
        
        return result
    
    def _execute_parallel_step(self, step: Dict[str, Any], step_index: int, result: Dict[str, Any], workflow_state: Dict[str, Any]) -> tuple:
        """Execute a single step in parallel workflow"""
        step_name = step.get("name", f"step_{step_index}")
        agent_id = step["agent"]
        
        self.session_logger.log_operation_start(
            f"ParallelStep.{step_name}",
            {
                "step_index": step_index,
                "agent_id": agent_id,
                "thread_id": threading.get_ident()
            }
        )
        
        step_start_time = time.time()
        
        try:
            # Prepare step input (thread-safe)
            step_input = self._prepare_step_input(step, result, workflow_state)
            
            # Execute agent
            agent = self.agents.get(agent_id)
            if not agent:
                raise WorkflowExecutionError(f"Agent not found: {agent_id}")
            
            step_result = agent.execute_with_monitoring(step_input)
            step_output_key = step.get("output", f"step_{step_index}_output")
            
            step_duration = time.time() - step_start_time
            
            self.session_logger.log_operation_end(
                f"ParallelStep.{step_name}",
                duration=step_duration,
                success=True,
                details={
                    "step_index": step_index,
                    "agent_id": agent_id,
                    "thread_id": threading.get_ident()
                }
            )
            
            return step_result, step_output_key
            
        except Exception as e:
            step_duration = time.time() - step_start_time
            
            self.session_logger.log_operation_end(
                f"ParallelStep.{step_name}",
                duration=step_duration,
                success=False,
                details={
                    "error": str(e),
                    "step_index": step_index,
                    "agent_id": agent_id,
                    "thread_id": threading.get_ident()
                }
            )
            
            raise
    
    def _execute_conditional_workflow(self, workflow_config: Dict[str, Any], workflow_state: Dict[str, Any]) -> Dict[str, Any]:
        """Execute workflow based on conditions"""
        condition = workflow_config.get("condition")
        if not condition:
            raise WorkflowExecutionError("Conditional workflow missing condition")
        
        # Evaluate condition (simplified - in production would use a proper expression parser)
        condition_result = self._evaluate_condition(condition, workflow_state)
        
        self.session_logger.log_decision(
            decision=f"Conditional workflow condition evaluated to: {condition_result}",
            reasoning=f"Condition '{condition}' was evaluated against workflow state",
            alternatives=["Execute all steps", "Skip workflow entirely"]
        )
        
        if condition_result:
            # Execute steps if condition is true
            return self._execute_sequential_workflow(workflow_config, workflow_state)
        else:
            # Return empty result if condition is false
            return {
                "workflow_state": workflow_state,
                "condition_result": False,
                "skipped": True
            }
    
    def _prepare_step_input(self, step: Dict[str, Any], current_result: Dict[str, Any], workflow_state: Dict[str, Any]) -> Dict[str, Any]:
        """Prepare input data for a workflow step"""
        step_input_config = step.get("input", {})
        
        if isinstance(step_input_config, str):
            # Simple JSONPath-like reference
            return self._resolve_data_reference(step_input_config, current_result, workflow_state)
        elif isinstance(step_input_config, dict):
            # Complex input mapping
            resolved_input = {}
            for key, value in step_input_config.items():
                if isinstance(value, str) and value.startswith("$."):
                    resolved_input[key] = self._resolve_data_reference(value, current_result, workflow_state)
                else:
                    resolved_input[key] = value
            return resolved_input
        else:
            # Use entire current result as input
            return current_result
    
    def _resolve_data_reference(self, reference: str, current_result: Dict[str, Any], workflow_state: Dict[str, Any]) -> Any:
        """Resolve JSONPath-like data references"""
        if reference.startswith("$."):
            path = reference[2:]  # Remove $.
            
            # Simple path resolution (in production would use a proper JSONPath library)
            if path.startswith("input"):
                return self._get_nested_value(workflow_state["input"], path[6:])  # Remove "input."
            else:
                return self._get_nested_value(current_result, path)
        
        return reference
    
    def _get_nested_value(self, data: Dict[str, Any], path: str) -> Any:
        """Get nested value from data using dot notation"""
        if not path:
            return data
        
        keys = path.split(".")
        current = data
        
        for key in keys:
            if isinstance(current, dict) and key in current:
                current = current[key]
            else:
                return None
        
        return current
    
    def _store_step_result(self, output_key: str, step_result: Dict[str, Any], result: Dict[str, Any]) -> None:
        """Store step result in workflow result"""
        if output_key.startswith("$."):
            # JSONPath-like storage
            path = output_key[2:]
            self._set_nested_value(result, path, step_result)
        else:
            result[output_key] = step_result
    
    def _set_nested_value(self, data: Dict[str, Any], path: str, value: Any) -> None:
        """Set nested value in data using dot notation"""
        keys = path.split(".")
        current = data
        
        for key in keys[:-1]:
            if key not in current:
                current[key] = {}
            current = current[key]
        
        current[keys[-1]] = value
    
    def _evaluate_condition(self, condition: str, workflow_state: Dict[str, Any]) -> bool:
        """Evaluate workflow condition (simplified implementation)"""
        # In production, would use a proper expression parser
        # For now, handle simple equality checks
        
        if "==" in condition:
            left, right = condition.split("==", 1)
            left_val = self._resolve_data_reference(left.strip(), {}, workflow_state)
            right_val = right.strip().strip('"\'')
            return left_val == right_val
        
        # Default to true for unhandled conditions
        return True
    
    def _should_continue_on_error(self, error_handling: Dict[str, Any], error: Exception) -> bool:
        """Determine if workflow should continue after an error"""
        strategy = error_handling.get("strategy", "stop")
        
        if strategy == "log-and-continue":
            return True
        elif strategy == "retry-with-backoff":
            # For now, don't continue (retry logic would be implemented here)
            return False
        else:
            return False
    
    def _calculate_workflow_quality(self, result: Dict[str, Any]) -> float:
        """Calculate quality score for workflow execution"""
        # Simple quality calculation based on result completeness
        if not result:
            return 0.0
        
        quality_score = 80.0
        
        # Check for execution metadata
        if "execution_metadata" in result:
            quality_score += 10.0
        
        # Check for errors
        error_count = 0
        for key, value in result.items():
            if isinstance(value, dict) and "errors" in value:
                error_count += len(value["errors"])
        
        if error_count > 0:
            quality_score -= min(error_count * 5, 30)
        
        return max(0.0, min(100.0, quality_score))
    
    def _summarize_result(self, result: Dict[str, Any]) -> Dict[str, Any]:
        """Create a summary of workflow result"""
        summary = {
            "keys_count": len(result),
            "has_metadata": "execution_metadata" in result
        }
        
        # Count entities, relationships, etc.
        for key, value in result.items():
            if isinstance(value, dict):
                if "entities" in value:
                    summary["entities_count"] = len(value["entities"])
                if "relationships" in value:
                    summary["relationships_count"] = len(value["relationships"])
                if "nodesCreated" in value:
                    summary["nodes_created"] = value["nodesCreated"]
        
        return summary
    
    def _suggest_workflow_workaround(self, error: Exception, workflow_config: Dict[str, Any]) -> str:
        """Suggest workarounds for workflow execution errors"""
        error_type = type(error).__name__
        
        workarounds = {
            "AgentExecutionError": "Check agent configuration and input data format",
            "TimeoutError": "Increase step timeout values or optimize agent performance",
            "ConnectionError": "Verify database connections and network availability",
            "MemoryError": "Reduce batch sizes or increase available memory",
            "FileNotFoundError": "Ensure all required files exist and paths are correct"
        }
        
        return workarounds.get(error_type, "Review workflow configuration and agent dependencies")
    
    def get_execution_history(self) -> List[Dict[str, Any]]:
        """Get workflow execution history"""
        return self.execution_history.copy()
    
    def get_agent_status(self) -> Dict[str, Any]:
        """Get status of all agents"""
        status = {}
        for agent_id, agent in self.agents.items():
            status[agent_id] = agent.get_status()
        return status
    
    def get_shared_state(self) -> Dict[str, Any]:
        """Get current shared state"""
        return self.shared_state.copy()
    
    def clear_shared_state(self) -> None:
        """Clear shared state (useful for testing)"""
        self.shared_state.clear()
        self.session_logger.log_info("Shared state cleared")
    
    def get_metrics_summary(self) -> Dict[str, Any]:
        """Get summary of tracked metrics"""
        return {
            "total_executions": len(self.execution_history),
            "successful_executions": len([h for h in self.execution_history if h["success"]]),
            "failed_executions": len([h for h in self.execution_history if not h["success"]]),
            "agents_count": len(self.agents),
            "shared_state_size": len(self.shared_state)
        }