from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List
import time
import json
from datetime import datetime
from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment
from mlflow_agent_tracker import mlflow_tracker, start_agent_tracking, end_agent_tracking, log_agent_metrics

class AgentError(Exception):
    """Base exception for agent-related errors"""
    pass

class AgentConfigurationError(AgentError):
    """Raised when agent configuration is invalid"""
    pass

class AgentExecutionError(AgentError):
    """Raised when agent execution fails"""
    pass

class BaseAgent(ABC):
    """Base class for all AI Lego Brick agents with comprehensive logging"""
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        self.agent_id = agent_id
        self.config = config
        self.shared_state = shared_state or {}
        self.capabilities = config.get("capabilities", [])
        self.description = config.get("description", f"Agent {agent_id}")
        
        # Initialize session logger
        self.session_logger = logger.create_session_logger(f"Agent.{agent_id}")
        
        # Log agent initialization
        self.session_logger.log_operation_start(
            f"Agent.{agent_id}.init",
            {
                "agent_type": self.__class__.__name__,
                "capabilities": self.capabilities,
                "config_keys": list(config.keys())
            }
        )
        
        # Validate configuration
        self._validate_configuration()
        
        # Track successful initialization
        ai_tracker.record_success(
            component=f"agent-{agent_id}",
            description=f"Successfully initialized {self.__class__.__name__}",
            time_saved=0.1
        )
        
        self.session_logger.log_operation_end(
            f"Agent.{agent_id}.init",
            duration=0.1,
            success=True,
            details={"validation": "passed"}
        )
    
    @abstractmethod
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute the agent's main functionality"""
        pass
    
    @abstractmethod
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        pass
    
    @abstractmethod
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return self.capabilities
    
    def can_handle(self, task_type: str) -> bool:
        """Check if agent can handle a specific task type"""
        return task_type in self.capabilities
    
    def execute_with_monitoring(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute agent with comprehensive monitoring and error handling"""
        execution_id = f"{self.agent_id}_{int(time.time())}"
        
        self.session_logger.log_operation_start(
            f"Agent.{self.agent_id}.execute",
            {
                "execution_id": execution_id,
                "input_keys": list(input_data.keys()) if input_data else [],
                "input_size": len(str(input_data)) if input_data else 0
            }
        )
        
        start_time = time.time()
        success = False
        result = {}
        error_info = None
        
        # Start MLflow agent tracking
        mlflow_run_id = None
        try:
            mlflow_run_id = start_agent_tracking(
                agent_id=self.agent_id,
                agent_type=self.__class__.__name__,
                input_data=input_data,
                capabilities=self.capabilities
            )
            
            self.session_logger.log_info(f"Started MLflow tracking for agent {self.agent_id}: {mlflow_run_id}")
            
        except Exception as e:
            # Log MLflow start failure but continue execution
            self.session_logger.log_error(e, {"context": "mlflow_agent_start"})
            ai_tracker.record_failure(
                component=f"agent-mlflow-start-{self.agent_id}",
                description=f"Failed to start MLflow tracking: {str(e)}",
                error_type=type(e).__name__,
                workaround="Agent will continue without MLflow tracking"
            )
        
        try:
            # Log decision to execute
            self.session_logger.log_decision(
                decision=f"Executing {self.agent_id} with provided input",
                reasoning=f"Agent has required capabilities: {self.capabilities}",
                alternatives=["Skip execution", "Delegate to another agent"]
            )
            
            # Pre-execution validation
            self._validate_input(input_data)
            
            # Execute main functionality
            result = self.execute(input_data)
            
            # Post-execution validation
            self._validate_output(result)
            
            success = True
            duration = time.time() - start_time
            
            # Log successful execution
            self.session_logger.log_operation_end(
                f"Agent.{self.agent_id}.execute",
                duration=duration,
                success=True,
                details={
                    "execution_id": execution_id,
                    "output_keys": list(result.keys()) if result else [],
                    "output_size": len(str(result)) if result else 0
                }
            )
            
            # Log performance metrics
            self.session_logger.log_performance(
                metric=f"agent_{self.agent_id}_execution_time",
                value=duration * 1000,
                unit="ms",
                context={
                    "input_size": len(str(input_data)) if input_data else 0,
                    "output_size": len(str(result)) if result else 0,
                    "execution_id": execution_id
                }
            )
            
            # Log MLflow agent metrics
            try:
                if mlflow_run_id:
                    agent_metrics = {
                        "execution_duration_seconds": duration,
                        "input_size_bytes": len(str(input_data)) if input_data else 0,
                        "output_size_bytes": len(str(result)) if result else 0,
                        "execution_quality": self._calculate_execution_quality(result)
                    }
                    
                    # Add agent-specific metrics if available
                    if hasattr(self, '_get_agent_specific_metrics'):
                        agent_specific = self._get_agent_specific_metrics(result)
                        agent_metrics.update(agent_specific)
                    
                    log_agent_metrics(self.agent_id, agent_metrics)
                    
                    # Log artifact if result contains significant data
                    if result and len(str(result)) > 100:
                        mlflow_tracker.log_agent_artifact(
                            self.agent_id, 
                            result, 
                            "execution_output", 
                            "json"
                        )
                    
                    self.session_logger.log_info(f"Logged MLflow metrics for agent {self.agent_id}")
                    
            except Exception as e:
                self.session_logger.log_error(e, {"context": "mlflow_agent_metrics"})
            
            # Track successful execution
            ai_tracker.record_success(
                component=f"agent-{self.agent_id}",
                description=f"Successfully executed {self.agent_id} in {duration:.3f}s",
                time_saved=duration,
                accuracy=self._calculate_execution_quality(result)
            )
            
            # End MLflow agent tracking (success)
            try:
                if mlflow_run_id:
                    end_agent_tracking(
                        agent_id=self.agent_id,
                        success=True,
                        output_data=result
                    )
                    self.session_logger.log_info(f"Completed MLflow tracking for successful agent {self.agent_id}")
            except Exception as e:
                self.session_logger.log_error(e, {"context": "mlflow_agent_end_success"})
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            error_info = {
                "error_type": type(e).__name__,
                "error_message": str(e),
                "execution_id": execution_id
            }
            
            # Log failed execution
            self.session_logger.log_operation_end(
                f"Agent.{self.agent_id}.execute",
                duration=duration,
                success=False,
                details=error_info
            )
            
            # Log the error with context
            self.session_logger.log_error(e, {
                "agent_id": self.agent_id,
                "execution_id": execution_id,
                "input_data": input_data
            })
            
            # End MLflow agent tracking (failure)
            try:
                if mlflow_run_id:
                    end_agent_tracking(
                        agent_id=self.agent_id,
                        success=False,
                        error_info=error_info
                    )
                    self.session_logger.log_info(f"Completed MLflow tracking for failed agent {self.agent_id}")
            except Exception as mlflow_error:
                self.session_logger.log_error(mlflow_error, {"context": "mlflow_agent_end_failure"})
            
            # Track execution failure
            ai_tracker.record_failure(
                component=f"agent-{self.agent_id}",
                description=f"Failed to execute {self.agent_id}: {str(e)}",
                error_type=type(e).__name__,
                workaround=self._suggest_workaround(e)
            )
            
            # Re-raise as AgentExecutionError
            raise AgentExecutionError(f"Agent {self.agent_id} execution failed: {str(e)}") from e
    
    def _validate_input(self, input_data: Dict[str, Any]) -> None:
        """Validate input data format and content"""
        if not isinstance(input_data, dict):
            raise AgentExecutionError(f"Input must be a dictionary, got {type(input_data)}")
        
        # Log input validation
        self.session_logger.log_decision(
            decision="Input validation passed",
            reasoning="Input data is properly formatted dictionary",
            alternatives=["Reject invalid input", "Attempt input transformation"]
        )
    
    def _validate_output(self, output_data: Dict[str, Any]) -> None:
        """Validate output data format and content"""
        if not isinstance(output_data, dict):
            raise AgentExecutionError(f"Output must be a dictionary, got {type(output_data)}")
        
        # Check for required output schema if defined
        expected_schema = self.config.get("outputs", {}).get("schema", {})
        if expected_schema:
            self._validate_against_schema(output_data, expected_schema)
        
        # Log output validation
        self.session_logger.log_decision(
            decision="Output validation passed",
            reasoning="Output data conforms to expected schema",
            alternatives=["Return partial results", "Retry with different parameters"]
        )
    
    def _validate_against_schema(self, data: Dict[str, Any], schema: Dict[str, Any]) -> None:
        """Basic schema validation (simplified JSON Schema validation)"""
        required_properties = schema.get("properties", {})
        
        for prop_name, prop_schema in required_properties.items():
            if prop_name not in data:
                continue  # Optional properties
            
            expected_type = prop_schema.get("type")
            actual_value = data[prop_name]
            
            if expected_type == "string" and not isinstance(actual_value, str):
                raise AgentExecutionError(f"Property '{prop_name}' must be string, got {type(actual_value)}")
            elif expected_type == "number" and not isinstance(actual_value, (int, float)):
                raise AgentExecutionError(f"Property '{prop_name}' must be number, got {type(actual_value)}")
            elif expected_type == "array" and not isinstance(actual_value, list):
                raise AgentExecutionError(f"Property '{prop_name}' must be array, got {type(actual_value)}")
            elif expected_type == "object" and not isinstance(actual_value, dict):
                raise AgentExecutionError(f"Property '{prop_name}' must be object, got {type(actual_value)}")
    
    def _calculate_execution_quality(self, result: Dict[str, Any]) -> float:
        """Calculate a quality score for the execution (0-100)"""
        # Base quality score
        quality_score = 80.0
        
        # Adjust based on result completeness
        if not result:
            quality_score -= 30.0
        
        # Adjust based on presence of errors
        if "errors" in result and result["errors"]:
            error_count = len(result["errors"])
            quality_score -= min(error_count * 10, 40)
        
        # Ensure score is within bounds
        return max(0.0, min(100.0, quality_score))
    
    def _suggest_workaround(self, error: Exception) -> str:
        """Suggest potential workarounds for common errors"""
        error_type = type(error).__name__
        
        workarounds = {
            "FileNotFoundError": "Check file paths and ensure files exist",
            "PermissionError": "Verify file permissions and access rights",
            "MemoryError": "Reduce batch size or increase available memory",
            "TimeoutError": "Increase timeout duration or optimize processing",
            "ConnectionError": "Check network connectivity and service availability",
            "ValidationError": "Review input data format and required fields"
        }
        
        return workarounds.get(error_type, "Review agent configuration and input data")
    
    def update_shared_state(self, key: str, value: Any) -> None:
        """Update shared state between agents"""
        self.shared_state[key] = value
        self.session_logger.log_decision(
            decision=f"Updated shared state key '{key}'",
            reasoning="Sharing data between agents for pipeline coordination",
            alternatives=["Store in temporary files", "Pass directly to next agent"]
        )
    
    def get_shared_state(self, key: str, default: Any = None) -> Any:
        """Get value from shared state"""
        value = self.shared_state.get(key, default)
        self.session_logger.log_decision(
            decision=f"Retrieved shared state key '{key}'",
            reasoning="Accessing shared data from previous pipeline steps",
            alternatives=["Load from files", "Request from previous agent"]
        )
        return value
    
    def log_agent_specific_metrics(self, metrics: Dict[str, Any]) -> None:
        """Log agent-specific performance metrics"""
        for metric_name, metric_value in metrics.items():
            self.session_logger.log_performance(
                metric=f"agent_{self.agent_id}_{metric_name}",
                value=metric_value,
                unit="units",
                context={"agent_id": self.agent_id}
            )
    
    def get_status(self) -> Dict[str, Any]:
        """Get current agent status and statistics"""
        return {
            "agent_id": self.agent_id,
            "agent_type": self.__class__.__name__,
            "capabilities": self.capabilities,
            "description": self.description,
            "shared_state_keys": list(self.shared_state.keys()),
            "timestamp": datetime.now().isoformat()
        }
    
    def __str__(self) -> str:
        return f"{self.__class__.__name__}(id={self.agent_id}, capabilities={self.capabilities})"
    
    def __repr__(self) -> str:
        return self.__str__()