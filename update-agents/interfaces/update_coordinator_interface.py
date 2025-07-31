"""
Interface for update coordination functionality
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional
from .update_models import UpdateTask, UpdateResult, UpdatePlan


class UpdateCoordinatorInterface(ABC):
    """Interface for coordinating updates"""
    
    @abstractmethod
    def coordinate_update(self, task: UpdateTask) -> UpdateResult:
        """
        Coordinate an update task with proper planning and validation
        This replaces the stub coordinator that caused silent failures
        """
        pass
    
    @abstractmethod
    def create_update_plan(self, task: UpdateTask) -> UpdatePlan:
        """Create a detailed plan for an update task"""
        pass
    
    @abstractmethod
    def validate_plan(self, plan: UpdatePlan) -> 'PlanValidationResult':
        """Validate an update plan before execution"""
        pass
    
    @abstractmethod
    def execute_plan(self, plan: UpdatePlan) -> UpdateResult:
        """Execute a validated update plan"""
        pass


class AgentInterface(ABC):
    """Base interface for all update agents"""
    
    @abstractmethod
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the agent with configuration"""
        pass
    
    @abstractmethod
    def process(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Process input data and return results"""
        pass
    
    @abstractmethod
    def get_agent_type(self) -> str:
        """Return the type/name of this agent"""
        pass