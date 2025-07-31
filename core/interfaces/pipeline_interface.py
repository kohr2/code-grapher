"""
Pipeline Interface

Defines the contract for pipeline orchestration services.
"""

from abc import ABC, abstractmethod
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from enum import Enum


class PipelineStatus(Enum):
    """Pipeline execution status"""
    PENDING = "pending"
    RUNNING = "running"
    SUCCESS = "success"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class PipelineResult:
    """Result of pipeline execution"""
    status: PipelineStatus
    message: str
    data: Optional[Dict[str, Any]] = None
    errors: Optional[List[str]] = None
    execution_time: Optional[float] = None
    
    @classmethod
    def success(cls, data: Dict[str, Any], message: str = "Pipeline completed successfully", execution_time: float = None) -> 'PipelineResult':
        return cls(
            status=PipelineStatus.SUCCESS,
            message=message,
            data=data,
            execution_time=execution_time
        )
    
    @classmethod
    def failure(cls, errors: List[str], message: str = "Pipeline execution failed") -> 'PipelineResult':
        return cls(
            status=PipelineStatus.FAILED,
            message=message,
            errors=errors
        )


class PipelineInterface(ABC):
    """Interface for pipeline orchestration"""
    
    @abstractmethod
    async def run_enhanced_pipeline(self, target_directory: str, **kwargs) -> PipelineResult:
        """
        Run the enhanced codebase analysis pipeline
        
        Args:
            target_directory: Directory to analyze
            **kwargs: Additional pipeline configuration
            
        Returns:
            PipelineResult with execution details
        """
        pass
    
    @abstractmethod
    async def validate_pipeline_config(self, config: Dict[str, Any]) -> bool:
        """
        Validate pipeline configuration
        
        Args:
            config: Pipeline configuration to validate
            
        Returns:
            True if configuration is valid
        """
        pass
    
    @abstractmethod
    async def get_pipeline_status(self, pipeline_id: str) -> PipelineStatus:
        """
        Get status of running pipeline
        
        Args:
            pipeline_id: Unique pipeline identifier
            
        Returns:
            Current pipeline status
        """
        pass