"""
Data models for update operations
"""
from dataclasses import dataclass
from typing import Any, Dict, List, Optional
from enum import Enum


class UpdateTaskType(Enum):
    """Types of update tasks"""
    SURGICAL_UPDATE = "surgical_update"
    FULL_REFRESH = "full_refresh" 
    RELATIONSHIP_UPDATE = "relationship_update"
    ENTITY_UPDATE = "entity_update"


class UpdateStatus(Enum):
    """Status of update operations"""
    PENDING = "pending"
    IN_PROGRESS = "in_progress"
    COMPLETED = "completed"
    FAILED = "failed"
    ROLLED_BACK = "rolled_back"


@dataclass
class UpdateTask:
    """Represents an update task"""
    task_id: str
    task_type: UpdateTaskType
    description: str
    input_data: Dict[str, Any]
    metadata: Optional[Dict[str, Any]] = None
    priority: int = 1  # 1 = high, 5 = low
    
    def is_valid(self) -> bool:
        """Check if task is valid"""
        return bool(self.task_id and self.task_type and self.input_data)


@dataclass
class UpdateResult:
    """Result of an update operation"""
    task_id: str
    status: UpdateStatus
    success: bool
    message: str
    entities_processed: int = 0
    relationships_processed: int = 0
    errors: List[str] = None
    warnings: List[str] = None
    execution_time: float = 0.0
    metadata: Optional[Dict[str, Any]] = None
    
    def __post_init__(self):
        if self.errors is None:
            self.errors = []
        if self.warnings is None:
            self.warnings = []
    
    @classmethod
    def success(cls, task_id: str, message: str, **kwargs) -> 'UpdateResult':
        """Create a successful result"""
        return cls(
            task_id=task_id,
            status=UpdateStatus.COMPLETED,
            success=True,
            message=message,
            **kwargs
        )
    
    @classmethod
    def failure(cls, task_id: str, errors: List[str], **kwargs) -> 'UpdateResult':
        """Create a failed result"""
        return cls(
            task_id=task_id,
            status=UpdateStatus.FAILED,
            success=False,
            message=f"Update failed with {len(errors)} errors",
            errors=errors,
            **kwargs
        )


@dataclass
class UpdatePlan:
    """Plan for executing an update"""
    plan_id: str
    task: UpdateTask
    steps: List['UpdateStep']
    estimated_time: float
    risk_level: str  # "low", "medium", "high"
    rollback_plan: Optional['RollbackPlan'] = None
    
    def is_valid(self) -> bool:
        """Check if plan is valid"""
        return bool(self.plan_id and self.task and self.steps)


@dataclass 
class UpdateStep:
    """Individual step in an update plan"""
    step_id: str
    description: str
    operation: str
    parameters: Dict[str, Any]
    dependencies: List[str] = None  # Step IDs this depends on
    estimated_time: float = 0.0
    
    def __post_init__(self):
        if self.dependencies is None:
            self.dependencies = []


@dataclass
class PlanValidationResult:
    """Result of validating an update plan"""
    is_valid: bool
    errors: List[str]
    warnings: List[str]
    risk_assessment: str
    
    def __post_init__(self):
        if self.errors is None:
            self.errors = []
        if self.warnings is None:
            self.warnings = []


@dataclass
class RollbackPlan:
    """Plan for rolling back an update"""
    rollback_id: str
    original_plan_id: str
    rollback_steps: List[UpdateStep]
    estimated_time: float


@dataclass
class GitDiff:
    """Represents a git diff for updates"""
    commit_hash: str
    author: str
    timestamp: str
    message: str
    files_changed: List['FileChange']
    
    def get_changed_files(self) -> List[str]:
        """Get list of changed file paths"""
        return [fc.file_path for fc in self.files_changed]


@dataclass
class FileChange:
    """Represents a change to a file"""
    file_path: str
    change_type: str  # "added", "modified", "deleted"
    lines_added: int = 0
    lines_removed: int = 0
    content_before: Optional[str] = None
    content_after: Optional[str] = None