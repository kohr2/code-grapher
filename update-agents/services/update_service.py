"""
Main Update Service - Public Interface for Update Intelligence
Orchestrates all update operations through clean service boundaries
"""
import time
from typing import Any, Dict, List, Optional
from ..interfaces.update_models import (
    UpdateResult, GitDiff, UpdatePlan, UpdateTask, UpdateTaskType
)
from ..agents.coordination_agent import CoordinationAgent
from .surgical_update_service import SurgicalUpdateService
from shared.interfaces.logger_interface import LoggerInterface
from shared.interfaces.graph_operations_interface import GraphOperationsInterface
from shared.interfaces.service_interface import ServiceInterface

# Import AI services
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent.parent / "ai-services"))
from interfaces.ai_services_interface import AIServicesInterface


class UpdateIntelligenceInterface:
    """Interface for update intelligence operations"""
    
    def analyze_git_diff(self, commit_hash: str) -> 'DiffAnalysis':
        """Analyze git diff for a commit"""
        pass
    
    def plan_surgical_update(self, diff_analysis: 'DiffAnalysis') -> UpdatePlan:
        """Plan a surgical update from diff analysis"""
        pass
    
    def execute_update(self, update_plan: UpdatePlan) -> UpdateResult:
        """Execute an update plan"""
        pass
    
    def get_update_status(self, task_id: str) -> Optional[UpdateResult]:
        """Get status of an update task"""
        pass


class UpdateService(UpdateIntelligenceInterface, ServiceInterface):
    """
    Main update service that orchestrates all update intelligence operations
    Provides clean interface for surgical updates, git diff analysis, and coordination
    """
    
    def __init__(self,
                 graph_service: GraphOperationsInterface,
                 ai_service: AIServicesInterface,
                 logger: Optional[LoggerInterface] = None):
        """Initialize update service"""
        self.graph_service = graph_service
        self.ai_service = ai_service
        self.logger = logger
        
        # Initialize sub-services
        self.surgical_updater = SurgicalUpdateService(graph_service, ai_service, logger)
        self.coordination_agent = CoordinationAgent(graph_service, logger)
        
        # Service state
        self._initialized = False
        self._active_updates: Dict[str, UpdateTask] = {}
        
        if self.logger:
            self.logger.log_info("UpdateService initialized")
    
    # ServiceInterface implementation
    
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the update service"""
        try:
            # Initialize coordination agent
            self.coordination_agent.initialize(config.get('coordination', {}))
            
            # Update configuration
            self.config = config
            self._initialized = True
            
            if self.logger:
                self.logger.log_info("UpdateService initialization completed")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize UpdateService: {e}")
            raise
    
    def shutdown(self) -> None:
        """Shutdown the update service"""
        try:
            self._active_updates.clear()
            self._initialized = False
            
            if self.logger:
                self.logger.log_info("UpdateService shutdown completed")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Error during UpdateService shutdown: {e}")
    
    def health_check(self) -> Dict[str, Any]:
        """Get health status of update service"""
        try:
            return {
                "status": "healthy" if self._initialized else "unhealthy",
                "initialized": self._initialized,
                "active_updates": len(self._active_updates),
                "services": {
                    "surgical_updater": self.surgical_updater is not None,
                    "coordination_agent": self.coordination_agent is not None,
                    "graph_service": self.graph_service is not None,
                    "ai_service": self.ai_service is not None
                }
            }
            
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": str(e)
            }
    
    def get_service_name(self) -> str:
        """Return service name"""
        return "UpdateService"
    
    # UpdateIntelligenceInterface implementation
    
    def analyze_git_diff(self, commit_hash: str) -> 'DiffAnalysis':
        """Analyze git diff for a commit"""
        if self.logger:
            self.logger.log_info(f"Analyzing git diff for commit: {commit_hash}")
        
        try:
            # Create git diff analysis (placeholder implementation)
            # In real implementation, would use GitPython or similar
            diff_analysis = DiffAnalysis(
                commit_hash=commit_hash,
                files_changed=[],
                analysis_time=time.time(),
                complexity="medium",
                risk_level="low"
            )
            
            return diff_analysis
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Git diff analysis failed for {commit_hash}: {e}")
            raise
    
    def plan_surgical_update(self, diff_analysis: 'DiffAnalysis') -> UpdatePlan:
        """Plan surgical update from diff analysis"""
        if self.logger:
            self.logger.log_info(f"Planning surgical update for commit: {diff_analysis.commit_hash}")
        
        try:
            # Create GitDiff object from analysis
            git_diff = self._create_git_diff_from_analysis(diff_analysis)
            
            # Use surgical updater to create plan
            plan = self.surgical_updater.create_update_plan(git_diff)
            
            return plan
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Surgical update planning failed: {e}")
            raise
    
    def execute_update(self, update_plan: UpdatePlan) -> UpdateResult:
        """Execute update plan using coordination agent"""
        if self.logger:
            self.logger.log_info(f"Executing update plan: {update_plan.plan_id}")
        
        try:
            # Store active update
            self._active_updates[update_plan.task.task_id] = update_plan.task
            
            # Execute through coordination agent
            result = self.coordination_agent.coordinate_update(update_plan.task)
            
            # Clean up active update
            if update_plan.task.task_id in self._active_updates:
                del self._active_updates[update_plan.task.task_id]
            
            return result
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Update execution failed for plan {update_plan.plan_id}: {e}")
            
            # Clean up on error
            if update_plan.task.task_id in self._active_updates:
                del self._active_updates[update_plan.task.task_id]
            
            raise
    
    def get_update_status(self, task_id: str) -> Optional[UpdateResult]:
        """Get status of update task"""
        return self.coordination_agent.get_task_status(task_id)
    
    # High-level convenience methods
    
    def surgical_update_from_commit(self, commit_hash: str) -> UpdateResult:
        """Perform complete surgical update from git commit"""
        if self.logger:
            self.logger.log_info(f"Starting complete surgical update for commit: {commit_hash}")
        
        start_time = time.time()
        
        try:
            # Step 1: Analyze the diff
            diff_analysis = self.analyze_git_diff(commit_hash)
            
            # Step 2: Plan the update
            update_plan = self.plan_surgical_update(diff_analysis)
            
            # Step 3: Execute the plan
            result = self.execute_update(update_plan)
            
            result.execution_time = time.time() - start_time
            
            if self.logger:
                self.logger.log_info(
                    f"Complete surgical update finished: {commit_hash}, "
                    f"Success: {result.success}, Time: {result.execution_time:.2f}s"
                )
            
            return result
        
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Complete surgical update failed for {commit_hash}: {e}")
            
            return UpdateResult.failure(
                f"surgical_{commit_hash}",
                [str(e)],
                execution_time=time.time() - start_time
            )
    
    def update_from_git_diff(self, diff: GitDiff) -> UpdateResult:
        """Update directly from GitDiff object"""
        if self.logger:
            self.logger.log_info(f"Updating from git diff: {diff.commit_hash}")
        
        try:
            # Use surgical updater directly
            return self.surgical_updater.update_from_diff(diff)
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Git diff update failed: {e}")
            raise
    
    def validate_update_plan(self, plan: UpdatePlan) -> 'PlanValidationResult':
        """Validate an update plan"""
        return self.coordination_agent.validate_plan(plan)
    
    def get_active_updates(self) -> Dict[str, UpdateTask]:
        """Get currently active updates"""
        return self._active_updates.copy()
    
    def cancel_update(self, task_id: str) -> bool:
        """Cancel an active update"""
        if task_id in self._active_updates:
            del self._active_updates[task_id]
            if self.logger:
                self.logger.log_info(f"Cancelled update: {task_id}")
            return True
        return False
    
    # Private helper methods
    
    def _create_git_diff_from_analysis(self, analysis: 'DiffAnalysis') -> GitDiff:
        """Create GitDiff object from DiffAnalysis"""
        from ..interfaces.update_models import FileChange
        
        # Convert analysis to GitDiff format
        return GitDiff(
            commit_hash=analysis.commit_hash,
            author="unknown",  # Would be extracted from git
            timestamp=str(analysis.analysis_time),
            message=f"Surgical update for {analysis.commit_hash}",
            files_changed=[
                FileChange(
                    file_path=file_info.get('path', ''),
                    change_type=file_info.get('change_type', 'modified'),
                    lines_added=file_info.get('lines_added', 0),
                    lines_removed=file_info.get('lines_removed', 0)
                )
                for file_info in analysis.files_changed
            ]
        )


# Additional data classes for API

class DiffAnalysis:
    """Analysis result from git diff"""
    
    def __init__(self, commit_hash: str, files_changed: List[Dict[str, Any]], 
                 analysis_time: float, complexity: str, risk_level: str):
        self.commit_hash = commit_hash
        self.files_changed = files_changed
        self.analysis_time = analysis_time
        self.complexity = complexity
        self.risk_level = risk_level
    
    def get_summary(self) -> Dict[str, Any]:
        """Get summary of diff analysis"""
        return {
            "commit_hash": self.commit_hash,
            "files_changed": len(self.files_changed),
            "complexity": self.complexity,
            "risk_level": self.risk_level,
            "analysis_time": self.analysis_time
        }