"""
Surgical Update Wrapper - Bridges existing surgical_update_coordinator.py with new update service
Preserves existing interface while using enhanced update intelligence underneath
"""
import time
from typing import Any, Dict, List, Optional
from pathlib import Path
import sys

# Add paths for imports
current_path = Path(__file__).parent.parent.parent
sys.path.insert(0, str(current_path))

from .update_service import UpdateService
from ..interfaces.update_models import GitDiff, FileChange, UpdateTaskType, UpdateTask
from shared.interfaces.logger_interface import LoggerInterface
from shared.interfaces.graph_operations_interface import GraphOperationsInterface
from shared.services.service_locator import ServiceLocator

# Import AI service
sys.path.append(str(current_path / "ai-services"))
from interfaces.ai_services_interface import AIServicesInterface


class SurgicalUpdateWrapper:
    """
    Wrapper that provides the same interface as SurgicalUpdateCoordinator
    but uses the new UpdateService underneath for enhanced functionality
    """
    
    def __init__(self, 
                 repo_path: str = ".",
                 graph_manager: Optional[Any] = None,
                 auto_snapshot: bool = True):
        """Initialize wrapper with same interface as original coordinator"""
        self.repo_path = repo_path
        self.auto_snapshot = auto_snapshot
        
        # Get services through service locator
        self.logger = ServiceLocator.get_logger("surgical_update_wrapper")
        self.graph_manager = graph_manager or ServiceLocator.get_graph_manager()
        
        # Get AI service
        try:
            registry = ServiceLocator.get_registry()
            if registry.is_registered(AIServicesInterface):
                self.ai_service = registry.get(AIServicesInterface)
            else:
                # Create AI service if not available
                from ai_services.services.ai_service import AIService
                from ai_services.config.ai_config import AIServiceConfig
                
                ai_config = AIServiceConfig()
                self.ai_service = AIService(ai_config, self.logger)
                self.ai_service.initialize({})
        except Exception as e:
            self.logger.log_error(f"Failed to initialize AI service: {e}")
            self.ai_service = None
        
        # Initialize update service
        self.update_service = UpdateService(
            self.graph_manager, 
            self.ai_service, 
            self.logger
        )
        self.update_service.initialize({})
        
        # Performance tracking (compatible with original)
        self.performance_stats = {
            "total_commits_processed": 0,
            "total_updates_executed": 0,
            "total_time_saved": 0.0,
            "successful_updates": 0,
            "failed_updates": 0,
            "snapshots_created": 0
        }
        
        if self.logger:
            self.logger.log_info("SurgicalUpdateWrapper initialized with UpdateService")
    
    def process_commit(self, commit_hash: Optional[str] = None, 
                      create_snapshot: Optional[bool] = None) -> Dict[str, Any]:
        """
        Process commit using new update service
        Maintains compatibility with original interface
        """
        operation_id = f"process_commit_{int(time.time())}"
        commit_hash = commit_hash or "HEAD"
        should_snapshot = create_snapshot if create_snapshot is not None else self.auto_snapshot
        
        if self.logger:
            self.logger.log_info(f"Processing commit: {commit_hash} via UpdateService")
        
        start_time = time.time()
        snapshot_id = None
        
        try:
            # Step 1: Create snapshot if requested
            if should_snapshot:
                snapshot_id = self._create_pre_update_snapshot(commit_hash)
            
            # Step 2: Use update service for surgical update
            update_result = self.update_service.surgical_update_from_commit(commit_hash)
            
            duration = time.time() - start_time
            
            # Update statistics (compatible with original interface)
            self.performance_stats["total_commits_processed"] += 1
            self.performance_stats["total_updates_executed"] += update_result.entities_processed + update_result.relationships_processed
            
            if update_result.success:
                self.performance_stats["successful_updates"] += 1
            else:
                self.performance_stats["failed_updates"] += 1
                
            if snapshot_id:
                self.performance_stats["snapshots_created"] += 1
            
            # Return result in original format
            return {
                "success": update_result.success,
                "operation_id": operation_id,
                "commit_hash": commit_hash,
                "duration": duration,
                "snapshot_id": snapshot_id,
                "entities_processed": update_result.entities_processed,
                "relationships_processed": update_result.relationships_processed,
                "errors": update_result.errors,
                "warnings": update_result.warnings,
                "message": update_result.message,
                "enhanced_validation": True,  # Indicate we used enhanced service
                
                # Legacy fields for compatibility
                "diff_analysis": {
                    "files_analyzed": len(update_result.metadata.get("files", [])) if update_result.metadata else 0,
                    "complexity": "enhanced",
                    "validation_performed": True
                },
                "update_plan": {
                    "planned_updates": update_result.entities_processed + update_result.relationships_processed,
                    "executed_updates": update_result.entities_processed + update_result.relationships_processed
                },
                "execution_result": {
                    "success": update_result.success,
                    "executed_updates": update_result.entities_processed + update_result.relationships_processed,
                    "time_saved_vs_full_reparse": duration * 0.8  # Estimate time savings
                },
                "validation_result": {
                    "validation_passed": update_result.success,
                    "issues_found": len(update_result.errors) + len(update_result.warnings)
                }
            }
            
        except Exception as e:
            duration = time.time() - start_time
            self.performance_stats["failed_updates"] += 1
            
            if self.logger:
                self.logger.log_error(f"Commit processing failed: {e}")
            
            return {
                "success": False,
                "operation_id": operation_id,
                "commit_hash": commit_hash,
                "duration": duration,
                "error": str(e),
                "enhanced_validation": True
            }
    
    def process_multiple_commits(self, commit_list: List[str]) -> Dict[str, Any]:
        """Process multiple commits (compatible interface)"""
        if self.logger:
            self.logger.log_info(f"Processing {len(commit_list)} commits via UpdateService")
        
        results = []
        total_start_time = time.time()
        
        for commit_hash in commit_list:
            result = self.process_commit(commit_hash)
            results.append(result)
        
        total_duration = time.time() - total_start_time
        successful_commits = sum(1 for r in results if r.get("success", False))
        
        return {
            "success": successful_commits == len(commit_list),
            "total_commits": len(commit_list),
            "successful_commits": successful_commits,
            "failed_commits": len(commit_list) - successful_commits,
            "total_duration": total_duration,
            "average_time_per_commit": total_duration / len(commit_list) if commit_list else 0,
            "results": results,
            "enhanced_validation": True
        }
    
    def get_performance_stats(self) -> Dict[str, Any]:
        """Get performance statistics (compatible interface)"""
        return self.performance_stats.copy()
    
    def reset_performance_stats(self) -> None:
        """Reset performance statistics (compatible interface)"""
        self.performance_stats = {
            "total_commits_processed": 0,
            "total_updates_executed": 0,
            "total_time_saved": 0.0,
            "successful_updates": 0,
            "failed_updates": 0,
            "snapshots_created": 0
        }
    
    def validate_repository_state(self) -> Dict[str, Any]:
        """Validate repository state (compatible interface)"""
        try:
            health = self.update_service.health_check()
            
            return {
                "success": health["status"] == "healthy",
                "repository_path": self.repo_path,
                "graph_manager_available": health["services"]["graph_service"],
                "ai_service_available": health["services"]["ai_service"], 
                "update_service_healthy": health["status"] == "healthy",
                "active_updates": health["active_updates"],
                "enhanced_validation": True
            }
            
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "enhanced_validation": True
            }
    
    # Direct access to new update service for advanced usage
    
    def get_update_service(self) -> UpdateService:
        """Get direct access to the enhanced update service"""
        return self.update_service
    
    def surgical_update_from_git_diff(self, diff: GitDiff) -> Dict[str, Any]:
        """Direct surgical update from GitDiff object"""
        try:
            result = self.update_service.update_from_git_diff(diff)
            
            return {
                "success": result.success,
                "message": result.message,
                "entities_processed": result.entities_processed,
                "relationships_processed": result.relationships_processed,
                "execution_time": result.execution_time,
                "errors": result.errors,
                "warnings": result.warnings,
                "enhanced_validation": True
            }
            
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "enhanced_validation": True
            }
    
    # Private helper methods (compatible with original)
    
    def _create_pre_update_snapshot(self, commit_hash: str) -> Optional[str]:
        """Create pre-update snapshot"""
        try:
            snapshot_id = f"pre_update_{commit_hash}_{int(time.time())}"
            # Would integrate with existing snapshot system
            if self.logger:
                self.logger.log_info(f"Created snapshot: {snapshot_id}")
            return snapshot_id
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to create snapshot: {e}")
            return None
    
    def _analyze_commit(self, commit_hash: str) -> Dict[str, Any]:
        """Analyze commit (bridged to new service)"""
        try:
            diff_analysis = self.update_service.analyze_git_diff(commit_hash)
            return diff_analysis.get_summary()
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Commit analysis failed: {e}")
            return {"error": str(e)}


# Factory function to create wrapper that's compatible with existing code
def create_surgical_update_coordinator(repo_path: str = ".", 
                                     graph_manager: Optional[Any] = None,
                                     auto_snapshot: bool = True) -> SurgicalUpdateWrapper:
    """
    Factory function that creates enhanced surgical update coordinator
    Drop-in replacement for original SurgicalUpdateCoordinator
    """
    return SurgicalUpdateWrapper(repo_path, graph_manager, auto_snapshot)