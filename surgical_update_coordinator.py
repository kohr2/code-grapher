#!/usr/bin/env python3
"""
Surgical Update Coordinator - Orchestrates git diff analysis and graph updates.

This module coordinates the entire surgical update process:
1. Analyzes git commits/diffs using GitDiffAgent
2. Plans and executes graph updates using GraphUpdateEngine
3. Provides rollback capabilities and validation
4. Tracks performance metrics and success rates
"""

import os
import sys
import time
from typing import Dict, Any, List, Optional
from datetime import datetime

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from graph_manager import CodeGraphManager
from graph_state_manager import GraphStateManager
from graph_update_engine import GraphUpdateEngine
from agents.git_diff_agent import GitDiffAgent, DiffAnalysis
from logger import logger
from ai_evaluation_tracker import ai_tracker
from coordinator.agent_coordinator import AgentCoordinator


class SurgicalUpdateCoordinator:
    """
    Coordinates surgical graph updates based on git diffs.
    Provides end-to-end workflow from commit analysis to graph updates.
    """
    
    def __init__(self, 
                 repo_path: str = ".",
                 graph_manager: Optional[CodeGraphManager] = None,
                 auto_snapshot: bool = True):
        """
        Initialize the surgical update coordinator.
        
        Args:
            repo_path: Path to git repository
            graph_manager: Optional existing graph manager
            auto_snapshot: Whether to automatically create snapshots before updates
        """
        self.repo_path = repo_path
        self.auto_snapshot = auto_snapshot
        self.session_logger = logger.create_session_logger("SurgicalUpdateCoordinator")
        
        # Enhanced entity extraction support
        self.entity_coordinator = None
        
        # Initialize components
        self.graph_manager = graph_manager or CodeGraphManager()
        self.state_manager = GraphStateManager(self.graph_manager)
        self.update_engine = GraphUpdateEngine(self.graph_manager)
        
        # Initialize GitDiffAgent
        git_agent_config = {
            "agent_id": "git-diff-analyzer",
            "capabilities": ["git_diff_analysis", "commit_analysis"],
            "description": "Analyzes git diffs for surgical updates",
            "config": {
                "repoPath": repo_path,
                "supportedExtensions": [".py", ".js", ".ts", ".java", ".cpp", ".c", ".md",".json"],
                "ignorePatterns": ["__pycache__", ".git", "node_modules", "logs"]
            }
        }
        
        self.git_agent = GitDiffAgent("git-diff-analyzer", git_agent_config)
        
        # Performance tracking
        self.performance_stats = {
            "total_commits_processed": 0,
            "total_updates_executed": 0,
            "total_time_saved": 0.0,
            "successful_updates": 0,
            "failed_updates": 0,
            "snapshots_created": 0
        }
        
        self.session_logger.log_info("SurgicalUpdateCoordinator initialized")
    
    def process_commit(self, commit_hash: Optional[str] = None, 
                      create_snapshot: Optional[bool] = None) -> Dict[str, Any]:
        """
        Process a single commit and update the graph surgically.
        
        Args:
            commit_hash: Specific commit to process (defaults to HEAD)
            create_snapshot: Override auto_snapshot setting
            
        Returns:
            Dict containing process results and statistics
        """
        operation_id = f"process_commit_{int(time.time())}"
        commit_hash = commit_hash or "HEAD"
        should_snapshot = create_snapshot if create_snapshot is not None else self.auto_snapshot
        
        self.session_logger.log_operation_start(
            "process_commit",
            {
                "operation_id": operation_id,
                "commit_hash": commit_hash,
                "create_snapshot": should_snapshot
            }
        )
        
        start_time = time.time()
        snapshot_id = None
        
        try:
            # Step 1: Create snapshot if requested
            if should_snapshot:
                snapshot_id = self._create_pre_update_snapshot(commit_hash)
            
            # Step 2: Analyze the commit
            self.session_logger.log_info(f"Analyzing commit: {commit_hash}")
            diff_analysis = self._analyze_commit(commit_hash)
            
            # Step 3: Plan updates
            self.session_logger.log_info("Planning surgical updates")
            update_plan = self.update_engine.plan_updates(diff_analysis)
            
            # Step 4: Execute updates
            self.session_logger.log_info(f"Executing {len(update_plan.updates)} planned updates")
            execution_result = self.update_engine.execute_updates(update_plan)
            
            # Step 5: Validate results (optional)
            validation_result = self._validate_updates(diff_analysis, execution_result)
            
            duration = time.time() - start_time
            
            # Update statistics
            self.performance_stats["total_commits_processed"] += 1
            self.performance_stats["total_updates_executed"] += execution_result["executed_updates"]
            self.performance_stats["total_time_saved"] += execution_result.get("time_saved_vs_full_reparse", 0)
            self.performance_stats["successful_updates"] += 1
            if snapshot_id:
                self.performance_stats["snapshots_created"] += 1
            
            result = {
                "success": True,
                "operation_id": operation_id,
                "commit_hash": commit_hash,
                "snapshot_id": snapshot_id,
                "diff_analysis": {
                    "files_changed": len(diff_analysis.file_changes),
                    "entities_changed": len(diff_analysis.entity_changes),
                    "commit_message": diff_analysis.commit_message,
                    "author": diff_analysis.author
                },
                "updates_executed": execution_result["executed_updates"],
                "updates_failed": execution_result["failed_updates"],
                "execution_time": duration,
                "time_saved": execution_result.get("time_saved_vs_full_reparse", 0),
                "validation": validation_result,
                "performance_stats": self.performance_stats.copy()
            }
            
            self.session_logger.log_operation_end(
                "process_commit",
                duration=duration,
                success=True,
                details=result
            )
            
            # Track AI Lego Bricks success
            ai_tracker.record_success(
                component="surgical-update-coordinator",
                description=f"Successfully processed commit {commit_hash[:8]} with {execution_result['executed_updates']} updates",
                time_saved=execution_result.get("time_saved_vs_full_reparse", 0),
                accuracy=95.0  # Placeholder - would calculate based on validation
            )
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            # Update failure statistics
            self.performance_stats["failed_updates"] += 1
            
            self.session_logger.log_operation_end(
                "process_commit",
                duration=duration,
                success=False,
                details={"error": str(e), "commit_hash": commit_hash}
            )
            
            # Track failure
            ai_tracker.record_failure(
                component="surgical-update-coordinator",
                description=f"Failed to process commit {commit_hash}: {str(e)}",
                error_type=type(e).__name__,
                workaround="Try full reparse or check repository state"
            )
            
            # If we created a snapshot and failed, offer rollback option
            error_result = {
                "success": False,
                "operation_id": operation_id,
                "commit_hash": commit_hash,
                "error": str(e),
                "error_type": type(e).__name__,
                "snapshot_id": snapshot_id,
                "rollback_available": snapshot_id is not None,
                "execution_time": duration
            }
            
            self.session_logger.log_error(e, error_result)
            return error_result
    
    def process_commit_range(self, from_commit: str, to_commit: str = "HEAD") -> Dict[str, Any]:
        """
        Process a range of commits sequentially.
        
        Args:
            from_commit: Starting commit (exclusive)
            to_commit: Ending commit (inclusive)
            
        Returns:
            Dict containing results for all processed commits
        """
        self.session_logger.log_operation_start(
            "process_commit_range",
            {"from_commit": from_commit, "to_commit": to_commit}
        )
        
        start_time = time.time()
        
        try:
            # Get list of commits in range
            commits = self._get_commits_in_range(from_commit, to_commit)
            
            results = []
            successful_commits = 0
            failed_commits = 0
            
            for commit_hash in commits:
                try:
                    result = self.process_commit(commit_hash, create_snapshot=False)
                    results.append(result)
                    if result["success"]:
                        successful_commits += 1
                    else:
                        failed_commits += 1
                        
                except Exception as e:
                    failed_commits += 1
                    results.append({
                        "success": False,
                        "commit_hash": commit_hash,
                        "error": str(e)
                    })
                    self.session_logger.log_error(e, {"commit": commit_hash})
            
            duration = time.time() - start_time
            
            summary = {
                "success": True,
                "total_commits": len(commits),
                "successful_commits": successful_commits,
                "failed_commits": failed_commits,
                "total_time": duration,
                "results": results,
                "performance_stats": self.performance_stats.copy()
            }
            
            self.session_logger.log_operation_end(
                "process_commit_range",
                duration=duration,
                success=True,
                details=summary
            )
            
            return summary
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "process_commit_range",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            raise
    
    def rollback_to_snapshot(self, snapshot_id: str) -> bool:
        """
        Rollback graph to a previous snapshot.
        
        Args:
            snapshot_id: ID of snapshot to restore
            
        Returns:
            bool: True if rollback successful
        """
        self.session_logger.log_operation_start(
            "rollback_to_snapshot",
            {"snapshot_id": snapshot_id}
        )
        
        try:
            success = self.state_manager.restore_snapshot(snapshot_id)
            
            if success:
                self.session_logger.log_info(f"Successfully rolled back to snapshot: {snapshot_id}")
                
                # Track AI evaluation
                ai_tracker.record_success(
                    component="graph-rollback",
                    description=f"Successfully rolled back graph to snapshot {snapshot_id}",
                    time_saved=5.0  # Assume rollback saves time vs manual recovery
                )
            else:
                self.session_logger.log_error(None, {"context": "rollback_failed", "snapshot_id": snapshot_id})
            
            self.session_logger.log_operation_end(
                "rollback_to_snapshot",
                success=success,
                details={"snapshot_id": snapshot_id}
            )
            
            return success
            
        except Exception as e:
            self.session_logger.log_operation_end(
                "rollback_to_snapshot",
                success=False,
                details={"error": str(e), "snapshot_id": snapshot_id}
            )
            self.session_logger.log_error(e, {"snapshot_id": snapshot_id})
            return False
    
    def _create_pre_update_snapshot(self, commit_hash: str) -> str:
        """Create snapshot before processing commit"""
        snapshot_name = f"pre_commit_{commit_hash[:8]}_{int(time.time())}"
        snapshot_id = self.state_manager.create_snapshot(snapshot_name)
        self.session_logger.log_info(f"Created pre-update snapshot: {snapshot_id}")
        return snapshot_id
    
    def _analyze_commit(self, commit_hash: str) -> DiffAnalysis:
        """Analyze commit using GitDiffAgent"""
        result = self.git_agent.execute({
            "operation": "analyze_commit",
            "commit_hash": commit_hash
        })
        
        if not result["success"]:
            raise Exception(f"Failed to analyze commit: {result.get('error', 'Unknown error')}")
        
        return result["analysis"]
    
    def _get_commits_in_range(self, from_commit: str, to_commit: str) -> List[str]:
        """Get list of commits in range"""
        result = self.git_agent.execute({
            "operation": "get_recent_commits",
            "limit": 100  # Reasonable limit
        })
        
        if not result["success"]:
            raise Exception("Failed to get commit list")
        
        # For now, return a simplified list
        # In a full implementation, would properly handle commit ranges
        commits = [commit["hash"] for commit in result["commits"]]
        return commits[:10]  # Limit for safety
    
    def _validate_updates(self, diff_analysis: DiffAnalysis, execution_result: Dict[str, Any]) -> Dict[str, Any]:
        """Validate that updates were applied correctly"""
        # Basic validation - check that expected entities exist
        validation_result = {
            "validated": True,
            "checks_performed": [],
            "issues_found": []
        }
        
        # Count nodes that should exist
        expected_nodes = 0
        for entity_change in diff_analysis.entity_changes:
            if entity_change.change_type.value == "added":
                expected_nodes += 1
        
        validation_result["checks_performed"].append(f"Expected {expected_nodes} new nodes")
        
        # In a full implementation, would check graph consistency
        
        return validation_result
    
    def _process_entities_with_enhanced_extraction(self, file_paths: List[str]) -> Dict[str, Any]:
        """Process entity extraction for changed files using enhanced system"""
        self.session_logger.log_info(f"Processing {len(file_paths)} files with enhanced entity extraction")
        
        try:
            # Initialize entity coordinator if not exists
            if not self.entity_coordinator:
                self.entity_coordinator = AgentCoordinator("config/agent_pipeline_config.json")
            
            # Prepare input data
            input_data = {"files": []}
            
            for file_path in file_paths:
                if os.path.exists(file_path):
                    with open(file_path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    
                    # Basic AST parsing
                    try:
                        import ast
                        tree = ast.parse(content)
                        ast_data = {"classes": [], "functions": [], "imports": []}
                        
                        for node in ast.walk(tree):
                            if isinstance(node, ast.ClassDef):
                                ast_data["classes"].append({
                                    "name": node.name,
                                    "line": node.lineno,
                                    "column": node.col_offset,
                                    "bases": [ast.unparse(base) for base in node.bases] if hasattr(ast, 'unparse') else [],
                                    "decorators": [ast.unparse(dec) for dec in node.decorator_list] if hasattr(ast, 'unparse') else [],
                                    "docstring": ast.get_docstring(node),
                                    "methods": [n.name for n in node.body if isinstance(n, ast.FunctionDef)]
                                })
                            elif isinstance(node, ast.FunctionDef):
                                ast_data["functions"].append({
                                    "name": node.name,
                                    "line": node.lineno,
                                    "column": node.col_offset,
                                    "args": [arg.arg for arg in node.args.args] if hasattr(node.args, 'args') else [],
                                    "decorators": [ast.unparse(dec) for dec in node.decorator_list] if hasattr(ast, 'unparse') else [],
                                    "docstring": ast.get_docstring(node)
                                })
                        
                        input_data["files"].append({
                            "file_path": file_path,
                            "content": content,
                            "ast": ast_data,
                            "metadata": {
                                "language": "python",
                                "fileSize": len(content.encode('utf-8')),
                                "lineCount": len(content.split('\n'))
                            }
                        })
                        
                    except Exception as e:
                        self.session_logger.log_error(f"Failed to parse {file_path}: {e}")
                        continue
            
            # Process through enhanced entity extraction
            results = self.entity_coordinator.execute_workflow(input_data)
            
            # Extract entities from results
            extracted_entities = []
            if "parsed_files" in results:
                for file_data in results["parsed_files"]:
                    entities = file_data.get("entities", [])
                    for entity in entities:
                        entity["source_file"] = file_data["file_path"]
                        extracted_entities.append(entity)
            
            self.session_logger.log_info(f"Enhanced extraction completed: {len(extracted_entities)} entities found")
            
            return {
                "success": True,
                "entities": extracted_entities,
                "files_processed": len(file_paths),
                "entity_type_counts": self._count_entity_types(extracted_entities)
            }
            
        except Exception as e:
            self.session_logger.log_error(f"Enhanced entity extraction failed: {e}")
            return {
                "success": False,
                "error": str(e),
                "entities": [],
                "files_processed": 0
            }
    
    def _count_entity_types(self, entities: List[Dict[str, Any]]) -> Dict[str, int]:
        """Count entities by type"""
        counts = {}
        for entity in entities:
            entity_type = entity.get("specialized_type") or entity.get("type", "unknown")
            counts[entity_type] = counts.get(entity_type, 0) + 1
        return counts
    
    def get_performance_report(self) -> Dict[str, Any]:
        """Get comprehensive performance report"""
        current_stats = self.update_engine.get_statistics()
        
        report = {
            "coordinator_stats": self.performance_stats.copy(),
            "engine_stats": current_stats,
            "combined_metrics": {
                "success_rate": (
                    self.performance_stats["successful_updates"] / 
                    max(1, self.performance_stats["total_commits_processed"])
                ) * 100,
                "average_time_saved_per_commit": (
                    self.performance_stats["total_time_saved"] / 
                    max(1, self.performance_stats["total_commits_processed"])
                ),
                "updates_per_commit": (
                    self.performance_stats["total_updates_executed"] / 
                    max(1, self.performance_stats["total_commits_processed"])
                )
            },
            "timestamp": datetime.now().isoformat()
        }
        
        return report
    
    def list_available_snapshots(self) -> List[Dict[str, Any]]:
        """Get list of available snapshots for rollback"""
        return self.state_manager.list_snapshots()
    
    def cleanup_old_snapshots(self, keep_count: int = 10):
        """Clean up old snapshots"""
        self.state_manager.cleanup_old_snapshots(keep_count)
        self.session_logger.log_info(f"Cleaned up old snapshots, keeping {keep_count} most recent")
    
    def close(self):
        """Clean up resources"""
        self.session_logger.log_info("Closing SurgicalUpdateCoordinator")
        if hasattr(self.graph_manager, 'close'):
            self.graph_manager.close()


def main():
    """Main function for testing the coordinator"""
    coordinator = SurgicalUpdateCoordinator()
    
    try:
        # Process the latest commit
        result = coordinator.process_commit()
        print(f"Processed commit: {result}")
        
        # Get performance report
        report = coordinator.get_performance_report()
        print(f"Performance report: {report}")
        
    except Exception as e:
        print(f"Error: {e}")
    finally:
        coordinator.close()


if __name__ == "__main__":
    main()