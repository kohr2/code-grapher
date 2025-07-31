"""
Git Diff Agent for analyzing git commits and extracting code changes.
"""
import git
import time
from typing import Dict, Any, List, Optional
from dataclasses import dataclass
from enum import Enum
from pathlib import Path

from logger import logger


class ChangeType(Enum):
    """Types of changes that can occur in code"""
    ADDED = "added"
    MODIFIED = "modified"
    DELETED = "deleted"
    RENAMED = "renamed"


@dataclass
class CodeEntityChange:
    """Represents a change to a code entity"""
    entity_type: str  # function, class, import, etc.
    entity_name: str
    change_type: ChangeType
    line_number: int
    file_path: str
    old_value: Optional[str] = None
    new_value: Optional[str] = None


@dataclass
class FileChange:
    """Represents a file change"""
    file_path: str
    change_type: ChangeType
    old_path: Optional[str] = None
    entity_changes: List[CodeEntityChange] = None
    
    def __post_init__(self):
        if self.entity_changes is None:
            self.entity_changes = []


@dataclass
class DiffAnalysis:
    """Results of analyzing a git diff"""
    commit_hash: str
    file_changes: List[FileChange]
    summary: Dict[str, Any]
    timestamp: float = None
    
    def __post_init__(self):
        if self.timestamp is None:
            self.timestamp = time.time()


class GitDiffAgent:
    """Agent for analyzing git diffs and detecting code changes"""
    
    def __init__(self, repo_path: str = "."):
        self.repo_path = Path(repo_path)
        self.repo = git.Repo(repo_path)
        logger.log_info(f"GitDiffAgent initialized for repo: {repo_path}")
    
    def execute(self, operation: Dict[str, Any]) -> Dict[str, Any]:
        """Execute a git diff operation"""
        try:
            op_type = operation.get("operation")
            
            if op_type == "analyze_commit":
                commit_hash = operation.get("commit_hash")
                analysis = self._analyze_commit(commit_hash)
                return {
                    "success": True,
                    "analysis": analysis
                }
            
            elif op_type == "get_recent_commits":
                limit = operation.get("limit", 10)
                commits = self._get_recent_commits(limit)
                return {
                    "success": True,
                    "commits": commits
                }
            
            else:
                return {
                    "success": False,
                    "error": f"Unknown operation: {op_type}"
                }
                
        except Exception as e:
            logger.log_error(f"GitDiffAgent operation failed: {e}")
            return {
                "success": False,
                "error": str(e)
            }
    
    def _analyze_commit(self, commit_hash: str) -> DiffAnalysis:
        """Analyze a specific commit for code changes"""
        commit = self.repo.commit(commit_hash)
        
        # Get the parent commit for comparison
        if commit.parents:
            parent_commit = commit.parents[0]
            diffs = parent_commit.diff(commit)
        else:
            # Initial commit - compare against empty tree
            diffs = commit.diff(git.NULL_TREE)
        
        file_changes = []
        
        for diff in diffs:
            change_type = self._get_change_type(diff)
            file_path = diff.a_path or diff.b_path
            
            # Only analyze Python files for now
            if not file_path.endswith('.py'):
                continue
                
            file_change = FileChange(
                file_path=file_path,
                change_type=change_type,
                old_path=diff.a_path if diff.a_path != diff.b_path else None
            )
            
            # Analyze entity changes within the file
            entity_changes = self._analyze_file_changes(diff)
            file_change.entity_changes = entity_changes
            
            file_changes.append(file_change)
        
        summary = {
            "total_files": len(file_changes),
            "total_entities": sum(len(fc.entity_changes) for fc in file_changes),
            "change_types": {ct.value: 0 for ct in ChangeType}
        }
        
        # Count change types
        for file_change in file_changes:
            summary["change_types"][file_change.change_type.value] += 1
            for entity_change in file_change.entity_changes:
                summary["change_types"][entity_change.change_type.value] += 1
        
        return DiffAnalysis(
            commit_hash=commit_hash,
            file_changes=file_changes,
            summary=summary
        )
    
    def _get_change_type(self, diff) -> ChangeType:
        """Determine the type of change from a git diff"""
        if diff.new_file:
            return ChangeType.ADDED
        elif diff.deleted_file:
            return ChangeType.DELETED
        elif diff.renamed_file:
            return ChangeType.RENAMED
        else:
            return ChangeType.MODIFIED
    
    def _analyze_file_changes(self, diff) -> List[CodeEntityChange]:
        """Analyze changes within a file to detect entity changes"""
        entity_changes = []
        
        # For now, create a simple analysis based on line changes
        # This would be enhanced with proper AST analysis
        
        if hasattr(diff, 'diff') and diff.diff:
            diff_text = diff.diff.decode('utf-8', errors='ignore')
            lines = diff_text.split('\n')
            
            line_num = 0
            for line in lines:
                line_num += 1
                
                # Simple pattern matching for common Python constructs
                if line.startswith('+') and not line.startswith('+++'):
                    # Added line
                    clean_line = line[1:].strip()
                    entity_change = self._detect_entity_from_line(clean_line, line_num, diff.b_path, ChangeType.ADDED)
                    if entity_change:
                        entity_changes.append(entity_change)
                        
                elif line.startswith('-') and not line.startswith('---'):
                    # Deleted line
                    clean_line = line[1:].strip()
                    entity_change = self._detect_entity_from_line(clean_line, line_num, diff.a_path, ChangeType.DELETED)
                    if entity_change:
                        entity_changes.append(entity_change)
        
        return entity_changes
    
    def _detect_entity_from_line(self, line: str, line_num: int, file_path: str, change_type: ChangeType) -> Optional[CodeEntityChange]:
        """Detect code entities from a single line of code"""
        line = line.strip()
        
        if line.startswith('def '):
            # Function definition
            func_name = line.split('(')[0].replace('def ', '').strip()
            return CodeEntityChange(
                entity_type="function",
                entity_name=func_name,
                change_type=change_type,
                line_number=line_num,
                file_path=file_path,
                new_value=line if change_type == ChangeType.ADDED else None,
                old_value=line if change_type == ChangeType.DELETED else None
            )
        
        elif line.startswith('class '):
            # Class definition
            class_name = line.split('(')[0].split(':')[0].replace('class ', '').strip()
            return CodeEntityChange(
                entity_type="class",
                entity_name=class_name,
                change_type=change_type,
                line_number=line_num,
                file_path=file_path,
                new_value=line if change_type == ChangeType.ADDED else None,
                old_value=line if change_type == ChangeType.DELETED else None
            )
        
        elif line.startswith('import ') or line.startswith('from '):
            # Import statement
            import_name = line.split(' import ')[0] if ' import ' in line else line
            return CodeEntityChange(
                entity_type="import",
                entity_name=import_name,
                change_type=change_type,
                line_number=line_num,
                file_path=file_path,
                new_value=line if change_type == ChangeType.ADDED else None,
                old_value=line if change_type == ChangeType.DELETED else None
            )
        
        return None
    
    def _get_recent_commits(self, limit: int) -> List[Dict[str, Any]]:
        """Get recent commits from the repository"""
        commits = []
        
        for commit in self.repo.iter_commits(max_count=limit):
            commits.append({
                "hash": commit.hexsha,
                "message": commit.message.strip(),
                "author": commit.author.name,
                "timestamp": commit.committed_date,
                "files_changed": len(commit.stats.files)
            })
        
        return commits