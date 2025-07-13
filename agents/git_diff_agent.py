import ast
import os
import re
import time
from typing import Dict, Any, List, Optional, Set, Tuple
from pathlib import Path
import subprocess
from dataclasses import dataclass
from enum import Enum

try:
    from git import Repo, InvalidGitRepositoryError
    from unidiff import PatchSet
except ImportError:
    # Fallback for development - will install later
    Repo = None
    InvalidGitRepositoryError = Exception
    PatchSet = None

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError


class ChangeType(Enum):
    """Types of changes that can occur in a git diff"""
    ADDED = "added"
    MODIFIED = "modified"
    DELETED = "deleted"
    RENAMED = "renamed"


@dataclass
class FileChange:
    """Represents a file-level change in a git diff"""
    file_path: str
    change_type: ChangeType
    old_path: Optional[str] = None  # For renames
    lines_added: int = 0
    lines_removed: int = 0
    is_binary: bool = False


@dataclass
class CodeEntityChange:
    """Represents a change to a specific code entity (function, class, etc.)"""
    entity_type: str  # 'function', 'class', 'variable', 'import'
    entity_name: str
    change_type: ChangeType
    file_path: str
    line_number: Optional[int] = None
    old_signature: Optional[str] = None
    new_signature: Optional[str] = None
    context: Optional[Dict[str, Any]] = None


@dataclass
class DiffAnalysis:
    """Complete analysis of a git diff"""
    commit_hash: str
    commit_message: str
    author: str
    timestamp: str
    file_changes: List[FileChange]
    entity_changes: List[CodeEntityChange]
    metadata: Dict[str, Any]


class GitDiffAgent(BaseAgent):
    """
    Agent for analyzing git diffs and extracting meaningful code changes.
    Identifies added, modified, deleted, and renamed entities at the code level.
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        super().__init__(agent_id, config, shared_state)
        
        # Agent-specific configuration
        self.repo_path = config.get("config", {}).get("repoPath", ".")
        self.supported_extensions = config.get("config", {}).get("supportedExtensions", [".py", ".js", ".ts", ".java", ".cpp", ".c"])
        self.ignore_patterns = config.get("config", {}).get("ignorePatterns", ["__pycache__", ".git", "node_modules"])
        
        # Initialize git repository
        self._init_repository()
        
        self.session_logger.log_info(
            f"GitDiffAgent initialized for repo: {self.repo_path}"
        )
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        config_section = self.config.get("config", {})
        
        # Basic validation - most settings have defaults
        repo_path = config_section.get("repoPath", ".")
        if not os.path.exists(repo_path):
            raise AgentConfigurationError(f"Repository path does not exist: {repo_path}")
    
    def _init_repository(self):
        """Initialize git repository connection"""
        try:
            if Repo is None:
                self.session_logger.log_info("GitPython not available, will use subprocess fallback")
                self.repo = None
                return
            
            self.repo = Repo(self.repo_path)
            if self.repo.bare:
                raise AgentConfigurationError(f"Repository at {self.repo_path} is bare")
            
            self.session_logger.log_info(f"Connected to git repository: {self.repo.working_dir}")
            
        except InvalidGitRepositoryError:
            raise AgentConfigurationError(f"No git repository found at {self.repo_path}")
        except Exception as e:
            self.session_logger.log_error(e, {"context": "git_init"})
            raise AgentConfigurationError(f"Failed to initialize git repository: {str(e)}")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return [
            "git_diff_analysis",
            "commit_analysis", 
            "code_change_extraction",
            "entity_change_detection",
            "file_change_tracking"
        ]
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute git diff analysis based on input parameters"""
        operation = input_data.get("operation", "analyze_commit")
        
        if operation == "analyze_commit":
            return self._analyze_commit(input_data)
        elif operation == "analyze_diff":
            return self._analyze_diff_text(input_data)
        elif operation == "get_recent_commits":
            return self._get_recent_commits(input_data)
        elif operation == "compare_commits":
            return self._compare_commits(input_data)
        else:
            raise AgentExecutionError(f"Unknown operation: {operation}")
    
    def _analyze_commit(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze a specific commit and extract changes"""
        commit_hash = input_data.get("commit_hash")
        if not commit_hash:
            # Use HEAD if no commit specified
            commit_hash = "HEAD"
        
        self.session_logger.log_operation_start(
            "analyze_commit",
            {"commit_hash": commit_hash}
        )
        
        start_time = time.time()
        
        try:
            # Get commit information
            if self.repo:
                commit = self.repo.commit(commit_hash)
                diff_data = self._get_commit_diff_gitpython(commit)
            else:
                diff_data = self._get_commit_diff_subprocess(commit_hash)
            
            # Analyze the diff
            analysis = self._perform_diff_analysis(diff_data)
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "analyze_commit",
                duration=duration,
                success=True,
                details={
                    "files_changed": len(analysis.file_changes),
                    "entities_changed": len(analysis.entity_changes)
                }
            )
            
            return {
                "success": True,
                "analysis": analysis,
                "metadata": {
                    "execution_time": duration,
                    "agent_id": self.agent_id
                }
            }
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "analyze_commit",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            self.session_logger.log_error(e, {"commit_hash": commit_hash})
            raise AgentExecutionError(f"Failed to analyze commit {commit_hash}: {str(e)}")
    
    def _get_commit_diff_gitpython(self, commit) -> Dict[str, Any]:
        """Get diff data using GitPython"""
        try:
            # Get parent commit for comparison
            if commit.parents:
                parent = commit.parents[0]
                diff = parent.diff(commit, create_patch=True)
            else:
                # Initial commit - compare against empty tree
                diff = commit.diff(None, create_patch=True)
            
            diff_text = ""
            for diff_item in diff:
                if diff_item.diff:
                    diff_text += diff_item.diff.decode('utf-8', errors='ignore')
            
            return {
                "commit_hash": commit.hexsha,
                "commit_message": commit.message.strip(),
                "author": f"{commit.author.name} <{commit.author.email}>",
                "timestamp": commit.committed_date,
                "diff_text": diff_text,
                "files_changed": [item.a_path or item.b_path for item in diff]
            }
            
        except Exception as e:
            self.session_logger.log_error(e, {"context": "gitpython_diff"})
            raise
    
    def _get_commit_diff_subprocess(self, commit_hash: str) -> Dict[str, Any]:
        """Get diff data using subprocess git commands"""
        try:
            # Get commit info
            commit_info = subprocess.run(
                ["git", "show", "--format=%H|%s|%an <%ae>|%ct", "--name-only", commit_hash],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            lines = commit_info.stdout.strip().split('\n')
            header = lines[0].split('|')
            files_changed = lines[1:] if len(lines) > 1 else []
            
            # Get the actual diff
            diff_result = subprocess.run(
                ["git", "show", "--format=", commit_hash],
                cwd=self.repo_path,
                capture_output=True,
                text=True,
                check=True
            )
            
            return {
                "commit_hash": header[0],
                "commit_message": header[1],
                "author": header[2],
                "timestamp": header[3],
                "diff_text": diff_result.stdout,
                "files_changed": files_changed
            }
            
        except subprocess.CalledProcessError as e:
            self.session_logger.log_error(e, {"context": "subprocess_git", "commit": commit_hash})
            raise AgentExecutionError(f"Git command failed: {e}")
    
    def _perform_diff_analysis(self, diff_data: Dict[str, Any]) -> DiffAnalysis:
        """Perform comprehensive analysis of the diff data"""
        self.session_logger.log_info("Starting comprehensive diff analysis")
        
        # Parse file changes
        file_changes = self._parse_file_changes(diff_data["diff_text"])
        
        # Extract entity changes from each file
        entity_changes = []
        for file_change in file_changes:
            if self._is_supported_file(file_change.file_path):
                file_entities = self._extract_entity_changes(file_change, diff_data["diff_text"])
                entity_changes.extend(file_entities)
        
        # Create analysis object
        analysis = DiffAnalysis(
            commit_hash=diff_data["commit_hash"],
            commit_message=diff_data["commit_message"],
            author=diff_data["author"],
            timestamp=diff_data["timestamp"],
            file_changes=file_changes,
            entity_changes=entity_changes,
            metadata={
                "files_analyzed": len(file_changes),
                "supported_files": len([f for f in file_changes if self._is_supported_file(f.file_path)]),
                "total_lines_added": sum(f.lines_added for f in file_changes),
                "total_lines_removed": sum(f.lines_removed for f in file_changes)
            }
        )
        
        self.session_logger.log_info(f"Diff analysis complete: {len(file_changes)} files, {len(entity_changes)} entities")
        
        return analysis
    
    def _parse_file_changes(self, diff_text: str) -> List[FileChange]:
        """Parse file-level changes from diff text"""
        file_changes = []
        
        if PatchSet is None:
            # Fallback to basic parsing
            return self._parse_file_changes_basic(diff_text)
        
        try:
            patches = PatchSet(diff_text)
            
            for patch in patches:
                change_type = ChangeType.MODIFIED
                if patch.is_added_file:
                    change_type = ChangeType.ADDED
                elif patch.is_removed_file:
                    change_type = ChangeType.DELETED
                elif patch.is_rename:
                    change_type = ChangeType.RENAMED
                
                file_change = FileChange(
                    file_path=patch.path,
                    change_type=change_type,
                    old_path=patch.source_file if patch.is_rename else None,
                    lines_added=patch.added,
                    lines_removed=patch.removed,
                    is_binary=patch.is_binary_file
                )
                
                file_changes.append(file_change)
                
        except Exception as e:
            self.session_logger.log_error(e, {"context": "unidiff_parsing"})
            # Fallback to basic parsing
            return self._parse_file_changes_basic(diff_text)
        
        return file_changes
    
    def _parse_file_changes_basic(self, diff_text: str) -> List[FileChange]:
        """Basic diff parsing without unidiff library"""
        file_changes = []
        current_file = None
        lines_added = 0
        lines_removed = 0
        
        for line in diff_text.split('\n'):
            if line.startswith('diff --git'):
                # Save previous file if any
                if current_file:
                    file_changes.append(FileChange(
                        file_path=current_file,
                        change_type=ChangeType.MODIFIED,
                        lines_added=lines_added,
                        lines_removed=lines_removed
                    ))
                    lines_added = lines_removed = 0
                
                # Extract file path
                match = re.search(r'b/(.+)$', line)
                current_file = match.group(1) if match else None
                
            elif line.startswith('+') and not line.startswith('+++'):
                lines_added += 1
            elif line.startswith('-') and not line.startswith('---'):
                lines_removed += 1
        
        # Don't forget the last file
        if current_file:
            file_changes.append(FileChange(
                file_path=current_file,
                change_type=ChangeType.MODIFIED,
                lines_added=lines_added,
                lines_removed=lines_removed
            ))
        
        return file_changes
    
    def _extract_entity_changes(self, file_change: FileChange, diff_text: str) -> List[CodeEntityChange]:
        """Extract entity-level changes from a file diff"""
        entity_changes = []
        
        # Only analyze Python files for now (can be extended)
        if not file_change.file_path.endswith('.py'):
            return entity_changes
        
        try:
            # Extract the file's diff section
            file_diff = self._extract_file_diff(diff_text, file_change.file_path)
            
            # Analyze Python-specific changes
            if file_diff:
                entity_changes.extend(self._analyze_python_changes(file_change, file_diff))
                
        except Exception as e:
            self.session_logger.log_error(e, {"file": file_change.file_path, "context": "entity_extraction"})
        
        return entity_changes
    
    def _extract_file_diff(self, diff_text: str, file_path: str) -> str:
        """Extract the diff section for a specific file"""
        lines = diff_text.split('\n')
        file_diff_lines = []
        in_file_diff = False
        
        for line in lines:
            if line.startswith('diff --git') and file_path in line:
                in_file_diff = True
                file_diff_lines = [line]
            elif in_file_diff:
                if line.startswith('diff --git'):
                    break  # Start of next file
                file_diff_lines.append(line)
        
        return '\n'.join(file_diff_lines)
    
    def _analyze_python_changes(self, file_change: FileChange, file_diff: str) -> List[CodeEntityChange]:
        """Analyze Python-specific code changes"""
        entity_changes = []
        
        # Look for function definitions
        function_patterns = [
            (r'^\+.*def\s+([a-zA-Z_]\w*)\s*\(', ChangeType.ADDED, 'function'),
            (r'^\-.*def\s+([a-zA-Z_]\w*)\s*\(', ChangeType.DELETED, 'function'),
        ]
        
        # Look for class definitions
        class_patterns = [
            (r'^\+.*class\s+([a-zA-Z_]\w*)\s*[\(:]', ChangeType.ADDED, 'class'),
            (r'^\-.*class\s+([a-zA-Z_]\w*)\s*[\(:]', ChangeType.DELETED, 'class'),
        ]
        
        # Look for import changes
        import_patterns = [
            (r'^\+.*(?:from\s+\S+\s+)?import\s+(.+)', ChangeType.ADDED, 'import'),
            (r'^\-.*(?:from\s+\S+\s+)?import\s+(.+)', ChangeType.DELETED, 'import'),
        ]
        
        all_patterns = function_patterns + class_patterns + import_patterns
        
        line_number = 0
        for line in file_diff.split('\n'):
            line_number += 1
            
            for pattern, change_type, entity_type in all_patterns:
                match = re.search(pattern, line)
                if match:
                    entity_name = match.group(1).strip()
                    
                    entity_change = CodeEntityChange(
                        entity_type=entity_type,
                        entity_name=entity_name,
                        change_type=change_type,
                        file_path=file_change.file_path,
                        line_number=line_number,
                        context={"diff_line": line.strip()}
                    )
                    
                    entity_changes.append(entity_change)
        
        return entity_changes
    
    def _is_supported_file(self, file_path: str) -> bool:
        """Check if file type is supported for analysis"""
        return any(file_path.endswith(ext) for ext in self.supported_extensions)
    
    def _get_recent_commits(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Get list of recent commits"""
        limit = input_data.get("limit", 10)
        
        try:
            if self.repo:
                commits = list(self.repo.iter_commits(max_count=limit))
                commit_data = []
                for commit in commits:
                    commit_data.append({
                        "hash": commit.hexsha,
                        "message": commit.message.strip(),
                        "author": f"{commit.author.name} <{commit.author.email}>",
                        "date": commit.committed_date
                    })
            else:
                # Subprocess fallback
                result = subprocess.run(
                    ["git", "log", f"--max-count={limit}", "--format=%H|%s|%an <%ae>|%ct"],
                    cwd=self.repo_path,
                    capture_output=True,
                    text=True,
                    check=True
                )
                
                commit_data = []
                for line in result.stdout.strip().split('\n'):
                    if line:
                        parts = line.split('|')
                        commit_data.append({
                            "hash": parts[0],
                            "message": parts[1],
                            "author": parts[2],
                            "date": parts[3]
                        })
            
            return {
                "success": True,
                "commits": commit_data
            }
            
        except Exception as e:
            self.session_logger.log_error(e)
            raise AgentExecutionError(f"Failed to get recent commits: {str(e)}")
    
    def _analyze_diff_text(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Analyze raw diff text"""
        diff_text = input_data.get("diff_text", "")
        
        if not diff_text:
            raise AgentExecutionError("No diff text provided")
        
        # Create mock diff data structure
        diff_data = {
            "commit_hash": "manual_analysis",
            "commit_message": "Manual diff analysis",
            "author": "Unknown",
            "timestamp": str(int(time.time())),
            "diff_text": diff_text,
            "files_changed": []
        }
        
        analysis = self._perform_diff_analysis(diff_data)
        
        return {
            "success": True,
            "analysis": analysis
        }
    
    def _compare_commits(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Compare two commits and return detailed analysis"""
        commit1 = input_data.get("commit1", "HEAD~1")
        commit2 = input_data.get("commit2", "HEAD")
        
        try:
            if self.repo:
                diff = self.repo.git.diff(commit1, commit2, create_patch=True)
            else:
                result = subprocess.run(
                    ["git", "diff", commit1, commit2],
                    cwd=self.repo_path,
                    capture_output=True,
                    text=True,
                    check=True
                )
                diff = result.stdout
            
            # Analyze the comparison diff
            diff_data = {
                "commit_hash": f"{commit1}..{commit2}",
                "commit_message": f"Comparison between {commit1} and {commit2}",
                "author": "Comparison",
                "timestamp": str(int(time.time())),
                "diff_text": diff,
                "files_changed": []
            }
            
            analysis = self._perform_diff_analysis(diff_data)
            
            return {
                "success": True,
                "analysis": analysis,
                "comparison": {
                    "from_commit": commit1,
                    "to_commit": commit2
                }
            }
            
        except Exception as e:
            self.session_logger.log_error(e)
            raise AgentExecutionError(f"Failed to compare commits: {str(e)}")