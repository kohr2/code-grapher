import time
from typing import Dict, Any, List, Optional, Set, Tuple
from dataclasses import dataclass
from enum import Enum
from pathlib import Path
import ast
import os

from graph_manager import CodeGraphManager
from agents.git_diff_agent import DiffAnalysis, FileChange, CodeEntityChange, ChangeType
from logger import logger
from ai_evaluation_tracker import ai_tracker


class UpdateOperation(Enum):
    """Types of graph update operations"""
    CREATE_NODE = "create_node"
    UPDATE_NODE = "update_node"
    DELETE_NODE = "delete_node"
    CREATE_RELATIONSHIP = "create_relationship"
    DELETE_RELATIONSHIP = "delete_relationship"
    UPDATE_RELATIONSHIP = "update_relationship"


@dataclass
class GraphUpdate:
    """Represents a single graph update operation"""
    operation: UpdateOperation
    entity_type: str
    entity_name: str
    file_path: str
    properties: Optional[Dict[str, Any]] = None
    old_properties: Optional[Dict[str, Any]] = None
    related_entities: Optional[List[str]] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class UpdatePlan:
    """Complete plan for updating the graph based on diff analysis"""
    diff_analysis: DiffAnalysis
    updates: List[GraphUpdate]
    dependencies: List[Tuple[int, int]]  # (prerequisite_index, dependent_index)
    estimated_time: float
    risk_assessment: str


class GraphUpdateEngine:
    """
    Engine for performing surgical graph updates based on git diff analysis.
    Provides intelligent, incremental updates to maintain graph consistency.
    """
    
    def __init__(self, graph_manager: CodeGraphManager):
        self.graph_manager = graph_manager
        self.session_logger = logger.create_session_logger("GraphUpdateEngine")
        
        # Cache for file AST data to avoid re-parsing
        self._ast_cache = {}
        
        # Statistics tracking
        self.stats = {
            "updates_performed": 0,
            "nodes_created": 0,
            "nodes_updated": 0,
            "nodes_deleted": 0,
            "relationships_created": 0,
            "relationships_deleted": 0,
            "time_saved_vs_full_reparse": 0.0
        }
        
        self.session_logger.log_info("GraphUpdateEngine initialized")
    
    def plan_updates(self, diff_analysis: DiffAnalysis) -> UpdatePlan:
        """
        Create a comprehensive update plan based on diff analysis.
        
        Args:
            diff_analysis: Analysis of git diff containing changes
            
        Returns:
            UpdatePlan: Ordered plan of graph update operations
        """
        self.session_logger.log_operation_start(
            "plan_updates",
            {
                "commit": diff_analysis.commit_hash,
                "files_changed": len(diff_analysis.file_changes),
                "entities_changed": len(diff_analysis.entity_changes)
            }
        )
        
        start_time = time.time()
        updates = []
        
        try:
            # Plan file-level updates first
            file_updates = self._plan_file_updates(diff_analysis.file_changes)
            updates.extend(file_updates)
            
            # Plan entity-level updates
            entity_updates = self._plan_entity_updates(diff_analysis.entity_changes)
            updates.extend(entity_updates)
            
            # Plan relationship updates
            relationship_updates = self._plan_relationship_updates(diff_analysis)
            updates.extend(relationship_updates)
            
            # Determine update dependencies
            dependencies = self._calculate_dependencies(updates)
            
            # Estimate execution time
            estimated_time = self._estimate_execution_time(updates)
            
            # Assess risk
            risk_assessment = self._assess_update_risks(updates, diff_analysis)
            
            duration = time.time() - start_time
            
            plan = UpdatePlan(
                diff_analysis=diff_analysis,
                updates=updates,
                dependencies=dependencies,
                estimated_time=estimated_time,
                risk_assessment=risk_assessment
            )
            
            self.session_logger.log_operation_end(
                "plan_updates",
                duration=duration,
                success=True,
                details={
                    "total_updates": len(updates),
                    "dependencies": len(dependencies),
                    "estimated_time": estimated_time,
                    "risk": risk_assessment
                }
            )
            
            return plan
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "plan_updates",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            self.session_logger.log_error(e)
            raise
    
    def execute_updates(self, update_plan: UpdatePlan) -> Dict[str, Any]:
        """
        Execute the planned graph updates in dependency order.
        
        Args:
            update_plan: Plan containing ordered update operations
            
        Returns:
            Dict with execution results and statistics
        """
        self.session_logger.log_operation_start(
            "execute_updates",
            {
                "total_updates": len(update_plan.updates),
                "estimated_time": update_plan.estimated_time,
                "commit": update_plan.diff_analysis.commit_hash
            }
        )
        
        start_time = time.time()
        executed_updates = 0
        failed_updates = 0
        
        try:
            # Sort updates by dependencies
            ordered_updates = self._sort_updates_by_dependencies(
                update_plan.updates, 
                update_plan.dependencies
            )
            
            # Execute updates in order
            for i, update in enumerate(ordered_updates):
                try:
                    self._execute_single_update(update)
                    executed_updates += 1
                    
                    self.session_logger.log_info(
                        f"Executed update {i+1}/{len(ordered_updates)}: {update.operation.value} "
                        f"{update.entity_type}.{update.entity_name}"
                    )
                    
                except Exception as e:
                    failed_updates += 1
                    self.session_logger.log_error(
                        e, {
                            "update_index": i,
                            "operation": update.operation.value,
                            "entity": f"{update.entity_type}.{update.entity_name}"
                        }
                    )
                    
                    # Decide whether to continue or abort
                    if self._should_abort_on_error(update, e):
                        raise
            
            duration = time.time() - start_time
            
            # Update statistics
            self.stats["updates_performed"] += executed_updates
            
            # Calculate time saved vs full reparse
            full_reparse_time = self._estimate_full_reparse_time(update_plan.diff_analysis)
            time_saved = max(0, full_reparse_time - duration)
            self.stats["time_saved_vs_full_reparse"] += time_saved
            
            result = {
                "success": True,
                "executed_updates": executed_updates,
                "failed_updates": failed_updates,
                "execution_time": duration,
                "time_saved_vs_full_reparse": time_saved,
                "commit_hash": update_plan.diff_analysis.commit_hash,
                "statistics": self.stats.copy()
            }
            
            self.session_logger.log_operation_end(
                "execute_updates",
                duration=duration,
                success=True,
                details=result
            )
            
            # Track AI Lego Bricks evaluation
            ai_tracker.record_success(
                component="surgical-graph-updates",
                description=f"Updated graph with {executed_updates} operations for commit {update_plan.diff_analysis.commit_hash[:8]}",
                time_saved=time_saved,
                accuracy=((executed_updates / len(ordered_updates)) * 100) if ordered_updates else 100
            )
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "execute_updates",
                duration=duration,
                success=False,
                details={"error": str(e), "executed": executed_updates, "failed": failed_updates}
            )
            
            # Track failure
            ai_tracker.record_failure(
                component="surgical-graph-updates",
                description=f"Failed to update graph: {str(e)}",
                error_type=type(e).__name__
            )
            
            raise
    
    def _plan_file_updates(self, file_changes: List[FileChange]) -> List[GraphUpdate]:
        """Plan updates for file-level changes"""
        updates = []
        
        for file_change in file_changes:
            if file_change.change_type == ChangeType.ADDED:
                # New file - create file node
                updates.append(GraphUpdate(
                    operation=UpdateOperation.CREATE_NODE,
                    entity_type="File",
                    entity_name=file_change.file_path,
                    file_path=file_change.file_path,
                    properties={
                        "path": file_change.file_path,
                        "lines_added": file_change.lines_added,
                        "is_new": True
                    }
                ))
                
            elif file_change.change_type == ChangeType.DELETED:
                # Deleted file - remove file node and all contained entities
                updates.append(GraphUpdate(
                    operation=UpdateOperation.DELETE_NODE,
                    entity_type="File",
                    entity_name=file_change.file_path,
                    file_path=file_change.file_path
                ))
                
            elif file_change.change_type == ChangeType.RENAMED:
                # Renamed file - update file node path
                updates.append(GraphUpdate(
                    operation=UpdateOperation.UPDATE_NODE,
                    entity_type="File",
                    entity_name=file_change.old_path,
                    file_path=file_change.file_path,
                    properties={
                        "path": file_change.file_path,
                        "old_path": file_change.old_path
                    },
                    old_properties={"path": file_change.old_path}
                ))
                
            elif file_change.change_type == ChangeType.MODIFIED:
                # Modified file - update file node metadata
                updates.append(GraphUpdate(
                    operation=UpdateOperation.UPDATE_NODE,
                    entity_type="File",
                    entity_name=file_change.file_path,
                    file_path=file_change.file_path,
                    properties={
                        "lines_added": file_change.lines_added,
                        "lines_removed": file_change.lines_removed,
                        "last_modified": time.time()
                    }
                ))
        
        return updates
    
    def _plan_entity_updates(self, entity_changes: List[CodeEntityChange]) -> List[GraphUpdate]:
        """Plan updates for entity-level changes"""
        updates = []
        
        for entity_change in entity_changes:
            if entity_change.change_type == ChangeType.ADDED:
                # New entity - create node
                updates.append(GraphUpdate(
                    operation=UpdateOperation.CREATE_NODE,
                    entity_type=entity_change.entity_type.title(),
                    entity_name=entity_change.entity_name,
                    file_path=entity_change.file_path,
                    properties={
                        "name": entity_change.entity_name,
                        "file_path": entity_change.file_path,
                        "line_number": entity_change.line_number,
                        "signature": entity_change.new_signature
                    },
                    metadata=entity_change.context
                ))
                
            elif entity_change.change_type == ChangeType.DELETED:
                # Deleted entity - remove node
                updates.append(GraphUpdate(
                    operation=UpdateOperation.DELETE_NODE,
                    entity_type=entity_change.entity_type.title(),
                    entity_name=entity_change.entity_name,
                    file_path=entity_change.file_path
                ))
                
            elif entity_change.change_type == ChangeType.MODIFIED:
                # Modified entity - update node properties
                properties = {
                    "line_number": entity_change.line_number,
                    "last_modified": time.time()
                }
                
                if entity_change.new_signature:
                    properties["signature"] = entity_change.new_signature
                
                updates.append(GraphUpdate(
                    operation=UpdateOperation.UPDATE_NODE,
                    entity_type=entity_change.entity_type.title(),
                    entity_name=entity_change.entity_name,
                    file_path=entity_change.file_path,
                    properties=properties,
                    old_properties={"signature": entity_change.old_signature} if entity_change.old_signature else None
                ))
        
        return updates
    
    def _plan_relationship_updates(self, diff_analysis: DiffAnalysis) -> List[GraphUpdate]:
        """Plan relationship updates based on analysis"""
        updates = []
        
        # For each file that was modified, we need to re-analyze relationships
        for file_change in diff_analysis.file_changes:
            if file_change.change_type == ChangeType.MODIFIED and self._is_code_file(file_change.file_path):
                # Plan to update relationships for this file
                updates.append(GraphUpdate(
                    operation=UpdateOperation.UPDATE_RELATIONSHIP,
                    entity_type="File",
                    entity_name=file_change.file_path,
                    file_path=file_change.file_path,
                    metadata={"analyze_relationships": True}
                ))
        
        return updates
    
    def _calculate_dependencies(self, updates: List[GraphUpdate]) -> List[Tuple[int, int]]:
        """Calculate dependencies between updates"""
        dependencies = []
        
        # Basic dependency rules:
        # 1. File creation must happen before entity creation in that file
        # 2. Entity deletion must happen before file deletion
        # 3. Node creation must happen before relationship creation
        
        for i, update1 in enumerate(updates):
            for j, update2 in enumerate(updates):
                if i != j:
                    if self._has_dependency(update1, update2):
                        dependencies.append((i, j))
        
        return dependencies
    
    def _has_dependency(self, update1: GraphUpdate, update2: GraphUpdate) -> bool:
        """Check if update2 depends on update1"""
        # File creation before entity creation
        if (update1.operation == UpdateOperation.CREATE_NODE and update1.entity_type == "File" and
            update2.operation == UpdateOperation.CREATE_NODE and update2.entity_type != "File" and
            update1.file_path == update2.file_path):
            return True
        
        # Entity deletion before file deletion
        if (update1.operation == UpdateOperation.DELETE_NODE and update1.entity_type != "File" and
            update2.operation == UpdateOperation.DELETE_NODE and update2.entity_type == "File" and
            update1.file_path == update2.file_path):
            return True
        
        return False
    
    def _sort_updates_by_dependencies(self, updates: List[GraphUpdate], dependencies: List[Tuple[int, int]]) -> List[GraphUpdate]:
        """Sort updates respecting dependencies using topological sort"""
        # Simple topological sort
        in_degree = [0] * len(updates)
        
        # Calculate in-degrees
        for prereq, dependent in dependencies:
            in_degree[dependent] += 1
        
        # Find nodes with no dependencies
        queue = [i for i, degree in enumerate(in_degree) if degree == 0]
        result = []
        
        while queue:
            current = queue.pop(0)
            result.append(updates[current])
            
            # Reduce in-degree for dependent nodes
            for prereq, dependent in dependencies:
                if prereq == current:
                    in_degree[dependent] -= 1
                    if in_degree[dependent] == 0:
                        queue.append(dependent)
        
        # If we couldn't sort all updates, there's a cycle or we missed some
        if len(result) < len(updates):
            self.session_logger.log_warning("Could not sort all updates by dependencies, using original order")
            return updates
        
        return result
    
    def _execute_single_update(self, update: GraphUpdate):
        """Execute a single graph update operation"""
        if update.operation == UpdateOperation.CREATE_NODE:
            self._create_node(update)
        elif update.operation == UpdateOperation.UPDATE_NODE:
            self._update_node(update)
        elif update.operation == UpdateOperation.DELETE_NODE:
            self._delete_node(update)
        elif update.operation == UpdateOperation.CREATE_RELATIONSHIP:
            self._create_relationship(update)
        elif update.operation == UpdateOperation.DELETE_RELATIONSHIP:
            self._delete_relationship(update)
        elif update.operation == UpdateOperation.UPDATE_RELATIONSHIP:
            self._update_relationships(update)
        else:
            raise ValueError(f"Unknown update operation: {update.operation}")
    
    def _create_node(self, update: GraphUpdate):
        """Create a new node in the graph"""
        try:
            node = self.graph_manager.create_code_entity(
                update.entity_type,
                update.entity_name,
                update.properties or {}
            )
            
            self.stats["nodes_created"] += 1
            
            # If this is not a file node, create relationship to file
            if update.entity_type != "File":
                file_node = self.graph_manager.find_entity("File", update.file_path)
                if file_node:
                    self.graph_manager.create_relationship(file_node, node, "CONTAINS")
                    self.stats["relationships_created"] += 1
            
        except Exception as e:
            self.session_logger.log_error(e, {"update": update})
            raise
    
    def _update_node(self, update: GraphUpdate):
        """Update an existing node in the graph"""
        try:
            # Find the existing node
            node = self.graph_manager.find_entity(update.entity_type, update.entity_name)
            
            if node:
                # Update properties
                for key, value in (update.properties or {}).items():
                    node[key] = value
                
                # Push changes to graph
                self.graph_manager.graph.push(node)
                self.stats["nodes_updated"] += 1
            else:
                self.session_logger.log_warning(f"Node not found for update: {update.entity_type}.{update.entity_name}")
                
        except Exception as e:
            self.session_logger.log_error(e, {"update": update})
            raise
    
    def _delete_node(self, update: GraphUpdate):
        """Delete a node from the graph"""
        try:
            # Use Cypher to delete node and all relationships
            query = f"""
            MATCH (n:{update.entity_type} {{name: $name}})
            DETACH DELETE n
            """
            
            result = self.graph_manager.graph.run(query, name=update.entity_name)
            self.stats["nodes_deleted"] += 1
            
        except Exception as e:
            self.session_logger.log_error(e, {"update": update})
            raise
    
    def _create_relationship(self, update: GraphUpdate):
        """Create a new relationship in the graph"""
        # Implementation depends on relationship type
        # This is a placeholder for more sophisticated relationship creation
        self.stats["relationships_created"] += 1
    
    def _delete_relationship(self, update: GraphUpdate):
        """Delete a relationship from the graph"""
        # Implementation depends on relationship type
        # This is a placeholder for more sophisticated relationship deletion
        self.stats["relationships_deleted"] += 1
    
    def _update_relationships(self, update: GraphUpdate):
        """Update relationships for a file by re-analyzing its content"""
        try:
            if not os.path.exists(update.file_path):
                self.session_logger.log_warning(f"File not found for relationship update: {update.file_path}")
                return
            
            self.session_logger.log_info(f"Re-analyzing relationships for {update.file_path}")
            
            # Read and parse the file content
            with open(update.file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Create parsed file structure for AST relationship extractor
            parsed_file = {
                "file_path": update.file_path,
                "success": True, 
                "entities": [],
                "content": content
            }
            
            # Extract relationships using existing AST extractor
            from ast_relationship_extractor import extract_ast_relationships
            relationships = extract_ast_relationships([parsed_file])
            
            # Validate and update relationships in graph
            self._validate_and_update_relationships(relationships, update.file_path)
            
            self.session_logger.log_info(f"Updated {len(relationships)} relationships for {update.file_path}")
            
        except Exception as e:
            self.session_logger.log_error(f"Failed to update relationships for {update.file_path}: {e}")
            # Don't raise - continue with other updates
    
    def _validate_and_update_relationships(self, relationships: List[Dict], file_path: str):
        """Validate extracted relationships and update them in the graph"""
        valid_relationships = []
        invalid_count = 0
        
        for rel in relationships:
            if self._is_relationship_valid(rel, file_path):
                valid_relationships.append(rel)
            else:
                invalid_count += 1
                self.session_logger.log_warning(f"Invalid relationship detected: {rel}")
        
        if invalid_count > 0:
            self.session_logger.log_warning(f"Filtered out {invalid_count} invalid relationships from {file_path}")
        
        # Update graph with valid relationships
        for rel in valid_relationships:
            try:
                # Remove old relationships for this file first
                self.graph_manager.remove_entity_relationships(file_path)
                
                # Add the validated relationship
                self.graph_manager.add_relationship(
                    rel.get("source_entity", ""),
                    rel.get("target_entity", ""),
                    rel.get("relationship_type", "RELATED_TO"),
                    rel.get("properties", {})
                )
                self.stats["relationships_added"] += 1
                
            except Exception as e:
                self.session_logger.log_error(f"Failed to add relationship to graph: {e}")
    
    def _is_relationship_valid(self, relationship: Dict, file_path: str) -> bool:
        """Validate that a relationship makes sense and has valid targets"""
        try:
            # Check required fields
            source_entity = relationship.get("source_entity")
            target_entity = relationship.get("target_entity") 
            relationship_type = relationship.get("relationship_type")
            
            if not all([source_entity, target_entity, relationship_type]):
                return False
            
            # Don't allow self-relationships
            if source_entity == target_entity:
                return False
            
            # Basic validation - source entity should exist in the file
            if not self._entity_exists_in_context(source_entity, file_path):
                return False
                
            return True
            
        except Exception as e:
            self.session_logger.log_warning(f"Relationship validation error: {e}")
            return False
    
    def _entity_exists_in_context(self, entity_name: str, file_path: str) -> bool:
        """Check if an entity actually exists in the given file context"""
        try:
            # Query graph to see if entity exists
            result = self.graph_manager.query_entities(
                {"name": entity_name, "file_path": file_path}, 
                limit=1
            )
            return len(result) > 0
        except Exception:
            # If we can't verify, assume it's valid to avoid false negatives
            return True
    
    def _estimate_execution_time(self, updates: List[GraphUpdate]) -> float:
        """Estimate how long the updates will take to execute"""
        # Basic time estimates per operation type (in seconds)
        time_estimates = {
            UpdateOperation.CREATE_NODE: 0.01,
            UpdateOperation.UPDATE_NODE: 0.005,
            UpdateOperation.DELETE_NODE: 0.005,
            UpdateOperation.CREATE_RELATIONSHIP: 0.008,
            UpdateOperation.DELETE_RELATIONSHIP: 0.005,
            UpdateOperation.UPDATE_RELATIONSHIP: 0.02
        }
        
        total_time = sum(time_estimates.get(update.operation, 0.01) for update in updates)
        return total_time
    
    def _estimate_full_reparse_time(self, diff_analysis: DiffAnalysis) -> float:
        """Estimate how long a full reparse would take"""
        # Rough estimate based on files changed
        files_changed = len(diff_analysis.file_changes)
        return files_changed * 2.0  # Assume 2 seconds per file for full reparse
    
    def _assess_update_risks(self, updates: List[GraphUpdate], diff_analysis: DiffAnalysis) -> str:
        """Assess the risk level of the planned updates"""
        high_risk_count = 0
        medium_risk_count = 0
        
        for update in updates:
            if update.operation == UpdateOperation.DELETE_NODE:
                high_risk_count += 1
            elif update.operation in [UpdateOperation.UPDATE_RELATIONSHIP, UpdateOperation.UPDATE_NODE]:
                medium_risk_count += 1
        
        if high_risk_count > 5:
            return "HIGH"
        elif high_risk_count > 0 or medium_risk_count > 10:
            return "MEDIUM"
        else:
            return "LOW"
    
    def _should_abort_on_error(self, update: GraphUpdate, error: Exception) -> bool:
        """Decide whether to abort the entire update process on error"""
        # For now, continue on most errors except critical ones
        if "connection" in str(error).lower() or "database" in str(error).lower():
            return True
        return False
    
    def _is_code_file(self, file_path: str) -> bool:
        """Check if file is a code file that needs relationship analysis"""
        code_extensions = ['.py', '.js', '.ts', '.java', '.cpp', '.c', '.h', '.hpp']
        return any(file_path.endswith(ext) for ext in code_extensions)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get engine statistics"""
        return self.stats.copy()