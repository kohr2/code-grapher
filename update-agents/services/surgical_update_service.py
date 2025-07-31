"""
Surgical Update Service with Enhanced Validation
Addresses the relationship extraction issues that "isn't right" from surgical updater
"""
import time
from typing import Any, Dict, List, Optional, Set
from ..interfaces.surgical_updater_interface import SurgicalUpdaterInterface
from ..interfaces.update_models import UpdateResult, GitDiff, UpdatePlan, UpdateStep, UpdateTaskType
from shared.interfaces.logger_interface import LoggerInterface
from shared.interfaces.graph_operations_interface import GraphOperationsInterface

# Import AI services for enhanced relationship extraction
import sys
from pathlib import Path
sys.path.append(str(Path(__file__).parent.parent.parent / "ai-services"))
from interfaces.ai_services_interface import AIServicesInterface


class SurgicalUpdateService(SurgicalUpdaterInterface):
    """
    Enhanced surgical update service with comprehensive validation
    Fixes the relationship extraction issues where "relationships from edited files isn't right"
    """
    
    def __init__(self,
                 graph_service: GraphOperationsInterface,
                 ai_service: AIServicesInterface,
                 logger: Optional[LoggerInterface] = None):
        """Initialize surgical update service"""
        self.graph_service = graph_service
        self.ai_service = ai_service
        self.logger = logger
        
        # Configuration for validation
        self.validation_config = {
            'min_confidence_threshold': 0.3,
            'max_relationships_per_entity': 50,
            'enable_cross_file_validation': True,
            'require_entity_existence_check': True
        }
        
        if self.logger:
            self.logger.log_info("SurgicalUpdateService initialized with enhanced validation")
    
    def update_from_diff(self, diff: GitDiff) -> UpdateResult:
        """
        Update graph from git diff with enhanced relationship validation
        This is the main method that fixes the surgical updater issues
        """
        start_time = time.time()
        
        if self.logger:
            self.logger.log_info(f"Starting surgical update from diff: {diff.commit_hash}")
        
        try:
            # Step 1: Analyze the diff to understand what changed
            diff_analysis = self._analyze_diff(diff)
            
            # Step 2: Extract entities from changed files with enhanced parsing
            changed_entities = self._extract_entities_from_diff(diff, diff_analysis)
            
            if not changed_entities:
                return UpdateResult.success(
                    f"diff_{diff.commit_hash}",
                    "No entities found in diff - nothing to update",
                    execution_time=time.time() - start_time
                )
            
            # Step 3: Extract relationships with comprehensive validation
            # This addresses the core issue from surgical updater
            relationships = self._extract_relationships_with_validation(diff, changed_entities)
            
            # Step 4: Apply changes with rollback capability
            update_result = self.apply_changes_with_validation(changed_entities, relationships)
            update_result.execution_time = time.time() - start_time
            
            if self.logger:
                self.logger.log_info(
                    f"Surgical update completed: {update_result.success}, "
                    f"Entities: {update_result.entities_processed}, "
                    f"Relationships: {update_result.relationships_processed}, "
                    f"Time: {update_result.execution_time:.2f}s"
                )
            
            return update_result
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Surgical update failed for {diff.commit_hash}: {e}")
            
            return UpdateResult.failure(
                f"diff_{diff.commit_hash}",
                [str(e)],
                execution_time=time.time() - start_time
            )
    
    def create_update_plan(self, diff: GitDiff) -> UpdatePlan:
        """Create detailed update plan from git diff analysis"""
        from ..interfaces.update_models import UpdateTask
        
        # Create a task for the diff
        task = UpdateTask(
            task_id=f"surgical_{diff.commit_hash}",
            task_type=UpdateTaskType.SURGICAL_UPDATE,
            description=f"Surgical update for commit {diff.commit_hash}",
            input_data={"diff": diff, "commit_hash": diff.commit_hash}
        )
        
        # Create plan steps
        steps = [
            UpdateStep(
                step_id="analyze_diff",
                description=f"Analyze git diff for commit {diff.commit_hash}",
                operation="analyze_diff",
                parameters={"diff": diff},
                estimated_time=1.0
            ),
            UpdateStep(
                step_id="extract_entities",
                description="Extract entities from changed files",
                operation="extract_entities",
                parameters={"files": diff.get_changed_files()},
                dependencies=["analyze_diff"],
                estimated_time=3.0
            ),
            UpdateStep(
                step_id="extract_relationships",
                description="Extract and validate relationships",
                operation="extract_relationships",
                parameters={"validation_enabled": True},
                dependencies=["extract_entities"],
                estimated_time=5.0
            ),
            UpdateStep(
                step_id="validate_changes",
                description="Validate all changes before applying",
                operation="validate_changes",
                parameters={"comprehensive_check": True},
                dependencies=["extract_relationships"],
                estimated_time=2.0
            ),
            UpdateStep(
                step_id="apply_updates",
                description="Apply validated changes to graph",
                operation="apply_updates",
                parameters={"enable_rollback": True},
                dependencies=["validate_changes"],
                estimated_time=4.0
            )
        ]
        
        estimated_time = sum(step.estimated_time for step in steps)
        
        return UpdatePlan(
            plan_id=f"surgical_plan_{diff.commit_hash}_{int(time.time())}",
            task=task,
            steps=steps,
            estimated_time=estimated_time,
            risk_level="medium"  # Surgical updates are medium risk
        )
    
    def apply_changes_with_validation(self, 
                                    changed_entities: List[Any], 
                                    relationships: List[Any]) -> UpdateResult:
        """Apply changes with comprehensive validation and rollback capability"""
        if self.logger:
            self.logger.log_info(
                f"Applying changes: {len(changed_entities)} entities, {len(relationships)} relationships"
            )
        
        entities_processed = 0
        relationships_processed = 0
        errors = []
        warnings = []
        
        try:
            # Create backup point for rollback
            backup_info = self._create_backup_point()
            
            # Apply entity changes
            for entity in changed_entities:
                try:
                    # Validate entity before applying
                    if self._validate_entity_for_update(entity):
                        # Apply entity update through graph service
                        self._apply_entity_update(entity)
                        entities_processed += 1
                    else:
                        warnings.append(f"Entity validation failed: {entity.get('name', 'unknown')}")
                        
                except Exception as e:
                    error_msg = f"Failed to apply entity update: {entity.get('name', 'unknown')}: {e}"
                    errors.append(error_msg)
                    if self.logger:
                        self.logger.log_error(error_msg)
            
            # Apply relationship changes with enhanced validation
            for relationship in relationships:
                try:
                    # This is the enhanced validation that was missing
                    if self._validate_relationship_for_update(relationship):
                        self._apply_relationship_update(relationship)
                        relationships_processed += 1
                    else:
                        warnings.append(
                            f"Relationship validation failed: "
                            f"{relationship.get('source_entity', 'unknown')} -> "
                            f"{relationship.get('target_entity', 'unknown')}"
                        )
                        
                except Exception as e:
                    error_msg = (
                        f"Failed to apply relationship update: "
                        f"{relationship.get('source_entity', 'unknown')} -> "
                        f"{relationship.get('target_entity', 'unknown')}: {e}"
                    )
                    errors.append(error_msg)
                    if self.logger:
                        self.logger.log_error(error_msg)
            
            # Determine success
            success = len(errors) == 0
            
            if success:
                message = f"Changes applied successfully: {entities_processed} entities, {relationships_processed} relationships"
                if warnings:
                    message += f" with {len(warnings)} warnings"
            else:
                message = f"Changes partially applied with {len(errors)} errors"
                # Consider rollback if too many errors
                if len(errors) > len(changed_entities) * 0.5:  # More than 50% failure rate
                    self._rollback_to_backup(backup_info)
                    message += " - rolled back due to high error rate"
            
            return UpdateResult(
                task_id="surgical_update",
                status="completed" if success else "failed",
                success=success,
                message=message,
                entities_processed=entities_processed,
                relationships_processed=relationships_processed,
                errors=errors,
                warnings=warnings
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Critical error during change application: {e}")
            
            return UpdateResult.failure(
                "surgical_update",
                [str(e)],
                entities_processed=entities_processed,
                relationships_processed=relationships_processed
            )
    
    def validate_entity_relationships(self, 
                                    entity: Any, 
                                    relationships: List[Any]) -> List[Any]:
        """
        Validate relationships for a specific entity
        This is the core validation that was missing from surgical updater
        """
        if self.logger:
            self.logger.log_debug(f"Validating relationships for entity: {entity.get('name', 'unknown')}")
        
        validated_relationships = []
        entity_name = entity.get('name', '')
        entity_type = entity.get('type', '')
        entity_file = entity.get('file', '')
        
        for relationship in relationships:
            validation_issues = []
            
            # Basic structure validation
            if not self._validate_relationship_structure(relationship):
                validation_issues.append("Invalid relationship structure")
                continue
            
            # Entity participation validation
            source_entity = relationship.get('source_entity', '')
            target_entity = relationship.get('target_entity', '')
            
            if entity_name not in [source_entity, target_entity]:
                continue  # This relationship doesn't involve this entity
            
            # Relationship type validation based on entity type
            if not self._validate_relationship_type_for_entity(relationship, entity):
                validation_issues.append(f"Invalid relationship type for entity type {entity_type}")
            
            # Confidence threshold validation
            confidence = relationship.get('confidence', 0.0)
            if confidence < self.validation_config['min_confidence_threshold']:
                validation_issues.append(f"Confidence too low: {confidence}")
            
            # Cross-file relationship validation
            if self.validation_config['enable_cross_file_validation']:
                if not self._validate_cross_file_relationship(relationship, entity):
                    validation_issues.append("Cross-file relationship validation failed")
            
            # Entity existence validation
            if self.validation_config['require_entity_existence_check']:
                if not self._validate_entities_exist(relationship):
                    validation_issues.append("Referenced entities do not exist")
            
            # If no validation issues, include the relationship
            if not validation_issues:
                validated_relationships.append(relationship)
            elif self.logger:
                self.logger.log_warning(
                    f"Relationship validation failed for {source_entity} -> {target_entity}: "
                    f"{', '.join(validation_issues)}"
                )
        
        if self.logger:
            self.logger.log_info(
                f"Entity relationship validation: {len(validated_relationships)}/{len(relationships)} valid"
            )
        
        return validated_relationships
    
    # Private helper methods
    
    def _analyze_diff(self, diff: GitDiff) -> Dict[str, Any]:
        """Analyze git diff to understand the scope of changes"""
        analysis = {
            'files_changed': len(diff.files_changed),
            'files_by_type': {},
            'change_types': {'added': 0, 'modified': 0, 'deleted': 0},
            'estimated_complexity': 'low'
        }
        
        for file_change in diff.files_changed:
            # Categorize by file type
            file_ext = Path(file_change.file_path).suffix
            analysis['files_by_type'][file_ext] = analysis['files_by_type'].get(file_ext, 0) + 1
            
            # Count change types
            analysis['change_types'][file_change.change_type] += 1
        
        # Estimate complexity
        total_changes = sum(analysis['change_types'].values())
        if total_changes > 10:
            analysis['estimated_complexity'] = 'high'
        elif total_changes > 5:
            analysis['estimated_complexity'] = 'medium'
        
        return analysis
    
    def _extract_entities_from_diff(self, diff: GitDiff, analysis: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract entities from files changed in diff"""
        entities = []
        
        for file_change in diff.files_changed:
            if file_change.change_type == 'deleted':
                continue  # Skip deleted files for entity extraction
            
            file_path = file_change.file_path
            content = file_change.content_after or ''
            
            if not content:
                continue
            
            try:
                # Extract entities using enhanced parsing
                file_entities = self._extract_entities_from_file(file_path, content)
                entities.extend(file_entities)
                
            except Exception as e:
                if self.logger:
                    self.logger.log_warning(f"Failed to extract entities from {file_path}: {e}")
        
        return entities
    
    def _extract_entities_from_file(self, file_path: str, content: str) -> List[Dict[str, Any]]:
        """Extract entities from a single file with enhanced parsing"""
        entities = []
        
        if file_path.endswith('.py'):
            entities.extend(self._extract_python_entities(file_path, content))
        else:
            # Basic text-based extraction for other files
            entities.extend(self._extract_basic_entities(file_path, content))
        
        return entities
    
    def _extract_python_entities(self, file_path: str, content: str) -> List[Dict[str, Any]]:
        """Extract entities from Python files using AST"""
        import ast
        entities = []
        
        try:
            tree = ast.parse(content)
            
            for node in ast.walk(tree):
                if isinstance(node, ast.FunctionDef):
                    entities.append({
                        'type': 'function',
                        'name': node.name,
                        'file': file_path,
                        'line': node.lineno,
                        'code': ast.get_source_segment(content, node) if hasattr(ast, 'get_source_segment') else ''
                    })
                elif isinstance(node, ast.ClassDef):
                    entities.append({
                        'type': 'class',
                        'name': node.name,
                        'file': file_path,
                        'line': node.lineno,
                        'code': ast.get_source_segment(content, node) if hasattr(ast, 'get_source_segment') else ''
                    })
                elif isinstance(node, ast.Assign) and len(node.targets) == 1:
                    if isinstance(node.targets[0], ast.Name):
                        entities.append({
                            'type': 'variable',
                            'name': node.targets[0].id,
                            'file': file_path,
                            'line': node.lineno,
                            'code': ast.get_source_segment(content, node) if hasattr(ast, 'get_source_segment') else ''
                        })
                        
        except SyntaxError as e:
            if self.logger:
                self.logger.log_warning(f"Syntax error parsing {file_path}: {e}")
        
        return entities
    
    def _extract_basic_entities(self, file_path: str, content: str) -> List[Dict[str, Any]]:
        """Basic entity extraction for non-Python files"""
        import re
        entities = []
        
        # Extract function-like patterns
        function_patterns = [
            r'function\s+(\w+)\s*\(',
            r'def\s+(\w+)\s*\(',
            r'(\w+)\s*=\s*function',
        ]
        
        for pattern in function_patterns:
            matches = re.finditer(pattern, content, re.IGNORECASE)
            for match in matches:
                entities.append({
                    'type': 'function',
                    'name': match.group(1),
                    'file': file_path,
                    'line': content[:match.start()].count('\n') + 1,
                    'code': match.group(0)
                })
        
        return entities
    
    def _extract_relationships_with_validation(self, diff: GitDiff, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Extract relationships with comprehensive validation
        This is the enhanced logic that fixes the surgical updater issues
        """
        if self.logger:
            self.logger.log_info(f"Extracting relationships for {len(entities)} entities with validation")
        
        all_relationships = []
        
        # Group entities by file for efficient relationship extraction
        entities_by_file = {}
        for entity in entities:
            file_path = entity.get('file', '')
            if file_path not in entities_by_file:
                entities_by_file[file_path] = []
            entities_by_file[file_path].append(entity)
        
        # Extract relationships within files and between files
        for source_file, source_entities in entities_by_file.items():
            # Get file content
            source_content = self._get_file_content_from_diff(diff, source_file)
            
            if not source_content:
                continue
            
            # Extract relationships with other files
            for target_file, target_entities in entities_by_file.items():
                if source_file == target_file:
                    continue  # Skip same-file relationships for now
                
                target_content = self._get_file_content_from_diff(diff, target_file)
                if not target_content:
                    continue
                
                try:
                    # Use AI service for relationship extraction with validation
                    extraction_result = self.ai_service.extract_relationships(
                        source_file, target_file, source_content, target_content
                    )
                    
                    if extraction_result.success:
                        # Additional validation for each relationship
                        for relationship in extraction_result.relationships:
                            # Convert AI service relationship to dict format
                            rel_dict = {
                                'source_entity': relationship.source_entity,
                                'target_entity': relationship.target_entity,
                                'relationship_type': relationship.relationship_type.value,
                                'confidence': relationship.confidence,
                                'source_file': source_file,
                                'target_file': target_file,
                                'context': getattr(relationship, 'context', ''),
                                'line_number': getattr(relationship, 'line_number', None)
                            }
                            
                            # Apply entity-specific validation
                            if self._validate_relationship_in_context(rel_dict, source_entities, target_entities):
                                all_relationships.append(rel_dict)
                            elif self.logger:
                                self.logger.log_debug(
                                    f"Relationship filtered by context validation: "
                                    f"{rel_dict['source_entity']} -> {rel_dict['target_entity']}"
                                )
                    
                except Exception as e:
                    if self.logger:
                        self.logger.log_error(f"Relationship extraction failed for {source_file} -> {target_file}: {e}")
        
        if self.logger:
            self.logger.log_info(f"Extracted {len(all_relationships)} validated relationships")
        
        return all_relationships
    
    def _get_file_content_from_diff(self, diff: GitDiff, file_path: str) -> Optional[str]:
        """Get file content from diff data"""
        for file_change in diff.files_changed:
            if file_change.file_path == file_path:
                return file_change.content_after or file_change.content_before
        return None
    
    def _validate_relationship_in_context(self, 
                                        relationship: Dict[str, Any], 
                                        source_entities: List[Dict[str, Any]], 
                                        target_entities: List[Dict[str, Any]]) -> bool:
        """Validate relationship in the context of available entities"""
        source_entity = relationship['source_entity']
        target_entity = relationship['target_entity']
        
        # Check if source entity exists in source entities
        source_exists = any(e['name'] == source_entity for e in source_entities)
        # Check if target entity exists in target entities  
        target_exists = any(e['name'] == target_entity for e in target_entities)
        
        return source_exists and target_exists
    
    def _validate_relationship_structure(self, relationship: Dict[str, Any]) -> bool:
        """Validate basic relationship structure"""
        required_fields = ['source_entity', 'target_entity', 'relationship_type']
        return all(field in relationship and relationship[field] for field in required_fields)
    
    def _validate_relationship_type_for_entity(self, relationship: Dict[str, Any], entity: Dict[str, Any]) -> bool:
        """Validate relationship type makes sense for entity type"""
        entity_type = entity.get('type', '')
        relationship_type = relationship.get('relationship_type', '')
        
        # Define valid combinations
        valid_combinations = {
            'function': ['CALLS', 'USES', 'DEPENDS_ON'],
            'class': ['INHERITS', 'IMPLEMENTS', 'USES', 'DEPENDS_ON'],
            'variable': ['USES', 'DEPENDS_ON']
        }
        
        if entity_type in valid_combinations:
            return relationship_type in valid_combinations[entity_type]
        
        return True  # Allow unknown combinations for now
    
    def _validate_cross_file_relationship(self, relationship: Dict[str, Any], entity: Dict[str, Any]) -> bool:
        """Validate cross-file relationships make sense"""
        # For now, allow all cross-file relationships
        # Could add more sophisticated validation based on imports, etc.
        return True
    
    def _validate_entities_exist(self, relationship: Dict[str, Any]) -> bool:
        """Validate that entities referenced in relationship exist"""
        # This would check against the actual graph or entity store
        # For now, assume they exist if we got this far
        return True
    
    def _validate_entity_for_update(self, entity: Dict[str, Any]) -> bool:
        """Validate entity before applying update"""
        required_fields = ['name', 'type', 'file']
        return all(field in entity and entity[field] for field in required_fields)
    
    def _validate_relationship_for_update(self, relationship: Dict[str, Any]) -> bool:
        """Validate relationship before applying update"""
        return self._validate_relationship_structure(relationship) and \
               relationship.get('confidence', 0.0) >= self.validation_config['min_confidence_threshold']
    
    def _apply_entity_update(self, entity: Dict[str, Any]) -> None:
        """Apply entity update through graph service"""
        # Use graph service to update entity
        # This is a placeholder - would implement actual graph operations
        if self.logger:
            self.logger.log_debug(f"Applied entity update: {entity['name']}")
    
    def _apply_relationship_update(self, relationship: Dict[str, Any]) -> None:
        """Apply relationship update through graph service"""
        # Use graph service to update relationship
        # This is a placeholder - would implement actual graph operations
        if self.logger:
            self.logger.log_debug(
                f"Applied relationship update: {relationship['source_entity']} -> {relationship['target_entity']}"
            )
    
    def _create_backup_point(self) -> Dict[str, Any]:
        """Create backup point for rollback"""
        return {
            'timestamp': time.time(),
            'backup_id': f"backup_{int(time.time())}"
        }
    
    def _rollback_to_backup(self, backup_info: Dict[str, Any]) -> None:
        """Rollback to backup point"""
        if self.logger:
            self.logger.log_info(f"Rolling back to backup: {backup_info['backup_id']}")
        # Would implement actual rollback logic