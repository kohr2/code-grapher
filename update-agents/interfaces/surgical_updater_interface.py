"""
Interface for surgical update functionality
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional
from .update_models import UpdateResult, GitDiff


class SurgicalUpdaterInterface(ABC):
    """Interface for surgical graph updates"""
    
    @abstractmethod
    def update_from_diff(self, diff: GitDiff) -> UpdateResult:
        """
        Update graph from git diff with enhanced relationship validation
        This addresses the relationship extraction issues from surgical updater
        """
        pass
    
    @abstractmethod
    def create_update_plan(self, diff: GitDiff) -> 'UpdatePlan':
        """Create update plan from git diff analysis"""
        pass
    
    @abstractmethod
    def apply_changes_with_validation(self, 
                                    changed_entities: List[Any], 
                                    relationships: List[Any]) -> UpdateResult:
        """Apply changes with comprehensive validation and rollback capability"""
        pass
    
    @abstractmethod
    def validate_entity_relationships(self, 
                                    entity: Any, 
                                    relationships: List[Any]) -> List[Any]:
        """
        Validate relationships for a specific entity
        This was the missing validation causing surgical updater issues
        """
        pass