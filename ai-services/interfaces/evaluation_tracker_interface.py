"""
Evaluation tracking interface
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional
from evaluation_models import EvaluationResult, EvaluationCategory


class EvaluationTrackerInterface(ABC):
    """Interface for AI evaluation tracking"""
    
    @abstractmethod
    def track_evaluation(self, category: EvaluationCategory, result: Any,
                        metadata: Optional[Dict[str, Any]] = None) -> None:
        """
        Track an AI evaluation result
        
        Args:
            category: Category of evaluation
            result: Evaluation result
            metadata: Optional metadata
        """
        pass
    
    @abstractmethod
    def get_evaluation_summary(self, category: Optional[EvaluationCategory] = None) -> Dict[str, Any]:
        """
        Get summary of evaluations
        
        Args:
            category: Optional category filter
            
        Returns:
            Evaluation summary statistics
        """
        pass
    
    @abstractmethod
    def get_evaluation_history(self, limit: int = 100) -> List[EvaluationResult]:
        """
        Get evaluation history
        
        Args:
            limit: Maximum number of results
            
        Returns:
            List of evaluation results
        """
        pass
    
    @abstractmethod
    def clear_evaluations(self) -> None:
        """Clear all evaluation data"""
        pass