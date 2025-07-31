"""
Main AI services interface - orchestrates all AI functionality
"""
from abc import ABC, abstractmethod
from typing import Any, Dict, List, Optional
from .ai_provider_interface import AIProviderInterface
from ..models.relationship_models import RelationshipExtractionResult


class AIServicesInterface(ABC):
    """Main interface for AI services orchestration"""
    
    @abstractmethod
    def get_provider(self, provider_type: Optional[str] = None) -> AIProviderInterface:
        """
        Get AI provider instance
        
        Args:
            provider_type: Optional provider type, uses default if not specified
            
        Returns:
            AI provider instance
        """
        pass
    
    @abstractmethod
    def extract_relationships(self, source_file: str, target_file: str,
                            source_code: str, target_code: str) -> RelationshipExtractionResult:
        """
        Extract relationships between files with validation
        
        Args:
            source_file: Source file path
            target_file: Target file path  
            source_code: Source file content
            target_code: Target file content
            
        Returns:
            Validated relationship extraction result
        """
        pass
    
    @abstractmethod
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """
        Generate AI-powered description for entity
        
        Args:
            entity: Entity information
            context: Optional business context
            
        Returns:
            Generated description
        """
        pass
    
    @abstractmethod
    def generate_descriptions_batch(self, entities: List[Dict[str, Any]], 
                                  context: Optional[str] = None) -> Dict[str, str]:
        """
        Generate descriptions for multiple entities efficiently
        
        Args:
            entities: List of entities
            context: Optional business context
            
        Returns:
            Mapping of entity IDs to descriptions
        """
        pass
    
    @abstractmethod
    def track_evaluation(self, category: str, result: Any, metadata: Optional[Dict[str, Any]] = None) -> None:
        """
        Track AI evaluation result
        
        Args:
            category: Evaluation category
            result: Result to track
            metadata: Optional metadata
        """
        pass
    
    @abstractmethod
    def get_evaluation_summary(self) -> Dict[str, Any]:
        """Get summary of AI evaluation metrics"""
        pass
    
    @abstractmethod
    def semantic_search(self, query: str, limit: int = 5) -> List[Dict[str, Any]]:
        """
        Perform semantic search using AI embeddings
        
        Args:
            query: Search query
            limit: Maximum results
            
        Returns:
            Search results
        """
        pass
    
    @abstractmethod
    def health_check(self) -> Dict[str, Any]:
        """Get health status of all AI services"""
        pass