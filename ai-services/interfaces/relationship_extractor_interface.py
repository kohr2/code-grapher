"""
Relationship extraction interface
"""
from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional
from relationship_models import Relationship, RelationshipExtractionResult


class RelationshipExtractorInterface(ABC):
    """Interface for relationship extraction services"""
    
    @abstractmethod
    def extract_relationships(self, source_file: str, target_file: str,
                            source_code: str, target_code: str,
                            relationship_types: Optional[List[str]] = None) -> RelationshipExtractionResult:
        """
        Extract relationships between two code files
        
        Args:
            source_file: Path to source file
            target_file: Path to target file
            source_code: Content of source file
            target_code: Content of target file
            relationship_types: Optional list of relationship types to extract
            
        Returns:
            RelationshipExtractionResult with extracted and validated relationships
        """
        pass
    
    @abstractmethod
    def validate_relationships(self, relationships: List[Relationship], 
                             context: Dict[str, Any]) -> List[Relationship]:
        """
        Validate extracted relationships
        
        Args:
            relationships: List of relationships to validate
            context: Context information for validation
            
        Returns:
            List of valid relationships
        """
        pass
    
    @abstractmethod
    def get_supported_relationship_types(self) -> List[str]:
        """Get list of supported relationship types"""
        pass