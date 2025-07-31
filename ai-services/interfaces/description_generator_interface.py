"""
Description generation interface
"""
from abc import ABC, abstractmethod
from typing import Dict, Any, Optional, List


class DescriptionGeneratorInterface(ABC):
    """Interface for AI-powered description generation"""
    
    @abstractmethod
    def generate_entity_description(self, entity: Dict[str, Any], 
                                   context: Optional[str] = None) -> str:
        """
        Generate description for a code entity
        
        Args:
            entity: Entity information (type, name, properties, etc.)
            context: Optional business context from primer
            
        Returns:
            Generated description string
        """
        pass
    
    @abstractmethod
    def generate_file_summary(self, file_path: str, content: str,
                            entities: List[Dict[str, Any]]) -> str:
        """
        Generate summary for an entire file
        
        Args:
            file_path: Path to the file
            content: File content
            entities: Entities found in the file
            
        Returns:
            Generated file summary
        """
        pass
    
    @abstractmethod
    def generate_relationship_description(self, relationship: Dict[str, Any]) -> str:
        """
        Generate description for a relationship
        
        Args:
            relationship: Relationship information
            
        Returns:
            Generated relationship description
        """
        pass