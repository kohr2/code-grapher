#!/usr/bin/env python3
"""
TypeScript Relationship Extractor Interface
Defines the contract for JavaScript/TypeScript code relationship extraction
Mirrors the AST relationship extraction approach used for Python
"""

from abc import ABC, abstractmethod
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from enum import Enum


class JSRelationshipType(Enum):
    """JavaScript/TypeScript relationship types"""
    IMPORTS = "IMPORTS"
    EXPORTS = "EXPORTS"
    CALLS = "CALLS"
    EXTENDS = "EXTENDS"
    IMPLEMENTS = "IMPLEMENTS"
    DECORATES = "DECORATES"
    TYPE_DEPENDENCY = "TYPE_DEPENDENCY"
    REFERENCES = "REFERENCES"
    ASSIGNS = "ASSIGNS"
    RETURNS = "RETURNS"


@dataclass
class JSRelationshipExtraction:
    """JavaScript/TypeScript relationship extraction result"""
    source_file: str
    target_file: str
    source_entity: str
    target_entity: str
    relationship_type: JSRelationshipType
    confidence: float
    relationship_strength: str  # "strong", "medium", "weak"
    line_number: Optional[int] = None
    context: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None


@dataclass
class JSEntityInfo:
    """JavaScript/TypeScript entity information"""
    name: str
    type: str  # "function", "class", "interface", "type", "variable", "import", "export"
    file_path: str
    line_number: int
    specialized_type: Optional[str] = None
    category: Optional[str] = None
    metadata: Optional[Dict[str, Any]] = None
    
    # Type-specific properties
    parameters: Optional[List[Dict[str, Any]]] = None
    return_type: Optional[str] = None
    decorators: Optional[List[str]] = None
    base_classes: Optional[List[str]] = None
    interfaces: Optional[List[str]] = None
    type_parameters: Optional[List[str]] = None
    is_async: bool = False
    is_exported: bool = False
    is_default_export: bool = False


class TSRelationshipExtractorInterface(ABC):
    """Interface for TypeScript/JavaScript relationship extraction"""
    
    @abstractmethod
    def extract_relationships(self, file_paths: List[str], 
                            project_root: Optional[str] = None) -> List[JSRelationshipExtraction]:
        """
        Extract all relationships from TypeScript/JavaScript files
        
        Args:
            file_paths: List of TypeScript/JavaScript files to analyze
            project_root: Root directory of the project (for resolving imports)
            
        Returns:
            List of relationship extractions
        """
        pass
    
    @abstractmethod
    def extract_entities(self, file_paths: List[str],
                        project_root: Optional[str] = None) -> List[JSEntityInfo]:
        """
        Extract all entities from TypeScript/JavaScript files
        
        Args:
            file_paths: List of TypeScript/JavaScript files to analyze
            project_root: Root directory of the project
            
        Returns:
            List of entity information
        """
        pass
    
    @abstractmethod
    def analyze_file(self, file_path: str,
                    project_root: Optional[str] = None) -> Dict[str, Any]:
        """
        Analyze a single TypeScript/JavaScript file
        
        Args:
            file_path: Path to the file to analyze
            project_root: Root directory of the project
            
        Returns:
            Dictionary containing entities and relationships for the file
        """
        pass
    
    @abstractmethod
    def get_supported_extensions(self) -> List[str]:
        """
        Get list of supported file extensions
        
        Returns:
            List of file extensions (e.g., ['.ts', '.tsx', '.js', '.jsx'])
        """
        pass
    
    @abstractmethod
    def validate_project_structure(self, project_root: str) -> bool:
        """
        Validate that the project structure is compatible with the extractor
        
        Args:
            project_root: Root directory of the project
            
        Returns:
            True if the project can be analyzed, False otherwise
        """
        pass


class JSEntityClassifierInterface(ABC):
    """Interface for JavaScript/TypeScript entity classification"""
    
    @abstractmethod
    def classify_entity(self, entity: JSEntityInfo, 
                       file_context: Dict[str, Any]) -> Dict[str, Any]:
        """
        Classify a JavaScript/TypeScript entity with specialized types
        
        Args:
            entity: The entity to classify
            file_context: Context information about the file
            
        Returns:
            Classification result with specialized type and metadata
        """
        pass
    
    @abstractmethod
    def get_entity_categories(self) -> List[str]:
        """
        Get list of available entity categories
        
        Returns:
            List of category names
        """
        pass
    
    @abstractmethod
    def get_specialized_types(self) -> List[str]:
        """
        Get list of available specialized types
        
        Returns:
            List of specialized type names
        """
        pass