"""
Simple AI Service Implementation

A minimal AI service implementation that uses existing AST-based relationship extraction.
This bypasses the complex ai-services module import issues while providing working functionality.
"""

import sys
import os
from typing import Dict, Any, List, Optional
from abc import ABC, abstractmethod

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(__file__))))

from shared.interfaces.logger_interface import LoggerInterface


class AIServicesInterface(ABC):
    """Minimal AI services interface for relationship extraction"""
    
    @abstractmethod
    async def extract_relationships(self, parsed_files: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract relationships from parsed files"""
        pass
    
    @abstractmethod
    async def generate_description(self, entity_data: dict, primer_context: str = "") -> str:
        """Generate description for an entity"""
        pass


class SimpleAIService(AIServicesInterface):
    """
    Simple AI service implementation using existing AST relationship extraction
    
    This provides working functionality without the complex ai-services module dependencies.
    """
    
    def __init__(self, logger: LoggerInterface):
        self.logger = logger
        self._ast_extractor = None
        
    def initialize(self, config: Dict[str, Any] = None) -> bool:
        """Initialize the AI service"""
        try:
            # Import AST relationship extractor
            from ast_relationship_extractor import extract_ast_relationships
            self._extract_relationships_func = extract_ast_relationships
            
            self.logger.log_info("Simple AI service initialized with AST relationship extraction")
            return True
            
        except ImportError as e:
            self.logger.log_error(f"Failed to import AST relationship extractor: {e}")
            return False
    
    async def extract_relationships(self, parsed_files: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Extract relationships using AST-based approach
        
        Args:
            parsed_files: List of parsed file data with entities
            
        Returns:
            List of relationship dictionaries
        """
        try:
            if not self._extract_relationships_func:
                self.logger.log_warning("AST relationship extractor not available")
                return []
            
            # Fix parsed files format for AST extractor compatibility
            # AST extractor expects 'success' field but pipeline provides 'parse_success'
            for file_data in parsed_files:
                if 'parse_success' in file_data and 'success' not in file_data:
                    file_data['success'] = file_data['parse_success']
            
            # Use existing AST relationship extraction
            relationships = self._extract_relationships_func(parsed_files)
            
            self.logger.log_info(f"Extracted {len(relationships)} relationships using AST approach")
            return relationships
            
        except Exception as e:
            self.logger.log_error(f"Relationship extraction failed: {e}")
            return []
    
    async def generate_description(self, entity_data: dict, primer_context: str = "") -> str:
        """
        Generate simple description for an entity
        
        Args:
            entity_data: Entity information
            primer_context: Business context (not used in simple implementation)
            
        Returns:
            Generated description string
        """
        try:
            entity_name = entity_data.get('name', 'Unknown')
            entity_type = entity_data.get('type', 'unknown')
            file_path = entity_data.get('file_path', 'Unknown file')
            
            # Generate simple description based on entity type
            if entity_type == 'function':
                return f"A function named {entity_name} defined in {file_path}"
            elif entity_type == 'class':
                return f"A class named {entity_name} defined in {file_path}"
            elif entity_type == 'interface':
                return f"An interface named {entity_name} defined in {file_path}"
            elif entity_type == 'import':
                return f"An import statement for {entity_name} in {file_path}"
            else:
                return f"A {entity_type} named {entity_name} in {file_path}"
                
        except Exception as e:
            self.logger.log_error(f"Description generation failed: {e}")
            return f"A {entity_data.get('type', 'code entity')} named {entity_data.get('name', 'Unknown')}"
    
    def health_check(self) -> Dict[str, Any]:
        """Check service health"""
        return {
            'status': 'healthy' if self._extract_relationships_func else 'degraded',
            'service_type': 'simple_ai_service',
            'ast_extractor_available': self._extract_relationships_func is not None
        }
    
    def shutdown(self):
        """Shutdown the service"""
        self.logger.log_info("Simple AI service shutting down")