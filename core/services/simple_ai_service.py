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
            # Import multi-language relationship extractor instead of just AST
            from shared.services.multi_language_parser import extract_multi_language_relationships
            self._extract_relationships_func = extract_multi_language_relationships
            
            self.logger.log_info("Simple AI service initialized with multi-language relationship extraction")
            return True
            
        except ImportError as e:
            self.logger.log_error(f"Failed to import multi-language relationship extractor: {e}")
            return False
    
    async def extract_relationships(self, parsed_files: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Extract relationships using multi-language approach
        
        Args:
            parsed_files: List of parsed file data with entities
            
        Returns:
            List of relationship dictionaries
        """
        try:
            if not self._extract_relationships_func:
                self.logger.log_warning("Multi-language relationship extractor not available")
                return []
            
            # Fix parsed files format for AST extractor compatibility
            # AST extractor expects 'success' field but pipeline provides 'parse_success'
            for file_data in parsed_files:
                if 'parse_success' in file_data and 'success' not in file_data:
                    file_data['success'] = file_data['parse_success']
            
            # Use existing AST relationship extraction
            relationships = self._extract_relationships_func(parsed_files)
            print(f"   🔍 DEBUG: AI service received {len(relationships)} relationships from multi-language parser")
            
            # Convert RelationshipExtraction objects to dictionaries
            relationship_dicts = []
            print(f"   🔍 DEBUG: Starting conversion of {len(relationships)} relationships")
            for i, rel in enumerate(relationships):
                if i < 10 or i % 20 == 0:  # Show first 10 and every 20th
                    print(f"   🔍 DEBUG: Processing relationship {i+1}/{len(relationships)}")
                if hasattr(rel, 'source_entity'):  # It's a RelationshipExtraction object
                    rel_type = rel.relationship_type.value if hasattr(rel.relationship_type, 'value') else str(rel.relationship_type)
                    print(f"   🔍 DEBUG: Converting relationship {i+1}: {rel.source_entity} -{rel_type}-> {rel.target_entity}")
                    relationship_dicts.append(
                        {
                            "source": rel.source_entity,
                            "target": rel.target_entity,
                            "type": rel_type,
                            "source_file": rel.source_file,
                            "target_file": rel.target_file,
                            "confidence": rel.confidence,
                            "strength": rel.relationship_strength,
                            "line_number": rel.line_number,
                            "context": rel.context,
                            "properties": {
                                "relationship_strength": rel.relationship_strength,
                                "confidence": rel.confidence,
                            },
                        }
                    )
                elif isinstance(rel, dict):  # It's already a dictionary
                    relationship_dicts.append(rel)
                else:
                    # Skip invalid relationships
                    self.logger.log_warning(f"Skipping invalid relationship object: {type(rel)}")
                    continue
            
            # Debug: Show final relationship types
            rel_types = {}
            for rel_dict in relationship_dicts:
                rel_type = rel_dict.get("type", "UNKNOWN")
                rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
            print(f"   🔍 DEBUG: AI service final relationship types: {rel_types}")
            
            self.logger.log_info(f"Extracted {len(relationship_dicts)} relationships using AST approach")
            return relationship_dicts
            
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