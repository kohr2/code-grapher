"""
Simple AI Service Implementation

A minimal AI service implementation that uses existing AST-based relationship extraction.
This bypasses the complex ai-services module import issues while providing working functionality.
"""

import sys
import os
import asyncio
import requests
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
        self._description_cache = {}  # Cache for generated descriptions
        self._max_descriptions = 1000  # Reasonable limit to prevent runaway AI calls
        self._description_count = 0
        
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
            
            # Convert RelationshipExtraction objects to dictionaries
            relationship_dicts = []
            for rel in relationships:
                if hasattr(rel, 'source_entity'):  # It's a RelationshipExtraction object
                    rel_type = rel.relationship_type.value if hasattr(rel.relationship_type, 'value') else str(rel.relationship_type)
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
            
            self.logger.log_info(f"Extracted {len(relationship_dicts)} relationships using AST approach")
            return relationship_dicts
            
        except Exception as e:
            self.logger.log_error(f"Relationship extraction failed: {e}")
            return []
    
    async def generate_description(self, entity_data: dict, primer_context: str = "") -> str:
        """
        Generate AI description for an entity with optimization to prevent infinite loops
        
        Args:
            entity_data: Entity information
            primer_context: Business context
            
        Returns:
            Generated description string
        """
        try:
            entity_name = entity_data.get('name', 'Unknown')
            entity_type = entity_data.get('type', 'unknown')
            file_path = entity_data.get('file_path', 'Unknown file')
            
            # Create cache key
            cache_key = f"{entity_type}:{entity_name}:{file_path}"
            
            # Check cache first
            if cache_key in self._description_cache:
                return self._description_cache[cache_key]
            
            # Limit AI descriptions to prevent runaway AI calls
            if self._description_count >= self._max_descriptions:
                fallback_desc = f"A {entity_type} named {entity_name} in {file_path}"
                self._description_cache[cache_key] = fallback_desc
                self.logger.log_warning(f"AI description limit reached ({self._description_count}/{self._max_descriptions}), using fallback for {entity_name}")
                return fallback_desc
            
            # Generate AI description
            self.logger.log_info(f"Generating AI description {self._description_count + 1}/{self._max_descriptions} for {entity_name} ({entity_type})")
            description = await self._generate_ai_description(entity_data, primer_context)
            self.logger.log_info(f"AI description generated for {entity_name}: {description[:100]}...")
            
            # Cache the result
            self._description_cache[cache_key] = description
            self._description_count += 1
            
            return description
                
        except Exception as e:
            self.logger.log_error(f"Description generation failed: {e}")
            fallback_desc = f"A {entity_data.get('type', 'code entity')} named {entity_data.get('name', 'Unknown')}"
            return fallback_desc
    
    async def _generate_ai_description(self, entity_data: dict, primer_context: str = "") -> str:
        """
        Generate AI description using Ollama
        
        Args:
            entity_data: Entity information
            primer_context: Business context
            
        Returns:
            AI-generated description string
        """
        try:
            self.logger.log_info("Starting AI description generation...")
            entity_name = entity_data.get('name', 'Unknown')
            entity_type = entity_data.get('type', 'unknown')
            file_path = entity_data.get('file_path', 'Unknown file')
            code_snippet = entity_data.get('code_snippet', '')
            
            # Prepare code content
            if code_snippet:
                code_content = code_snippet
            else:
                # Read first 20 lines of file if no code snippet
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        lines = f.readlines()[:20]
                        code_content = ''.join(lines)
                except:
                    code_content = f"// {entity_type} {entity_name}"
            
            # Create line number context
            line_numbers = entity_data.get('line_numbers', [])
            if line_numbers:
                if len(line_numbers) == 1:
                    line_context = f"at line {line_numbers[0]}"
                else:
                    line_ranges = self._create_line_ranges(line_numbers)
                    line_context = f"at lines {', '.join(line_ranges)}"
            else:
                line_context = ""
            
            # Create very concise prompt
            prompt = f"""What does this {entity_type} do? One sentence:

{code_content}"""

            # Call Ollama API
            self.logger.log_info("Calling Ollama API...")
            ollama_url = "http://localhost:11434/api/generate"
            payload = {
                "model": "tinyllama:latest",
                "prompt": prompt,
                "stream": False,
                "options": {
                    "temperature": 0.3,
                    "max_tokens": 100
                }
            }
            
            self.logger.log_info(f"Sending request to Ollama with prompt length: {len(prompt)}")
            self.logger.log_info(f"Prompt preview: {prompt[:200]}...")
            response = requests.post(ollama_url, json=payload, timeout=15)
            self.logger.log_info(f"Received response from Ollama with status: {response.status_code}")
            response.raise_for_status()
            
            result = response.json()
            description = result.get('response', '').strip()
            
            # Clean up description
            if description and not description.startswith("I cannot") and not description.startswith("I don't"):
                return description
            else:
                return f"A {entity_type} named {entity_name} in {file_path}"
                
        except Exception as e:
            self.logger.log_warning(f"AI description generation failed: {e}")
            return f"A {entity_data.get('type', 'code entity')} named {entity_data.get('name', 'Unknown')}"
    
    def _create_line_ranges(self, line_numbers: List[int]) -> List[str]:
        """Create line ranges from a list of line numbers"""
        if not line_numbers:
            return []
        
        line_numbers = sorted(set(line_numbers))
        ranges = []
        start = line_numbers[0]
        end = line_numbers[0]
        
        for i in range(1, len(line_numbers)):
            if line_numbers[i] == end + 1:
                end = line_numbers[i]
            else:
                if start == end:
                    ranges.append(str(start))
                else:
                    ranges.append(f"{start}-{end}")
                start = line_numbers[i]
                end = line_numbers[i]
        
        # Add the last range
        if start == end:
            ranges.append(str(start))
        else:
            ranges.append(f"{start}-{end}")
        
        return ranges
    
    def health_check(self) -> Dict[str, Any]:
        """Check service health"""
        return {
            'status': 'healthy' if self._extract_relationships_func else 'degraded',
            'service_type': 'simple_ai_service',
            'ast_extractor_available': self._extract_relationships_func is not None,
            'description_count': self._description_count,
            'max_descriptions': self._max_descriptions,
            'cache_size': len(self._description_cache)
        }
    
    def reset_description_count(self):
        """Reset the description count (useful for testing or if limit is reached)"""
        self._description_count = 0
        self.logger.log_info("AI description count reset to 0")
    
    def clear_description_cache(self):
        """Clear the description cache (useful for testing or memory management)"""
        cache_size = len(self._description_cache)
        self._description_cache.clear()
        self.logger.log_info(f"Description cache cleared ({cache_size} entries removed)")
    
    def shutdown(self):
        """Shutdown the service"""
        self.logger.log_info("Simple AI service shutting down")