"""
Description generation service for AI-powered entity descriptions
"""
import time
from typing import Dict, Any, Optional, List
from pathlib import Path
from description_generator_interface import DescriptionGeneratorInterface
from ai_provider_interface import AIProviderInterface
from shared.interfaces.logger_interface import LoggerInterface


class DescriptionService(DescriptionGeneratorInterface):
    """Service for generating AI-powered descriptions of code entities"""
    
    def __init__(self, provider: AIProviderInterface, logger: Optional[LoggerInterface] = None):
        """Initialize description service"""
        self.provider = provider
        self.logger = logger
        self._business_context = None
        self._load_business_context()
    
    def generate_entity_description(self, entity: Dict[str, Any], 
                                   context: Optional[str] = None) -> str:
        """Generate description for a code entity"""
        try:
            entity_type = entity.get('type', 'unknown')
            entity_name = entity.get('name', 'unnamed')
            
            if self.logger:
                self.logger.log_debug(f"Generating description for {entity_type}: {entity_name}")
            
            # Use business context if available
            business_context = context or self._business_context
            
            # Create comprehensive prompt
            prompt = self._create_entity_prompt(entity, business_context)
            
            # Generate description using AI provider
            description = self.provider.generate_description(entity, business_context)
            
            # Post-process and clean up description
            cleaned_description = self._clean_description(description)
            
            if self.logger:
                self.logger.log_debug(f"Generated description for {entity_name}: {len(cleaned_description)} chars")
            
            return cleaned_description
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to generate entity description: {e}")
            return f"Error generating description for {entity.get('name', 'unknown')}: {e}"
    
    def generate_file_summary(self, file_path: str, content: str,
                            entities: List[Dict[str, Any]]) -> str:
        """Generate summary for an entire file"""
        try:
            if self.logger:
                self.logger.log_debug(f"Generating file summary for {file_path}")
            
            # Create file summary prompt
            prompt = self._create_file_summary_prompt(file_path, content, entities)
            
            # Generate summary
            response = self.provider.generate_text(prompt)
            
            if response.success:
                summary = self._clean_description(response.content)
                
                if self.logger:
                    self.logger.log_debug(f"Generated file summary for {file_path}: {len(summary)} chars")
                
                return summary
            else:
                return f"Error generating file summary: {response.error}"
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to generate file summary: {e}")
            return f"Error generating file summary for {file_path}: {e}"
    
    def generate_relationship_description(self, relationship: Dict[str, Any]) -> str:
        """Generate description for a relationship"""
        try:
            source_entity = relationship.get('source_entity', 'unknown')
            target_entity = relationship.get('target_entity', 'unknown')
            relationship_type = relationship.get('relationship_type', 'RELATED_TO')
            
            # Create relationship-specific prompt
            prompt = self._create_relationship_prompt(relationship)
            
            # Generate description
            response = self.provider.generate_text(prompt)
            
            if response.success:
                description = self._clean_description(response.content)
                return description
            else:
                return f"{source_entity} {relationship_type.lower().replace('_', ' ')} {target_entity}"
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to generate relationship description: {e}")
            return f"Relationship between {relationship.get('source_entity', 'unknown')} and {relationship.get('target_entity', 'unknown')}"
    
    def _load_business_context(self) -> None:
        """Load business context from PRIMER.md file"""
        try:
            # Look for PRIMER.md in current directory or project root
            primer_paths = [
                Path.cwd() / "PRIMER.md",
                Path.cwd().parent / "PRIMER.md",
                Path(__file__).parent.parent.parent / "PRIMER.md"
            ]
            
            for primer_path in primer_paths:
                if primer_path.exists():
                    with open(primer_path, 'r', encoding='utf-8') as f:
                        self._business_context = f.read()
                    
                    if self.logger:
                        self.logger.log_info(f"Loaded business context from {primer_path}")
                    break
            
            if not self._business_context and self.logger:
                self.logger.log_info("No PRIMER.md found, proceeding without business context")
                
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Failed to load business context: {e}")
    
    def _create_entity_prompt(self, entity: Dict[str, Any], business_context: Optional[str]) -> str:
        """Create prompt for entity description generation"""
        entity_type = entity.get('type', 'unknown').lower()
        entity_name = entity.get('name', 'unnamed')
        properties = entity.get('properties', {})
        
        prompt = f"Generate a concise, technical description for this {entity_type}:\n\n"
        prompt += f"Name: {entity_name}\n"
        
        # Add entity-specific information
        if entity_type == 'function':
            prompt += f"Purpose: Analyze this function and describe its primary purpose and functionality.\n"
            if 'parameters' in properties:
                prompt += f"Parameters: {properties['parameters']}\n"
            if 'return_type' in properties:
                prompt += f"Returns: {properties['return_type']}\n"
        
        elif entity_type == 'class':
            prompt += f"Purpose: Analyze this class and describe its role and responsibilities.\n"
            if 'methods' in properties:
                prompt += f"Key Methods: {properties['methods']}\n"
            if 'inheritance' in properties:
                prompt += f"Inheritance: {properties['inheritance']}\n"
        
        elif entity_type == 'variable':
            prompt += f"Purpose: Describe the purpose and usage of this variable.\n"
            if 'type' in properties:
                prompt += f"Type: {properties['type']}\n"
        
        # Add file context
        if 'file_path' in entity:
            prompt += f"File: {entity['file_path']}\n"
        
        # Add business context if available
        if business_context:
            prompt += f"\nBusiness Context:\n{business_context[:500]}...\n"
        
        prompt += f"\nGenerate a clear, concise description (1-2 sentences) that explains the {entity_type}'s purpose and role in the codebase."
        
        if business_context:
            prompt += " Consider the business context when explaining its significance."
        
        return prompt
    
    def _create_file_summary_prompt(self, file_path: str, content: str, entities: List[Dict[str, Any]]) -> str:
        """Create prompt for file summary generation"""
        filename = Path(file_path).name
        
        prompt = f"Generate a concise summary for this code file:\n\n"
        prompt += f"File: {filename}\n"
        prompt += f"Size: {len(content)} characters\n"
        
        # Add entity summary
        if entities:
            entity_summary = {}
            for entity in entities:
                entity_type = entity.get('type', 'unknown')
                entity_summary[entity_type] = entity_summary.get(entity_type, 0) + 1
            
            prompt += f"Contains: "
            prompt += ", ".join([f"{count} {etype}{'s' if count > 1 else ''}" 
                               for etype, count in entity_summary.items()])
            prompt += "\n"
        
        # Add code sample (first 500 chars)
        prompt += f"\nCode Preview:\n```python\n{content[:500]}...\n```\n"
        
        # Add business context if available
        if self._business_context:
            prompt += f"\nBusiness Context:\n{self._business_context[:300]}...\n"
        
        prompt += f"\nGenerate a clear summary (2-3 sentences) explaining:"
        prompt += f"\n1. The file's primary purpose and functionality"
        prompt += f"\n2. Its role in the overall system architecture"
        if self._business_context:
            prompt += f"\n3. Its business significance"
        
        return prompt
    
    def _create_relationship_prompt(self, relationship: Dict[str, Any]) -> str:
        """Create prompt for relationship description generation"""
        source_entity = relationship.get('source_entity', 'unknown')
        target_entity = relationship.get('target_entity', 'unknown')
        relationship_type = relationship.get('relationship_type', 'RELATED_TO')
        confidence = relationship.get('confidence', 0.0)
        
        prompt = f"Generate a natural description for this code relationship:\n\n"
        prompt += f"Source: {source_entity}\n"
        prompt += f"Target: {target_entity}\n"
        prompt += f"Relationship: {relationship_type}\n"
        prompt += f"Confidence: {confidence:.2f}\n"
        
        if 'context' in relationship:
            prompt += f"Context: {relationship['context']}\n"
        
        prompt += f"\nGenerate a clear, natural language description of how {source_entity} {relationship_type.lower().replace('_', ' ')} {target_entity}."
        prompt += f" Make it understandable to developers reviewing the code architecture."
        
        return prompt
    
    def _clean_description(self, description: str) -> str:
        """Clean and format the generated description"""
        if not description:
            return "No description available"
        
        # Remove common AI response prefixes
        prefixes_to_remove = [
            "This function",
            "This class", 
            "This variable",
            "This method",
            "The function",
            "The class",
            "The variable", 
            "The method"
        ]
        
        cleaned = description.strip()
        
        # Remove quotes if the entire description is quoted
        if cleaned.startswith('"') and cleaned.endswith('"'):
            cleaned = cleaned[1:-1]
        
        # Capitalize first letter
        if cleaned and not cleaned[0].isupper():
            cleaned = cleaned[0].upper() + cleaned[1:]
        
        # Ensure it ends with proper punctuation
        if cleaned and not cleaned.endswith(('.', '!', '?')):
            cleaned += '.'
        
        # Limit length
        if len(cleaned) > 500:
            cleaned = cleaned[:497] + "..."
        
        return cleaned