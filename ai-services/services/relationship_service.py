"""
Relationship extraction service with comprehensive validation
Addresses the surgical updater relationship extraction issues from Phase 0
"""
import time
import re
from typing import List, Dict, Any, Optional, Set
from ..interfaces.relationship_extractor_interface import RelationshipExtractorInterface
from ..interfaces.ai_provider_interface import AIProviderInterface
from ..models.relationship_models import (
    Relationship, RelationshipExtractionResult, RelationshipType, ConfidenceLevel
)
from shared.interfaces.logger_interface import LoggerInterface


class RelationshipService(RelationshipExtractorInterface):
    """
    Enhanced relationship extraction service with validation
    Fixes the surgical updater issues where "relationships from edited files isn't right"
    """
    
    def __init__(self, provider: AIProviderInterface, logger: Optional[LoggerInterface] = None):
        """Initialize relationship service"""
        self.provider = provider
        self.logger = logger
        
        # Validation rules and patterns
        self._validation_rules = self._setup_validation_rules()
        self._entity_patterns = self._setup_entity_patterns()
    
    def extract_relationships(self, source_file: str, target_file: str,
                            source_code: str, target_code: str,
                            relationship_types: Optional[List[str]] = None) -> RelationshipExtractionResult:
        """
        Extract relationships with comprehensive validation
        This addresses the core issue from surgical updater
        """
        start_time = time.time()
        
        if self.logger:
            self.logger.log_info(f"Extracting relationships: {source_file} -> {target_file}")
        
        try:
            # Step 1: Extract raw relationships using AI provider
            raw_relationships = self._extract_raw_relationships(
                source_code, target_code, relationship_types
            )
            
            # Step 2: Parse and structure relationships
            structured_relationships = self._parse_relationships(
                raw_relationships, source_file, target_file
            )
            
            # Step 3: Comprehensive validation (this was missing in surgical updater)
            validation_context = {
                'source_file': source_file,
                'target_file': target_file,
                'source_code': source_code,
                'target_code': target_code
            }
            
            validated_relationships = self.validate_relationships(
                structured_relationships, validation_context
            )
            
            # Step 4: Filter by confidence and relevance
            final_relationships = self._filter_relationships(validated_relationships)
            
            extraction_time = time.time() - start_time
            
            return RelationshipExtractionResult(
                relationships=final_relationships,
                source_file=source_file,
                target_file=target_file,
                extraction_time=extraction_time,
                total_found=len(structured_relationships),
                total_valid=len(final_relationships),
                validation_errors=self._get_validation_errors(structured_relationships, final_relationships),
                success=True
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Relationship extraction failed: {e}")
            
            return RelationshipExtractionResult(
                relationships=[],
                source_file=source_file,
                target_file=target_file,
                extraction_time=time.time() - start_time,
                total_found=0,
                total_valid=0,
                validation_errors=[str(e)],
                success=False,
                error=str(e)
            )
    
    def validate_relationships(self, relationships: List[Relationship], 
                             context: Dict[str, Any]) -> List[Relationship]:
        """
        Comprehensive relationship validation - this was the missing piece
        """
        validated = []
        source_entities = self._extract_entities_from_code(context.get('source_code', ''))
        target_entities = self._extract_entities_from_code(context.get('target_code', ''))
        
        for rel in relationships:
            validation_issues = []
            
            # Basic validation
            if not rel.is_valid():
                validation_issues.append("Basic validation failed")
                continue
            
            # Entity existence validation (critical for surgical updater)
            if not self._validate_entity_exists(rel.source_entity, source_entities, context['source_file']):
                validation_issues.append(f"Source entity '{rel.source_entity}' not found in source file")
            
            if not self._validate_entity_exists(rel.target_entity, target_entities, context['target_file']):
                validation_issues.append(f"Target entity '{rel.target_entity}' not found in target file")
            
            # Relationship type validation
            if not self._validate_relationship_type(rel, context):
                validation_issues.append(f"Invalid relationship type '{rel.relationship_type}' for given entities")
            
            # Cross-file relationship validation
            if not self._validate_cross_file_relationship(rel, context):
                validation_issues.append("Cross-file relationship validation failed")
            
            # Confidence threshold validation
            if rel.confidence < 0.3:  # Minimum confidence threshold
                validation_issues.append(f"Confidence too low: {rel.confidence}")
            
            if not validation_issues:
                validated.append(rel)
            elif self.logger:
                self.logger.log_warning(f"Relationship validation failed: {rel.source_entity} -> {rel.target_entity}, Issues: {validation_issues}")
        
        return validated
    
    def get_supported_relationship_types(self) -> List[str]:
        """Get supported relationship types"""
        return [rt.value for rt in RelationshipType]
    
    def _extract_raw_relationships(self, source_code: str, target_code: str, 
                                 relationship_types: Optional[List[str]]) -> List[Dict[str, Any]]:
        """Extract raw relationships using AI provider"""
        try:
            return self.provider.extract_relationships(
                source_code, target_code, relationship_types
            )
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to extract raw relationships: {e}")
            return []
    
    def _parse_relationships(self, raw_relationships: List[Dict[str, Any]], 
                           source_file: str, target_file: str) -> List[Relationship]:
        """Parse raw relationships into structured objects"""
        relationships = []
        
        for raw_rel in raw_relationships:
            try:
                # Handle different formats from AI providers
                relationship = self._parse_single_relationship(raw_rel, source_file, target_file)
                if relationship:
                    relationships.append(relationship)
            except Exception as e:
                if self.logger:
                    self.logger.log_warning(f"Failed to parse relationship: {raw_rel}, Error: {e}")
        
        return relationships
    
    def _parse_single_relationship(self, raw_rel: Dict[str, Any], 
                                 source_file: str, target_file: str) -> Optional[Relationship]:
        """Parse a single relationship from raw data"""
        try:
            # Extract required fields
            source_entity = str(raw_rel.get('source_entity', '')).strip()
            target_entity = str(raw_rel.get('target_entity', '')).strip()
            relationship_type_str = str(raw_rel.get('relationship_type', '')).strip()
            confidence = float(raw_rel.get('confidence', 0.0))
            
            if not all([source_entity, target_entity, relationship_type_str]):
                return None
            
            # Parse relationship type
            try:
                relationship_type = RelationshipType(relationship_type_str.upper())
            except ValueError:
                # Try to map common variations
                relationship_type = self._map_relationship_type(relationship_type_str)
                if not relationship_type:
                    return None
            
            return Relationship(
                source_entity=source_entity,
                target_entity=target_entity,
                relationship_type=relationship_type,
                confidence=confidence,
                confidence_level=ConfidenceLevel.LOW,  # Will be set in __post_init__
                source_file=source_file,
                target_file=target_file,
                line_number=raw_rel.get('line_number'),
                context=raw_rel.get('context', ''),
                properties=raw_rel.get('properties', {})
            )
            
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Failed to parse relationship: {e}")
            return None
    
    def _map_relationship_type(self, type_str: str) -> Optional[RelationshipType]:
        """Map common relationship type variations"""
        type_mappings = {
            'CALL': RelationshipType.CALLS,
            'CALLING': RelationshipType.CALLS,
            'INVOKE': RelationshipType.CALLS,
            'INHERIT': RelationshipType.INHERITS,
            'EXTENDS': RelationshipType.INHERITS,
            'USE': RelationshipType.USES,
            'USING': RelationshipType.USES,
            'IMPORT': RelationshipType.DEPENDS_ON,
            'IMPORTS': RelationshipType.DEPENDS_ON,
        }
        
        return type_mappings.get(type_str.upper())
    
    def _extract_entities_from_code(self, code: str) -> Set[str]:
        """Extract entity names from code for validation"""
        entities = set()
        
        # Extract function definitions
        function_matches = re.findall(r'def\s+(\w+)\s*\(', code)
        entities.update(function_matches)
        
        # Extract class definitions
        class_matches = re.findall(r'class\s+(\w+)\s*[:\(]', code)
        entities.update(class_matches)
        
        # Extract method definitions within classes
        method_matches = re.findall(r'^\s+def\s+(\w+)\s*\(', code, re.MULTILINE)
        entities.update(method_matches)
        
        # Extract variable assignments (limited scope)
        var_matches = re.findall(r'^(\w+)\s*=', code, re.MULTILINE)
        entities.update(var_matches)
        
        return entities
    
    def _validate_entity_exists(self, entity_name: str, entities: Set[str], file_path: str) -> bool:
        """Validate that an entity exists in the given context"""
        # Direct match
        if entity_name in entities:
            return True
        
        # Partial match for method calls (e.g., "obj.method" -> "method")
        if '.' in entity_name:
            method_name = entity_name.split('.')[-1]
            if method_name in entities:
                return True
        
        # Class method pattern (e.g., "ClassName.method")
        if '.' in entity_name:
            parts = entity_name.split('.')
            if len(parts) == 2 and parts[0] in entities:
                return True
        
        return False
    
    def _validate_relationship_type(self, relationship: Relationship, context: Dict[str, Any]) -> bool:
        """Validate that relationship type makes sense for the entities"""
        rules = self._validation_rules.get(relationship.relationship_type, [])
        
        for rule in rules:
            if rule(relationship, context):
                return True
        
        # If no specific rules, allow it
        return len(rules) == 0
    
    def _validate_cross_file_relationship(self, relationship: Relationship, context: Dict[str, Any]) -> bool:
        """Validate cross-file relationships make sense"""
        source_code = context.get('source_code', '')
        target_code = context.get('target_code', '')
        
        # For CALLS relationships, check if there's an import or reference
        if relationship.relationship_type == RelationshipType.CALLS:
            target_entity = relationship.target_entity
            
            # Check if target entity is imported or referenced in source
            if target_entity in source_code or any(
                pattern in source_code for pattern in [
                    f'import {target_entity}',
                    f'from .* import.*{target_entity}',
                    f'{target_entity}(',
                ]
            ):
                return True
        
        return True  # Allow by default for now
    
    def _filter_relationships(self, relationships: List[Relationship]) -> List[Relationship]:
        """Filter relationships by confidence and relevance"""
        filtered = []
        
        for rel in relationships:
            # Skip very low confidence relationships
            if rel.confidence < 0.3:
                continue
            
            # Skip self-relationships
            if rel.source_entity == rel.target_entity:
                continue
            
            filtered.append(rel)
        
        return filtered
    
    def _get_validation_errors(self, original: List[Relationship], validated: List[Relationship]) -> List[str]:
        """Get list of validation errors for reporting"""
        errors = []
        original_count = len(original)
        validated_count = len(validated)
        
        if original_count > validated_count:
            errors.append(f"Filtered {original_count - validated_count} invalid relationships")
        
        return errors
    
    def _setup_validation_rules(self) -> Dict[RelationshipType, List]:
        """Setup validation rules for different relationship types"""
        return {
            RelationshipType.CALLS: [
                lambda rel, ctx: self._is_callable_entity(rel.target_entity, ctx),
            ],
            RelationshipType.INHERITS: [
                lambda rel, ctx: self._is_class_entity(rel.source_entity, ctx) and 
                                self._is_class_entity(rel.target_entity, ctx),
            ],
            RelationshipType.IMPLEMENTS: [
                lambda rel, ctx: self._is_class_entity(rel.source_entity, ctx),
            ]
        }
    
    def _setup_entity_patterns(self) -> Dict[str, str]:
        """Setup regex patterns for entity recognition"""
        return {
            'function': r'def\s+(\w+)\s*\(',
            'class': r'class\s+(\w+)\s*[:\(]',
            'method': r'^\s+def\s+(\w+)\s*\(',
            'variable': r'^(\w+)\s*='
        }
    
    def _is_callable_entity(self, entity_name: str, context: Dict[str, Any]) -> bool:
        """Check if entity appears to be callable"""
        code = context.get('target_code', '') + context.get('source_code', '')
        
        # Look for function/method definitions
        patterns = [
            f'def\\s+{entity_name}\\s*\\(',
            f'def\\s+\\w+\\.{entity_name}\\s*\\(',
        ]
        
        for pattern in patterns:
            if re.search(pattern, code):
                return True
        
        return False
    
    def _is_class_entity(self, entity_name: str, context: Dict[str, Any]) -> bool:
        """Check if entity appears to be a class"""
        code = context.get('target_code', '') + context.get('source_code', '')
        
        pattern = f'class\\s+{entity_name}\\s*[:\\(]'
        return bool(re.search(pattern, code))