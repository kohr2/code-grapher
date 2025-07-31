"""
Classification Service

Extracted from entity_classifier.py to provide entity classification as a service.
Phase 5 verticalization - dedicated service for code entity classification.
"""

from typing import Dict, List, Any, Optional
import asyncio

from shared.interfaces.logger_interface import LoggerInterface


class ClassificationService:
    """
    Service for classifying code entities into specialized types
    
    Extracted from entity_classifier.py with service interface compliance.
    """
    
    def __init__(self, logger: LoggerInterface):
        self.logger = logger
        
        # Classification rules and patterns
        self._classification_rules = self._initialize_classification_rules()
    
    def _initialize_classification_rules(self) -> Dict[str, Any]:
        """Initialize classification rules and patterns"""
        return {
            'function_patterns': {
                'api_endpoint': ['@app.route', '@api.route', '@blueprint.route'],
                'test_function': ['test_', 'Test', '@pytest', '@unittest'],
                'property_getter': ['@property'],
                'static_method': ['@staticmethod'],
                'class_method': ['@classmethod'],
                'async_function': ['async def'],
                'generator': ['yield'],
                'decorator': ['@'],
                'main_function': ['if __name__ == "__main__"'],
                'init_function': ['__init__', '__new__'],
                'magic_method': ['__', '__'],
                'private_method': ['_'],
                'utility_function': ['util', 'helper', 'common'],
                'validation_function': ['validate', 'check', 'verify'],
                'conversion_function': ['convert', 'transform', 'parse'],
                'handler_function': ['handle', 'process', 'execute'],
                'factory_function': ['create', 'build', 'make', 'factory'],
                'config_function': ['config', 'setup', 'configure'],
                'logging_function': ['log', 'debug', 'error', 'warn'],
                'database_function': ['db_', 'query', 'insert', 'update', 'delete'],
                'cache_function': ['cache', 'memoize'],
                'serialization_function': ['serialize', 'deserialize', 'json', 'pickle']
            },
            'class_patterns': {
                'model': ['Model', 'Entity', 'Schema'],
                'controller': ['Controller', 'Handler', 'Manager'],
                'service': ['Service', 'Provider', 'Client'],
                'repository': ['Repository', 'DAO', 'Store'],
                'factory': ['Factory', 'Builder', 'Creator'],
                'exception': ['Exception', 'Error'],
                'test_class': ['Test', 'Spec'],
                'config_class': ['Config', 'Settings'],
                'interface': ['Interface', 'Protocol', 'ABC'],
                'enum': ['Enum', 'Choice'],
                'decorator_class': ['decorator'],
                'context_manager': ['__enter__', '__exit__'],
                'iterator': ['__iter__', '__next__'],
                'serializer': ['Serializer', 'Converter'],
                'validator': ['Validator', 'Checker'],
                'middleware': ['Middleware', 'Filter'],
                'plugin': ['Plugin', 'Extension'],
                'singleton': ['Singleton'],
                'observer': ['Observer', 'Listener'],
                'command': ['Command', 'Action'],
                'strategy': ['Strategy', 'Algorithm']
            },
            'variable_patterns': {
                'constant': ['UPPER_CASE', 'ALL_CAPS'],
                'global_variable': ['global'],
                'class_variable': ['cls.', 'self.'],
                'instance_variable': ['self.'],
                'private_variable': ['_'],
                'protected_variable': ['__'],
                'configuration': ['config', 'settings', 'env'],
                'database_connection': ['db', 'connection', 'session'],
                'logger': ['logger', 'log'],
                'cache': ['cache', 'memo'],
                'lock': ['lock', 'mutex'],
                'queue': ['queue', 'buffer'],
                'pool': ['pool'],
                'registry': ['registry', 'catalog']
            }
        }
    
    async def classify_entities(self, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        Classify a list of code entities
        
        Args:
            entities: List of entities to classify
            
        Returns:
            List of entities with classification information added
        """
        self.logger.log_info(f"Classifying {len(entities)} entities")
        
        try:
            classified_entities = []
            
            for entity in entities:
                classified_entity = await self.classify_single_entity(entity)
                classified_entities.append(classified_entity)
            
            # Log classification statistics
            classification_stats = self._calculate_classification_stats(classified_entities)
            self.logger.log_info(f"Classification complete: {classification_stats}")
            
            return classified_entities
            
        except Exception as e:
            self.logger.log_error(f"Entity classification failed: {e}")
            # Return entities without classification as fallback
            return entities
    
    async def classify_single_entity(self, entity: Dict[str, Any]) -> Dict[str, Any]:
        """
        Classify a single code entity
        
        Args:
            entity: Entity to classify
            
        Returns:
            Entity with classification information added
        """
        try:
            # Create a copy to avoid modifying the original
            classified_entity = entity.copy()
            
            entity_type = entity.get('type', 'unknown')
            entity_name = entity.get('name', '')
            entity_content = entity.get('properties', {}).get('code_snippet', '')
            
            # Classify based on entity type
            if entity_type == 'function':
                classification = self._classify_function(entity_name, entity_content, entity.get('properties', {}))
            elif entity_type == 'class':
                classification = self._classify_class(entity_name, entity_content, entity.get('properties', {}))
            elif entity_type == 'variable':
                classification = self._classify_variable(entity_name, entity_content, entity.get('properties', {}))
            else:
                classification = {'specialized_type': entity_type, 'confidence': 0.5}
            
            # Add classification to entity
            classified_entity['classification'] = classification
            
            return classified_entity
            
        except Exception as e:
            self.logger.log_warning(f"Failed to classify entity {entity.get('name', 'unknown')}: {e}")
            
            # Return entity with basic classification
            entity_copy = entity.copy()
            entity_copy['classification'] = {
                'specialized_type': entity.get('type', 'unknown'),
                'confidence': 0.0,
                'error': str(e)
            }
            return entity_copy
    
    def _classify_function(self, name: str, content: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """Classify a function entity"""
        patterns = self._classification_rules['function_patterns']
        
        # Check for specific patterns in order of specificity
        for specialized_type, keywords in patterns.items():
            confidence = self._calculate_pattern_confidence(name, content, keywords)
            if confidence > 0.7:
                return {
                    'specialized_type': specialized_type,
                    'confidence': confidence,
                    'matched_patterns': [kw for kw in keywords if kw.lower() in (name + content).lower()]
                }
        
        # Default function classification
        return {
            'specialized_type': 'function',
            'confidence': 0.5,
            'matched_patterns': []
        }
    
    def _classify_class(self, name: str, content: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """Classify a class entity"""
        patterns = self._classification_rules['class_patterns']
        
        # Check for specific patterns
        for specialized_type, keywords in patterns.items():
            confidence = self._calculate_pattern_confidence(name, content, keywords)
            if confidence > 0.7:
                return {
                    'specialized_type': specialized_type,
                    'confidence': confidence,
                    'matched_patterns': [kw for kw in keywords if kw.lower() in (name + content).lower()]
                }
        
        # Check inheritance patterns
        inheritance_info = properties.get('inheritance', [])
        if inheritance_info:
            for base_class in inheritance_info:
                for specialized_type, keywords in patterns.items():
                    if any(kw.lower() in base_class.lower() for kw in keywords):
                        return {
                            'specialized_type': specialized_type,
                            'confidence': 0.8,
                            'matched_patterns': [base_class],
                            'inference_source': 'inheritance'
                        }
        
        # Default class classification
        return {
            'specialized_type': 'class',
            'confidence': 0.5,
            'matched_patterns': []
        }
    
    def _classify_variable(self, name: str, content: str, properties: Dict[str, Any]) -> Dict[str, Any]:
        """Classify a variable entity"""
        patterns = self._classification_rules['variable_patterns']
        
        # Check for specific patterns
        for specialized_type, keywords in patterns.items():
            confidence = self._calculate_pattern_confidence(name, content, keywords)
            if confidence > 0.7:
                return {
                    'specialized_type': specialized_type,
                    'confidence': confidence,
                    'matched_patterns': [kw for kw in keywords if kw.lower() in (name + content).lower()]
                }
        
        # Check naming conventions
        if name.isupper():
            return {
                'specialized_type': 'constant',
                'confidence': 0.9,
                'matched_patterns': ['UPPER_CASE'],
                'inference_source': 'naming_convention'
            }
        elif name.startswith('_'):
            return {
                'specialized_type': 'private_variable',
                'confidence': 0.8,
                'matched_patterns': ['_prefix'],
                'inference_source': 'naming_convention'
            }
        
        # Default variable classification
        return {
            'specialized_type': 'variable',
            'confidence': 0.5,
            'matched_patterns': []
        }
    
    def _calculate_pattern_confidence(self, name: str, content: str, keywords: List[str]) -> float:
        """Calculate confidence score for pattern matching"""
        try:
            combined_text = (name + " " + content).lower()
            matches = 0
            total_keywords = len(keywords)
            
            if total_keywords == 0:
                return 0.0
            
            for keyword in keywords:
                if keyword.lower() in combined_text:
                    matches += 1
            
            # Base confidence from keyword matches
            base_confidence = matches / total_keywords
            
            # Boost confidence for exact name matches
            for keyword in keywords:
                if keyword.lower() == name.lower():
                    base_confidence = min(1.0, base_confidence + 0.3)
                    break
            
            # Boost confidence for decorator matches (high specificity)
            if any(keyword.startswith('@') and keyword in content for keyword in keywords):
                base_confidence = min(1.0, base_confidence + 0.2)
            
            return base_confidence
            
        except Exception as e:
            self.logger.log_warning(f"Pattern confidence calculation failed: {e}")
            return 0.0
    
    def _calculate_classification_stats(self, classified_entities: List[Dict[str, Any]]) -> Dict[str, int]:
        """Calculate classification statistics"""
        stats = {}
        
        for entity in classified_entities:
            classification = entity.get('classification', {})
            specialized_type = classification.get('specialized_type', 'unknown')
            
            if specialized_type in stats:
                stats[specialized_type] += 1
            else:
                stats[specialized_type] = 1
        
        return stats
    
    async def get_classification_rules(self) -> Dict[str, Any]:
        """Get the current classification rules"""
        return self._classification_rules
    
    async def update_classification_rules(self, new_rules: Dict[str, Any]) -> None:
        """Update classification rules"""
        try:
            self._classification_rules.update(new_rules)
            self.logger.log_info("Classification rules updated")
        except Exception as e:
            self.logger.log_error(f"Failed to update classification rules: {e}")
            raise
    
    async def health_check(self) -> bool:
        """Check if the classification service is healthy"""
        try:
            # Test with a simple entity
            test_entity = {
                'name': 'test_function',
                'type': 'function',
                'properties': {'code_snippet': 'def test_function(): pass'}
            }
            
            classified = await self.classify_single_entity(test_entity)
            return 'classification' in classified
            
        except Exception as e:
            self.logger.log_error(f"Classification service health check failed: {e}")
            return False