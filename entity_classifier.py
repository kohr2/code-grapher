#!/usr/bin/env python3
"""
Entity Classification System
Provides intelligent classification of code entities beyond basic AST types
"""

import re
from typing import Dict, List, Any, Optional, Set
from enum import Enum
from dataclasses import dataclass


class EntityCategory(Enum):
    """High-level entity categories"""
    DATA = "data"
    BEHAVIOR = "behavior"
    CONFIGURATION = "configuration"
    INTERFACE = "interface"
    UTILITY = "utility"
    ARCHITECTURAL = "architectural"


class SpecializedType(Enum):
    """Specialized entity types"""
    # Data-related
    DATA_CLASS = "data_class"
    ENUM_CLASS = "enum_class"
    PYDANTIC_MODEL = "pydantic_model"
    DATACLASS = "dataclass"
    NAMED_TUPLE = "named_tuple"
    
    # Behavioral
    PURE_FUNCTION = "pure_function"
    FACTORY_FUNCTION = "factory_function"
    DECORATOR_FUNCTION = "decorator_function"
    CALLBACK_FUNCTION = "callback_function"
    VALIDATOR_FUNCTION = "validator_function"
    
    # Configuration
    CONFIG_CLASS = "config_class"
    SETTINGS_CLASS = "settings_class"
    CONSTANTS_MODULE = "constants_module"
    
    # Interface/Abstract
    ABSTRACT_CLASS = "abstract_class"
    PROTOCOL_CLASS = "protocol_class"
    INTERFACE_CLASS = "interface_class"
    MIXIN_CLASS = "mixin_class"
    
    # Utility
    HELPER_FUNCTION = "helper_function"
    UTILITY_CLASS = "utility_class"
    SINGLETON_CLASS = "singleton_class"
    
    # Architectural
    CONTROLLER_CLASS = "controller_class"
    SERVICE_CLASS = "service_class"
    REPOSITORY_CLASS = "repository_class"
    MODEL_CLASS = "model_class"
    MANAGER_CLASS = "manager_class"
    HANDLER_CLASS = "handler_class"
    AGENT_CLASS = "agent_class"
    
    # Special functions
    PROPERTY_METHOD = "property_method"
    CLASSMETHOD = "classmethod"
    STATICMETHOD = "staticmethod"
    ASYNC_FUNCTION = "async_function"
    GENERATOR_FUNCTION = "generator_function"


@dataclass
class ClassificationResult:
    """Result of entity classification"""
    original_type: str
    specialized_type: SpecializedType
    category: EntityCategory
    confidence: float
    characteristics: List[str]
    metadata: Dict[str, Any]


class EntityClassifier:
    """Intelligent entity classification system"""
    
    def __init__(self):
        self.data_class_indicators = {
            'dataclass', 'pydantic', 'BaseModel', 'dataclasses.dataclass',
            'pydantic.BaseModel', 'NamedTuple', 'TypedDict'
        }
        
        self.config_indicators = {
            'config', 'settings', 'configuration', 'env', 'environment'
        }
        
        self.interface_indicators = {
            'ABC', 'Protocol', 'Interface', 'Abstract', 'Mixin'
        }
        
        self.architectural_patterns = {
            'controller': 'controller_class',
            'service': 'service_class', 
            'repository': 'repository_class',
            'model': 'model_class',
            'manager': 'manager_class',
            'handler': 'handler_class',
            'agent': 'agent_class'
        }
    
    def classify_entity(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> ClassificationResult:
        """Classify an entity based on its characteristics"""
        entity_type = entity.get("type", "unknown")
        
        if entity_type == "function":
            return self._classify_function(entity, file_context)
        elif entity_type == "class":
            return self._classify_class(entity, file_context)
        elif entity_type == "import":
            return self._classify_import(entity, file_context)
        else:
            return ClassificationResult(
                original_type=entity_type,
                specialized_type=SpecializedType.HELPER_FUNCTION,
                category=EntityCategory.UTILITY,
                confidence=0.3,
                characteristics=["unknown_type"],
                metadata={}
            )
    
    def _classify_function(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> ClassificationResult:
        """Classify function entities"""
        name = entity.get("name", "")
        decorators = entity.get("decorators", [])
        return_type = entity.get("return_type", "")
        parameters = entity.get("parameter_info", [])
        is_async = entity.get("is_async", False)
        
        characteristics = []
        metadata = {}
        
        # Check for async functions
        if is_async:
            return ClassificationResult(
                original_type="function",
                specialized_type=SpecializedType.ASYNC_FUNCTION,
                category=EntityCategory.BEHAVIOR,
                confidence=1.0,
                characteristics=["async"],
                metadata={"async": True}
            )
        
        # Check for special method types
        for decorator in decorators:
            if "property" in decorator.lower():
                return ClassificationResult(
                    original_type="function",
                    specialized_type=SpecializedType.PROPERTY_METHOD,
                    category=EntityCategory.INTERFACE,
                    confidence=1.0,
                    characteristics=["property"],
                    metadata={"decorator": decorator}
                )
            elif "classmethod" in decorator.lower():
                return ClassificationResult(
                    original_type="function",
                    specialized_type=SpecializedType.CLASSMETHOD,
                    category=EntityCategory.BEHAVIOR,
                    confidence=1.0,
                    characteristics=["classmethod"],
                    metadata={"decorator": decorator}
                )
            elif "staticmethod" in decorator.lower():
                return ClassificationResult(
                    original_type="function",
                    specialized_type=SpecializedType.STATICMETHOD,
                    category=EntityCategory.UTILITY,
                    confidence=1.0,
                    characteristics=["staticmethod"],
                    metadata={"decorator": decorator}
                )
        
        # Check for factory patterns
        if any(keyword in name.lower() for keyword in ["create", "make", "build", "factory"]):
            if return_type and not return_type.lower() in ["none", "bool", "str", "int"]:
                characteristics.append("factory_pattern")
                return ClassificationResult(
                    original_type="function",
                    specialized_type=SpecializedType.FACTORY_FUNCTION,
                    category=EntityCategory.BEHAVIOR,
                    confidence=0.8,
                    characteristics=characteristics,
                    metadata={"return_type": return_type}
                )
        
        # Check for validator functions
        if any(keyword in name.lower() for keyword in ["validate", "check", "verify", "is_valid"]):
            characteristics.append("validator_pattern")
            return ClassificationResult(
                original_type="function",
                specialized_type=SpecializedType.VALIDATOR_FUNCTION,
                category=EntityCategory.UTILITY,
                confidence=0.7,
                characteristics=characteristics,
                metadata={"validation": True}
            )
        
        # Check for decorator functions
        if len(parameters) >= 1 and any("func" in p.get("name", "").lower() for p in parameters):
            if return_type and "callable" in return_type.lower():
                characteristics.append("decorator_pattern")
                return ClassificationResult(
                    original_type="function",
                    specialized_type=SpecializedType.DECORATOR_FUNCTION,
                    category=EntityCategory.UTILITY,
                    confidence=0.8,
                    characteristics=characteristics,
                    metadata={"decorator_function": True}
                )
        
        # Check for pure functions (no side effects indicators)
        if self._is_likely_pure_function(entity):
            characteristics.append("pure_function")
            return ClassificationResult(
                original_type="function",
                specialized_type=SpecializedType.PURE_FUNCTION,
                category=EntityCategory.BEHAVIOR,
                confidence=0.6,
                characteristics=characteristics,
                metadata={"pure": True}
            )
        
        # Default to helper function
        return ClassificationResult(
            original_type="function",
            specialized_type=SpecializedType.HELPER_FUNCTION,
            category=EntityCategory.UTILITY,
            confidence=0.4,
            characteristics=characteristics,
            metadata={}
        )
    
    def _classify_class(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> ClassificationResult:
        """Classify class entities"""
        name = entity.get("name", "")
        bases = entity.get("base_classes", [])
        decorators = entity.get("decorators", [])
        methods_info = entity.get("methods_info", [])
        
        characteristics = []
        metadata = {}
        
        # Check for dataclass patterns
        for decorator in decorators:
            if any(indicator in decorator for indicator in self.data_class_indicators):
                characteristics.append("dataclass_decorator")
                return ClassificationResult(
                    original_type="class",
                    specialized_type=SpecializedType.DATACLASS,
                    category=EntityCategory.DATA,
                    confidence=1.0,
                    characteristics=characteristics,
                    metadata={"decorator": decorator}
                )
        
        # Check for Pydantic models
        for base in bases:
            if any(indicator in base for indicator in ['BaseModel', 'pydantic']):
                characteristics.append("pydantic_model")
                return ClassificationResult(
                    original_type="class",
                    specialized_type=SpecializedType.PYDANTIC_MODEL,
                    category=EntityCategory.DATA,
                    confidence=1.0,
                    characteristics=characteristics,
                    metadata={"base_class": base}
                )
        
        # Check for abstract classes
        for base in bases:
            if any(indicator in base for indicator in self.interface_indicators):
                characteristics.append("abstract_interface")
                return ClassificationResult(
                    original_type="class",
                    specialized_type=SpecializedType.ABSTRACT_CLASS,
                    category=EntityCategory.INTERFACE,
                    confidence=0.9,
                    characteristics=characteristics,
                    metadata={"base_class": base}
                )
        
        # Check for architectural patterns by name
        name_lower = name.lower()
        for pattern, class_type in self.architectural_patterns.items():
            if pattern in name_lower:
                characteristics.append(f"architectural_{pattern}")
                return ClassificationResult(
                    original_type="class",
                    specialized_type=SpecializedType(class_type),
                    category=EntityCategory.ARCHITECTURAL,
                    confidence=0.8,
                    characteristics=characteristics,
                    metadata={"pattern": pattern}
                )
        
        # Check for configuration classes
        if any(indicator in name_lower for indicator in self.config_indicators):
            characteristics.append("config_pattern")
            return ClassificationResult(
                original_type="class",
                specialized_type=SpecializedType.CONFIG_CLASS,
                category=EntityCategory.CONFIGURATION,
                confidence=0.7,
                characteristics=characteristics,
                metadata={"config": True}
            )
        
        # Check for singleton pattern
        if self._is_singleton_class(methods_info):
            characteristics.append("singleton_pattern")
            return ClassificationResult(
                original_type="class",
                specialized_type=SpecializedType.SINGLETON_CLASS,
                category=EntityCategory.ARCHITECTURAL,
                confidence=0.8,
                characteristics=characteristics,
                metadata={"singleton": True}
            )
        
        # Check for utility classes (all static methods)
        if self._is_utility_class(methods_info):
            characteristics.append("utility_class")
            return ClassificationResult(
                original_type="class",
                specialized_type=SpecializedType.UTILITY_CLASS,
                category=EntityCategory.UTILITY,
                confidence=0.7,
                characteristics=characteristics,
                metadata={"all_static": True}
            )
        
        # Default classification
        return ClassificationResult(
            original_type="class",
            specialized_type=SpecializedType.DATA_CLASS,
            category=EntityCategory.DATA,
            confidence=0.3,
            characteristics=characteristics,
            metadata={}
        )
    
    def _classify_import(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> ClassificationResult:
        """Classify import entities"""
        module = entity.get("module", "")
        name = entity.get("name", "")
        
        characteristics = []
        metadata = {"module": module}
        
        # Standard library detection could be added here
        # Framework detection (Django, Flask, FastAPI, etc.)
        # Configuration imports
        
        return ClassificationResult(
            original_type="import",
            specialized_type=SpecializedType.HELPER_FUNCTION,  # No specialized import types yet
            category=EntityCategory.UTILITY,
            confidence=0.5,
            characteristics=characteristics,
            metadata=metadata
        )
    
    def _is_likely_pure_function(self, entity: Dict[str, Any]) -> bool:
        """Determine if a function is likely pure (no side effects)"""
        name = entity.get("name", "").lower()
        
        # Functions that suggest pure behavior
        pure_indicators = ["get", "calculate", "compute", "format", "parse", "convert", "transform"]
        impure_indicators = ["set", "update", "delete", "create", "save", "load", "print", "log"]
        
        if any(indicator in name for indicator in pure_indicators):
            return True
        if any(indicator in name for indicator in impure_indicators):
            return False
        
        return False
    
    def _is_singleton_class(self, methods_info: List[Dict[str, Any]]) -> bool:
        """Check if class follows singleton pattern"""
        method_names = [method.get("name", "") for method in methods_info]
        return "instance" in method_names or "get_instance" in method_names
    
    def _is_utility_class(self, methods_info: List[Dict[str, Any]]) -> bool:
        """Check if class is a utility class (mostly static methods)"""
        if not methods_info:
            return False
        
        static_count = sum(1 for method in methods_info if method.get("is_staticmethod", False))
        total_methods = len(methods_info)
        
        return static_count >= total_methods * 0.8  # 80% or more static methods


def classify_entities(entities: List[Dict[str, Any]], file_context: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Classify a list of entities and return enhanced entity data"""
    classifier = EntityClassifier()
    enhanced_entities = []
    
    for entity in entities:
        classification = classifier.classify_entity(entity, file_context)
        
        # Add classification results to entity
        enhanced_entity = entity.copy()
        enhanced_entity.update({
            "specialized_type": classification.specialized_type.value,
            "category": classification.category.value,
            "classification_confidence": classification.confidence,
            "characteristics": classification.characteristics,
            "classification_metadata": classification.metadata
        })
        
        enhanced_entities.append(enhanced_entity)
    
    return enhanced_entities