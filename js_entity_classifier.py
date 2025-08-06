#!/usr/bin/env python3
"""
JavaScript/TypeScript Entity Classification System
Provides intelligent classification of JavaScript/TypeScript entities
Similar to entity_classifier.py but adapted for JS/TS specific patterns
"""

import re
from typing import Dict, List, Any, Optional, Set
from enum import Enum
from dataclasses import dataclass


class JSEntityCategory(Enum):
    """High-level JavaScript/TypeScript entity categories"""
    DATA = "data"
    BEHAVIOR = "behavior"  
    CONFIGURATION = "configuration"
    INTERFACE = "interface"
    UTILITY = "utility"
    ARCHITECTURAL = "architectural"
    COMPONENT = "component"  # React/Vue/Angular components
    MODULE = "module"       # ES6 modules/exports


class JSSpecializedType(Enum):
    """Specialized JavaScript/TypeScript entity types"""
    # Data-related
    INTERFACE = "interface"
    TYPE_ALIAS = "type_alias"
    ENUM = "enum"
    CLASS_COMPONENT = "class_component"
    FUNCTIONAL_COMPONENT = "functional_component"
    
    # Behavioral
    ASYNC_FUNCTION = "async_function"
    GENERATOR_FUNCTION = "generator_function"
    ARROW_FUNCTION = "arrow_function"
    CALLBACK_FUNCTION = "callback_function"
    PURE_FUNCTION = "pure_function"
    FACTORY_FUNCTION = "factory_function"
    VALIDATOR_FUNCTION = "validator_function"
    
    # Configuration
    CONFIG_OBJECT = "config_object" 
    CONSTANTS_MODULE = "constants_module"
    ENV_CONFIG = "env_config"
    
    # Interface/Abstract
    ABSTRACT_CLASS = "abstract_class"
    MIXIN_CLASS = "mixin_class"
    PROTOCOL_INTERFACE = "protocol_interface"
    
    # Utility
    HELPER_FUNCTION = "helper_function"
    UTILITY_CLASS = "utility_class"
    SINGLETON_CLASS = "singleton_class"
    STATIC_CLASS = "static_class"
    
    # Architectural
    CONTROLLER_CLASS = "controller_class"
    SERVICE_CLASS = "service_class"
    REPOSITORY_CLASS = "repository_class"
    MODEL_CLASS = "model_class"
    MANAGER_CLASS = "manager_class"
    HANDLER_CLASS = "handler_class"
    MIDDLEWARE_FUNCTION = "middleware_function"
    
    # Module/Import/Export
    DEFAULT_EXPORT = "default_export"
    NAMED_EXPORT = "named_export"
    NAMESPACE_IMPORT = "namespace_import"
    NAMED_IMPORT = "named_import"
    SIDE_EFFECT_IMPORT = "side_effect_import"
    
    # Framework-specific
    REACT_COMPONENT = "react_component"
    REACT_HOOK = "react_hook"
    VUE_COMPONENT = "vue_component"
    ANGULAR_COMPONENT = "angular_component"
    ANGULAR_SERVICE = "angular_service"
    ANGULAR_DIRECTIVE = "angular_directive"
    
    # Method types
    CONSTRUCTOR = "constructor"
    GETTER_METHOD = "getter_method"
    SETTER_METHOD = "setter_method"
    STATIC_METHOD = "static_method"
    PRIVATE_METHOD = "private_method"


@dataclass
class JSClassificationResult:
    """Result of JavaScript/TypeScript entity classification"""
    original_type: str
    specialized_type: JSSpecializedType
    category: JSEntityCategory
    confidence: float
    characteristics: List[str]
    metadata: Dict[str, Any]


class JSEntityClassifier:
    """Intelligent JavaScript/TypeScript entity classification system"""
    
    def __init__(self):
        self.react_indicators = {
            'jsx', 'tsx', 'component', 'react', 'usestate', 'useeffect', 'props'
        }
        
        self.vue_indicators = {
            'vue', 'template', 'script', 'style', 'props', 'data', 'methods'
        }
        
        self.angular_indicators = {
            'component', 'directive', 'service', 'injectable', 'input', 'output'
        }
        
        self.config_indicators = {
            'config', 'settings', 'configuration', 'env', 'environment', 'constants'
        }
        
        self.architectural_patterns = {
            'controller': 'controller_class',
            'service': 'service_class',
            'repository': 'repository_class', 
            'model': 'model_class',
            'manager': 'manager_class',
            'handler': 'handler_class',
            'middleware': 'middleware_function'
        }
        
        # Built-in JavaScript/TypeScript types to ignore
        self.builtin_types = {
            'string', 'number', 'boolean', 'object', 'function', 'undefined', 'null',
            'any', 'unknown', 'void', 'never', 'Array', 'Promise', 'Date', 'RegExp',
            'Map', 'Set', 'WeakMap', 'WeakSet', 'Error', 'JSON', 'Math', 'console'
        }
    
    def classify_entity(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify a JavaScript/TypeScript entity based on its characteristics"""
        entity_type = entity.get("type", "unknown")
        
        if entity_type == "function":
            return self._classify_function(entity, file_context)
        elif entity_type == "class":
            return self._classify_class(entity, file_context)
        elif entity_type == "interface":
            return self._classify_interface(entity, file_context)
        elif entity_type == "type":
            return self._classify_type_alias(entity, file_context)
        elif entity_type == "variable":
            return self._classify_variable(entity, file_context)
        elif entity_type == "import":
            return self._classify_import(entity, file_context)
        elif entity_type == "export":
            return self._classify_export(entity, file_context)
        else:
            return JSClassificationResult(
                original_type=entity_type,
                specialized_type=JSSpecializedType.HELPER_FUNCTION,
                category=JSEntityCategory.UTILITY,
                confidence=0.3,
                characteristics=["unknown_type"],
                metadata={}
            )
    
    def _classify_function(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify function entities"""
        name = entity.get("name", "")
        parameters = entity.get("parameters", [])
        return_type = entity.get("return_type", "")
        is_async = entity.get("is_async", False)
        decorators = entity.get("decorators", [])
        file_path = entity.get("file_path", "")
        
        characteristics = []
        metadata = {}
        
        # Check for async functions
        if is_async:
            return JSClassificationResult(
                original_type="function",
                specialized_type=JSSpecializedType.ASYNC_FUNCTION,
                category=JSEntityCategory.BEHAVIOR,
                confidence=1.0,
                characteristics=["async"],
                metadata={"async": True}
            )
        
        # Check for React components
        if self._is_react_component(name, parameters, return_type, file_path):
            characteristics.append("react_component")
            return JSClassificationResult(
                original_type="function",
                specialized_type=JSSpecializedType.REACT_COMPONENT,
                category=JSEntityCategory.COMPONENT,
                confidence=0.9,
                characteristics=characteristics,
                metadata={"framework": "React"}
            )
        
        # Check for React hooks
        if self._is_react_hook(name):
            characteristics.append("react_hook")
            return JSClassificationResult(
                original_type="function",
                specialized_type=JSSpecializedType.REACT_HOOK,
                category=JSEntityCategory.BEHAVIOR,
                confidence=0.9,
                characteristics=characteristics,
                metadata={"framework": "React", "hook_type": name}
            )
        
        # Check for middleware functions
        if self._is_middleware_function(name, parameters):
            characteristics.append("middleware")
            return JSClassificationResult(
                original_type="function",
                specialized_type=JSSpecializedType.MIDDLEWARE_FUNCTION,
                category=JSEntityCategory.ARCHITECTURAL,
                confidence=0.8,
                characteristics=characteristics,
                metadata={"middleware": True}
            )
        
        # Check for factory patterns
        if any(keyword in name.lower() for keyword in ["create", "make", "build", "factory"]):
            if return_type and return_type not in self.builtin_types:
                characteristics.append("factory_pattern")
                return JSClassificationResult(
                    original_type="function",
                    specialized_type=JSSpecializedType.FACTORY_FUNCTION,
                    category=JSEntityCategory.BEHAVIOR,
                    confidence=0.8,
                    characteristics=characteristics,
                    metadata={"return_type": return_type}
                )
        
        # Check for validator functions
        if any(keyword in name.lower() for keyword in ["validate", "check", "verify", "is", "has"]):
            if return_type == "boolean" or "boolean" in return_type.lower():
                characteristics.append("validator_pattern")
                return JSClassificationResult(
                    original_type="function",
                    specialized_type=JSSpecializedType.VALIDATOR_FUNCTION,
                    category=JSEntityCategory.UTILITY,
                    confidence=0.7,
                    characteristics=characteristics,
                    metadata={"validation": True}
                )
        
        # Check for callback functions
        if self._is_callback_function(name, parameters):
            characteristics.append("callback_pattern")
            return JSClassificationResult(
                original_type="function",
                specialized_type=JSSpecializedType.CALLBACK_FUNCTION,
                category=JSEntityCategory.BEHAVIOR,
                confidence=0.6,
                characteristics=characteristics,
                metadata={"callback": True}
            )
        
        # Default to helper function
        return JSClassificationResult(
            original_type="function",
            specialized_type=JSSpecializedType.HELPER_FUNCTION,
            category=JSEntityCategory.UTILITY,
            confidence=0.4,
            characteristics=characteristics,
            metadata={}
        )
    
    def _classify_class(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify class entities"""
        name = entity.get("name", "")
        base_classes = entity.get("base_classes", [])
        interfaces = entity.get("interfaces", [])
        decorators = entity.get("decorators", [])
        metadata_info = entity.get("metadata", {})
        file_path = entity.get("file_path", "")
        
        characteristics = []
        metadata = {}
        
        # Check for Angular components
        if any("@Component" in dec for dec in decorators):
            characteristics.append("angular_component")
            return JSClassificationResult(
                original_type="class",
                specialized_type=JSSpecializedType.ANGULAR_COMPONENT,
                category=JSEntityCategory.COMPONENT,
                confidence=1.0,
                characteristics=characteristics,
                metadata={"framework": "Angular"}
            )
        
        # Check for Angular services
        if any("@Injectable" in dec for dec in decorators):
            characteristics.append("angular_service")
            return JSClassificationResult(
                original_type="class",
                specialized_type=JSSpecializedType.ANGULAR_SERVICE,
                category=JSEntityCategory.ARCHITECTURAL,
                confidence=1.0,
                characteristics=characteristics,
                metadata={"framework": "Angular"}
            )
        
        # Check for React class components
        if any("Component" in base for base in base_classes) or any("PureComponent" in base for base in base_classes):
            characteristics.append("react_class_component")
            return JSClassificationResult(
                original_type="class",
                specialized_type=JSSpecializedType.CLASS_COMPONENT,
                category=JSEntityCategory.COMPONENT,
                confidence=1.0,
                characteristics=characteristics,
                metadata={"framework": "React"}
            )
        
        # Check for abstract classes
        if metadata_info.get("is_abstract", False):
            characteristics.append("abstract_class")
            return JSClassificationResult(
                original_type="class",
                specialized_type=JSSpecializedType.ABSTRACT_CLASS,
                category=JSEntityCategory.INTERFACE,
                confidence=1.0,
                characteristics=characteristics,
                metadata={"abstract": True}
            )
        
        # Check for architectural patterns by name
        name_lower = name.lower()
        for pattern, class_type in self.architectural_patterns.items():
            if pattern in name_lower:
                characteristics.append(f"architectural_{pattern}")
                return JSClassificationResult(
                    original_type="class",
                    specialized_type=JSSpecializedType(class_type),
                    category=JSEntityCategory.ARCHITECTURAL,
                    confidence=0.8,
                    characteristics=characteristics,
                    metadata={"pattern": pattern}
                )
        
        # Check for utility classes (mostly static methods)
        if self._is_utility_class(metadata_info):
            characteristics.append("utility_class")
            return JSClassificationResult(
                original_type="class",
                specialized_type=JSSpecializedType.UTILITY_CLASS,
                category=JSEntityCategory.UTILITY,
                confidence=0.7,
                characteristics=characteristics,
                metadata={"all_static": True}
            )
        
        # Default classification
        return JSClassificationResult(
            original_type="class",
            specialized_type=JSSpecializedType.UTILITY_CLASS,
            category=JSEntityCategory.UTILITY,
            confidence=0.3,
            characteristics=characteristics,
            metadata={}
        )
    
    def _classify_interface(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify TypeScript interface entities"""
        name = entity.get("name", "")
        interfaces = entity.get("interfaces", [])  # Extended interfaces
        metadata_info = entity.get("metadata", {})
        
        characteristics = ["typescript_interface"]
        
        # Check for React component interfaces
        if "props" in name.lower() or "state" in name.lower():
            characteristics.append("react_interface")
            return JSClassificationResult(
                original_type="interface",
                specialized_type=JSSpecializedType.INTERFACE,
                category=JSEntityCategory.COMPONENT,
                confidence=0.8,
                characteristics=characteristics,
                metadata={"framework": "React", "interface_type": "component"}
            )
        
        # Check for config interfaces
        if any(indicator in name.lower() for indicator in self.config_indicators):
            characteristics.append("config_interface")
            return JSClassificationResult(
                original_type="interface",
                specialized_type=JSSpecializedType.INTERFACE,
                category=JSEntityCategory.CONFIGURATION,
                confidence=0.7,
                characteristics=characteristics,
                metadata={"config": True}
            )
        
        return JSClassificationResult(
            original_type="interface",
            specialized_type=JSSpecializedType.INTERFACE,
            category=JSEntityCategory.INTERFACE,
            confidence=0.9,
            characteristics=characteristics,
            metadata={}
        )
    
    def _classify_type_alias(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify TypeScript type alias entities"""
        name = entity.get("name", "")
        metadata_info = entity.get("metadata", {})
        
        characteristics = ["typescript_type"]
        
        return JSClassificationResult(
            original_type="type",
            specialized_type=JSSpecializedType.TYPE_ALIAS,
            category=JSEntityCategory.INTERFACE,
            confidence=0.9,
            characteristics=characteristics,
            metadata=metadata_info
        )
    
    def _classify_variable(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify variable entities"""
        name = entity.get("name", "")
        metadata_info = entity.get("metadata", {})
        declaration_kind = metadata_info.get("declaration_kind", "var")
        
        characteristics = [f"variable_{declaration_kind}"]
        
        # Check for constants
        if declaration_kind == "const" or name.isupper():
            if any(indicator in name.lower() for indicator in self.config_indicators):
                characteristics.append("config_constant")
                return JSClassificationResult(
                    original_type="variable",
                    specialized_type=JSSpecializedType.CONFIG_OBJECT,
                    category=JSEntityCategory.CONFIGURATION,
                    confidence=0.7,
                    characteristics=characteristics,
                    metadata=metadata_info
                )
        
        return JSClassificationResult(
            original_type="variable",
            specialized_type=JSSpecializedType.HELPER_FUNCTION,  # Generic variable classification
            category=JSEntityCategory.DATA,
            confidence=0.5,
            characteristics=characteristics,
            metadata=metadata_info
        )
    
    def _classify_import(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify import entities"""
        name = entity.get("name", "")
        metadata_info = entity.get("metadata", {})
        import_type = metadata_info.get("import_type", "named")
        module = metadata_info.get("module", "")
        
        characteristics = [f"import_{import_type}"]
        
        if import_type == "default":
            specialized_type = JSSpecializedType.DEFAULT_EXPORT
        elif import_type == "namespace":
            specialized_type = JSSpecializedType.NAMESPACE_IMPORT
        elif import_type == "named":
            specialized_type = JSSpecializedType.NAMED_IMPORT
        else:
            specialized_type = JSSpecializedType.SIDE_EFFECT_IMPORT
        
        return JSClassificationResult(
            original_type="import",
            specialized_type=specialized_type,
            category=JSEntityCategory.MODULE,
            confidence=1.0,
            characteristics=characteristics,
            metadata=metadata_info
        )
    
    def _classify_export(self, entity: Dict[str, Any], file_context: Dict[str, Any]) -> JSClassificationResult:
        """Classify export entities"""
        name = entity.get("name", "")
        metadata_info = entity.get("metadata", {})
        export_type = metadata_info.get("export_type", "named")
        
        characteristics = [f"export_{export_type}"]
        
        if export_type == "default":
            specialized_type = JSSpecializedType.DEFAULT_EXPORT
        else:
            specialized_type = JSSpecializedType.NAMED_EXPORT
        
        return JSClassificationResult(
            original_type="export",
            specialized_type=specialized_type,
            category=JSEntityCategory.MODULE,
            confidence=1.0,
            characteristics=characteristics,
            metadata=metadata_info
        )
    
    def _is_react_component(self, name: str, parameters: List[Dict], return_type: str, file_path: str) -> bool:
        """Check if function is a React component"""
        # File extension check
        if file_path.endswith(('.jsx', '.tsx')):
            # Function name starts with capital letter (React convention)
            if name and name[0].isupper():
                # Has props parameter or returns JSX
                has_props = any("props" in param.get("name", "").lower() for param in parameters)
                returns_jsx = "JSX" in return_type or "ReactElement" in return_type
                return has_props or returns_jsx
        return False
    
    def _is_react_hook(self, name: str) -> bool:
        """Check if function is a React hook"""
        return name.startswith("use") and len(name) > 3 and name[3].isupper()
    
    def _is_middleware_function(self, name: str, parameters: List[Dict]) -> bool:
        """Check if function is middleware (Express.js style)"""
        if len(parameters) >= 3:
            param_names = [param.get("name", "").lower() for param in parameters[:3]]
            # Common middleware signature: (req, res, next)
            return ("req" in param_names[0] and "res" in param_names[1] and 
                   "next" in param_names[2])
        return False
    
    def _is_callback_function(self, name: str, parameters: List[Dict]) -> bool:
        """Check if function appears to be a callback"""
        callback_indicators = ["callback", "cb", "handler", "listener", "on"]
        return any(indicator in name.lower() for indicator in callback_indicators)
    
    def _is_utility_class(self, metadata: Dict[str, Any]) -> bool:
        """Check if class is a utility class (mostly static methods)"""
        method_count = metadata.get("method_count", 0)
        if method_count == 0:
            return False
        
        # This would need to be enhanced to check actual static method count
        # For now, use naming conventions
        return False


def classify_js_entities(entities: List[Dict[str, Any]], file_context: Dict[str, Any]) -> List[Dict[str, Any]]:
    """Classify a list of JavaScript/TypeScript entities and return enhanced entity data"""
    classifier = JSEntityClassifier()
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