#!/usr/bin/env python3
"""
AI-Powered Relationship Extraction System using Ollama
Analyzes code to extract rich relationships beyond basic CONTAINS/IMPORTS_FROM
"""

import json
import re
from typing import Dict, List, Any, Optional, Tuple
from dataclasses import dataclass
from pathlib import Path
from enum import Enum

from ollama_client import OllamaClient
from logger import logger
# Removed ai_evaluation_tracker dependency


class RelationshipType(Enum):
    """Types of relationships we want to extract"""
    CALLS = "CALLS"
    INHERITS = "INHERITS"  
    USES = "USES"
    IMPLEMENTS = "IMPLEMENTS"
    DEPENDS_ON = "DEPENDS_ON"
    OVERRIDES = "OVERRIDES"
    DECORATES = "DECORATES"
    INSTANTIATES = "INSTANTIATES"
    DEFINES = "DEFINES"
    CONFIGURES = "CONFIGURES"
    TRANSFORMS = "TRANSFORMS"
    VALIDATES = "VALIDATES"
    DATA_FLOW = "DATA_FLOW"
    STATE_MUTATION = "STATE_MUTATION"
    EVENT_HANDLING = "EVENT_HANDLING"


@dataclass
class RelationshipExtraction:
    """Result of relationship extraction"""
    source_file: str
    target_file: str
    source_entity: str
    target_entity: str
    relationship_type: RelationshipType
    confidence: float  # 0.0 to 1.0 confidence score
    relationship_strength: str  # "weak", "medium", "strong"
    line_number: Optional[int] = None
    context: Optional[str] = None


class PromptTemplate:
    """Template system for relationship extraction prompts"""
    
    def __init__(self, template_file: Optional[str] = None):
        self.templates = self._load_default_templates()
        if template_file:
            self._load_custom_templates(template_file)
    
    def _load_default_templates(self) -> Dict[str, str]:
        """Load default prompt templates"""
        return {
            "CALLS": """
You are a code analysis assistant. Your task is to analyze two code files and determine if there are function/method calls from the source file to functions/methods defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for function/method calls in the source code that reference functions/methods defined in the target code
2. SPECIFIC PATTERNS TO FIND:
   - Direct calls: target_function(), obj.target_method()
   - Imported calls: from target_module import target_function; target_function()
   - Module calls: import target_module; target_module.function()
   - Method chaining: obj.target_method().other_method()
   - Callback functions: callback=target_function, handler=target_method
   - Function passing: process(target_function), map(target_method, data)
   - Conditional calls: if condition: target_function()
3. EXAMINE THE IMPORTS section carefully - if source imports from target, look for usage of those imports
4. EXAMINE THE ENTITY SIGNATURES section - match function names between source and target
5. DO NOT include constructor calls (use INSTANTIATES for those)
6. DO NOT include inheritance (use INHERITS for those)
7. BE THOROUGH - even subtle or indirect function usage counts as CALLS

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "function_name_that_makes_call",
            "target_entity": "function_name_being_called",
            "line_number": 123,
            "context": "brief description of the call",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no CALLS relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",
            
            "INHERITS": """
You are a code analysis assistant. Analyze the code and determine if there are class inheritance relationships from the source file to classes defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for class definitions in the source code that inherit from classes defined in the target code
2. Consider syntax like: class ChildClass(ParentClass):
3. Consider imports like: from target_module import ParentClass; class ChildClass(ParentClass):

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "child_class_name",
            "target_entity": "parent_class_name",
            "line_number": 123,
            "context": "brief description of inheritance",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no INHERITS relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "USES": """
You are a code analysis assistant. Analyze the code and determine if there are USES relationships from the source file to classes/types defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for type annotations, variable declarations using classes/types from target file
2. SPECIFIC PATTERNS TO FIND:
   - Type annotations: def func(param: TargetClass) -> TargetType
   - Variable type hints: var: TargetClass = None
   - Generic types: List[TargetClass], Dict[str, TargetClass]
   - Union types: Union[TargetClass, str], Optional[TargetClass]
   - Class attribute types: class MyClass: attr: TargetClass
   - Exception types: except TargetException as e
   - isinstance checks: isinstance(obj, TargetClass)
   - Type checking: if type(obj) == TargetClass
3. EXAMINE THE IMPORTS section - if target types are imported, look for their usage as types
4. EXAMINE THE ENTITY SIGNATURES section - look for target types used in function signatures
5. Consider usage as types but not instantiation or inheritance
6. BE THOROUGH - any reference to target types/classes as type indicators counts

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "function_or_class_that_uses",
            "target_entity": "class_or_type_being_used",
            "line_number": 123,
            "context": "brief description of usage",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no USES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "INSTANTIATES": """
You are a code analysis assistant. Analyze the code and determine if there are INSTANTIATES relationships from the source file to classes defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for object creation using classes from the target file
2. Consider patterns like: obj = ClassName(), var = ClassName(args)
3. Consider factory patterns and constructor calls

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "function_or_method_that_instantiates",
            "target_entity": "class_being_instantiated",
            "line_number": 123,
            "context": "brief description of instantiation",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no INSTANTIATES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "DEPENDS_ON": """
You are a code analysis assistant. Analyze the code and determine if there are logical DEPENDS_ON relationships from the source file to components defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for logical dependencies where source file functionality depends on target file components
2. SPECIFIC PATTERNS TO FIND:
   - Configuration dependencies: source reads config values defined in target
   - Constant dependencies: source uses constants/enums from target
   - Data schema dependencies: source processes data formats defined in target
   - Event dependencies: source responds to events defined in target
   - Service dependencies: source requires services/utilities from target to function
   - Plugin dependencies: source extends interfaces defined in target
   - Validation dependencies: source validates against rules/schemas in target
   - Error handling: source handles exceptions defined in target
3. EXAMINE THE IMPORTS section - look for imported constants, configs, schemas
4. EXAMINE THE ENTITY SIGNATURES section - look for architectural patterns
5. LOOK FOR INDIRECT DEPENDENCIES - where source module couldn't function without target module
6. DO NOT include direct imports/calls (use CALLS/USES for those)
7. Focus on LOGICAL/ARCHITECTURAL dependencies, not just code references

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "component_that_depends",
            "target_entity": "component_being_depended_on",
            "line_number": 123,
            "context": "brief description of dependency",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no DEPENDS_ON relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "OVERRIDES": """
You are a code analysis assistant. Analyze the code and determine if there are method OVERRIDES relationships from the source file to methods defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for method definitions in source that override methods from parent classes in target
2. Consider methods with same name/signature in child classes
3. Look for @override decorators or similar patterns
4. Consider abstract method implementations
5. Must be actual method overriding in inheritance hierarchy

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "method_that_overrides",
            "target_entity": "method_being_overridden",
            "line_number": 123,
            "context": "brief description of override",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no OVERRIDES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "DECORATES": """
You are a code analysis assistant. Analyze the code and determine if there are DECORATES relationships from the source file to decorators defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for @decorator usage in source that references decorators defined in target
2. Consider function decorators, class decorators, property decorators
3. Look for patterns like @target_decorator or @module.decorator_name
4. Consider imported decorators being applied to functions/classes

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "function_or_class_being_decorated",
            "target_entity": "decorator_being_used",
            "line_number": 123,
            "context": "brief description of decoration",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no DECORATES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "IMPLEMENTS": """
You are a code analysis assistant. Analyze the code and determine if there are IMPLEMENTS relationships from the source file to interfaces/protocols defined in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for classes in source that implement interfaces/protocols/abstract classes from target
2. Consider typing.Protocol implementations, abc.ABC abstract class implementations
3. Look for duck typing patterns where source class implements target interface contract
4. Consider explicit interface declarations and implicit protocol adherence
5. DO NOT include simple inheritance (use INHERITS for that)

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "class_that_implements",
            "target_entity": "interface_or_protocol_being_implemented",
            "line_number": 123,
            "context": "brief description of implementation",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no IMPLEMENTS relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "DEFINES": """
You are a code analysis assistant. Analyze the code and determine if there are DEFINES relationships from the source file to definitions/schemas/structures in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for definitions in source that create/define structures, schemas, or specifications from target
2. SPECIFIC PATTERNS TO FIND:
   - Schema definitions: source defines data schemas using target structures
   - Configuration definitions: source defines config using target templates
   - Constant definitions: source defines constants using target enums/values
   - Type definitions: source defines new types based on target types
   - Rule definitions: source defines business rules using target frameworks
   - Template definitions: source creates templates using target patterns
   - API definitions: source defines endpoints using target specifications
3. EXAMINE THE IMPORTS section - look for imported definition frameworks
4. EXAMINE THE ENTITY SIGNATURES section - look for definition patterns
5. Focus on CREATION/DEFINITION relationships, not just usage
6. Look for builder patterns, factory patterns creating definitions

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_defines",
            "target_entity": "structure_being_defined_with",
            "line_number": 123,
            "context": "brief description of definition",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no DEFINES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "CONFIGURES": """
You are a code analysis assistant. Analyze the code and determine if there are CONFIGURES relationships from the source file to configuration systems in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for configuration setup/modification relationships from source to target configuration systems
2. SPECIFIC PATTERNS TO FIND:
   - Settings configuration: source configures settings defined in target
   - Environment configuration: source sets up env vars from target configs
   - Database configuration: source configures DB using target connection settings
   - Service configuration: source configures services using target configuration classes
   - Plugin configuration: source configures plugins using target config frameworks
   - Middleware configuration: source sets up middleware using target configurations
   - Authentication configuration: source configures auth using target auth systems
3. EXAMINE THE IMPORTS section - look for imported configuration modules
4. EXAMINE THE ENTITY SIGNATURES section - look for config setup patterns
5. Focus on SETUP/CONFIGURATION relationships, not just reading config values
6. Look for initialization patterns that configure target systems

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_configures",
            "target_entity": "system_being_configured",
            "line_number": 123,
            "context": "brief description of configuration",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no CONFIGURES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "TRANSFORMS": """
You are a code analysis assistant. Analyze the code and determine if there are TRANSFORMS relationships from the source file to data transformation utilities in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for data transformation relationships where source uses target transformation functions/classes
2. SPECIFIC PATTERNS TO FIND:
   - Data mapping: source transforms data using target mappers/converters
   - Format conversion: source converts formats using target transformation utilities
   - Data serialization: source serializes/deserializes using target serializers
   - Data parsing: source parses data using target parsers
   - Data filtering: source filters data using target filter functions
   - Data aggregation: source aggregates data using target aggregation functions
   - Data validation transforms: source transforms and validates using target validators
3. EXAMINE THE IMPORTS section - look for imported transformation utilities
4. EXAMINE THE ENTITY SIGNATURES section - look for transformation patterns
5. Focus on DATA TRANSFORMATION relationships, not just data access
6. Look for pipeline patterns where data flows through transformations

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_transforms",
            "target_entity": "transformer_being_used",
            "line_number": 123,
            "context": "brief description of transformation",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no TRANSFORMS relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "VALIDATES": """
You are a code analysis assistant. Analyze the code and determine if there are VALIDATES relationships from the source file to validation systems in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for validation relationships where source uses target validation functions/schemas
2. SPECIFIC PATTERNS TO FIND:
   - Data validation: source validates data using target validators
   - Schema validation: source validates against schemas defined in target
   - Input validation: source validates user input using target validation rules
   - Business rule validation: source validates business logic using target rules
   - Type validation: source validates types using target type checkers
   - Format validation: source validates formats using target format validators
   - Constraint validation: source checks constraints using target constraint validators
3. EXAMINE THE IMPORTS section - look for imported validation utilities
4. EXAMINE THE ENTITY SIGNATURES section - look for validation patterns
5. Focus on VALIDATION relationships, not just data processing
6. Look for error handling patterns related to validation failures

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_validates",
            "target_entity": "validator_being_used",
            "line_number": 123,
            "context": "brief description of validation",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no VALIDATES relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "DATA_FLOW": """
You are a code analysis assistant. Analyze the code and determine if there are DATA_FLOW relationships from the source file to data processing functions in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for data flow relationships where source passes data to target functions for processing
2. SPECIFIC PATTERNS TO FIND:
   - Data pipeline: source feeds data into target processing functions
   - Data streaming: source streams data to target consumers/processors
   - Data aggregation flow: source sends data to target aggregators/collectors
   - Processing chain: source passes data through target processing functions
   - Data transformation pipeline: source data flows through target transformers
   - ETL patterns: source extracts data, target loads/transforms it
   - Message passing: source sends data messages to target handlers
   - Data filtering flow: source data flows through target filters
3. EXAMINE THE IMPORTS section - look for data processing imports
4. EXAMINE THE ENTITY SIGNATURES section - look for data parameter passing
5. Focus on DATA MOVEMENT/FLOW patterns, not just function calls
6. Look for return value usage where target function results feed into source processing
7. Identify pipeline patterns where data flows from source to target

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_produces_data",
            "target_entity": "entity_that_processes_data",
            "line_number": 123,
            "context": "brief description of data flow",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no DATA_FLOW relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "STATE_MUTATION": """
You are a code analysis assistant. Analyze the code and determine if there are STATE_MUTATION relationships from the source file to state management functions in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for state mutation relationships where source modifies state through target functions
2. SPECIFIC PATTERNS TO FIND:
   - State updates: source calls target functions that modify application state
   - Database mutations: source triggers target functions that modify database state
   - Cache updates: source calls target functions that update cache state
   - Session state changes: source modifies session state through target functions
   - Global state mutations: source modifies global variables through target setters
   - Object state changes: source modifies object properties through target methods
   - Configuration state updates: source updates config state through target functions
   - Counter/accumulator updates: source increments/decrements state through target
3. EXAMINE THE IMPORTS section - look for state management imports
4. EXAMINE THE ENTITY SIGNATURES section - look for setter methods, update functions
5. Focus on STATE MODIFICATION patterns, not just data access
6. Look for mutation verbs: set, update, modify, change, increment, decrement, add, remove
7. Identify side effects where target functions change persistent state

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_triggers_mutation",
            "target_entity": "entity_that_mutates_state",
            "line_number": 123,
            "context": "brief description of state mutation",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no STATE_MUTATION relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
""",

            "EVENT_HANDLING": """
You are a code analysis assistant. Analyze the code and determine if there are EVENT_HANDLING relationships from the source file to event processing functions in the target file.

Source File: {source_file}
Target File: {target_file}

Source Code:
{source_code}

Target Code (for reference):
{target_code}

IMPORTANT: You must respond with ONLY valid JSON. Do not include any explanation, commentary, or additional text. Only JSON.

Instructions:
1. Look for event handling relationships where source triggers or handles events through target functions
2. SPECIFIC PATTERNS TO FIND:
   - Event emission: source emits/triggers events that target functions handle
   - Event subscription: source subscribes to events handled by target functions
   - Event callbacks: source registers target functions as event callbacks
   - Signal handling: source connects signals to target signal handlers
   - Message handling: source sends messages to target message handlers
   - Observer pattern: source notifies target observers of events
   - Publisher-subscriber: source publishes events that target subscribes to
   - Event dispatching: source dispatches events to target event handlers
   - Webhook handling: source triggers webhooks handled by target functions
3. EXAMINE THE IMPORTS section - look for event system imports
4. EXAMINE THE ENTITY SIGNATURES section - look for handler, callback, listener functions
5. Focus on EVENT-DRIVEN patterns, not just function calls
6. Look for event keywords: on_, handle_, listen_, emit_, trigger_, dispatch_, notify_
7. Identify async patterns where events are processed asynchronously by target

Response format (JSON ONLY):
{{
    "has_relationship": true/false,
    "relationships": [
        {{
            "source_entity": "entity_that_triggers_event",
            "target_entity": "entity_that_handles_event",
            "line_number": 123,
            "context": "brief description of event handling",
            "confidence": 0.95,
            "relationship_strength": "strong"
        }}
    ]
}}

Additional Instructions:
- confidence: Float from 0.0 to 1.0 indicating how certain you are this relationship exists
- relationship_strength: "weak" (indirect/conditional), "medium" (clear but simple), "strong" (direct/critical)

If no EVENT_HANDLING relationships exist, return: {{"has_relationship": false, "relationships": []}}

JSON RESPONSE:
"""
        }


class AIRelationshipExtractor:
    """AI-powered relationship extractor using Ollama"""
    
    def __init__(self, template_file: Optional[str] = None):
        self.ollama_client = OllamaClient()
        self.prompt_templates = PromptTemplate(template_file)
        self.session_logger = logger.create_session_logger("AIRelationshipExtractor")
        
        # Log initialization
        logger.logger.info("Initialized AI relationship extractor with Ollama")
    
    def extract_relationships(self, 
                            source_file: str, 
                            target_file: str,
                            source_code: str, 
                            target_code: str,
                            relationship_types: Optional[List[RelationshipType]] = None) -> List[RelationshipExtraction]:
        """Extract relationships between two code files"""
        
        if relationship_types is None:
            relationship_types = [RelationshipType.CALLS, RelationshipType.INHERITS, 
                                RelationshipType.USES, RelationshipType.INSTANTIATES,
                                RelationshipType.DEPENDS_ON, RelationshipType.OVERRIDES,
                                RelationshipType.DECORATES, RelationshipType.IMPLEMENTS,
                                RelationshipType.DEFINES, RelationshipType.CONFIGURES,
                                RelationshipType.TRANSFORMS, RelationshipType.VALIDATES,
                                RelationshipType.DATA_FLOW, RelationshipType.STATE_MUTATION,
                                RelationshipType.EVENT_HANDLING]
        
        self.session_logger.log_operation_start(
            "extract_relationships",
            {
                "source_file": Path(source_file).name,
                "target_file": Path(target_file).name,
                "relationship_types": [rt.value for rt in relationship_types],
                "source_code_length": len(source_code),
                "target_code_length": len(target_code)
            }
        )
        
        all_relationships = []
        
        # Extract each type of relationship
        for relationship_type in relationship_types:
            try:
                relationships = self._extract_single_relationship_type(
                    source_file, target_file, source_code, target_code, relationship_type
                )
                all_relationships.extend(relationships)
                
                # Log successful extraction
                if relationships:
                    logger.logger.info(f"Successfully extracted {len(relationships)} {relationship_type.value} relationships")
                
            except Exception as e:
                self.session_logger.log_error(e, {
                    "relationship_type": relationship_type.value,
                    "source_file": source_file,
                    "target_file": target_file
                })
                
                # Track extraction failure
                logger.logger.error(f"Failed to extract {relationship_type.value} relationships: {str(e)}")
        
        return all_relationships
    
    def _extract_single_relationship_type(self,
                                        source_file: str,
                                        target_file: str, 
                                        source_code: str,
                                        target_code: str,
                                        relationship_type: RelationshipType) -> List[RelationshipExtraction]:
        """Extract a single type of relationship"""
        
        # Get the appropriate prompt template
        template = self.prompt_templates.templates.get(relationship_type.value)
        if not template:
            raise ValueError(f"No template found for relationship type: {relationship_type.value}")
        
        # Fill in the template
        prompt = template.format(
            source_file=source_file,
            target_file=target_file,
            source_code=source_code,
            target_code=target_code
        )
        
        # Generate response using Ollama
        response = self.ollama_client.generate_response(
            prompt,
            max_tokens=1000, 
            temperature=0.1  # Low temperature for consistent binary decisions
        )
        
        if response.get("error"):
            raise Exception(f"Ollama API error: {response['error']}")
        
        # Parse the JSON response
        try:
            result = json.loads(response["response"])
            
            # Validate response format
            if not isinstance(result, dict) or "has_relationship" not in result:
                raise ValueError("Invalid response format from AI")
            
            relationships = []
            
            if result.get("has_relationship", False):
                for rel_data in result.get("relationships", []):
                    relationship = RelationshipExtraction(
                        source_file=source_file,
                        target_file=target_file,
                        source_entity=rel_data.get("source_entity", ""),
                        target_entity=rel_data.get("target_entity", ""),
                        relationship_type=relationship_type,
                        confidence=float(rel_data.get("confidence", 0.8)),  # Default to 0.8 if not provided
                        relationship_strength=rel_data.get("relationship_strength", "medium"),  # Default to medium
                        line_number=rel_data.get("line_number"),
                        context=rel_data.get("context", "")
                    )
                    relationships.append(relationship)
            
            return relationships
            
        except json.JSONDecodeError as e:
            # Try to extract JSON from response if it's embedded
            json_match = re.search(r'\{.*\}', response["response"], re.DOTALL)
            if json_match:
                try:
                    result = json.loads(json_match.group(0))
                    # Same processing as above...
                    relationships = []
                    if result.get("has_relationship", False):
                        for rel_data in result.get("relationships", []):
                            relationship = RelationshipExtraction(
                                source_file=source_file,
                                target_file=target_file,
                                source_entity=rel_data.get("source_entity", ""),
                                target_entity=rel_data.get("target_entity", ""),
                                relationship_type=relationship_type,
                                confidence=float(rel_data.get("confidence", 0.8)),  # Default to 0.8 if not provided
                                relationship_strength=rel_data.get("relationship_strength", "medium"),  # Default to medium
                                line_number=rel_data.get("line_number"),
                                context=rel_data.get("context", "")
                            )
                            relationships.append(relationship)
                    return relationships
                except json.JSONDecodeError:
                    pass
            
            # Track JSON parsing failure
            logger.logger.error(f"Failed to parse JSON from AI response: {str(e)}")
            
            raise ValueError(f"Failed to parse JSON response: {response['response'][:200]}...")
    
    def get_supported_relationship_types(self) -> List[RelationshipType]:
        """Get list of supported relationship types"""
        return [rt for rt in RelationshipType if rt.value in self.prompt_templates.templates]


# Convenience function for easy usage
def extract_code_relationships(source_file: str, target_file: str,
                             source_code: str, target_code: str,
                             template_file: Optional[str] = None) -> List[RelationshipExtraction]:
    """Extract relationships between two code files"""
    extractor = AIRelationshipExtractor(template_file)
    return extractor.extract_relationships(source_file, target_file, source_code, target_code)