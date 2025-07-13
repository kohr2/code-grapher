import ast
import time
from typing import Dict, Any, List, Optional, Set, Tuple
import re
import hashlib

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError


class EntityExtractionAgent(BaseAgent):
    """
    Agent for extracting code entities (classes, functions, variables, etc.) from parsed AST data
    Provides detailed entity analysis with scope tracking and type inference
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        # Agent-specific configuration - set before super().__init__() to allow validation
        agent_config = config.get("config", {})
        self.entity_types = agent_config.get("entityTypes", ["class", "function", "method", "variable", "constant", "module", "package", "import"])
        self.extraction_rules = agent_config.get("extractionRules", {})
        self.scope_analysis = agent_config.get("scopeAnalysis", {})
        
        super().__init__(agent_id, config, shared_state)
        
        # Extraction options
        self.include_private = self.extraction_rules.get("includePrivate", True)
        self.infer_types = self.extraction_rules.get("inferTypes", True)
        self.capture_decorators = self.extraction_rules.get("captureDecorators", True)
        self.extract_docstrings = self.extraction_rules.get("extractDocstrings", True)
        self.include_parameters = self.extraction_rules.get("includeParameters", True)
        
        # Scope tracking options
        self.track_local_scope = self.scope_analysis.get("trackLocalScope", True)
        self.track_global_scope = self.scope_analysis.get("trackGlobalScope", True)
        self.track_class_scope = self.scope_analysis.get("trackClassScope", True)
        
        # Entity ID generation
        self.entity_counter = 0
        
        self.session_logger.log_info(
            f"EntityExtractionAgent initialized with entity types: {self.entity_types}"
        )
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        if not self.entity_types:
            raise AgentConfigurationError("entityTypes cannot be empty")
        
        valid_types = {"class", "function", "method", "variable", "constant", "module", "package", "import"}
        invalid_types = set(self.entity_types) - valid_types
        if invalid_types:
            raise AgentConfigurationError(f"Invalid entity types: {invalid_types}")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return ["entity-identification", "scope-analysis", "type-inference", "relationship-detection"]
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """Execute entity extraction on parsed code data"""
        
        self.session_logger.log_operation_start(
            "EntityExtractionAgent.execute",
            {
                "input_type": type(input_data).__name__,
                "has_parsed_files": "parsed_files" in input_data,
                "entity_types": self.entity_types
            }
        )
        
        start_time = time.time()
        
        try:
            # Extract parsed files from input
            parsed_files = self._extract_parsed_files(input_data)
            
            if not parsed_files:
                raise AgentExecutionError("No parsed files found in input data")
            
            self.session_logger.log_decision(
                decision=f"Processing {len(parsed_files)} parsed files for entity extraction",
                reasoning="Found parsed file data from previous parsing step",
                alternatives=["Request file re-parsing", "Skip empty files"]
            )
            
            # Extract entities from all files
            all_entities = []
            file_statistics = {}
            errors = []
            
            for i, parsed_file in enumerate(parsed_files):
                try:
                    file_path = parsed_file.get("file_path", f"unknown_file_{i}")
                    
                    self.session_logger.log_info(f"Extracting entities from file {i+1}/{len(parsed_files)}: {file_path}")
                    
                    file_entities, file_stats = self._extract_entities_from_file(parsed_file)
                    
                    # Add file context to entities
                    for entity in file_entities:
                        entity["source_file"] = file_path
                        entity["file_index"] = i
                    
                    all_entities.extend(file_entities)
                    file_statistics[file_path] = file_stats
                    
                    # Update shared state with progress
                    self.update_shared_state(
                        "entity_extraction_progress",
                        {
                            "completed_files": i + 1,
                            "total_files": len(parsed_files),
                            "entities_found": len(all_entities),
                            "current_file": file_path
                        }
                    )
                    
                except Exception as e:
                    error_info = {
                        "file": parsed_file.get("file_path", f"file_{i}"),
                        "error": str(e),
                        "error_type": type(e).__name__
                    }
                    errors.append(error_info)
                    
                    self.session_logger.log_error(e, {
                        "file": parsed_file.get("file_path"),
                        "file_index": i
                    })
            
            # Post-process entities
            processed_entities = self._post_process_entities(all_entities)
            
            # Calculate comprehensive statistics
            statistics = self._calculate_extraction_statistics(processed_entities, file_statistics)
            
            duration = time.time() - start_time
            
            # Compile results
            result = {
                "entities": processed_entities,
                "total_entities": len(processed_entities),
                "entities_by_type": self._group_entities_by_type(processed_entities),
                "file_statistics": file_statistics,
                "global_statistics": statistics,
                "processing_time_seconds": duration,
                "files_processed": len(parsed_files),
                "extraction_errors": errors,
                "entity_relationships": self._detect_basic_relationships(processed_entities)
            }
            
            # Log extraction results
            self.session_logger.log_code_analysis(
                "entity_extraction",
                {
                    "total_entities": len(processed_entities),
                    "files_processed": len(parsed_files),
                    "errors": len(errors),
                    "duration": duration,
                    "entities_by_type": result["entities_by_type"]
                }
            )
            
            # Log agent-specific metrics
            self.log_agent_specific_metrics({
                "entities_extracted": len(processed_entities),
                "extraction_rate": len(processed_entities) / max(duration, 0.001),
                "error_rate": len(errors) / max(len(parsed_files), 1),
                "avg_entities_per_file": len(processed_entities) / max(len(parsed_files), 1)
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_error(e, {"input_data_keys": list(input_data.keys())})
            raise AgentExecutionError(f"Entity extraction failed: {str(e)}") from e
    
    def _extract_parsed_files(self, input_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Extract parsed file data from input"""
        parsed_files = []
        
        # Handle different input formats
        if "parsed_files" in input_data:
            parsed_files = input_data["parsed_files"]
        elif "parsed" in input_data and isinstance(input_data["parsed"], list):
            parsed_files = input_data["parsed"]
        elif "parsed" in input_data and "parsed_files" in input_data["parsed"]:
            parsed_files = input_data["parsed"]["parsed_files"]
        elif isinstance(input_data, list):
            # Direct list of parsed files
            parsed_files = input_data
        
        # Ensure we have a list
        if not isinstance(parsed_files, list):
            if isinstance(parsed_files, dict) and "ast" in parsed_files:
                # Single parsed file
                parsed_files = [parsed_files]
            else:
                parsed_files = []
        
        return parsed_files
    
    def _extract_entities_from_file(self, parsed_file: Dict[str, Any]) -> Tuple[List[Dict[str, Any]], Dict[str, Any]]:
        """Extract entities from a single parsed file"""
        file_path = parsed_file.get("file_path", "unknown")
        ast_data = parsed_file.get("ast", {})
        metadata = parsed_file.get("metadata", {})
        
        entities = []
        file_stats = {
            "total_entities": 0,
            "entities_by_type": {},
            "scopes_analyzed": 0,
            "private_entities": 0,
            "public_entities": 0
        }
        
        # Handle different AST formats
        if ast_data.get("type") == "SyntaxError":
            # Handle files with syntax errors
            self.session_logger.log_decision(
                decision="Skipping entity extraction for file with syntax errors",
                reasoning="AST contains syntax error, cannot reliably extract entities",
                alternatives=["Attempt partial extraction", "Use text-based extraction"]
            )
            return entities, file_stats
        
        # Extract module-level entity
        if "module" in self.entity_types:
            module_entity = self._create_module_entity(file_path, metadata, ast_data)
            entities.append(module_entity)
            file_stats["entities_by_type"]["module"] = 1
        
        # Extract imports
        if "import" in self.entity_types and "imports" in ast_data:
            import_entities = self._extract_import_entities(ast_data["imports"], file_path)
            entities.extend(import_entities)
            file_stats["entities_by_type"]["import"] = len(import_entities)
        
        # Extract classes
        if "class" in self.entity_types and "classes" in ast_data:
            class_entities = self._extract_class_entities(ast_data["classes"], file_path)
            entities.extend(class_entities)
            file_stats["entities_by_type"]["class"] = len(class_entities)
        
        # Extract functions
        if "function" in self.entity_types and "functions" in ast_data:
            function_entities = self._extract_function_entities(ast_data["functions"], file_path, scope="module")
            entities.extend(function_entities)
            file_stats["entities_by_type"]["function"] = len(function_entities)
        
        # Extract variables (if available)
        if "variable" in self.entity_types and "variables" in ast_data:
            variable_entities = self._extract_variable_entities(ast_data["variables"], file_path, scope="module")
            entities.extend(variable_entities)
            file_stats["entities_by_type"]["variable"] = len(variable_entities)
        
        # Update file statistics
        file_stats["total_entities"] = len(entities)
        file_stats["private_entities"] = len([e for e in entities if e.get("visibility") == "private"])
        file_stats["public_entities"] = len([e for e in entities if e.get("visibility") == "public"])
        
        return entities, file_stats
    
    def _create_module_entity(self, file_path: str, metadata: Dict[str, Any], ast_data: Dict[str, Any]) -> Dict[str, Any]:
        """Create entity representing the module/file itself"""
        module_name = self._extract_module_name(file_path)
        
        entity = {
            "id": self._generate_entity_id("module", module_name, file_path),
            "name": module_name,
            "type": "module",
            "scope": "global",
            "location": {
                "file": file_path,
                "line": 1,
                "column": 0
            },
            "attributes": {
                "file_path": file_path,
                "language": metadata.get("language", "unknown"),
                "size_bytes": metadata.get("fileSize", 0),
                "line_count": metadata.get("lineCount", 0),
                "has_classes": len(ast_data.get("classes", [])) > 0,
                "has_functions": len(ast_data.get("functions", [])) > 0,
                "import_count": len(ast_data.get("imports", []))
            },
            "visibility": "public",
            "docstring": self._extract_module_docstring(ast_data)
        }
        
        return entity
    
    def _extract_import_entities(self, imports: List[Dict[str, Any]], file_path: str) -> List[Dict[str, Any]]:
        """Extract import entities"""
        entities = []
        
        for import_info in imports:
            entity = {
                "id": self._generate_entity_id("import", import_info.get("name", ""), file_path),
                "name": import_info.get("name", "unknown"),
                "type": "import",
                "scope": "module",
                "location": {
                    "file": file_path,
                    "line": import_info.get("line", 0),
                    "column": 0
                },
                "attributes": {
                    "import_type": import_info.get("type", "import"),
                    "module": import_info.get("module"),
                    "alias": import_info.get("alias"),
                    "level": import_info.get("level", 0)
                },
                "visibility": "public"
            }
            
            entities.append(entity)
        
        return entities
    
    def _extract_class_entities(self, classes: List[Dict[str, Any]], file_path: str) -> List[Dict[str, Any]]:
        """Extract class entities and their methods"""
        entities = []
        
        for class_info in classes:
            class_name = class_info.get("name", "UnknownClass")
            
            # Create class entity
            class_entity = {
                "id": self._generate_entity_id("class", class_name, file_path),
                "name": class_name,
                "type": "class",
                "scope": "module",
                "location": {
                    "file": file_path,
                    "line": class_info.get("line", 0),
                    "column": class_info.get("column", 0)
                },
                "attributes": {
                    "bases": class_info.get("bases", []),
                    "decorators": class_info.get("decorators", []),
                    "method_count": len(class_info.get("methods", [])),
                    "is_abstract": self._is_abstract_class(class_info),
                    "inheritance_depth": len(class_info.get("bases", []))
                },
                "visibility": self._determine_visibility(class_name),
                "docstring": class_info.get("docstring")
            }
            
            entities.append(class_entity)
            
            # Extract methods if configured
            if "method" in self.entity_types:
                method_entities = self._extract_method_entities(
                    class_info.get("methods", []), 
                    file_path, 
                    class_name
                )
                entities.extend(method_entities)
        
        return entities
    
    def _extract_method_entities(self, methods: List[Dict[str, Any]], file_path: str, class_name: str) -> List[Dict[str, Any]]:
        """Extract method entities from a class"""
        entities = []
        
        for method_info in methods:
            method_name = method_info.get("name", "unknown_method")
            
            entity = {
                "id": self._generate_entity_id("method", f"{class_name}.{method_name}", file_path),
                "name": method_name,
                "type": "method",
                "scope": "class",
                "location": {
                    "file": file_path,
                    "line": method_info.get("line", 0),
                    "column": 0
                },
                "attributes": {
                    "class_name": class_name,
                    "parameters": method_info.get("args", []),
                    "decorators": method_info.get("decorators", []),
                    "is_static": "@staticmethod" in method_info.get("decorators", []),
                    "is_class_method": "@classmethod" in method_info.get("decorators", []),
                    "is_property": "@property" in method_info.get("decorators", []),
                    "is_constructor": method_name == "__init__",
                    "is_magic_method": method_name.startswith("__") and method_name.endswith("__"),
                    "parameter_count": len(method_info.get("args", []))
                },
                "visibility": self._determine_visibility(method_name),
                "docstring": method_info.get("docstring")
            }
            
            # Add type information if available
            if self.include_parameters and "typed_args" in method_info:
                entity["attributes"]["typed_parameters"] = method_info["typed_args"]
            
            entities.append(entity)
        
        return entities
    
    def _extract_function_entities(self, functions: List[Dict[str, Any]], file_path: str, scope: str = "module") -> List[Dict[str, Any]]:
        """Extract function entities"""
        entities = []
        
        for function_info in functions:
            function_name = function_info.get("name", "unknown_function")
            
            entity = {
                "id": self._generate_entity_id("function", function_name, file_path),
                "name": function_name,
                "type": "function",
                "scope": scope,
                "location": {
                    "file": file_path,
                    "line": function_info.get("line", 0),
                    "column": function_info.get("column", 0)
                },
                "attributes": {
                    "parameters": function_info.get("args", []),
                    "defaults_count": function_info.get("defaults", 0),
                    "decorators": function_info.get("decorators", []),
                    "return_type": function_info.get("returns"),
                    "parameter_count": len(function_info.get("args", [])),
                    "is_async": function_name.startswith("async "),
                    "is_generator": self._is_generator_function(function_info),
                    "complexity_score": self._estimate_complexity(function_info)
                },
                "visibility": self._determine_visibility(function_name),
                "docstring": function_info.get("docstring")
            }
            
            # Add type information if available
            if self.include_parameters and "typed_args" in function_info:
                entity["attributes"]["typed_parameters"] = function_info["typed_args"]
            
            entities.append(entity)
        
        return entities
    
    def _extract_variable_entities(self, variables: List[Dict[str, Any]], file_path: str, scope: str = "module") -> List[Dict[str, Any]]:
        """Extract variable entities"""
        entities = []
        
        for variable_info in variables:
            variable_name = variable_info.get("name", "unknown_variable")
            
            # Determine if it's a constant (heuristic: all uppercase)
            entity_type = "constant" if variable_name.isupper() and "constant" in self.entity_types else "variable"
            
            entity = {
                "id": self._generate_entity_id(entity_type, variable_name, file_path),
                "name": variable_name,
                "type": entity_type,
                "scope": scope,
                "location": {
                    "file": file_path,
                    "line": variable_info.get("line", 0),
                    "column": 0
                },
                "attributes": {
                    "assignment_type": variable_info.get("type", "assignment"),
                    "value_type": variable_info.get("value_type"),
                    "inferred_type": self._infer_variable_type(variable_info) if self.infer_types else None,
                    "is_constant": entity_type == "constant",
                    "has_literal_value": "value" in variable_info
                },
                "visibility": self._determine_visibility(variable_name)
            }
            
            # Include literal value if available and safe to include
            if "value" in variable_info and self._is_safe_to_include_value(variable_info["value"]):
                entity["attributes"]["literal_value"] = variable_info["value"]
            
            entities.append(entity)
        
        return entities
    
    def _post_process_entities(self, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Post-process extracted entities for consistency and enhancement"""
        processed_entities = []
        
        for entity in entities:
            # Filter by visibility if configured
            if not self.include_private and entity.get("visibility") == "private":
                continue
            
            # Add computed attributes
            entity["extraction_timestamp"] = time.time()
            entity["complexity_category"] = self._categorize_complexity(entity)
            entity["importance_score"] = self._calculate_importance_score(entity)
            
            # Normalize entity data
            entity = self._normalize_entity(entity)
            
            processed_entities.append(entity)
        
        return processed_entities
    
    def _detect_basic_relationships(self, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Detect basic relationships between entities"""
        relationships = []
        
        # Create lookup maps for efficient searching
        entities_by_name = {e["name"]: e for e in entities}
        classes = [e for e in entities if e["type"] == "class"]
        methods = [e for e in entities if e["type"] == "method"]
        
        # Class-method containment relationships
        for method in methods:
            class_name = method.get("attributes", {}).get("class_name")
            if class_name and class_name in entities_by_name:
                relationship = {
                    "id": f"contains_{method['id']}",
                    "source": entities_by_name[class_name]["id"],
                    "target": method["id"],
                    "type": "contains",
                    "metadata": {
                        "relationship_type": "containment",
                        "source_type": "class",
                        "target_type": "method"
                    },
                    "strength": 1.0,
                    "confidence": 1.0
                }
                relationships.append(relationship)
        
        # Inheritance relationships
        for class_entity in classes:
            bases = class_entity.get("attributes", {}).get("bases", [])
            for base_class in bases:
                if base_class in entities_by_name:
                    relationship = {
                        "id": f"inherits_{class_entity['id']}_{entities_by_name[base_class]['id']}",
                        "source": class_entity["id"],
                        "target": entities_by_name[base_class]["id"],
                        "type": "inherits",
                        "metadata": {
                            "relationship_type": "inheritance",
                            "source_type": "class",
                            "target_type": "class"
                        },
                        "strength": 0.9,
                        "confidence": 0.8
                    }
                    relationships.append(relationship)
        
        return relationships
    
    def _generate_entity_id(self, entity_type: str, name: str, file_path: str) -> str:
        """Generate unique entity ID"""
        # Create deterministic ID based on type, name, and file
        id_string = f"{entity_type}:{name}:{file_path}"
        hash_object = hashlib.md5(id_string.encode())
        return f"{entity_type}_{hash_object.hexdigest()[:8]}"
    
    def _extract_module_name(self, file_path: str) -> str:
        """Extract module name from file path"""
        from pathlib import Path
        return Path(file_path).stem
    
    def _extract_module_docstring(self, ast_data: Dict[str, Any]) -> Optional[str]:
        """Extract module-level docstring"""
        # Look for docstring in AST body
        body = ast_data.get("body", [])
        if body and isinstance(body, list) and len(body) > 0:
            first_node = body[0]
            if isinstance(first_node, dict) and first_node.get("type") == "Expr":
                # Check if it's a string literal (docstring)
                return first_node.get("docstring")
        return None
    
    def _determine_visibility(self, name: str) -> str:
        """Determine visibility (public/private) based on naming conventions"""
        if name.startswith("_") and not name.startswith("__"):
            return "protected"
        elif name.startswith("__"):
            return "private"
        else:
            return "public"
    
    def _is_abstract_class(self, class_info: Dict[str, Any]) -> bool:
        """Determine if a class is abstract"""
        decorators = class_info.get("decorators", [])
        return any("abstractmethod" in str(dec) or "ABC" in str(dec) for dec in decorators)
    
    def _is_generator_function(self, function_info: Dict[str, Any]) -> bool:
        """Determine if a function is a generator (heuristic)"""
        # This would require more detailed AST analysis in a real implementation
        # For now, use simple heuristics
        name = function_info.get("name", "")
        return "yield" in name.lower() or "generator" in name.lower()
    
    def _estimate_complexity(self, function_info: Dict[str, Any]) -> int:
        """Estimate function complexity (simplified)"""
        # Simple complexity estimation based on parameter count and name patterns
        param_count = len(function_info.get("args", []))
        decorator_count = len(function_info.get("decorators", []))
        
        complexity = param_count + decorator_count
        
        # Add complexity for certain patterns
        name = function_info.get("name", "")
        if "init" in name or "setup" in name:
            complexity += 2
        if "process" in name or "handle" in name:
            complexity += 3
        
        return min(complexity, 10)  # Cap at 10
    
    def _infer_variable_type(self, variable_info: Dict[str, Any]) -> Optional[str]:
        """Infer variable type from available information"""
        if not self.infer_types:
            return None
        
        value_type = variable_info.get("value_type")
        if value_type:
            # Map AST node types to Python types
            type_mapping = {
                "Constant": "literal",
                "Str": "str",
                "Num": "number",
                "List": "list",
                "Dict": "dict",
                "Set": "set",
                "Tuple": "tuple",
                "Call": "function_result"
            }
            return type_mapping.get(value_type, "unknown")
        
        # Use naming conventions for type inference
        name = variable_info.get("name", "")
        if name.endswith("_list") or "list" in name.lower():
            return "list"
        elif name.endswith("_dict") or "dict" in name.lower():
            return "dict"
        elif name.endswith("_count") or "count" in name.lower():
            return "int"
        elif name.endswith("_flag") or "is_" in name.lower():
            return "bool"
        
        return "unknown"
    
    def _is_safe_to_include_value(self, value: Any) -> bool:
        """Check if value is safe to include in entity data"""
        # Only include simple, non-sensitive values
        if isinstance(value, (str, int, float, bool)) and len(str(value)) < 100:
            # Check for potentially sensitive patterns
            sensitive_patterns = ["password", "secret", "key", "token", "api"]
            value_str = str(value).lower()
            return not any(pattern in value_str for pattern in sensitive_patterns)
        return False
    
    def _categorize_complexity(self, entity: Dict[str, Any]) -> str:
        """Categorize entity complexity"""
        entity_type = entity["type"]
        attributes = entity.get("attributes", {})
        
        if entity_type == "class":
            method_count = attributes.get("method_count", 0)
            if method_count == 0:
                return "simple"
            elif method_count <= 5:
                return "moderate"
            else:
                return "complex"
        
        elif entity_type in ["function", "method"]:
            complexity_score = attributes.get("complexity_score", 0)
            if complexity_score <= 2:
                return "simple"
            elif complexity_score <= 5:
                return "moderate"
            else:
                return "complex"
        
        else:
            return "simple"
    
    def _calculate_importance_score(self, entity: Dict[str, Any]) -> float:
        """Calculate importance score for entity (0-1)"""
        score = 0.5  # Base score
        
        entity_type = entity["type"]
        attributes = entity.get("attributes", {})
        
        # Type-based scoring
        type_scores = {
            "class": 0.8,
            "function": 0.7,
            "method": 0.6,
            "module": 0.9,
            "variable": 0.3,
            "constant": 0.4,
            "import": 0.2
        }
        score = type_scores.get(entity_type, 0.5)
        
        # Visibility bonus
        if entity.get("visibility") == "public":
            score += 0.1
        
        # Documentation bonus
        if entity.get("docstring"):
            score += 0.1
        
        # Complexity bonus/penalty
        complexity = entity.get("complexity_category", "simple")
        if complexity == "complex":
            score += 0.1
        elif complexity == "simple":
            score -= 0.05
        
        # Special patterns
        name = entity["name"]
        if name in ["__init__", "main", "run", "execute"]:
            score += 0.2
        elif name.startswith("test_"):
            score += 0.05
        
        return max(0.0, min(1.0, score))
    
    def _normalize_entity(self, entity: Dict[str, Any]) -> Dict[str, Any]:
        """Normalize entity data for consistency"""
        # Ensure all required fields are present
        if "attributes" not in entity:
            entity["attributes"] = {}
        
        if "location" not in entity:
            entity["location"] = {
                "file": entity.get("source_file", "unknown"),
                "line": 0,
                "column": 0
            }
        
        # Normalize visibility
        if "visibility" not in entity:
            entity["visibility"] = self._determine_visibility(entity["name"])
        
        # Ensure docstring is string or None
        if "docstring" in entity and not isinstance(entity["docstring"], (str, type(None))):
            entity["docstring"] = str(entity["docstring"])
        
        return entity
    
    def _group_entities_by_type(self, entities: List[Dict[str, Any]]) -> Dict[str, int]:
        """Group entities by type and return counts"""
        type_counts = {}
        for entity in entities:
            entity_type = entity["type"]
            type_counts[entity_type] = type_counts.get(entity_type, 0) + 1
        return type_counts
    
    def _calculate_extraction_statistics(self, entities: List[Dict[str, Any]], file_statistics: Dict[str, Any]) -> Dict[str, Any]:
        """Calculate comprehensive extraction statistics"""
        if not entities:
            return {}
        
        # Basic counts
        total_entities = len(entities)
        entities_by_type = self._group_entities_by_type(entities)
        
        # Visibility distribution
        visibility_counts = {}
        for entity in entities:
            visibility = entity.get("visibility", "unknown")
            visibility_counts[visibility] = visibility_counts.get(visibility, 0) + 1
        
        # Complexity distribution
        complexity_counts = {}
        for entity in entities:
            complexity = entity.get("complexity_category", "unknown")
            complexity_counts[complexity] = complexity_counts.get(complexity, 0) + 1
        
        # Average importance score
        importance_scores = [entity.get("importance_score", 0.5) for entity in entities]
        avg_importance = sum(importance_scores) / len(importance_scores)
        
        # Documentation coverage
        documented_entities = len([e for e in entities if e.get("docstring")])
        documentation_coverage = documented_entities / total_entities
        
        return {
            "total_entities": total_entities,
            "entities_by_type": entities_by_type,
            "visibility_distribution": visibility_counts,
            "complexity_distribution": complexity_counts,
            "average_importance_score": avg_importance,
            "documentation_coverage": documentation_coverage,
            "files_with_entities": len(file_statistics),
            "average_entities_per_file": total_entities / max(len(file_statistics), 1)
        }