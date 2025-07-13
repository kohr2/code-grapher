import ast
import time
from typing import Dict, Any, List, Optional, Set, Tuple
import re
import json
from collections import defaultdict, deque

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError


class RelationshipAnalysisAgent(BaseAgent):
    """
    Agent for analyzing relationships between code entities
    Provides dependency analysis, call graph generation, and pattern detection
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        # Agent-specific configuration - set before super().__init__() to allow validation
        agent_config = config.get("config", {})
        self.relationship_types = agent_config.get("relationshipTypes", [
            "imports", "calls", "inherits", "implements", 
            "uses", "references", "contains", "depends-on",
            "overrides", "instantiates", "throws", "returns"
        ])
        self.analysis_depth = agent_config.get("analysisDepth", 3)
        self.include_transitive = agent_config.get("includeTransitive", True)
        self.track_call_flow = agent_config.get("trackCallFlow", True)
        self.detect_patterns = agent_config.get("detectPatterns", {})
        
        super().__init__(agent_id, config, shared_state)
        
        # Internal state for relationship tracking
        self.call_graph = defaultdict(list)
        self.dependency_graph = defaultdict(set)
        self.inheritance_hierarchy = defaultdict(list)
        self.import_map = defaultdict(set)
        self.relationships = []
        self.relationship_counter = 0
        
        self.session_logger.log_info(
            f"RelationshipAnalysisAgent initialized with types: {self.relationship_types}"
        )
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        if not self.relationship_types:
            raise AgentConfigurationError("relationshipTypes cannot be empty")
        
        valid_types = {
            "imports", "calls", "inherits", "implements", 
            "uses", "references", "contains", "depends-on",
            "overrides", "instantiates", "throws", "returns"
        }
        invalid_types = set(self.relationship_types) - valid_types
        if invalid_types:
            raise AgentConfigurationError(f"Invalid relationship types: {invalid_types}")
        
        if self.analysis_depth < 1 or self.analysis_depth > 10:
            raise AgentConfigurationError("analysisDepth must be between 1 and 10")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return ["dependency-analysis", "call-graph-generation", "inheritance-mapping"]
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute relationship analysis on entities and AST data
        
        Expected input format:
        {
            "entities": [...],  # From EntityExtractionAgent
            "ast": {...}        # From CodeParserAgent
        }
        """
        start_time = time.time()
        
        self.session_logger.log_operation_start(
            "RelationshipAnalysis.execute",
            {
                "entities_count": len(input_data.get("entities", [])),
                "has_ast": "ast" in input_data,
                "relationship_types": self.relationship_types
            }
        )
        
        try:
            # Extract entities and AST data
            entities = input_data.get("entities", [])
            ast_data = input_data.get("ast", {})
            
            if not entities:
                raise AgentExecutionError("No entities provided for relationship analysis")
            
            # Initialize internal state
            self._reset_analysis_state()
            
            # Build entity lookup maps
            entity_map = self._build_entity_map(entities)
            
            # Analyze different types of relationships
            self._analyze_import_relationships(entities, ast_data, entity_map)
            self._analyze_call_relationships(entities, ast_data, entity_map)
            self._analyze_inheritance_relationships(entities, entity_map)
            self._analyze_usage_relationships(entities, ast_data, entity_map)
            self._analyze_containment_relationships(entities, entity_map)
            
            # Build transitive relationships if enabled
            if self.include_transitive:
                self._build_transitive_relationships()
            
            # Generate graph structures
            call_graph = self._generate_call_graph()
            dependency_graph = self._generate_dependency_graph()
            
            # Detect patterns if enabled
            patterns = []
            if self.detect_patterns:
                patterns = self._detect_design_patterns(entities, entity_map)
            
            # Calculate relationship statistics
            relationship_stats = self._calculate_relationship_statistics()
            
            duration = time.time() - start_time
            
            result = {
                "relationships": self.relationships,
                "callGraph": call_graph,
                "dependencyGraph": dependency_graph,
                "patterns": patterns,
                "statistics": relationship_stats,
                "metadata": {
                    "analysisDepth": self.analysis_depth,
                    "relationshipTypes": self.relationship_types,
                    "processingTime": duration,
                    "totalRelationships": len(self.relationships)
                }
            }
            
            self.session_logger.log_operation_end(
                "RelationshipAnalysis.execute",
                duration=duration,
                success=True,
                details={
                    "relationships_found": len(self.relationships),
                    "call_graph_nodes": len(call_graph),
                    "patterns_detected": len(patterns)
                }
            )
            
            # Log performance metrics
            self.log_agent_specific_metrics({
                "relationships_per_second": len(self.relationships) / duration if duration > 0 else 0,
                "entities_analyzed": len(entities),
                "analysis_depth": self.analysis_depth
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "RelationshipAnalysis.execute",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            raise AgentExecutionError(f"Relationship analysis failed: {str(e)}") from e
    
    def _reset_analysis_state(self) -> None:
        """Reset internal analysis state for new execution"""
        self.call_graph.clear()
        self.dependency_graph.clear()
        self.inheritance_hierarchy.clear()
        self.import_map.clear()
        self.relationships.clear()
        self.relationship_counter = 0
    
    def _build_entity_map(self, entities: List[Dict[str, Any]]) -> Dict[str, Dict[str, Any]]:
        """Build lookup maps for entities by name and type"""
        entity_map = {}
        
        for entity in entities:
            entity_id = entity.get("id")
            entity_name = entity.get("name")
            entity_type = entity.get("type")
            
            if entity_id:
                entity_map[entity_id] = entity
            if entity_name:
                key = f"{entity_type}:{entity_name}"
                entity_map[key] = entity
        
        self.session_logger.log_decision(
            decision=f"Built entity map with {len(entity_map)} entries",
            reasoning="Entity lookup map enables efficient relationship analysis",
            alternatives=["Linear search through entities", "Build specialized indices"]
        )
        
        return entity_map
    
    def _analyze_import_relationships(self, entities: List[Dict[str, Any]], ast_data: Dict[str, Any], entity_map: Dict[str, str]) -> None:
        """Analyze import and dependency relationships"""
        if "imports" not in self.relationship_types:
            return
        
        self.session_logger.log_operation_start("analyze_import_relationships", {})
        
        import_entities = [e for e in entities if e.get("type") == "import"]
        
        for import_entity in import_entities:
            source_file = import_entity.get("location", {}).get("file")
            imported_name = import_entity.get("name")
            
            if source_file and imported_name:
                # Find entities that might be using this import
                for entity in entities:
                    entity_file = entity.get("location", {}).get("file")
                    
                    # Same file entities can use the import
                    if entity_file == source_file and entity.get("id") != import_entity.get("id"):
                        relationship = self._create_relationship(
                            source=entity.get("id"),
                            target=import_entity.get("id"),
                            relationship_type="imports",
                            metadata={
                                "imported_name": imported_name,
                                "import_type": "module_import"
                            }
                        )
                        self.relationships.append(relationship)
                        self.import_map[entity.get("id")].add(imported_name)
        
        self.session_logger.log_operation_end("analyze_import_relationships", duration=0.1, success=True)
    
    def _analyze_call_relationships(self, entities: List[Dict[str, Any]], ast_data: Dict[str, Any], entity_map: Dict[str, str]) -> None:
        """Analyze function/method call relationships"""
        if "calls" not in self.relationship_types:
            return
        
        self.session_logger.log_operation_start("analyze_call_relationships", {})
        
        function_entities = [e for e in entities if e.get("type") in ["function", "method"]]
        
        for func_entity in function_entities:
            # Analyze function body for calls (simplified - would need proper AST analysis)
            func_name = func_entity.get("name")
            func_id = func_entity.get("id")
            
            # Look for potential function calls in other functions
            for other_func in function_entities:
                if other_func.get("id") == func_id:
                    continue
                
                other_name = other_func.get("name")
                
                # Simple heuristic: if function name appears in another function's scope
                # In production, would parse AST properly
                if self._functions_have_call_relationship(func_entity, other_func):
                    relationship = self._create_relationship(
                        source=other_func.get("id"),
                        target=func_id,
                        relationship_type="calls",
                        metadata={
                            "call_type": "function_call",
                            "confidence": 0.8
                        }
                    )
                    self.relationships.append(relationship)
                    self.call_graph[other_func.get("id")].append(func_id)
        
        self.session_logger.log_operation_end("analyze_call_relationships", duration=0.1, success=True)
    
    def _analyze_inheritance_relationships(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> None:
        """Analyze class inheritance relationships"""
        if "inherits" not in self.relationship_types:
            return
        
        self.session_logger.log_operation_start("analyze_inheritance_relationships", {})
        
        class_entities = [e for e in entities if e.get("type") == "class"]
        
        for class_entity in class_entities:
            class_attributes = class_entity.get("attributes", {})
            parent_classes = class_attributes.get("parent_classes", [])
            
            for parent_name in parent_classes:
                # Find parent class entity
                parent_key = f"class:{parent_name}"
                parent_entity = entity_map.get(parent_key)
                
                if parent_entity:
                    relationship = self._create_relationship(
                        source=class_entity.get("id"),
                        target=parent_entity.get("id"),
                        relationship_type="inherits",
                        metadata={
                            "inheritance_type": "class_inheritance",
                            "parent_class": parent_name
                        }
                    )
                    self.relationships.append(relationship)
                    self.inheritance_hierarchy[parent_entity.get("id")].append(class_entity.get("id"))
        
        self.session_logger.log_operation_end("analyze_inheritance_relationships", duration=0.1, success=True)
    
    def _analyze_usage_relationships(self, entities: List[Dict[str, Any]], ast_data: Dict[str, Any], entity_map: Dict[str, str]) -> None:
        """Analyze usage relationships between entities"""
        if "uses" not in self.relationship_types:
            return
        
        self.session_logger.log_operation_start("analyze_usage_relationships", {})
        
        # Analyze variable and attribute usage
        for entity in entities:
            entity_type = entity.get("type")
            
            if entity_type in ["function", "method"]:
                # Functions can use variables, classes, etc.
                self._analyze_function_usage(entity, entities, entity_map)
            elif entity_type == "class":
                # Classes can use other classes
                self._analyze_class_usage(entity, entities, entity_map)
        
        self.session_logger.log_operation_end("analyze_usage_relationships", duration=0.1, success=True)
    
    def _analyze_containment_relationships(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> None:
        """Analyze containment relationships (file contains class, class contains method, etc.)"""
        if "contains" not in self.relationship_types:
            return
        
        self.session_logger.log_operation_start("analyze_containment_relationships", {})
        
        # Group entities by file
        file_entities = defaultdict(list)
        for entity in entities:
            file_path = entity.get("location", {}).get("file")
            if file_path:
                file_entities[file_path].append(entity)
        
        # Analyze containment within each file
        for file_path, file_entity_list in file_entities.items():
            self._analyze_file_containment(file_entity_list, entity_map)
        
        self.session_logger.log_operation_end("analyze_containment_relationships", duration=0.1, success=True)
    
    def _functions_have_call_relationship(self, func1: Dict[str, Any], func2: Dict[str, Any]) -> bool:
        """Simplified heuristic to determine if func2 calls func1"""
        # In production, would analyze AST properly
        # For now, use simple name-based heuristic
        func1_name = func1.get("name", "")
        func2_scope = func2.get("scope", "")
        
        # If function names suggest a call relationship
        return len(func1_name) > 3 and func1_name.lower() in func2_scope.lower()
    
    def _analyze_function_usage(self, func_entity: Dict[str, Any], all_entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> None:
        """Analyze what entities a function uses"""
        func_id = func_entity.get("id")
        func_attributes = func_entity.get("attributes", {})
        
        # Check parameters for type usage
        parameters = func_attributes.get("parameters", [])
        for param in parameters:
            param_type = param.get("type")
            if param_type:
                # Find entities matching this type
                type_key = f"class:{param_type}"
                if type_key in entity_map:
                    relationship = self._create_relationship(
                        source=func_id,
                        target=entity_map[type_key].get("id"),
                        relationship_type="uses",
                        metadata={
                            "usage_type": "parameter_type",
                            "parameter_name": param.get("name")
                        }
                    )
                    self.relationships.append(relationship)
    
    def _analyze_class_usage(self, class_entity: Dict[str, Any], all_entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> None:
        """Analyze what entities a class uses"""
        class_id = class_entity.get("id")
        class_attributes = class_entity.get("attributes", {})
        
        # Check for composition relationships
        attributes = class_attributes.get("attributes", [])
        for attr in attributes:
            attr_type = attr.get("type")
            if attr_type:
                type_key = f"class:{attr_type}"
                if type_key in entity_map:
                    relationship = self._create_relationship(
                        source=class_id,
                        target=entity_map[type_key].get("id"),
                        relationship_type="uses",
                        metadata={
                            "usage_type": "composition",
                            "attribute_name": attr.get("name")
                        }
                    )
                    self.relationships.append(relationship)
    
    def _analyze_file_containment(self, entities_in_file: List[Dict[str, Any]], entity_map: Dict[str, str]) -> None:
        """Analyze containment relationships within a file"""
        # Sort entities by line number to determine containment
        sorted_entities = sorted(entities_in_file, key=lambda e: e.get("location", {}).get("line", 0))
        
        for i, entity in enumerate(sorted_entities):
            entity_type = entity.get("type")
            
            if entity_type == "class":
                # Find methods and attributes contained in this class
                self._find_class_members(entity, sorted_entities[i+1:])
    
    def _find_class_members(self, class_entity: Dict[str, Any], potential_members: List[Dict[str, Any]]) -> None:
        """Find methods and attributes that belong to a class"""
        class_id = class_entity.get("id")
        class_line = class_entity.get("location", {}).get("line", 0)
        
        for member in potential_members:
            member_type = member.get("type")
            member_line = member.get("location", {}).get("line", 0)
            
            # Simple heuristic: if member is after class and is method/attribute
            if member_type in ["method", "attribute"] and member_line > class_line:
                # Check if member belongs to this class (simplified)
                member_scope = member.get("scope", "")
                if class_entity.get("name", "") in member_scope:
                    relationship = self._create_relationship(
                        source=class_id,
                        target=member.get("id"),
                        relationship_type="contains",
                        metadata={
                            "containment_type": f"class_contains_{member_type}",
                            "member_name": member.get("name")
                        }
                    )
                    self.relationships.append(relationship)
    
    def _build_transitive_relationships(self) -> None:
        """Build transitive relationships up to analysis_depth"""
        if not self.include_transitive:
            return
        
        self.session_logger.log_operation_start("build_transitive_relationships", {
            "analysis_depth": self.analysis_depth
        })
        
        # Build transitive dependencies
        for depth in range(2, self.analysis_depth + 1):
            self._find_transitive_dependencies(depth)
        
        self.session_logger.log_operation_end("build_transitive_relationships", duration=0.1, success=True)
    
    def _find_transitive_dependencies(self, depth: int) -> None:
        """Find transitive dependencies at specified depth"""
        new_relationships = []
        
        for relationship in self.relationships:
            source = relationship["source"]
            target = relationship["target"]
            rel_type = relationship["type"]
            
            # Find relationships where target is source of another relationship
            for other_rel in self.relationships:
                if other_rel["source"] == target and other_rel["type"] == rel_type:
                    # Create transitive relationship
                    transitive_rel = self._create_relationship(
                        source=source,
                        target=other_rel["target"],
                        relationship_type=rel_type,
                        metadata={
                            "transitive": True,
                            "depth": depth,
                            "intermediate": target
                        }
                    )
                    new_relationships.append(transitive_rel)
        
        # Add new transitive relationships
        self.relationships.extend(new_relationships)
    
    def _generate_call_graph(self) -> Dict[str, Any]:
        """Generate call graph structure"""
        return {
            "nodes": list(self.call_graph.keys()),
            "edges": dict(self.call_graph),
            "metrics": {
                "total_nodes": len(self.call_graph),
                "total_edges": sum(len(targets) for targets in self.call_graph.values()),
                "max_fanout": max(len(targets) for targets in self.call_graph.values()) if self.call_graph else 0
            }
        }
    
    def _generate_dependency_graph(self) -> Dict[str, Any]:
        """Generate dependency graph structure"""
        return {
            "nodes": list(self.dependency_graph.keys()),
            "edges": {k: list(v) for k, v in self.dependency_graph.items()},
            "metrics": {
                "total_nodes": len(self.dependency_graph),
                "total_edges": sum(len(deps) for deps in self.dependency_graph.values()),
                "strongly_connected_components": self._find_strongly_connected_components()
            }
        }
    
    def _find_strongly_connected_components(self) -> List[List[str]]:
        """Find strongly connected components in dependency graph (simplified)"""
        # Simplified implementation - in production would use Tarjan's algorithm
        return []
    
    def _detect_design_patterns(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> List[Dict[str, Any]]:
        """Detect common design patterns in the code"""
        patterns = []
        
        if self.detect_patterns.get("singletonPattern", False):
            patterns.extend(self._detect_singleton_pattern(entities, entity_map))
        
        if self.detect_patterns.get("factoryPattern", False):
            patterns.extend(self._detect_factory_pattern(entities, entity_map))
        
        if self.detect_patterns.get("observerPattern", False):
            patterns.extend(self._detect_observer_pattern(entities, entity_map))
        
        return patterns
    
    def _detect_singleton_pattern(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> List[Dict[str, Any]]:
        """Detect singleton pattern in classes"""
        patterns = []
        
        class_entities = [e for e in entities if e.get("type") == "class"]
        
        for class_entity in class_entities:
            class_name = class_entity.get("name", "")
            # Simple heuristic: look for classes with "Singleton" in name or common singleton methods
            if "singleton" in class_name.lower() or "getInstance" in str(class_entity.get("attributes", {})):
                patterns.append({
                    "pattern_type": "singleton",
                    "entities": [class_entity.get("id")],
                    "confidence": 0.7,
                    "description": f"Potential Singleton pattern in class {class_name}"
                })
        
        return patterns
    
    def _detect_factory_pattern(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> List[Dict[str, Any]]:
        """Detect factory pattern in classes"""
        patterns = []
        
        class_entities = [e for e in entities if e.get("type") == "class"]
        
        for class_entity in class_entities:
            class_name = class_entity.get("name", "")
            if "factory" in class_name.lower() or "builder" in class_name.lower():
                patterns.append({
                    "pattern_type": "factory",
                    "entities": [class_entity.get("id")],
                    "confidence": 0.6,
                    "description": f"Potential Factory pattern in class {class_name}"
                })
        
        return patterns
    
    def _detect_observer_pattern(self, entities: List[Dict[str, Any]], entity_map: Dict[str, str]) -> List[Dict[str, Any]]:
        """Detect observer pattern in classes"""
        patterns = []
        
        # Look for classes with observer-like methods
        class_entities = [e for e in entities if e.get("type") == "class"]
        
        for class_entity in class_entities:
            class_name = class_entity.get("name", "")
            attributes = str(class_entity.get("attributes", {}))
            
            if ("observer" in class_name.lower() or 
                "notify" in attributes.lower() or 
                "subscribe" in attributes.lower()):
                patterns.append({
                    "pattern_type": "observer",
                    "entities": [class_entity.get("id")],
                    "confidence": 0.5,
                    "description": f"Potential Observer pattern in class {class_name}"
                })
        
        return patterns
    
    def _calculate_relationship_statistics(self) -> Dict[str, Any]:
        """Calculate statistics about discovered relationships"""
        stats = {
            "total_relationships": len(self.relationships),
            "relationships_by_type": defaultdict(int),
            "entity_fanout": defaultdict(int),
            "entity_fanin": defaultdict(int)
        }
        
        for relationship in self.relationships:
            rel_type = relationship["type"]
            source = relationship["source"]
            target = relationship["target"]
            
            stats["relationships_by_type"][rel_type] += 1
            stats["entity_fanout"][source] += 1
            stats["entity_fanin"][target] += 1
        
        # Convert defaultdicts to regular dicts
        stats["relationships_by_type"] = dict(stats["relationships_by_type"])
        stats["entity_fanout"] = dict(stats["entity_fanout"])
        stats["entity_fanin"] = dict(stats["entity_fanin"])
        
        # Calculate additional metrics
        if stats["entity_fanout"]:
            stats["max_fanout"] = max(stats["entity_fanout"].values())
            stats["avg_fanout"] = sum(stats["entity_fanout"].values()) / len(stats["entity_fanout"])
        else:
            stats["max_fanout"] = 0
            stats["avg_fanout"] = 0
        
        return stats
    
    def _create_relationship(self, source: str, target: str, relationship_type: str, 
                           metadata: Optional[Dict[str, Any]] = None,
                           strength: float = 1.0, confidence: float = 1.0) -> Dict[str, Any]:
        """Create a standardized relationship record"""
        self.relationship_counter += 1
        
        return {
            "id": f"rel_{self.relationship_counter}",
            "source": source,
            "target": target,
            "type": relationship_type,
            "metadata": metadata or {},
            "strength": strength,
            "confidence": confidence,
            "created_at": time.time()
        }