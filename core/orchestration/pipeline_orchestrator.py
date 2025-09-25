"""
Pipeline Orchestrator

Refactored from monolithic core_pipeline.py to use service interfaces and dependency injection.
This is Phase 5 of the verticalization process.
"""

import os
import json
import time
from datetime import datetime
from pathlib import Path
from typing import Dict, Any, List, Optional
import asyncio

from core.interfaces.pipeline_interface import PipelineInterface, PipelineResult, PipelineStatus
from core.config.orchestration_config import OrchestrationConfig
from shared.interfaces.logger_interface import LoggerInterface
from shared.services.service_registry import ServiceRegistry

# Service interfaces from Phase 2-4
import sys
import os

# Add project root to Python path for proper imports
project_root = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

# Use the simple AI service interface directly
from core.services.simple_ai_service import AIServicesInterface

from shared.interfaces.graph_operations_interface import GraphOperationsInterface

try:
    from update_agents.interfaces.update_coordinator_interface import UpdateIntelligenceInterface
except ImportError:
    # Fallback for Phase 5 - we'll create a basic interface
    from abc import ABC, abstractmethod

    class UpdateIntelligenceInterface(ABC):
        @abstractmethod
        def analyze_git_diff(self, commit_hash: str) -> Dict[str, Any]:
            pass


from core.interfaces.rag_service_interface import RAGServiceInterface


class PipelineOrchestrator(PipelineInterface):
    """
    Pipeline orchestrator using service interfaces instead of direct imports.

    Refactored from 915-line core_pipeline.py into focused orchestration.
    """

    def __init__(self, services: ServiceRegistry, config: OrchestrationConfig, logger: LoggerInterface):
        self.services = services
        self.config = config
        self.logger = logger

        # Service dependencies
        self._ai_service = None
        self._graph_service = None
        self._rag_service = None

    def _get_ai_service(self) -> AIServicesInterface:
        """Get AI service with lazy initialization"""
        if self._ai_service is None:
            try:
                self._ai_service = self.services.get(AIServicesInterface)
            except Exception as e:
                self.logger.log_error(f"Failed to get AI service: {e}")
                raise RuntimeError(f"AI service unavailable: {e}")
        return self._ai_service

    def _get_graph_service(self) -> GraphOperationsInterface:
        """Get graph service with lazy initialization"""
        if self._graph_service is None:
            try:
                self._graph_service = self.services.get(GraphOperationsInterface)
            except Exception as e:
                self.logger.log_error(f"Failed to get graph service: {e}")
                raise RuntimeError(f"Graph service unavailable: {e}")
        return self._graph_service

    def _get_rag_service(self) -> RAGServiceInterface:
        """Get RAG service with lazy initialization"""
        if self._rag_service is None:
            try:
                self._rag_service = self.services.get(RAGServiceInterface)
            except Exception as e:
                self.logger.log_error(f"Failed to get RAG service: {e}")
                raise RuntimeError(f"RAG service unavailable: {e}")
        return self._rag_service

    async def run_enhanced_pipeline(self, target_directory: str, **kwargs) -> PipelineResult:
        """
        Run enhanced codebase analysis pipeline using service interfaces

        Orchestrates the complete pipeline:
        1. File discovery and parsing
        2. AI description generation
        3. Relationship extraction
        4. Graph building
        5. RAG embedding creation
        """
        self.logger.log_info(f"Starting enhanced pipeline for directory: {target_directory}")
        start_time = time.time()

        try:
            # Extract pipeline configuration from kwargs
            use_ai = kwargs.get("use_ai", self.config.pipeline.enable_ai_descriptions)
            enable_relationships = kwargs.get(
                "enable_relationships", self.config.pipeline.enable_relationship_extraction
            )
            enable_embeddings = kwargs.get("enable_embeddings", self.config.pipeline.enable_embeddings)
            primer_file_path = kwargs.get("primer_file_path")
            include_documentation = kwargs.get("include_documentation", True)
            file_limit = kwargs.get("file_limit")

            pipeline_data = {
                "target_directory": target_directory,
                "use_ai": use_ai,
                "enable_relationships": enable_relationships,
                "enable_embeddings": enable_embeddings,
                "primer_file_path": primer_file_path,
                "include_documentation": include_documentation,
            }

            # Step 1: Clear and prepare database
            self.logger.log_info("Clearing graph database...")
            graph_service = self._get_graph_service()
            await self._clear_database(graph_service)

            # Step 2: File discovery and parsing
            self.logger.log_info(
                "Discovering and parsing source files (Python, TypeScript, JavaScript, JSON, Markdown, COBOL)..."
            )
            if file_limit:
                self.logger.log_info(f"Limiting processing to {file_limit} files for quick testing")
            parsed_files = await self._parse_codebase(target_directory, file_limit)

            if not parsed_files:
                return PipelineResult.failure(
                    ["No source files found or parsed successfully (Python, TypeScript, JavaScript, JSON, Markdown, COBOL)"],
                    "Pipeline failed - no files to process",
                )

            successful_files = [f for f in parsed_files if f.get("success", False) or f.get("parse_success", False)]
            pipeline_data["files_processed"] = len(successful_files)
            pipeline_data["total_files"] = len(parsed_files)

            # Step 3: Generate AI descriptions (if enabled)
            descriptions = {}
            if use_ai:
                self.logger.log_info("Generating AI descriptions...")
                descriptions = await self._generate_descriptions(parsed_files, target_directory, primer_file_path)
                pipeline_data["descriptions_generated"] = len(descriptions)

            # Step 4: Extract relationships (if enabled)
            relationships = []
            if enable_relationships:
                self.logger.log_info("Extracting code relationships...")
                relationships = await self._extract_relationships(parsed_files, use_ai)
                pipeline_data["relationships_extracted"] = len(relationships)

            # Step 5: Build graph
            self.logger.log_info("Building knowledge graph...")
            graph_stats = await self._build_graph(parsed_files, relationships, descriptions)
            pipeline_data["graph_stats"] = graph_stats

            # Step 6: Create RAG embeddings (if enabled)
            if enable_embeddings:
                self.logger.log_info("Creating RAG embeddings...")
                rag_service = self._get_rag_service()
                embedding_result = await self._create_embeddings(parsed_files, rag_service)
                pipeline_data["embeddings_created"] = embedding_result.get("count", 0)

            # Calculate execution time
            execution_time = time.time() - start_time
            pipeline_data["execution_time"] = execution_time

            # Save results
            await self._save_pipeline_results(pipeline_data)

            self.logger.log_info(f"Pipeline completed successfully in {execution_time:.2f} seconds")

            return PipelineResult.success(
                data=pipeline_data,
                message=f"Pipeline completed successfully. Processed {len(successful_files)} files.",
                execution_time=execution_time,
            )

        except Exception as e:
            execution_time = time.time() - start_time
            error_msg = f"Pipeline failed after {execution_time:.2f} seconds: {str(e)}"
            self.logger.log_error(error_msg)

            return PipelineResult.failure([str(e)], error_msg)

    async def _clear_database(self, graph_service: GraphOperationsInterface) -> None:
        """Clear the graph database"""
        try:
            # Use graph service to clear database
            if hasattr(graph_service, "clear_all_data"):
                # Async version
                await graph_service.clear_all_data()
            elif hasattr(graph_service, "clear_database"):
                # Sync version
                result = graph_service.clear_database()
                if asyncio.iscoroutine(result):
                    await result
            else:
                self.logger.log_warning("No clear database method available on graph service")
            self.logger.log_info("Database cleared successfully")
        except Exception as e:
            self.logger.log_error(f"Failed to clear database: {e}")
            raise

    async def _parse_codebase(self, directory: str, file_limit: int = None) -> List[Dict[str, Any]]:
        """
        Parse codebase files to extract entities using multi-language parsing
        """
        try:
            # Find source files (Python, TypeScript, JavaScript, JSON, Markdown, COBOL)
            source_files = []
            supported_extensions = [".py", ".ts", ".tsx", ".js", ".jsx", ".json", ".md", ".markdown", ".cbl", ".cob", ".cobol"]

            for root, dirs, files in os.walk(directory):
                # Skip hidden directories and common non-source directories
                dirs[:] = [
                    d for d in dirs if not d.startswith(".") and d not in ["__pycache__", "node_modules", ".git", ".venv", "venv"]
                ]

                for file in files:
                    if not file.startswith("."):
                        for ext in supported_extensions:
                            if file.endswith(ext):
                                source_files.append(os.path.join(root, file))
                                break

            # Apply file limit if specified
            if file_limit and len(source_files) > file_limit:
                original_count = len(source_files)
                source_files = source_files[:file_limit]
                self.logger.log_info(f"Found {original_count} source files, limiting to {len(source_files)} for quick testing")
            else:
                self.logger.log_info(f"Found {len(source_files)} source files")

            if not source_files:
                return []

            # Use multi-language parsing implementation
            try:
                from shared.services.multi_language_parser import parse_and_extract_entities

                parsed_files = await asyncio.to_thread(parse_and_extract_entities, source_files)
            except ImportError:
                # Fallback to direct parsing for Python files only
                python_files = [f for f in source_files if f.endswith(".py")]
                parsed_files = await self._parse_files_direct(python_files)

            return parsed_files

        except Exception as e:
            self.logger.log_error(f"Failed to parse codebase: {e}")
            raise

    async def _generate_descriptions(
        self, parsed_files: List[Dict[str, Any]], project_root: str, primer_file_path: str = None
    ) -> Dict[str, Dict[str, str]]:
        """Generate AI descriptions using AI service"""
        try:
            ai_service = self._get_ai_service()

            # Load primer context (use custom path if provided)
            primer_context = await self._load_primer_context(project_root, primer_file_path)

            descriptions = {}
            successful_files = [f for f in parsed_files if f.get("success", False) or f.get("parse_success", False)]

            for file_data in successful_files:
                file_path = file_data["file_path"]
                entities = file_data.get("entities", [])
                descriptions[file_path] = {}

                for entity in entities:
                    # Include COBOL entity types in description generation
                    if entity.get("type") in ["function", "class", "program", "compilation_unit", "file", "paragraph", "data_item"]:
                        entity_data = {
                            "name": entity["name"],
                            "type": entity.get("type", "unknown"),
                            "file_path": file_path,
                            "line": entity.get("line", 1),
                            "properties": entity.get("properties", {}),
                        }

                        try:
                            description = await ai_service.generate_description(entity_data, primer_context)
                            descriptions[file_path][entity["name"]] = description
                        except Exception as e:
                            self.logger.log_warning(f"Failed to describe {entity['name']}: {e}")
                            descriptions[file_path][entity["name"]] = f"A {entity['type']} named {entity['name']}"

            return descriptions

        except Exception as e:
            self.logger.log_error(f"Failed to generate descriptions: {e}")
            # Return empty descriptions as fallback
            return {
                f["file_path"]: {} for f in parsed_files if f.get("success", False) or f.get("parse_success", False)
            }

    async def _load_primer_context(self, project_root: str, custom_primer_path: str = None) -> str:
        """Load business context from PRIMER.md file"""
        primer_paths = []

        # Use custom primer path if provided (highest priority)
        if custom_primer_path:
            primer_paths.append(custom_primer_path)

        # Check environment variable for custom primer path
        env_primer = os.getenv("PRIMER_FILE_PATH")
        if env_primer:
            primer_paths.append(env_primer)

        # Default primer locations
        primer_paths.extend(
            [
                os.path.join(project_root, "PRIMER.md"),
                os.path.join(project_root, "primer.md"),
                os.path.join(project_root, "BUSINESS_CONTEXT.md"),
                os.path.join(project_root, "business_context.md"),
            ]
        )

        for primer_path in primer_paths:
            try:
                if os.path.exists(primer_path):
                    with open(primer_path, "r", encoding="utf-8") as f:
                        content = f.read().strip()
                        if content:
                            self.logger.log_info(f"Loaded primer context from: {primer_path}")
                            return content
            except Exception as e:
                self.logger.log_warning(f"Could not read primer file {primer_path}: {e}")

        return ""

    async def _extract_relationships(self, parsed_files: List[Dict[str, Any]], use_ai: bool) -> List[Dict[str, Any]]:
        """Extract code relationships using AI service or AST parsing"""
        try:
            if use_ai:
                ai_service = self._get_ai_service()
                return await ai_service.extract_relationships(parsed_files)
            else:
                # Use direct AST-based relationship extraction
                return await self._extract_relationships_direct(parsed_files)

        except Exception as e:
            self.logger.log_error(f"Failed to extract relationships: {e}")
            return []

    async def _build_graph(
        self,
        parsed_files: List[Dict[str, Any]],
        relationships: List[Dict[str, Any]],
        descriptions: Dict[str, Dict[str, str]],
    ) -> Dict[str, Any]:
        """Build knowledge graph using graph service"""
        try:
            graph_service = self._get_graph_service()

            # Build graph directly using graph service
            graph_stats = await self._build_graph_direct(graph_service, parsed_files, relationships, descriptions)

            return graph_stats

        except Exception as e:
            self.logger.log_error(f"Failed to build graph: {e}")
            raise

    async def _create_embeddings(
        self, parsed_files: List[Dict[str, Any]], rag_service: RAGServiceInterface
    ) -> Dict[str, Any]:
        """Create RAG embeddings using RAG service"""
        try:
            # Extract entities for embedding
            entities = []
            for file_data in parsed_files:
                if file_data.get("success", False):
                    entities.extend(file_data.get("entities", []))

            result = await rag_service.create_embeddings(entities)
            return result

        except Exception as e:
            self.logger.log_error(f"Failed to create embeddings: {e}")
            return {"count": 0, "error": str(e)}

    async def _save_pipeline_results(self, pipeline_data: Dict[str, Any]) -> None:
        """Save pipeline results to file"""
        try:
            results = {
                "timestamp": datetime.now().isoformat(),
                "duration_seconds": pipeline_data.get("execution_time", 0),
                "files_processed": pipeline_data.get("files_processed", 0),
                "total_files": pipeline_data.get("total_files", 0),
                "graph_stats": pipeline_data.get("graph_stats", {}),
                "ai_enabled": pipeline_data.get("use_ai", False),
                "relationships_extracted": pipeline_data.get("relationships_extracted", 0),
                "descriptions_generated": pipeline_data.get("descriptions_generated", 0),
                "embeddings_created": pipeline_data.get("embeddings_created", 0),
            }

            results_file = "enhanced_pipeline_results.json"
            with open(results_file, "w") as f:
                json.dump(results, f, indent=2)

            self.logger.log_info(f"Results saved to: {results_file}")

        except Exception as e:
            self.logger.log_error(f"Failed to save results: {e}")

    async def validate_pipeline_config(self, config: Dict[str, Any]) -> bool:
        """Validate pipeline configuration"""
        try:
            # Check required configuration keys
            required_keys = ["target_directory"]
            for key in required_keys:
                if key not in config:
                    self.logger.log_error(f"Missing required config key: {key}")
                    return False

            # Validate target directory exists
            if not os.path.exists(config["target_directory"]):
                self.logger.log_error(f"Target directory does not exist: {config['target_directory']}")
                return False

            # Validate service availability
            try:
                self._get_graph_service()
            except Exception as e:
                self.logger.log_error(f"Graph service not available: {e}")
                return False

            if config.get("use_ai", True):
                try:
                    self._get_ai_service()
                except Exception as e:
                    self.logger.log_error(f"AI service not available but required: {e}")
                    return False

            return True

        except Exception as e:
            self.logger.log_error(f"Config validation failed: {e}")
            return False

    async def get_pipeline_status(self, pipeline_id: str) -> PipelineStatus:
        """Get status of running pipeline (placeholder for future implementation)"""
        # TODO: Implement pipeline tracking
        return PipelineStatus.PENDING

    async def _parse_files_direct(self, file_paths: List[str]) -> List[Dict[str, Any]]:
        """Direct AST parsing implementation without core_pipeline dependency"""
        import ast

        parsed_files = []

        for file_path in file_paths:
            try:
                with open(file_path, "r", encoding="utf-8") as f:
                    content = f.read()

                # Parse with AST
                tree = ast.parse(content)

                # Extract basic entities
                entities = []
                for node in ast.walk(tree):
                    if isinstance(node, ast.FunctionDef):
                        entities.append(
                            {
                                "name": node.name,
                                "type": "function",
                                "line": node.lineno,
                                "file_path": file_path,
                                "properties": {
                                    "docstring": ast.get_docstring(node) or "",
                                    "args": [arg.arg for arg in node.args.args],
                                },
                            }
                        )
                    elif isinstance(node, ast.ClassDef):
                        entities.append(
                            {
                                "name": node.name,
                                "type": "class",
                                "line": node.lineno,
                                "file_path": file_path,
                                "properties": {
                                    "docstring": ast.get_docstring(node) or "",
                                    "bases": [
                                        base.id if isinstance(base, ast.Name) else str(base) for base in node.bases
                                    ],
                                },
                            }
                        )

                parsed_files.append(
                    {
                        "file_path": file_path,
                        "success": True,
                        "entities": entities,
                        "imports": self._extract_imports(tree),
                        "content": content[:1000],  # First 1000 chars for context
                    }
                )

            except Exception as e:
                self.logger.log_warning(f"Failed to parse {file_path}: {e}")
                parsed_files.append({"file_path": file_path, "success": False, "error": str(e), "entities": []})

        successful_files = [f for f in parsed_files if f.get("success", False)]
        self.logger.log_info(f"Successfully parsed {len(successful_files)}/{len(file_paths)} files")

        return parsed_files

    def _extract_imports(self, tree) -> List[Dict[str, Any]]:
        """Extract import statements from AST"""
        import ast

        imports = []

        for node in ast.walk(tree):
            if isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append({"type": "import", "module": alias.name, "alias": alias.asname, "line": node.lineno})
            elif isinstance(node, ast.ImportFrom):
                for alias in node.names:
                    imports.append(
                        {
                            "type": "from_import",
                            "module": node.module,
                            "name": alias.name,
                            "alias": alias.asname,
                            "line": node.lineno,
                        }
                    )

        return imports

    async def _extract_relationships_direct(self, parsed_files: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Direct relationship extraction using multi-language extractors"""
        try:
            # Use the multi-language relationship extraction
            from shared.services.multi_language_parser import extract_multi_language_relationships

            relationships = extract_multi_language_relationships(parsed_files)
            print(f"   🔍 DEBUG: Multi-language parser returned {len(relationships)} relationships")

            # Convert RelationshipExtraction objects to dictionaries
            relationship_dicts = []
            for i, rel in enumerate(relationships):
                # Handle both RelationshipExtraction objects and dictionaries
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

            print(f"   🔍 DEBUG: Converted {len(relationship_dicts)} relationships to dictionaries")
            
            # Debug: Show relationship types
            rel_types = {}
            for rel_dict in relationship_dicts:
                rel_type = rel_dict.get("type", "UNKNOWN")
                rel_types[rel_type] = rel_types.get(rel_type, 0) + 1
            print(f"   🔍 DEBUG: Final relationship types: {rel_types}")
            
            self.logger.log_info(f"Extracted {len(relationship_dicts)} multi-language relationships")
            return relationship_dicts

        except Exception as e:
            self.logger.log_error(f"Multi-language relationship extraction failed: {e}")
            # Fallback to simple approach
            return await self._extract_relationships_fallback(parsed_files)

    async def _extract_relationships_fallback(self, parsed_files: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Fallback relationship extraction for when multi-language fails"""
        relationships = []

        # Simple containment relationships (file contains entities)
        for file_data in parsed_files:
            if not (file_data.get("success", False) or file_data.get("parse_success", False)):
                continue

            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])

            for entity in entities:
                relationships.append(
                    {
                        "source": file_path,
                        "target": entity["name"],
                        "type": "CONTAINS",
                        "properties": {
                            "entity_type": entity["type"],
                            "line": entity.get("line", entity.get("line_number", 0)),
                        },
                    }
                )

        self.logger.log_info(f"Extracted {len(relationships)} fallback relationships")
        return relationships

    async def _build_graph_direct(
        self,
        graph_service: GraphOperationsInterface,
        parsed_files: List[Dict[str, Any]],
        relationships: List[Dict[str, Any]],
        descriptions: Dict[str, Dict[str, str]],
    ) -> Dict[str, Any]:
        """Direct graph building using graph service"""
        entities_created = 0
        relationships_created = 0

        # Create file nodes
        for file_data in parsed_files:
            if not (file_data.get("success", False) or file_data.get("parse_success", False)):
                continue

            file_path = file_data["file_path"]
            entities = file_data.get("entities", [])
            
            print(f"   🔍 DEBUG: Processing file {file_path} with {len(entities)} entities")

            # Create file entity
            graph_service.create_entity_node(
                "file",
                file_path,
                {
                    "path": file_path,
                    "entity_count": len(entities),
                    "success": file_data.get("success", False),
                },
            )
            entities_created += 1

            # Create entity nodes
            for entity in entities:
                entity_name = entity["name"]
                entity_type = entity["type"]
                
                print(f"   🔍 DEBUG: Creating entity {entity_type}:{entity_name} from file {file_path}")

                # Get description if available
                description = descriptions.get(file_path, {}).get(entity_name, "")

                properties = entity.get("properties", {})
                properties["description"] = description
                properties["file_path"] = file_path
                properties["line"] = entity.get("line", entity.get("line_number", 0))

                graph_service.create_entity_node(entity_type, entity_name, properties)
                entities_created += 1

        # Create relationships
        print(f"   🔍 DEBUG: Processing {len(relationships)} relationships for graph creation")
        for i, rel in enumerate(relationships):
            try:
                # Ensure rel is a dictionary
                if not isinstance(rel, dict):
                    self.logger.log_warning(f"Skipping non-dictionary relationship: {type(rel)}")
                    continue
                
                rel_type = rel.get("type", "UNKNOWN")
                source = rel.get("source", "UNKNOWN")
                target = rel.get("target", "UNKNOWN")
                print(f"   🔍 DEBUG: Creating relationship {i+1}: {source} -{rel_type}-> {target}")
                    
                graph_service.add_relationship(source, target, rel_type, rel.get("properties", {}))
                relationships_created += 1
                print(f"   ✅ DEBUG: Successfully created {rel_type} relationship")
            except Exception as e:
                # Skip failed relationships
                print(f"   ❌ DEBUG: Failed to create relationship {rel.get('type', 'unknown')}: {e}")
                self.logger.log_warning(f"Failed to create relationship {rel.get('type', 'unknown')}: {e}")

        # Get final stats
        stats = graph_service.get_database_stats()

        return {
            "total_entities": entities_created,
            "total_relationships": relationships_created,
            "database_stats": stats,
        }
