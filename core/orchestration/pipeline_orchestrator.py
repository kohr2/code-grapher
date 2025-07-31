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
try:
    from ai_services.interfaces.ai_services_interface import AIServicesInterface
except ImportError:
    from interfaces.ai_services_interface import AIServicesInterface

try:
    from graph_ops.interfaces.graph_service_interface import GraphOperationsInterface
except ImportError:
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
    
    def __init__(self, 
                 services: ServiceRegistry, 
                 config: OrchestrationConfig,
                 logger: LoggerInterface):
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
            use_ai = kwargs.get('use_ai', self.config.pipeline.enable_ai_descriptions)
            enable_relationships = kwargs.get('enable_relationships', self.config.pipeline.enable_relationship_extraction)
            enable_embeddings = kwargs.get('enable_embeddings', self.config.pipeline.enable_embeddings)
            
            pipeline_data = {
                'target_directory': target_directory,
                'use_ai': use_ai,
                'enable_relationships': enable_relationships,
                'enable_embeddings': enable_embeddings
            }
            
            # Step 1: Clear and prepare database
            self.logger.log_info("Clearing graph database...")
            graph_service = self._get_graph_service()
            await self._clear_database(graph_service)
            
            # Step 2: File discovery and parsing
            self.logger.log_info("Discovering and parsing Python files...")
            parsed_files = await self._parse_codebase(target_directory)
            
            if not parsed_files:
                return PipelineResult.failure(
                    ["No Python files found or parsed successfully"],
                    "Pipeline failed - no files to process"
                )
            
            successful_files = [f for f in parsed_files if f.get("success", False)]
            pipeline_data['files_processed'] = len(successful_files)
            pipeline_data['total_files'] = len(parsed_files)
            
            # Step 3: Generate AI descriptions (if enabled)
            descriptions = {}
            if use_ai:
                self.logger.log_info("Generating AI descriptions...")
                descriptions = await self._generate_descriptions(parsed_files, target_directory)
                pipeline_data['descriptions_generated'] = len(descriptions)
            
            # Step 4: Extract relationships (if enabled)
            relationships = []
            if enable_relationships:
                self.logger.log_info("Extracting code relationships...")
                relationships = await self._extract_relationships(parsed_files, use_ai)
                pipeline_data['relationships_extracted'] = len(relationships)
            
            # Step 5: Build graph
            self.logger.log_info("Building knowledge graph...")
            graph_stats = await self._build_graph(parsed_files, relationships, descriptions)
            pipeline_data['graph_stats'] = graph_stats
            
            # Step 6: Create RAG embeddings (if enabled)
            if enable_embeddings:
                self.logger.log_info("Creating RAG embeddings...")
                rag_service = self._get_rag_service()
                embedding_result = await self._create_embeddings(parsed_files, rag_service)
                pipeline_data['embeddings_created'] = embedding_result.get('count', 0)
            
            # Calculate execution time
            execution_time = time.time() - start_time
            pipeline_data['execution_time'] = execution_time
            
            # Save results
            await self._save_pipeline_results(pipeline_data)
            
            self.logger.log_info(f"Pipeline completed successfully in {execution_time:.2f} seconds")
            
            return PipelineResult.success(
                data=pipeline_data,
                message=f"Pipeline completed successfully. Processed {len(successful_files)} files.",
                execution_time=execution_time
            )
            
        except Exception as e:
            execution_time = time.time() - start_time
            error_msg = f"Pipeline failed after {execution_time:.2f} seconds: {str(e)}"
            self.logger.log_error(error_msg)
            
            return PipelineResult.failure(
                [str(e)],
                error_msg
            )
    
    async def _clear_database(self, graph_service: GraphOperationsInterface) -> None:
        """Clear the graph database"""
        try:
            # Use graph service to clear database
            await graph_service.clear_all_data()
            self.logger.log_info("Database cleared successfully")
        except Exception as e:
            self.logger.log_error(f"Failed to clear database: {e}")
            raise
    
    async def _parse_codebase(self, directory: str) -> List[Dict[str, Any]]:
        """
        Parse codebase files to extract entities
        
        This delegates to the existing parse_and_extract_entities function
        but adds proper async support and service integration
        """
        try:
            # Find Python files
            python_files = []
            for root, dirs, files in os.walk(directory):
                # Skip hidden directories and common non-source directories
                dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['__pycache__', 'node_modules', '.git']]
                
                for file in files:
                    if file.endswith('.py') and not file.startswith('.'):
                        python_files.append(os.path.join(root, file))
            
            self.logger.log_info(f"Found {len(python_files)} Python files")
            
            if not python_files:
                return []
            
            # Import and use existing parsing logic
            # This maintains compatibility while allowing future refactoring
            from core_pipeline import parse_and_extract_entities
            
            if self.config.pipeline.parallel_processing:
                # Process files in batches for better performance
                batch_size = self.config.pipeline.batch_size
                parsed_files = []
                
                for i in range(0, len(python_files), batch_size):
                    batch = python_files[i:i + batch_size]
                    batch_result = await asyncio.to_thread(parse_and_extract_entities, batch)
                    parsed_files.extend(batch_result)
            else:
                parsed_files = await asyncio.to_thread(parse_and_extract_entities, python_files)
            
            return parsed_files
            
        except Exception as e:
            self.logger.log_error(f"Failed to parse codebase: {e}")
            raise
    
    async def _generate_descriptions(self, parsed_files: List[Dict[str, Any]], project_root: str) -> Dict[str, Dict[str, str]]:
        """Generate AI descriptions using AI service"""
        try:
            ai_service = self._get_ai_service()
            
            # Load primer context
            primer_context = await self._load_primer_context(project_root)
            
            descriptions = {}
            successful_files = [f for f in parsed_files if f.get("success", False)]
            
            for file_data in successful_files:
                file_path = file_data["file_path"]
                entities = file_data.get("entities", [])
                descriptions[file_path] = {}
                
                for entity in entities:
                    if entity.get("type") in ["function", "class"]:
                        entity_data = {
                            "name": entity["name"],
                            "type": entity.get("type", "unknown"),
                            "file_path": file_path,
                            "line": entity.get("line", 1),
                            "properties": entity.get("properties", {})
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
            return {f["file_path"]: {} for f in parsed_files if f.get("success", False)}
    
    async def _load_primer_context(self, project_root: str) -> str:
        """Load business context from PRIMER.md file"""
        primer_paths = [
            os.path.join(project_root, "PRIMER.md"),
            os.path.join(project_root, "primer.md"),
            os.path.join(project_root, "BUSINESS_CONTEXT.md"),
            os.path.join(project_root, "business_context.md")
        ]
        
        # Check environment variable for custom primer path
        custom_primer = os.getenv("PRIMER_FILE_PATH")
        if custom_primer:
            primer_paths.insert(0, custom_primer)
        
        for primer_path in primer_paths:
            try:
                if os.path.exists(primer_path):
                    with open(primer_path, 'r', encoding='utf-8') as f:
                        content = f.read().strip()
                        if content:
                            self.logger.log_info(f"Loaded primer context from: {primer_path}")
                            return content
            except Exception as e:
                self.logger.log_warning(f"Could not read primer file {primer_path}: {e}")
        
        return ""
    
    async def _extract_relationships(self, parsed_files: List[Dict[str, Any]], use_ai: bool) -> List[Dict[str, Any]]:
        """Extract code relationships using AI service"""
        try:
            if use_ai:
                ai_service = self._get_ai_service()
                return await ai_service.extract_relationships(parsed_files)
            else:
                # Use AST-based relationship extraction
                from core_pipeline import extract_enhanced_relationships
                return await asyncio.to_thread(extract_enhanced_relationships, parsed_files, False)
                
        except Exception as e:
            self.logger.log_error(f"Failed to extract relationships: {e}")
            return []
    
    async def _build_graph(self, parsed_files: List[Dict[str, Any]], relationships: List[Dict[str, Any]], descriptions: Dict[str, Dict[str, str]]) -> Dict[str, Any]:
        """Build knowledge graph using graph service"""
        try:
            graph_service = self._get_graph_service()
            
            # Use existing graph building logic through service
            from core_pipeline import create_enhanced_graph_with_entities
            graph_stats = await asyncio.to_thread(
                create_enhanced_graph_with_entities, 
                parsed_files, 
                relationships, 
                descriptions
            )
            
            return graph_stats
            
        except Exception as e:
            self.logger.log_error(f"Failed to build graph: {e}")
            raise
    
    async def _create_embeddings(self, parsed_files: List[Dict[str, Any]], rag_service: RAGServiceInterface) -> Dict[str, Any]:
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
                "duration_seconds": pipeline_data.get('execution_time', 0),
                "files_processed": pipeline_data.get('files_processed', 0),
                "total_files": pipeline_data.get('total_files', 0),
                "graph_stats": pipeline_data.get('graph_stats', {}),
                "ai_enabled": pipeline_data.get('use_ai', False),
                "relationships_extracted": pipeline_data.get('relationships_extracted', 0),
                "descriptions_generated": pipeline_data.get('descriptions_generated', 0),
                "embeddings_created": pipeline_data.get('embeddings_created', 0)
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
            required_keys = ['target_directory']
            for key in required_keys:
                if key not in config:
                    self.logger.log_error(f"Missing required config key: {key}")
                    return False
            
            # Validate target directory exists
            if not os.path.exists(config['target_directory']):
                self.logger.log_error(f"Target directory does not exist: {config['target_directory']}")
                return False
            
            # Validate service availability
            try:
                self._get_graph_service()
            except Exception as e:
                self.logger.log_error(f"Graph service not available: {e}")
                return False
            
            if config.get('use_ai', True):
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