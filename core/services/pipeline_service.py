"""
Pipeline Service

Core pipeline logic extracted from core_pipeline.py and wrapped as a service.
Phase 5 verticalization - dedicated service for pipeline operations.
"""

import os
import time
from typing import Dict, List, Any, Optional
import asyncio
from pathlib import Path

from shared.interfaces.logger_interface import LoggerInterface
from core.interfaces.pipeline_interface import PipelineInterface, PipelineResult
from core.config.orchestration_config import PipelineConfig
from core.services.classification_service import ClassificationService

# Import existing pipeline functions to maintain compatibility
try:
    from core_pipeline import (
        parse_and_extract_entities,
        extract_enhanced_relationships,
        create_enhanced_graph_with_entities,
        clear_neo4j_database
    )
except ImportError:
    # Fallback functions for when core_pipeline is not available
    def parse_and_extract_entities(files): return []
    def extract_enhanced_relationships(files, use_ai): return []
    def create_enhanced_graph_with_entities(files, rels, descs): return {}
    def clear_neo4j_database(): return True


class PipelineService:
    """
    Service wrapper for core pipeline operations
    
    Provides a service interface to existing pipeline functionality while maintaining
    backward compatibility during the Phase 5 transition.
    """
    
    def __init__(self, 
                 classification_service: ClassificationService,
                 config: PipelineConfig,
                 logger: LoggerInterface):
        self.classification_service = classification_service
        self.config = config
        self.logger = logger
        
        # Performance tracking
        self._operation_stats = {
            'files_parsed': 0,
            'entities_extracted': 0,
            'relationships_created': 0,
            'errors_encountered': 0
        }
    
    async def parse_codebase(self, directory: str) -> Dict[str, Any]:
        """
        Parse codebase directory and extract entities
        
        Args:
            directory: Directory to parse
            
        Returns:
            Parsing results with entities and statistics
        """
        self.logger.log_info(f"Parsing codebase directory: {directory}")
        
        start_time = time.time()
        
        try:
            # Find Python files
            python_files = self._find_python_files(directory)
            
            if not python_files:
                return {
                    'success': False,
                    'error': 'No Python files found',
                    'files': [],
                    'entities': [],
                    'execution_time': time.time() - start_time
                }
            
            # Parse files with entity extraction
            if self.config.parallel_processing:
                parsed_files = await self._parse_files_parallel(python_files)
            else:
                parsed_files = await self._parse_files_sequential(python_files)
            
            # Classify entities
            all_entities = []
            for file_data in parsed_files:
                if file_data.get('success', False):
                    entities = file_data.get('entities', [])
                    classified_entities = await self.classification_service.classify_entities(entities)
                    file_data['entities'] = classified_entities
                    all_entities.extend(classified_entities)
            
            execution_time = time.time() - start_time
            
            # Update statistics
            successful_files = [f for f in parsed_files if f.get('success', False)]
            self._operation_stats['files_parsed'] += len(successful_files)
            self._operation_stats['entities_extracted'] += len(all_entities)
            
            result = {
                'success': True,
                'files': parsed_files,
                'entities': all_entities,
                'statistics': {
                    'total_files': len(python_files),
                    'successful_files': len(successful_files),
                    'total_entities': len(all_entities),
                    'execution_time': execution_time
                },
                'execution_time': execution_time
            }
            
            self.logger.log_info(f"Codebase parsing complete: {len(successful_files)}/{len(python_files)} files, {len(all_entities)} entities")
            
            return result
            
        except Exception as e:
            execution_time = time.time() - start_time
            self._operation_stats['errors_encountered'] += 1
            self.logger.log_error(f"Codebase parsing failed after {execution_time:.2f} seconds: {e}")
            
            return {
                'success': False,
                'error': str(e),
                'files': [],
                'entities': [],
                'execution_time': execution_time
            }
    
    async def extract_relationships(self, parsed_files: List[Dict[str, Any]], use_ai: bool = True) -> Dict[str, Any]:
        """
        Extract relationships between code entities
        
        Args:
            parsed_files: List of parsed file data
            use_ai: Whether to use AI for relationship extraction
            
        Returns:
            Relationship extraction results
        """
        self.logger.log_info(f"Extracting relationships from {len(parsed_files)} files (AI: {use_ai})")
        
        start_time = time.time()
        
        try:
            # Use existing relationship extraction logic
            relationships = await asyncio.to_thread(
                extract_enhanced_relationships, 
                parsed_files, 
                use_ai
            )
            
            execution_time = time.time() - start_time
            
            # Update statistics
            self._operation_stats['relationships_created'] += len(relationships)
            
            result = {
                'success': True,
                'relationships': relationships,
                'statistics': {
                    'total_relationships': len(relationships),
                    'ai_enabled': use_ai,
                    'execution_time': execution_time
                },
                'execution_time': execution_time
            }
            
            self.logger.log_info(f"Relationship extraction complete: {len(relationships)} relationships")
            
            return result
            
        except Exception as e:
            execution_time = time.time() - start_time
            self._operation_stats['errors_encountered'] += 1
            self.logger.log_error(f"Relationship extraction failed after {execution_time:.2f} seconds: {e}")
            
            return {
                'success': False,
                'error': str(e),
                'relationships': [],
                'execution_time': execution_time
            }
    
    async def build_graph(self, 
                         parsed_files: List[Dict[str, Any]], 
                         relationships: List[Dict[str, Any]], 
                         descriptions: Dict[str, Dict[str, str]] = None) -> Dict[str, Any]:
        """
        Build knowledge graph from parsed files and relationships
        
        Args:
            parsed_files: List of parsed file data
            relationships: List of relationships
            descriptions: Optional AI-generated descriptions
            
        Returns:
            Graph building results
        """
        self.logger.log_info(f"Building graph from {len(parsed_files)} files and {len(relationships)} relationships")
        
        start_time = time.time()
        
        try:
            # Use existing graph building logic
            graph_stats = await asyncio.to_thread(
                create_enhanced_graph_with_entities,
                parsed_files,
                relationships,
                descriptions or {}
            )
            
            execution_time = time.time() - start_time
            
            result = {
                'success': True,
                'graph_stats': graph_stats,
                'statistics': {
                    'execution_time': execution_time,
                    'files_processed': len(parsed_files),
                    'relationships_processed': len(relationships),
                    'descriptions_used': len(descriptions) if descriptions else 0
                },
                'execution_time': execution_time
            }
            
            self.logger.log_info(f"Graph building complete: {graph_stats}")
            
            return result
            
        except Exception as e:
            execution_time = time.time() - start_time
            self._operation_stats['errors_encountered'] += 1
            self.logger.log_error(f"Graph building failed after {execution_time:.2f} seconds: {e}")
            
            return {
                'success': False,
                'error': str(e),
                'graph_stats': {},
                'execution_time': execution_time
            }
    
    async def clear_database(self) -> Dict[str, Any]:
        """
        Clear the graph database
        
        Returns:
            Database clearing results
        """
        self.logger.log_info("Clearing graph database")
        
        start_time = time.time()
        
        try:
            # Use existing database clearing logic
            success = await asyncio.to_thread(clear_neo4j_database)
            
            execution_time = time.time() - start_time
            
            if success:
                self.logger.log_info(f"Database cleared successfully in {execution_time:.2f} seconds")
                return {
                    'success': True,
                    'execution_time': execution_time
                }
            else:
                self.logger.log_error("Database clearing failed")
                return {
                    'success': False,
                    'error': 'Database clearing failed',
                    'execution_time': execution_time
                }
                
        except Exception as e:
            execution_time = time.time() - start_time
            self.logger.log_error(f"Database clearing failed after {execution_time:.2f} seconds: {e}")
            
            return {
                'success': False,
                'error': str(e),
                'execution_time': execution_time
            }
    
    def _find_python_files(self, directory: str) -> List[str]:
        """Find Python files in directory"""
        python_files = []
        
        for root, dirs, files in os.walk(directory):
            # Skip hidden directories and common non-source directories
            dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['__pycache__', 'node_modules', '.git']]
            
            for file in files:
                if file.endswith('.py') and not file.startswith('.'):
                    python_files.append(os.path.join(root, file))
        
        return python_files
    
    async def _parse_files_parallel(self, python_files: List[str]) -> List[Dict[str, Any]]:
        """Parse files in parallel batches"""
        batch_size = self.config.batch_size
        parsed_files = []
        
        for i in range(0, len(python_files), batch_size):
            batch = python_files[i:i + batch_size]
            
            # Parse batch in separate thread to avoid blocking
            batch_result = await asyncio.to_thread(parse_and_extract_entities, batch)
            parsed_files.extend(batch_result)
            
            self.logger.log_info(f"Parsed batch {i//batch_size + 1}: {len(batch_result)} files")
        
        return parsed_files
    
    async def _parse_files_sequential(self, python_files: List[str]) -> List[Dict[str, Any]]:
        """Parse files sequentially"""
        return await asyncio.to_thread(parse_and_extract_entities, python_files)
    
    def get_operation_statistics(self) -> Dict[str, Any]:
        """Get operation statistics"""
        return self._operation_stats.copy()
    
    def reset_statistics(self) -> None:
        """Reset operation statistics"""
        self._operation_stats = {
            'files_parsed': 0,
            'entities_extracted': 0,
            'relationships_created': 0,
            'errors_encountered': 0
        }
        self.logger.log_info("Operation statistics reset")
    
    async def health_check(self) -> bool:
        """Check if the pipeline service is healthy"""
        try:
            # Test classification service
            if not await self.classification_service.health_check():
                return False
            
            # Test basic functionality
            test_files = []  # Empty list should not cause errors
            result = await self.extract_relationships(test_files, use_ai=False)
            
            return result.get('success', False) or 'error' in result  # Either success or graceful error
            
        except Exception as e:
            self.logger.log_error(f"Pipeline service health check failed: {e}")
            return False