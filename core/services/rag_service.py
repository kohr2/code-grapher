"""
RAG Service

Extracted from rag_pipeline.py to implement the RAG Service Interface.
Phase 5 verticalization - dedicated service for Retrieval-Augmented Generation.
"""

import os
import time
import asyncio
from typing import List, Dict, Any, Optional
import numpy as np

import chromadb
from sentence_transformers import SentenceTransformer
from langchain.text_splitter import RecursiveCharacterTextSplitter

from core.interfaces.rag_service_interface import RAGServiceInterface, RAGQuery, RAGResult
from shared.interfaces.logger_interface import LoggerInterface
from shared.interfaces.graph_operations_interface import GraphOperationsInterface
from core.config.orchestration_config import RAGConfig

# Import AI evaluation tracker
try:
    from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment
except ImportError:
    # Fallback for missing AI tracker
    ai_tracker = None
    EvaluationCategory = None
    Sentiment = None

# Import AI service for semantic search
try:
    from ai_services.interfaces.ai_services_interface import AIServicesInterface
except ImportError:
    from interfaces.ai_services_interface import AIServicesInterface


class RAGService(RAGServiceInterface):
    """
    RAG service for code-to-graph-db queries with comprehensive logging
    
    Extracted from CodeRAGPipeline in rag_pipeline.py with service interface compliance.
    """
    
    def __init__(self, 
                 graph_service: GraphOperationsInterface,
                 ai_service: Optional[AIServicesInterface],
                 config: RAGConfig,
                 logger: LoggerInterface):
        
        self.graph_service = graph_service
        self.ai_service = ai_service
        self.config = config
        self.logger = logger
        
        # Initialize components
        self.embedding_model = None
        self.chroma_client = None
        self.collection = None
        self.text_splitter = None
        
        # Session logging
        self.session_logger = logger.create_session_logger("RAGService") if hasattr(logger, 'create_session_logger') else logger
        
    async def initialize(self) -> None:
        """Initialize the RAG service components"""
        self.logger.log_info("Initializing RAG service...")
        
        start_time = time.time()
        
        try:
            # Initialize embedding model
            self.logger.log_info(f"Loading sentence transformer model: {self.config.embedding_model}")
            self.embedding_model = await asyncio.to_thread(
                SentenceTransformer, self.config.embedding_model
            )
            
            # Initialize ChromaDB
            self.logger.log_info(f"Initializing ChromaDB at: {self.config.chroma_path}")
            self.chroma_client = chromadb.PersistentClient(path=self.config.chroma_path)
            self.collection = self.chroma_client.get_or_create_collection(
                name="code_embeddings",
                metadata={"description": "Code embeddings for RAG"}
            )
            
            # Initialize text splitter
            self.text_splitter = RecursiveCharacterTextSplitter(
                chunk_size=1000,
                chunk_overlap=200,
                length_function=len
            )
            
            initialization_time = time.time() - start_time
            self.logger.log_info(f"RAG service initialized successfully in {initialization_time:.2f} seconds")
            
        except Exception as e:
            self.logger.log_error(f"Failed to initialize RAG service: {e}")
            raise
    
    async def create_embeddings(self, entities: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Create embeddings for code entities
        
        Args:
            entities: List of code entities to embed
            
        Returns:
            Embedding creation results
        """
        if not self.embedding_model:
            await self.initialize()
        
        self.logger.log_info(f"Creating embeddings for {len(entities)} entities")
        
        start_time = time.time()
        
        try:
            # Prepare texts for embedding
            texts = []
            metadata = []
            ids = []
            
            for i, entity in enumerate(entities):
                # Create text representation of entity
                text_parts = []
                
                if entity.get('name'):
                    text_parts.append(f"Name: {entity['name']}")
                
                if entity.get('type'):
                    text_parts.append(f"Type: {entity['type']}")
                
                if entity.get('file_path'):
                    text_parts.append(f"File: {entity['file_path']}")
                
                if entity.get('properties', {}).get('docstring'):
                    text_parts.append(f"Documentation: {entity['properties']['docstring']}")
                
                if entity.get('properties', {}).get('code_snippet'):
                    text_parts.append(f"Code: {entity['properties']['code_snippet']}")
                
                # Join text parts
                text = " | ".join(text_parts)
                texts.append(text)
                
                # Create metadata
                entity_metadata = {
                    'entity_name': entity.get('name', ''),
                    'entity_type': entity.get('type', ''),
                    'file_path': entity.get('file_path', ''),
                    'line': entity.get('line', 0)
                }
                metadata.append(entity_metadata)
                
                # Create unique ID
                ids.append(f"entity_{i}_{entity.get('name', 'unknown')}")
            
            # Create embeddings in batches
            batch_size = 50
            total_embedded = 0
            
            for i in range(0, len(texts), batch_size):
                batch_texts = texts[i:i + batch_size]
                batch_metadata = metadata[i:i + batch_size]
                batch_ids = ids[i:i + batch_size]
                
                # Generate embeddings
                embeddings = await asyncio.to_thread(
                    self.embedding_model.encode, batch_texts
                )
                
                # Store in ChromaDB
                self.collection.add(
                    embeddings=embeddings.tolist(),
                    metadatas=batch_metadata,
                    documents=batch_texts,
                    ids=batch_ids
                )
                
                total_embedded += len(batch_texts)
                self.logger.log_info(f"Embedded batch {i//batch_size + 1}: {total_embedded}/{len(texts)} entities")
            
            execution_time = time.time() - start_time
            
            result = {
                'count': total_embedded,
                'execution_time': execution_time,
                'collection_name': 'code_embeddings',
                'embedding_model': self.config.embedding_model
            }
            
            self.logger.log_info(f"Created embeddings for {total_embedded} entities in {execution_time:.2f} seconds")
            
            # Track with AI evaluation if available
            if ai_tracker and EvaluationCategory:
                ai_tracker.track_evaluation(
                    EvaluationCategory.EMBEDDING_CREATION,
                    {
                        'entity_count': total_embedded,
                        'execution_time': execution_time
                    },
                    Sentiment.POSITIVE if total_embedded > 0 else Sentiment.NEUTRAL
                )
            
            return result
            
        except Exception as e:
            execution_time = time.time() - start_time
            self.logger.log_error(f"Failed to create embeddings after {execution_time:.2f} seconds: {e}")
            return {
                'count': 0,
                'error': str(e),
                'execution_time': execution_time
            }
    
    async def query_code_graph(self, query: RAGQuery) -> RAGResult:
        """
        Query the code graph using RAG
        
        Args:
            query: RAG query configuration
            
        Returns:
            Query results with metadata
        """
        if not self.embedding_model:
            await self.initialize()
        
        self.logger.log_info(f"Processing RAG query: {query.query}")
        
        start_time = time.time()
        
        try:
            results = []
            metadata = {
                'query_type': 'hybrid' if query.use_hybrid else 'vector_only',
                'max_results': query.max_results,
                'context_depth': query.context_depth
            }
            
            if query.use_hybrid:
                # Hybrid search: combine vector search with graph traversal
                
                # 1. Vector search for semantic similarity
                vector_results = await self._vector_search(query.query, limit=query.max_results // 2)
                
                # 2. Graph search for structural relationships
                graph_results = await self._graph_search(query.query, limit=query.max_results // 2)
                
                # 3. Combine and rank results
                combined_results = self._combine_search_results(vector_results, graph_results, query.max_results)
                results = combined_results
                
                metadata['vector_results_count'] = len(vector_results)
                metadata['graph_results_count'] = len(graph_results)
                
            else:
                # Vector-only search
                vector_results = await self._vector_search(query.query, limit=query.max_results)
                results = vector_results
                
                metadata['search_type'] = 'vector_only'
            
            # Add context if requested
            if query.context_depth > 0:
                results = await self._add_context_to_results(results, query.context_depth)
                metadata['context_added'] = True
            
            execution_time = time.time() - start_time
            
            # Track with AI evaluation if available
            if ai_tracker and EvaluationCategory:
                ai_tracker.track_evaluation(
                    EvaluationCategory.RAG_QUERY,
                    {
                        'query': query.query,
                        'result_count': len(results),
                        'execution_time': execution_time,
                        'hybrid_search': query.use_hybrid
                    },
                    Sentiment.POSITIVE if results else Sentiment.NEUTRAL
                )
            
            self.logger.log_info(f"RAG query completed: {len(results)} results in {execution_time:.2f} seconds")
            
            return RAGResult(
                query=query.query,
                results=results,
                metadata=metadata,
                execution_time=execution_time,
                hybrid_search_used=query.use_hybrid
            )
            
        except Exception as e:
            execution_time = time.time() - start_time
            self.logger.log_error(f"RAG query failed after {execution_time:.2f} seconds: {e}")
            
            return RAGResult(
                query=query.query,
                results=[],
                metadata={'error': str(e)},
                execution_time=execution_time,
                hybrid_search_used=query.use_hybrid
            )
    
    async def update_embeddings(self, updated_entities: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Update embeddings for modified entities
        
        Args:
            updated_entities: List of updated code entities
            
        Returns:
            Update results
        """
        self.logger.log_info(f"Updating embeddings for {len(updated_entities)} entities")
        
        try:
            # For now, we'll recreate embeddings for updated entities
            # In a more sophisticated implementation, we would:
            # 1. Identify existing embeddings to update
            # 2. Update only changed embeddings
            # 3. Handle entity deletions
            
            result = await self.create_embeddings(updated_entities)
            result['update_type'] = 'full_recreation'
            
            return result
            
        except Exception as e:
            self.logger.log_error(f"Failed to update embeddings: {e}")
            return {
                'count': 0,
                'error': str(e),
                'update_type': 'failed'
            }
    
    async def semantic_search(self, query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Perform semantic search on code embeddings
        
        Args:
            query: Search query
            limit: Maximum number of results
            
        Returns:
            Semantic search results
        """
        return await self._vector_search(query, limit)
    
    async def get_related_context(self, entity_id: str, depth: int = 2) -> Dict[str, Any]:
        """
        Get related context for an entity
        
        Args:
            entity_id: Entity identifier
            depth: Context depth
            
        Returns:
            Related context information
        """
        try:
            # Use graph service to get related entities
            related_entities = await self.graph_service.get_related_entities(entity_id, depth)
            
            # Enhance with embedding similarity if available
            if self.embedding_model:
                # Get the original entity's embedding
                entity_results = self.collection.get(ids=[entity_id])
                
                if entity_results['embeddings']:
                    entity_embedding = entity_results['embeddings'][0]
                    
                    # Find similar entities
                    similar_results = self.collection.query(
                        query_embeddings=[entity_embedding],
                        n_results=min(10, len(related_entities))
                    )
                    
                    return {
                        'entity_id': entity_id,
                        'related_entities': related_entities,
                        'similar_entities': similar_results,
                        'context_depth': depth
                    }
            
            return {
                'entity_id': entity_id,
                'related_entities': related_entities,
                'context_depth': depth
            }
            
        except Exception as e:
            self.logger.log_error(f"Failed to get related context for {entity_id}: {e}")
            return {
                'entity_id': entity_id,
                'error': str(e)
            }
    
    async def _vector_search(self, query: str, limit: int) -> List[Dict[str, Any]]:
        """Perform vector similarity search"""
        try:
            # Generate query embedding
            query_embedding = await asyncio.to_thread(
                self.embedding_model.encode, [query]
            )
            
            # Search in ChromaDB
            results = self.collection.query(
                query_embeddings=query_embedding.tolist(),
                n_results=min(limit, 100)  # Limit to prevent excessive results
            )
            
            # Format results
            formatted_results = []
            for i, (doc, metadata, distance) in enumerate(zip(
                results['documents'][0],
                results['metadatas'][0],
                results['distances'][0]
            )):
                formatted_results.append({
                    'content': doc,
                    'metadata': metadata,
                    'similarity_score': 1 - distance,  # Convert distance to similarity
                    'search_type': 'vector',
                    'rank': i + 1
                })
            
            return formatted_results
            
        except Exception as e:
            self.logger.log_error(f"Vector search failed: {e}")
            return []
    
    async def _graph_search(self, query: str, limit: int) -> List[Dict[str, Any]]:
        """Perform graph-based search"""
        try:
            # Use graph service for structural search
            graph_results = await self.graph_service.query_entities(query, {'limit': limit})
            
            # Format results to match vector search format
            formatted_results = []
            for i, result in enumerate(graph_results):
                formatted_results.append({
                    'content': result.get('name', ''),
                    'metadata': {
                        'entity_name': result.get('name', ''),
                        'entity_type': result.get('type', ''),
                        'file_path': result.get('file_path', ''),
                        'properties': result.get('properties', {})
                    },
                    'similarity_score': result.get('score', 0.5),
                    'search_type': 'graph',
                    'rank': i + 1
                })
            
            return formatted_results
            
        except Exception as e:
            self.logger.log_error(f"Graph search failed: {e}")
            return []
    
    def _combine_search_results(self, vector_results: List[Dict[str, Any]], 
                               graph_results: List[Dict[str, Any]], 
                               max_results: int) -> List[Dict[str, Any]]:
        """Combine and rank vector and graph search results"""
        try:
            # Simple combination strategy: interleave results by score
            all_results = vector_results + graph_results
            
            # Sort by similarity score (descending)
            all_results.sort(key=lambda x: x.get('similarity_score', 0), reverse=True)
            
            # Remove duplicates based on entity name
            seen_entities = set()
            unique_results = []
            
            for result in all_results:
                entity_key = result.get('metadata', {}).get('entity_name', result.get('content', ''))
                if entity_key not in seen_entities:
                    seen_entities.add(entity_key)
                    unique_results.append(result)
                    
                    if len(unique_results) >= max_results:
                        break
            
            # Update ranks after deduplication
            for i, result in enumerate(unique_results):
                result['rank'] = i + 1
                result['search_type'] = 'hybrid'
            
            return unique_results
            
        except Exception as e:
            self.logger.log_error(f"Failed to combine search results: {e}")
            return (vector_results + graph_results)[:max_results]
    
    async def _add_context_to_results(self, results: List[Dict[str, Any]], depth: int) -> List[Dict[str, Any]]:
        """Add contextual information to search results"""
        try:
            enhanced_results = []
            
            for result in results:
                enhanced_result = result.copy()
                
                # Get entity ID from metadata
                entity_name = result.get('metadata', {}).get('entity_name')
                if entity_name:
                    # Get related context
                    context = await self.get_related_context(entity_name, depth)
                    enhanced_result['context'] = context
                
                enhanced_results.append(enhanced_result)
            
            return enhanced_results
            
        except Exception as e:
            self.logger.log_error(f"Failed to add context to results: {e}")
            return results
    
    async def health_check(self) -> bool:
        """Check if the RAG service is healthy"""
        try:
            # Check if components are initialized
            if not self.embedding_model or not self.chroma_client:
                return False
            
            # Perform a simple query to test functionality
            test_results = await self.semantic_search("test query", limit=1)
            
            return True
            
        except Exception as e:
            self.logger.log_error(f"RAG service health check failed: {e}")
            return False