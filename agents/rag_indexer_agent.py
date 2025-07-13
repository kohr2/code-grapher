import time
import os
import hashlib
from typing import Dict, Any, List, Optional, Tuple
import json
import re
from pathlib import Path

from agents.base_agent import BaseAgent, AgentExecutionError, AgentConfigurationError

try:
    from sentence_transformers import SentenceTransformer
    SENTENCE_TRANSFORMERS_AVAILABLE = True
except ImportError:
    SENTENCE_TRANSFORMERS_AVAILABLE = False

try:
    import chromadb
    from chromadb.config import Settings
    CHROMADB_AVAILABLE = True
except ImportError:
    CHROMADB_AVAILABLE = False


class RAGIndexerAgent(BaseAgent):
    """
    Agent for indexing graph data for RAG (Retrieval-Augmented Generation)
    Creates vector embeddings and enables semantic search over code knowledge
    """
    
    def __init__(self, agent_id: str, config: Dict[str, Any], shared_state: Optional[Dict[str, Any]] = None):
        # Agent-specific configuration - set before super().__init__() to allow validation
        agent_config = config.get("config", {})
        self.embedding_config = agent_config.get("embeddingModel", {})
        self.vector_store_config = agent_config.get("vectorStore", {})
        self.chunking_config = agent_config.get("chunkingStrategy", {})
        self.indexing_options = agent_config.get("indexingOptions", {})
        
        # Embedding model configuration
        self.model_provider = self.embedding_config.get("provider", "sentence-transformers")
        self.model_name = self.embedding_config.get("model", "all-MiniLM-L6-v2")
        self.embedding_dimensions = self.embedding_config.get("dimensions", 384)
        
        # Vector store configuration
        self.vector_store_type = self.vector_store_config.get("type", "chromadb")
        self.vector_store_path = self.vector_store_config.get("path", "./chroma_db")
        self.collection_name = self.vector_store_config.get("collection", "code_embeddings")
        
        super().__init__(agent_id, config, shared_state)
        
        # Chunking strategy
        self.chunking_method = self.chunking_config.get("method", "semantic")
        self.max_chunk_size = self.chunking_config.get("maxChunkSize", 512)
        self.overlap = self.chunking_config.get("overlap", 50)
        self.preserve_code_blocks = self.chunking_config.get("preserveCodeBlocks", True)
        
        # Indexing options
        self.include_metadata = self.indexing_options.get("includeMetadata", True)
        self.generate_summaries = self.indexing_options.get("generateSummaries", True)
        self.extract_keywords = self.indexing_options.get("extractKeywords", True)
        
        # Initialize components
        self.embedding_model = None
        self.vector_store = None
        self.collection = None
        
        # Statistics
        self.documents_indexed = 0
        self.embeddings_generated = 0
        self.chunks_created = 0
        
        # Initialize embedding model and vector store
        self._initialize_embedding_model()
        self._initialize_vector_store()
        
        self.session_logger.log_info(
            f"RAGIndexerAgent initialized with model: {self.model_name}, store: {self.vector_store_type}"
        )
    
    def _validate_configuration(self) -> None:
        """Validate agent-specific configuration"""
        if not SENTENCE_TRANSFORMERS_AVAILABLE and self.model_provider == "sentence-transformers":
            raise AgentConfigurationError(
                "sentence-transformers not available. Install with: pip install sentence-transformers"
            )
        
        if not CHROMADB_AVAILABLE and self.vector_store_type == "chromadb":
            raise AgentConfigurationError(
                "chromadb not available. Install with: pip install chromadb"
            )
        
        if self.max_chunk_size < 50 or self.max_chunk_size > 2048:
            raise AgentConfigurationError("maxChunkSize must be between 50 and 2048")
        
        if self.overlap < 0 or self.overlap >= self.max_chunk_size:
            raise AgentConfigurationError("overlap must be between 0 and maxChunkSize")
        
        if self.embedding_dimensions < 100 or self.embedding_dimensions > 1536:
            raise AgentConfigurationError("embedding dimensions must be between 100 and 1536")
    
    def get_capabilities(self) -> List[str]:
        """Return list of agent capabilities"""
        return ["embedding-generation", "vector-indexing", "semantic-search"]
    
    def _initialize_embedding_model(self) -> None:
        """Initialize the embedding model"""
        if not SENTENCE_TRANSFORMERS_AVAILABLE:
            self.session_logger.log_error(
                Exception("SentenceTransformers not available"),
                {"workaround": "Install sentence-transformers: pip install sentence-transformers"}
            )
            return
        
        try:
            self.embedding_model = SentenceTransformer(self.model_name)
            
            # Verify model dimensions match configuration
            test_embedding = self.embedding_model.encode(["test"])
            actual_dimensions = test_embedding.shape[1]
            
            if actual_dimensions != self.embedding_dimensions:
                self.session_logger.log_decision(
                    decision=f"Updating embedding dimensions from {self.embedding_dimensions} to {actual_dimensions}",
                    reasoning=f"Model {self.model_name} produces {actual_dimensions}-dimensional embeddings",
                    alternatives=["Use different model", "Update configuration"]
                )
                self.embedding_dimensions = actual_dimensions
            
            self.session_logger.log_decision(
                decision=f"Successfully loaded embedding model {self.model_name}",
                reasoning="Model is required for generating semantic embeddings",
                alternatives=["Use OpenAI embeddings", "Use local transformer model"]
            )
            
        except Exception as e:
            self.session_logger.log_error(e, {"model_name": self.model_name})
            raise AgentConfigurationError(f"Failed to load embedding model: {str(e)}") from e
    
    def _initialize_vector_store(self) -> None:
        """Initialize the vector store"""
        if not CHROMADB_AVAILABLE:
            self.session_logger.log_error(
                Exception("ChromaDB not available"),
                {"workaround": "Install chromadb: pip install chromadb"}
            )
            return
        
        try:
            # Ensure vector store directory exists
            os.makedirs(self.vector_store_path, exist_ok=True)
            
            # Initialize ChromaDB client
            self.vector_store = chromadb.PersistentClient(
                path=self.vector_store_path,
                settings=Settings(anonymized_telemetry=False)
            )
            
            # Get or create collection
            self.collection = self.vector_store.get_or_create_collection(
                name=self.collection_name,
                metadata={
                    "description": "Code knowledge embeddings for RAG",
                    "embedding_model": self.model_name,
                    "dimensions": self.embedding_dimensions
                }
            )
            
            self.session_logger.log_decision(
                decision=f"Successfully initialized ChromaDB collection: {self.collection_name}",
                reasoning="Vector store is required for similarity search and retrieval",
                alternatives=["Use FAISS", "Use Pinecone", "Use Weaviate"]
            )
            
        except Exception as e:
            self.session_logger.log_error(e, {"vector_store_path": self.vector_store_path})
            raise AgentConfigurationError(f"Failed to initialize vector store: {str(e)}") from e
    
    def execute(self, input_data: Dict[str, Any]) -> Dict[str, Any]:
        """
        Execute RAG indexing on graph data
        
        Expected input format:
        {
            "graph": {...},           # From GraphBuilderAgent
            "entities": [...],        # From EntityExtractionAgent (optional)
            "relationships": [...]    # From RelationshipAnalysisAgent (optional)
        }
        """
        start_time = time.time()
        
        self.session_logger.log_operation_start(
            "RAGIndexer.execute",
            {
                "has_graph": "graph" in input_data,
                "entities_count": len(input_data.get("entities", [])),
                "relationships_count": len(input_data.get("relationships", [])),
                "chunking_method": self.chunking_method,
                "max_chunk_size": self.max_chunk_size
            }
        )
        
        try:
            # Reset counters
            self.documents_indexed = 0
            self.embeddings_generated = 0
            self.chunks_created = 0
            
            # Extract input data
            graph_data = input_data.get("graph", {})
            entities = input_data.get("entities", [])
            relationships = input_data.get("relationships", [])
            
            if not graph_data and not entities:
                raise AgentExecutionError("No graph data or entities provided for indexing")
            
            # Create documents from different data sources
            documents = []
            
            # Index entities
            if entities:
                entity_docs = self._create_documents_from_entities(entities)
                documents.extend(entity_docs)
            
            # Index relationships
            if relationships:
                relationship_docs = self._create_documents_from_relationships(relationships)
                documents.extend(relationship_docs)
            
            # Index graph statistics and metadata
            if graph_data:
                graph_docs = self._create_documents_from_graph(graph_data)
                documents.extend(graph_docs)
            
            # Chunk documents if needed
            chunked_documents = self._chunk_documents(documents)
            
            # Generate embeddings
            embeddings = self._generate_embeddings(chunked_documents)
            
            # Index documents in vector store
            self._index_documents(chunked_documents, embeddings)
            
            # Generate collection statistics
            collection_stats = self._get_collection_statistics()
            
            # Performance metrics
            indexing_performance = self._calculate_indexing_performance(start_time)
            
            duration = time.time() - start_time
            
            result = {
                "documentsIndexed": self.documents_indexed,
                "embeddingsGenerated": self.embeddings_generated,
                "collectionStats": collection_stats,
                "indexingPerformance": indexing_performance,
                "metadata": {
                    "embedding_model": self.model_name,
                    "embedding_dimensions": self.embedding_dimensions,
                    "chunking_method": self.chunking_method,
                    "max_chunk_size": self.max_chunk_size,
                    "collection_name": self.collection_name,
                    "processing_time": duration
                }
            }
            
            self.session_logger.log_operation_end(
                "RAGIndexer.execute",
                duration=duration,
                success=True,
                details={
                    "documents_indexed": self.documents_indexed,
                    "embeddings_generated": self.embeddings_generated,
                    "chunks_created": self.chunks_created
                }
            )
            
            # Log performance metrics
            self.log_agent_specific_metrics({
                "documents_per_second": self.documents_indexed / duration if duration > 0 else 0,
                "embeddings_per_second": self.embeddings_generated / duration if duration > 0 else 0,
                "average_chunk_size": sum(len(doc["content"]) for doc in chunked_documents) / len(chunked_documents) if chunked_documents else 0
            })
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            self.session_logger.log_operation_end(
                "RAGIndexer.execute",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            raise AgentExecutionError(f"RAG indexing failed: {str(e)}") from e
    
    def _create_documents_from_entities(self, entities: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Create documents from entity data"""
        documents = []
        
        for entity in entities:
            # Create primary document for entity
            content = self._format_entity_content(entity)
            
            if content.strip():
                doc = {
                    "id": f"entity_{entity.get('id', 'unknown')}",
                    "content": content,
                    "type": "entity",
                    "metadata": {
                        "entity_id": entity.get("id"),
                        "entity_name": entity.get("name"),
                        "entity_type": entity.get("type"),
                        "scope": entity.get("scope"),
                        "file_path": entity.get("location", {}).get("file"),
                        "line_number": entity.get("location", {}).get("line")
                    }
                }
                
                if self.include_metadata:
                    doc["metadata"].update(entity.get("attributes", {}))
                
                documents.append(doc)
                self.documents_indexed += 1
        
        return documents
    
    def _format_entity_content(self, entity: Dict[str, Any]) -> str:
        """Format entity data as searchable text content"""
        content_parts = []
        
        # Basic entity information
        entity_name = entity.get("name", "")
        entity_type = entity.get("type", "")
        entity_scope = entity.get("scope", "")
        
        content_parts.append(f"Entity: {entity_name}")
        content_parts.append(f"Type: {entity_type}")
        
        if entity_scope:
            content_parts.append(f"Scope: {entity_scope}")
        
        # Location information
        location = entity.get("location", {})
        if location:
            file_path = location.get("file", "")
            line_number = location.get("line", "")
            if file_path:
                content_parts.append(f"File: {file_path}")
            if line_number:
                content_parts.append(f"Line: {line_number}")
        
        # Docstring
        docstring = entity.get("docstring")
        if docstring:
            content_parts.append(f"Documentation: {docstring}")
        
        # Attributes
        attributes = entity.get("attributes", {})
        if attributes:
            for key, value in attributes.items():
                if isinstance(value, (str, int, float)):
                    content_parts.append(f"{key}: {value}")
                elif isinstance(value, list) and all(isinstance(v, str) for v in value):
                    content_parts.append(f"{key}: {', '.join(value)}")
        
        return "\n".join(content_parts)
    
    def _create_documents_from_relationships(self, relationships: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Create documents from relationship data"""
        documents = []
        
        for relationship in relationships:
            content = self._format_relationship_content(relationship)
            
            if content.strip():
                doc = {
                    "id": f"relationship_{relationship.get('id', 'unknown')}",
                    "content": content,
                    "type": "relationship",
                    "metadata": {
                        "relationship_id": relationship.get("id"),
                        "relationship_type": relationship.get("type"),
                        "source_id": relationship.get("source"),
                        "target_id": relationship.get("target"),
                        "strength": relationship.get("strength", 1.0),
                        "confidence": relationship.get("confidence", 1.0)
                    }
                }
                
                if self.include_metadata:
                    doc["metadata"].update(relationship.get("metadata", {}))
                
                documents.append(doc)
                self.documents_indexed += 1
        
        return documents
    
    def _format_relationship_content(self, relationship: Dict[str, Any]) -> str:
        """Format relationship data as searchable text content"""
        content_parts = []
        
        # Basic relationship information
        rel_type = relationship.get("type", "")
        source_id = relationship.get("source", "")
        target_id = relationship.get("target", "")
        
        content_parts.append(f"Relationship: {source_id} {rel_type} {target_id}")
        content_parts.append(f"Type: {rel_type}")
        
        # Strength and confidence
        strength = relationship.get("strength", 1.0)
        confidence = relationship.get("confidence", 1.0)
        content_parts.append(f"Strength: {strength}")
        content_parts.append(f"Confidence: {confidence}")
        
        # Metadata
        metadata = relationship.get("metadata", {})
        if metadata:
            for key, value in metadata.items():
                if isinstance(value, (str, int, float, bool)):
                    content_parts.append(f"{key}: {value}")
        
        return "\n".join(content_parts)
    
    def _create_documents_from_graph(self, graph_data: Dict[str, Any]) -> List[Dict[str, Any]]:
        """Create documents from graph metadata and statistics"""
        documents = []
        
        # Graph statistics document
        graph_stats = graph_data.get("graphStats", {})
        if graph_stats:
            content = self._format_graph_stats_content(graph_stats)
            
            doc = {
                "id": "graph_statistics",
                "content": content,
                "type": "graph_metadata",
                "metadata": {
                    "document_type": "graph_statistics",
                    "total_nodes": graph_stats.get("total_nodes", 0),
                    "total_relationships": graph_stats.get("total_relationships", 0),
                    "graph_density": graph_stats.get("graph_density", 0.0)
                }
            }
            
            documents.append(doc)
            self.documents_indexed += 1
        
        return documents
    
    def _format_graph_stats_content(self, graph_stats: Dict[str, Any]) -> str:
        """Format graph statistics as searchable content"""
        content_parts = []
        
        content_parts.append("Graph Statistics")
        
        total_nodes = graph_stats.get("total_nodes", 0)
        total_relationships = graph_stats.get("total_relationships", 0)
        graph_density = graph_stats.get("graph_density", 0.0)
        
        content_parts.append(f"Total nodes: {total_nodes}")
        content_parts.append(f"Total relationships: {total_relationships}")
        content_parts.append(f"Graph density: {graph_density:.4f}")
        
        # Nodes by label
        nodes_by_label = graph_stats.get("nodes_by_label", {})
        if nodes_by_label:
            content_parts.append("Node distribution:")
            for label, count in nodes_by_label.items():
                content_parts.append(f"  {label}: {count}")
        
        # Relationships by type
        relationships_by_type = graph_stats.get("relationships_by_type", {})
        if relationships_by_type:
            content_parts.append("Relationship distribution:")
            for rel_type, count in relationships_by_type.items():
                content_parts.append(f"  {rel_type}: {count}")
        
        return "\n".join(content_parts)
    
    def _chunk_documents(self, documents: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Chunk documents based on configured strategy"""
        if self.chunking_method == "none":
            return documents
        
        chunked_documents = []
        
        for doc in documents:
            content = doc["content"]
            
            if len(content) <= self.max_chunk_size:
                # Document is small enough, no chunking needed
                chunked_documents.append(doc)
                continue
            
            # Create chunks
            chunks = self._create_chunks(content)
            
            for i, chunk in enumerate(chunks):
                chunked_doc = {
                    "id": f"{doc['id']}_chunk_{i}",
                    "content": chunk,
                    "type": f"{doc['type']}_chunk",
                    "metadata": doc["metadata"].copy()
                }
                chunked_doc["metadata"]["chunk_index"] = i
                chunked_doc["metadata"]["total_chunks"] = len(chunks)
                chunked_doc["metadata"]["original_document_id"] = doc["id"]
                
                chunked_documents.append(chunked_doc)
                self.chunks_created += 1
        
        return chunked_documents
    
    def _create_chunks(self, content: str) -> List[str]:
        """Create chunks from content"""
        if self.chunking_method == "semantic":
            return self._create_semantic_chunks(content)
        else:
            return self._create_fixed_size_chunks(content)
    
    def _create_semantic_chunks(self, content: str) -> List[str]:
        """Create semantic chunks that preserve meaning"""
        # Split by paragraphs first
        paragraphs = content.split("\n\n")
        
        chunks = []
        current_chunk = ""
        
        for paragraph in paragraphs:
            # If adding this paragraph would exceed chunk size
            if len(current_chunk) + len(paragraph) + 2 > self.max_chunk_size:
                if current_chunk:
                    chunks.append(current_chunk.strip())
                    current_chunk = paragraph
                else:
                    # Single paragraph is too large, split it
                    sub_chunks = self._create_fixed_size_chunks(paragraph)
                    chunks.extend(sub_chunks)
            else:
                if current_chunk:
                    current_chunk += "\n\n" + paragraph
                else:
                    current_chunk = paragraph
        
        if current_chunk:
            chunks.append(current_chunk.strip())
        
        return chunks
    
    def _create_fixed_size_chunks(self, content: str) -> List[str]:
        """Create fixed-size chunks with overlap"""
        chunks = []
        start = 0
        
        while start < len(content):
            end = start + self.max_chunk_size
            
            # Try to end at word boundary
            if end < len(content):
                while end > start and content[end] != " " and content[end] != "\n":
                    end -= 1
                if end == start:
                    end = start + self.max_chunk_size
            
            chunk = content[start:end].strip()
            if chunk:
                chunks.append(chunk)
            
            # Move start position with overlap
            start = end - self.overlap
            if start <= 0 or start >= len(content):
                break
        
        return chunks
    
    def _generate_embeddings(self, documents: List[Dict[str, Any]]) -> List[List[float]]:
        """Generate embeddings for documents"""
        if not self.embedding_model:
            raise AgentExecutionError("Embedding model not initialized")
        
        contents = [doc["content"] for doc in documents]
        
        # Generate embeddings in batches for efficiency
        batch_size = 32
        embeddings = []
        
        for i in range(0, len(contents), batch_size):
            batch = contents[i:i + batch_size]
            batch_embeddings = self.embedding_model.encode(batch, convert_to_tensor=False)
            embeddings.extend(batch_embeddings.tolist())
            self.embeddings_generated += len(batch)
        
        return embeddings
    
    def _index_documents(self, documents: List[Dict[str, Any]], embeddings: List[List[float]]) -> None:
        """Index documents and embeddings in vector store"""
        if not self.collection:
            raise AgentExecutionError("Vector store collection not initialized")
        
        # Prepare data for ChromaDB
        ids = [doc["id"] for doc in documents]
        metadatas = [doc["metadata"] for doc in documents]
        documents_content = [doc["content"] for doc in documents]
        
        # Add documents to collection
        self.collection.add(
            ids=ids,
            embeddings=embeddings,
            metadatas=metadatas,
            documents=documents_content
        )
        
        self.session_logger.log_decision(
            decision=f"Successfully indexed {len(documents)} documents",
            reasoning="Documents indexed for semantic search and retrieval",
            alternatives=["Index in separate batches", "Use different vector store"]
        )
    
    def _get_collection_statistics(self) -> Dict[str, Any]:
        """Get statistics about the vector store collection"""
        if not self.collection:
            return {}
        
        try:
            count = self.collection.count()
            
            return {
                "total_documents": count,
                "collection_name": self.collection_name,
                "embedding_dimensions": self.embedding_dimensions,
                "model_name": self.model_name
            }
            
        except Exception as e:
            self.session_logger.log_error(e, {"context": "collection_statistics"})
            return {"error": "Failed to get collection statistics"}
    
    def _calculate_indexing_performance(self, start_time: float) -> Dict[str, Any]:
        """Calculate performance metrics for indexing operation"""
        duration = time.time() - start_time
        
        return {
            "total_time_seconds": duration,
            "documents_per_second": self.documents_indexed / duration if duration > 0 else 0,
            "embeddings_per_second": self.embeddings_generated / duration if duration > 0 else 0,
            "chunks_created": self.chunks_created,
            "average_time_per_document": duration / self.documents_indexed if self.documents_indexed > 0 else 0
        }
    
    def search(self, query: str, n_results: int = 10) -> List[Dict[str, Any]]:
        """Search for similar documents using vector similarity"""
        if not self.collection or not self.embedding_model:
            raise AgentExecutionError("Collection or embedding model not initialized")
        
        # Generate query embedding
        query_embedding = self.embedding_model.encode([query])
        
        # Search collection
        results = self.collection.query(
            query_embeddings=query_embedding.tolist(),
            n_results=n_results,
            include=["documents", "metadatas", "distances"]
        )
        
        # Format results
        formatted_results = []
        for i in range(len(results["documents"][0])):
            formatted_results.append({
                "document": results["documents"][0][i],
                "metadata": results["metadatas"][0][i],
                "distance": results["distances"][0][i]
            })
        
        return formatted_results