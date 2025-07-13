import os
from typing import List, Dict, Any, Optional
import chromadb
from sentence_transformers import SentenceTransformer
from langchain.text_splitter import RecursiveCharacterTextSplitter
from graph_manager import CodeGraphManager
from gemini_client import GeminiClient
from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment
import time
import numpy as np

class CodeRAGPipeline:
    """RAG pipeline for code-to-graph-db queries with comprehensive logging"""
    
    def __init__(self, 
                 graph_manager: CodeGraphManager,
                 embedding_model: str = "all-MiniLM-L6-v2",
                 chroma_path: str = "./chroma_db",
                 gemini_api_key: Optional[str] = None):
        
        self.graph_manager = graph_manager
        self.session_logger = logger.create_session_logger("RAGPipeline")
        
        self.session_logger.log_operation_start(
            "RAGPipeline.init",
            {
                "embedding_model": embedding_model,
                "chroma_path": chroma_path
            }
        )
        
        start_time = time.time()
        
        try:
            # Initialize embedding model
            self.session_logger.log_info("Loading sentence transformer model...")
            self.embedding_model = SentenceTransformer(embedding_model)
            
            # Initialize ChromaDB
            self.session_logger.log_info("Initializing ChromaDB...")
            self.chroma_client = chromadb.PersistentClient(path=chroma_path)
            self.collection = self.chroma_client.get_or_create_collection(
                name="code_embeddings",
                metadata={"description": "Code embeddings for RAG"}
            )
            
            # Initialize text splitter
            self.text_splitter = RecursiveCharacterTextSplitter(
                chunk_size=1000,
                chunk_overlap=200,
                length_function=len,
                separators=["\n\n", "\n", " ", ""]
            )
            
            # Initialize Gemini client
            self.session_logger.log_info("Initializing Gemini LLM client...")
            try:
                self.gemini_client = GeminiClient(api_key=gemini_api_key)
                self.llm_available = True
                self.session_logger.log_info("Gemini client initialized successfully")
            except Exception as e:
                self.session_logger.log_error(e, {"context": "gemini_initialization"})
                self.gemini_client = None
                self.llm_available = False
                self.session_logger.log_info("RAG pipeline will work without LLM generation capabilities")
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "RAGPipeline.init",
                duration=duration,
                success=True,
                details={
                    "model_loaded": True,
                    "chroma_initialized": True,
                    "collection_name": "code_embeddings"
                }
            )
            
            # Track successful initialization
            ai_tracker.record_success(
                component="rag_pipeline_init",
                description="Successfully initialized RAG pipeline with embeddings and vector store",
                time_saved=duration
            )
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "RAGPipeline.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Track initialization failure
            ai_tracker.record_failure(
                component="rag_pipeline_init",
                description=f"Failed to initialize RAG pipeline: {str(e)}",
                error_type=type(e).__name__,
                workaround="Check if models can be downloaded and ChromaDB can be created"
            )
            raise
    
    def index_code_content(self, content: str, metadata: Dict[str, Any]) -> bool:
        """Index code content with embeddings"""
        self.session_logger.log_operation_start(
            "index_code_content",
            {
                "content_length": len(content),
                "metadata": metadata
            }
        )
        
        start_time = time.time()
        
        try:
            # Split content into chunks
            chunks = self.text_splitter.split_text(content)
            
            self.session_logger.log_decision(
                decision=f"Split content into {len(chunks)} chunks",
                reasoning="Large content needs to be chunked for better retrieval granularity",
                alternatives=["Index as single document", "Use different chunk sizes"]
            )
            
            # Generate embeddings
            self.session_logger.log_info(f"Generating embeddings for {len(chunks)} chunks...")
            embeddings = self.embedding_model.encode(chunks)
            
            # Create IDs and prepare documents
            ids = [f"{metadata.get('file_path', 'unknown')}_{i}" for i in range(len(chunks))]
            documents = chunks
            metadatas = [dict(metadata, chunk_index=i) for i in range(len(chunks))]
            
            # Add to collection
            self.collection.add(
                documents=documents,
                embeddings=embeddings.tolist(),
                metadatas=metadatas,
                ids=ids
            )
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "index_code_content",
                duration=duration,
                success=True,
                details={
                    "chunks_created": len(chunks),
                    "embeddings_generated": len(embeddings),
                    "indexed_documents": len(documents)
                }
            )
            
            # Log performance metrics
            self.session_logger.log_performance(
                metric="content_indexing_time",
                value=duration * 1000,
                unit="ms",
                context={
                    "content_length": len(content),
                    "chunks": len(chunks),
                    "file": metadata.get('file_path', 'unknown')
                }
            )
            
            # Track successful indexing
            ai_tracker.record_success(
                component="content_indexing",
                description=f"Indexed {len(chunks)} chunks from {metadata.get('file_path')}",
                time_saved=duration
            )
            
            return True
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "index_code_content",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {
                "content_length": len(content),
                "metadata": metadata
            })
            
            # Track indexing failure
            ai_tracker.record_failure(
                component="content_indexing",
                description=f"Failed to index content: {str(e)}",
                error_type=type(e).__name__
            )
            
            return False
    
    def retrieve_relevant_content(self, query: str, k: int = 5) -> List[Dict[str, Any]]:
        """Retrieve relevant content for a query"""
        self.session_logger.log_operation_start(
            "retrieve_relevant_content",
            {"query": query, "k": k}
        )
        
        start_time = time.time()
        
        try:
            # Generate query embedding
            query_embedding = self.embedding_model.encode([query])
            
            # Search vector store
            results = self.collection.query(
                query_embeddings=query_embedding.tolist(),
                n_results=k
            )
            
            # Process results
            relevant_content = []
            distances = results.get('distances', [[]])[0]
            documents = results.get('documents', [[]])[0]
            metadatas = results.get('metadatas', [[]])[0]
            
            for i, doc in enumerate(documents):
                relevance_score = 1.0 - distances[i] if i < len(distances) else 0.0
                
                relevant_content.append({
                    "content": doc,
                    "metadata": metadatas[i] if i < len(metadatas) else {},
                    "relevance_score": relevance_score,
                    "distance": distances[i] if i < len(distances) else 1.0
                })
            
            # Sort by relevance
            relevant_content.sort(key=lambda x: x["relevance_score"], reverse=True)
            
            duration = time.time() - start_time
            
            # Log RAG operation
            relevance_scores = [item["relevance_score"] for item in relevant_content]
            self.session_logger.log_rag_operation(
                operation="vector_retrieval",
                query=query,
                results_count=len(relevant_content),
                relevance_scores=relevance_scores
            )
            
            self.session_logger.log_operation_end(
                "retrieve_relevant_content",
                duration=duration,
                success=True,
                details={
                    "results_returned": len(relevant_content),
                    "avg_relevance": np.mean(relevance_scores) if relevance_scores else 0
                }
            )
            
            # Log performance
            self.session_logger.log_performance(
                metric="vector_retrieval_time",
                value=duration * 1000,
                unit="ms",
                context={
                    "query_length": len(query),
                    "results": len(relevant_content),
                    "k": k
                }
            )
            
            # Evaluate retrieval quality
            avg_relevance = np.mean(relevance_scores) if relevance_scores else 0
            if avg_relevance > 0.7:
                ai_tracker.record_success(
                    component="vector_retrieval",
                    description=f"High quality retrieval (avg relevance: {avg_relevance:.3f})",
                    accuracy=avg_relevance * 100
                )
            elif avg_relevance < 0.3:
                ai_tracker.record_observation(
                    component="vector_retrieval",
                    observation=f"Low relevance scores (avg: {avg_relevance:.3f}) - may need better chunking or embeddings",
                    category=EvaluationCategory.ACCURACY
                )
            
            return relevant_content
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "retrieve_relevant_content",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {"query": query})
            
            # Track retrieval failure
            ai_tracker.record_failure(
                component="vector_retrieval",
                description=f"Failed to retrieve content: {str(e)}",
                error_type=type(e).__name__
            )
            
            return []
    
    def hybrid_retrieve(self, query: str, k: int = 10) -> List[Dict[str, Any]]:
        """Combine graph and vector retrieval for better results"""
        self.session_logger.log_operation_start(
            "hybrid_retrieve",
            {"query": query, "k": k}
        )
        
        start_time = time.time()
        
        try:
            self.session_logger.log_decision(
                decision="Using hybrid retrieval approach",
                reasoning="Combining graph structure with semantic similarity for better results",
                alternatives=["Graph-only retrieval", "Vector-only retrieval"]
            )
            
            # Get vector results
            vector_results = self.retrieve_relevant_content(query, k=k//2)
            
            # Get graph results
            graph_results = self.graph_manager.query_for_rag(query, limit=k//2)
            
            # Combine and re-rank results
            combined_results = []
            
            # Add vector results
            for result in vector_results:
                combined_results.append({
                    **result,
                    "source": "vector",
                    "hybrid_score": result["relevance_score"] * 0.6  # Weight vector results
                })
            
            # Add graph results
            for result in graph_results:
                # Convert graph result to common format
                content = self._graph_result_to_text(result)
                combined_results.append({
                    "content": content,
                    "metadata": result.get("properties", {}),
                    "relevance_score": result.get("relevance_score", 0.5),
                    "source": "graph",
                    "hybrid_score": result.get("relevance_score", 0.5) * 0.4  # Weight graph results
                })
            
            # Sort by hybrid score
            combined_results.sort(key=lambda x: x["hybrid_score"], reverse=True)
            
            # Take top k
            final_results = combined_results[:k]
            
            duration = time.time() - start_time
            
            # Calculate metrics
            vector_count = sum(1 for r in final_results if r["source"] == "vector")
            graph_count = sum(1 for r in final_results if r["source"] == "graph")
            avg_score = np.mean([r["hybrid_score"] for r in final_results]) if final_results else 0
            
            self.session_logger.log_rag_operation(
                operation="hybrid_retrieval",
                query=query,
                results_count=len(final_results),
                relevance_scores=[r["hybrid_score"] for r in final_results]
            )
            
            self.session_logger.log_operation_end(
                "hybrid_retrieve",
                duration=duration,
                success=True,
                details={
                    "vector_results": vector_count,
                    "graph_results": graph_count,
                    "total_results": len(final_results),
                    "avg_hybrid_score": avg_score
                }
            )
            
            # Track hybrid retrieval performance
            if avg_score > 0.6:
                ai_tracker.record_success(
                    component="hybrid_retrieval",
                    description=f"Successful hybrid retrieval with {vector_count} vector + {graph_count} graph results",
                    accuracy=avg_score * 100
                )
            
            return final_results
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "hybrid_retrieve",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {"query": query})
            
            # Track hybrid retrieval failure
            ai_tracker.record_failure(
                component="hybrid_retrieval",
                description=f"Failed hybrid retrieval: {str(e)}",
                error_type=type(e).__name__
            )
            
            return []
    
    def _graph_result_to_text(self, graph_result: Dict[str, Any]) -> str:
        """Convert graph result to text for unified processing"""
        properties = graph_result.get("properties", {})
        labels = graph_result.get("labels", [])
        
        # Create a text representation
        text_parts = []
        if labels:
            text_parts.append(f"Type: {', '.join(labels)}")
        
        for key, value in properties.items():
            if key not in ["created_at", "entity_type"]:
                text_parts.append(f"{key}: {value}")
        
        return " | ".join(text_parts)
    
    def get_collection_stats(self) -> Dict[str, Any]:
        """Get statistics about the indexed content"""
        try:
            count = self.collection.count()
            return {
                "total_documents": count,
                "collection_name": self.collection.name
            }
        except Exception as e:
            self.session_logger.log_error(e)
            return {"error": str(e)}
    
    def answer_question(self, question: str, use_hybrid: bool = True) -> Dict[str, Any]:
        """End-to-end question answering using RAG + LLM generation"""
        self.session_logger.log_operation_start(
            "answer_question",
            {"question": question, "use_hybrid": use_hybrid, "llm_available": self.llm_available}
        )
        
        start_time = time.time()
        
        try:
            # Step 1: Retrieve relevant context
            if use_hybrid:
                retrieved_context = self.hybrid_retrieve(question, k=8)
            else:
                retrieved_context = self.retrieve_relevant_content(question, k=8)
            
            self.session_logger.log_decision(
                decision=f"Retrieved {len(retrieved_context)} context items",
                reasoning="More context should provide better foundation for answer generation",
                alternatives=["Use fewer context items", "Use different retrieval strategy"]
            )
            
            # Step 2: Generate answer using LLM if available
            if self.llm_available and self.gemini_client:
                llm_response = self.gemini_client.generate_code_response(question, retrieved_context)
                
                duration = time.time() - start_time
                
                result = {
                    "question": question,
                    "answer": llm_response.get("response", ""),
                    "sources": retrieved_context,
                    "llm_metadata": {
                        "model": llm_response.get("model", "unknown"),
                        "processing_time": llm_response.get("processing_time", 0),
                        "tokens_used": llm_response.get("total_tokens", 0),
                        "context_used": llm_response.get("context_used", False)
                    },
                    "retrieval_method": "hybrid" if use_hybrid else "vector_only",
                    "total_processing_time": duration,
                    "success": "error" not in llm_response
                }
                
                # Log the full RAG operation
                self.session_logger.log_rag_operation(
                    operation="question_answering",
                    query=question,
                    results_count=1,
                    relevance_scores=[0.9] if result["success"] else [0.3]
                )
                
                # Evaluate answer quality
                if result["success"] and len(result["answer"]) > 100:
                    ai_tracker.record_success(
                        component="rag_question_answering",
                        description=f"Generated comprehensive answer ({len(result['answer'])} chars) with {len(retrieved_context)} sources",
                        time_saved=duration,
                        accuracy=90.0  # Placeholder - would implement actual answer quality metrics
                    )
                else:
                    ai_tracker.record_observation(
                        component="rag_question_answering",
                        observation=f"Generated answer may be insufficient (length: {len(result.get('answer', ''))}, success: {result['success']})",
                        category=EvaluationCategory.ACCURACY
                    )
                
            else:
                # Fallback: return context without LLM generation
                result = {
                    "question": question,
                    "answer": "LLM not available - here are the relevant code snippets:",
                    "sources": retrieved_context,
                    "llm_metadata": {"available": False},
                    "retrieval_method": "hybrid" if use_hybrid else "vector_only",
                    "total_processing_time": time.time() - start_time,
                    "success": True
                }
                
                ai_tracker.record_observation(
                    component="rag_question_answering",
                    observation="Returned context without LLM generation due to unavailable client",
                    category=EvaluationCategory.FUNCTIONALITY
                )
            
            self.session_logger.log_operation_end(
                "answer_question",
                duration=result["total_processing_time"],
                success=result["success"],
                details={
                    "context_items": len(retrieved_context),
                    "answer_length": len(result["answer"]),
                    "llm_used": self.llm_available
                }
            )
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "answer_question",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {"question": question})
            
            # Track question answering failure
            ai_tracker.record_failure(
                component="rag_question_answering",
                description=f"Failed to answer question: {str(e)}",
                error_type=type(e).__name__
            )
            
            return {
                "question": question,
                "answer": f"Error occurred while processing question: {str(e)}",
                "sources": [],
                "llm_metadata": {"error": str(e)},
                "retrieval_method": "hybrid" if use_hybrid else "vector_only",
                "total_processing_time": duration,
                "success": False
            }

    def clear_collection(self):
        """Clear all indexed content (for development/testing)"""
        self.session_logger.log_info("Clearing vector collection")
        try:
            # Delete and recreate collection
            self.chroma_client.delete_collection(name="code_embeddings")
            self.collection = self.chroma_client.create_collection(
                name="code_embeddings",
                metadata={"description": "Code embeddings for RAG"}
            )
            self.session_logger.log_info("Collection cleared successfully")
        except Exception as e:
            self.session_logger.log_error(e)