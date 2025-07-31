"""
RAG Service Interface

Defines the contract for Retrieval-Augmented Generation services.
"""

from abc import ABC, abstractmethod
from typing import List, Dict, Any, Optional
from dataclasses import dataclass


@dataclass
class RAGQuery:
    """RAG query configuration"""
    query: str
    max_results: int = 10
    context_depth: int = 2
    use_hybrid: bool = True
    filters: Optional[Dict[str, Any]] = None


@dataclass
class RAGResult:
    """RAG query result"""
    query: str
    results: List[Dict[str, Any]]
    metadata: Dict[str, Any]
    execution_time: float = 0.0
    hybrid_search_used: bool = False
    
    @property
    def result_count(self) -> int:
        return len(self.results)


class RAGServiceInterface(ABC):
    """Interface for RAG operations"""
    
    @abstractmethod
    async def create_embeddings(self, entities: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Create embeddings for code entities
        
        Args:
            entities: List of code entities to embed
            
        Returns:
            Embedding creation results
        """
        pass
    
    @abstractmethod
    async def query_code_graph(self, query: RAGQuery) -> RAGResult:
        """
        Query the code graph using RAG
        
        Args:
            query: RAG query configuration
            
        Returns:
            Query results with metadata
        """
        pass
    
    @abstractmethod
    async def update_embeddings(self, updated_entities: List[Dict[str, Any]]) -> Dict[str, Any]:
        """
        Update embeddings for modified entities
        
        Args:
            updated_entities: List of updated code entities
            
        Returns:
            Update results
        """
        pass
    
    @abstractmethod
    async def semantic_search(self, query: str, limit: int = 10) -> List[Dict[str, Any]]:
        """
        Perform semantic search on code embeddings
        
        Args:
            query: Search query
            limit: Maximum number of results
            
        Returns:
            Semantic search results
        """
        pass
    
    @abstractmethod
    async def get_related_context(self, entity_id: str, depth: int = 2) -> Dict[str, Any]:
        """
        Get related context for an entity
        
        Args:
            entity_id: Entity identifier
            depth: Context depth
            
        Returns:
            Related context information
        """
        pass