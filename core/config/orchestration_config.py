"""
Orchestration Configuration

Configuration management for core orchestration services.
"""

import os
from typing import Dict, Any, List, Optional
from dataclasses import dataclass, field
from pathlib import Path


@dataclass
class PipelineConfig:
    """Configuration for pipeline operations"""
    max_retries: int = 3
    timeout_seconds: int = 300
    enable_ai_descriptions: bool = True
    enable_relationship_extraction: bool = True
    enable_embeddings: bool = True
    batch_size: int = 100
    parallel_processing: bool = True


@dataclass
class RAGConfig:
    """Configuration for RAG operations"""
    embedding_model: str = "all-MiniLM-L6-v2"
    chroma_path: str = "./chroma_db"
    max_query_results: int = 10
    context_depth: int = 2
    hybrid_search_enabled: bool = True
    semantic_threshold: float = 0.7


@dataclass
class OrchestrationConfig:
    """Main orchestration configuration"""
    # Pipeline settings
    pipeline: PipelineConfig = field(default_factory=PipelineConfig)
    
    # RAG settings
    rag: RAGConfig = field(default_factory=RAGConfig)
    
    # Service settings
    service_timeout: int = 60
    max_concurrent_operations: int = 5
    enable_performance_monitoring: bool = True
    
    # Logging settings
    log_level: str = "INFO"
    log_to_file: bool = True
    log_directory: str = "./logs"
    
    @classmethod
    def from_env(cls) -> 'OrchestrationConfig':
        """Create configuration from environment variables"""
        config = cls()
        
        # Pipeline configuration
        config.pipeline.max_retries = int(os.getenv('PIPELINE_MAX_RETRIES', '3'))
        config.pipeline.timeout_seconds = int(os.getenv('PIPELINE_TIMEOUT', '300'))
        config.pipeline.enable_ai_descriptions = os.getenv('ENABLE_AI_DESCRIPTIONS', 'true').lower() == 'true'
        config.pipeline.enable_relationship_extraction = os.getenv('ENABLE_RELATIONSHIP_EXTRACTION', 'true').lower() == 'true'
        config.pipeline.enable_embeddings = os.getenv('ENABLE_EMBEDDINGS', 'true').lower() == 'true'
        config.pipeline.batch_size = int(os.getenv('PIPELINE_BATCH_SIZE', '100'))
        config.pipeline.parallel_processing = os.getenv('PIPELINE_PARALLEL', 'true').lower() == 'true'
        
        # RAG configuration
        config.rag.embedding_model = os.getenv('EMBEDDING_MODEL', 'all-MiniLM-L6-v2')
        config.rag.chroma_path = os.getenv('CHROMA_PATH', './chroma_db')
        config.rag.max_query_results = int(os.getenv('RAG_MAX_RESULTS', '10'))
        config.rag.context_depth = int(os.getenv('RAG_CONTEXT_DEPTH', '2'))
        config.rag.hybrid_search_enabled = os.getenv('RAG_HYBRID_SEARCH', 'true').lower() == 'true'
        config.rag.semantic_threshold = float(os.getenv('RAG_SEMANTIC_THRESHOLD', '0.7'))
        
        # Service configuration
        config.service_timeout = int(os.getenv('SERVICE_TIMEOUT', '60'))
        config.max_concurrent_operations = int(os.getenv('MAX_CONCURRENT_OPS', '5'))
        config.enable_performance_monitoring = os.getenv('ENABLE_PERFORMANCE_MONITORING', 'true').lower() == 'true'
        
        # Logging configuration
        config.log_level = os.getenv('LOG_LEVEL', 'INFO')
        config.log_to_file = os.getenv('LOG_TO_FILE', 'true').lower() == 'true'
        config.log_directory = os.getenv('LOG_DIRECTORY', './logs')
        
        return config
    
    def validate(self) -> List[str]:
        """Validate configuration and return any errors"""
        errors = []
        
        if self.pipeline.max_retries < 0:
            errors.append("Pipeline max_retries must be >= 0")
        
        if self.pipeline.timeout_seconds <= 0:
            errors.append("Pipeline timeout_seconds must be > 0")
        
        if self.pipeline.batch_size <= 0:
            errors.append("Pipeline batch_size must be > 0")
        
        if self.rag.max_query_results <= 0:
            errors.append("RAG max_query_results must be > 0")
        
        if self.rag.context_depth < 0:
            errors.append("RAG context_depth must be >= 0")
        
        if not (0.0 <= self.rag.semantic_threshold <= 1.0):
            errors.append("RAG semantic_threshold must be between 0.0 and 1.0")
        
        if self.service_timeout <= 0:
            errors.append("Service timeout must be > 0")
        
        if self.max_concurrent_operations <= 0:
            errors.append("Max concurrent operations must be > 0")
        
        # Check if log directory exists or can be created
        try:
            Path(self.log_directory).mkdir(parents=True, exist_ok=True)
        except Exception as e:
            errors.append(f"Cannot create log directory {self.log_directory}: {e}")
        
        return errors
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert configuration to dictionary"""
        return {
            'pipeline': {
                'max_retries': self.pipeline.max_retries,
                'timeout_seconds': self.pipeline.timeout_seconds,
                'enable_ai_descriptions': self.pipeline.enable_ai_descriptions,
                'enable_relationship_extraction': self.pipeline.enable_relationship_extraction,
                'enable_embeddings': self.pipeline.enable_embeddings,
                'batch_size': self.pipeline.batch_size,
                'parallel_processing': self.pipeline.parallel_processing
            },
            'rag': {
                'embedding_model': self.rag.embedding_model,
                'chroma_path': self.rag.chroma_path,
                'max_query_results': self.rag.max_query_results,
                'context_depth': self.rag.context_depth,
                'hybrid_search_enabled': self.rag.hybrid_search_enabled,
                'semantic_threshold': self.rag.semantic_threshold
            },
            'service_timeout': self.service_timeout,
            'max_concurrent_operations': self.max_concurrent_operations,
            'enable_performance_monitoring': self.enable_performance_monitoring,
            'log_level': self.log_level,
            'log_to_file': self.log_to_file,
            'log_directory': self.log_directory
        }