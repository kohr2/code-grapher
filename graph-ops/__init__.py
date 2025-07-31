"""
Graph Operations Vertical Slice

High-risk phase for migrating CodeGraphManager to service-oriented architecture.
This package wraps existing graph_manager.py functionality while providing
clean interfaces for dependency injection.
"""

# Make imports available at package level
from .interfaces.graph_repository_interface import GraphRepositoryInterface, GraphNode, GraphRelationship
from .interfaces.graph_service_interface import GraphOperationsInterface
from .interfaces.export_service_interface import ExportServiceInterface
from .repositories.connection_manager import ConnectionManager
from .repositories.neo4j_repository import Neo4jRepository, GraphOperationError
from .services.graph_service import GraphService
from .services.export_service import ExportService
from .config.database_config import DatabaseConfig

__all__ = [
    "GraphRepositoryInterface", 
    "GraphNode", 
    "GraphRelationship",
    "GraphOperationsInterface",
    "ExportServiceInterface",
    "ConnectionManager",
    "Neo4jRepository",
    "GraphOperationError",
    "GraphService", 
    "ExportService",
    "DatabaseConfig"
]