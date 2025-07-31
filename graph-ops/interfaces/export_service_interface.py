"""
Export Service Interface - Graph export operations
"""
from abc import ABC, abstractmethod
from typing import Dict, Any


class ExportServiceInterface(ABC):
    """Interface for graph export functionality"""
    
    @abstractmethod
    def export_d3_format(self) -> Dict[str, Any]:
        """Export graph in D3.js compatible format"""
        pass
    
    @abstractmethod
    def export_mermaid_format(self) -> str:
        """Export graph in Mermaid diagram format"""
        pass
    
    @abstractmethod
    def export_simple_format(self) -> Dict[str, Any]:
        """Export graph in simple JSON format"""
        pass
    
    @abstractmethod
    def export_graphml_format(self) -> str:
        """Export graph in GraphML format"""
        pass
    
    @abstractmethod
    def export_cypher_queries(self) -> List[str]:
        """Export graph as Cypher CREATE statements"""
        pass