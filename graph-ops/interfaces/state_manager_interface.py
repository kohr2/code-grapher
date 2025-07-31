"""
State Manager Interface - Graph state management
"""
from abc import ABC, abstractmethod
from typing import Dict, Any, List
from datetime import datetime


class StateManagerInterface(ABC):
    """Interface for graph state management operations"""
    
    @abstractmethod
    def save_graph_snapshot(self, snapshot_name: str, metadata: Dict[str, Any] = None) -> str:
        """Save current graph state as a snapshot"""
        pass
    
    @abstractmethod
    def restore_graph_snapshot(self, snapshot_id: str) -> bool:
        """Restore graph to a previous snapshot"""
        pass
    
    @abstractmethod
    def list_snapshots(self) -> List[Dict[str, Any]]:
        """List all available snapshots"""
        pass
    
    @abstractmethod
    def delete_snapshot(self, snapshot_id: str) -> bool:
        """Delete a specific snapshot"""
        pass
    
    @abstractmethod
    def get_graph_state_info(self) -> Dict[str, Any]:
        """Get information about current graph state"""
        pass
    
    @abstractmethod
    def backup_graph(self, backup_path: str) -> bool:
        """Create a full backup of the graph"""
        pass
    
    @abstractmethod
    def validate_graph_consistency(self) -> Dict[str, Any]:
        """Validate graph consistency and return report"""
        pass