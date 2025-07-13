#!/usr/bin/env python3
"""
Create initial graph snapshot before implementing surgical updates.
This preserves the current state for rollback and comparison.
"""

import os
import sys
from datetime import datetime

# Add current directory to path to import our modules
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from graph_manager import CodeGraphManager
from graph_state_manager import GraphStateManager
from logger import logger


def main():
    """Create initial snapshot of current graph state"""
    session_logger = logger.create_session_logger("InitialSnapshot")
    
    session_logger.log_info("Starting initial graph snapshot creation")
    
    try:
        # Initialize graph manager
        session_logger.log_info("Connecting to graph database")
        graph_manager = CodeGraphManager()
        
        # Get current graph stats before snapshot
        current_stats = graph_manager.get_graph_stats()
        session_logger.log_info(f"Current graph stats: {current_stats}")
        
        # Initialize state manager
        state_manager = GraphStateManager(graph_manager)
        
        # Create initial snapshot
        snapshot_id = state_manager.create_snapshot("initial_pre_surgical_updates")
        
        session_logger.log_info(f"Successfully created initial snapshot: {snapshot_id}")
        
        # List all snapshots to confirm
        snapshots = state_manager.list_snapshots()
        session_logger.log_info(f"Available snapshots: {len(snapshots)}")
        
        for snapshot in snapshots:
            print(f"Snapshot: {snapshot['snapshot_id']} created at {snapshot['created_at']}")
        
        # Clean up connections
        graph_manager.close()
        
        print(f"\n✅ Initial snapshot created successfully: {snapshot_id}")
        print(f"📊 Graph contains: {current_stats.get('total_nodes', 0)} nodes, {current_stats.get('total_relationships', 0)} relationships")
        print(f"📁 Snapshot saved to: graph_snapshots/{snapshot_id}")
        
        return True
        
    except Exception as e:
        session_logger.log_error(e)
        print(f"❌ Failed to create initial snapshot: {str(e)}")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)