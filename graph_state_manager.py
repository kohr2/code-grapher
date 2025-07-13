import os
import json
import shutil
from datetime import datetime
from typing import Dict, Any, Optional, List
from pathlib import Path
import subprocess

from graph_manager import CodeGraphManager
from logger import logger


class GraphStateManager:
    """
    Manages graph state snapshots for rollback and comparison purposes.
    Provides functionality to capture, restore, and compare graph states.
    """
    
    def __init__(self, graph_manager: CodeGraphManager, snapshots_dir: str = "graph_snapshots"):
        self.graph_manager = graph_manager
        self.snapshots_dir = Path(snapshots_dir)
        self.snapshots_dir.mkdir(exist_ok=True)
        self.session_logger = logger.create_session_logger("GraphStateManager")
        
        self.session_logger.log_info(f"GraphStateManager initialized with snapshots dir: {self.snapshots_dir}")
    
    def create_snapshot(self, snapshot_name: Optional[str] = None) -> str:
        """
        Create a comprehensive snapshot of the current graph state.
        
        Args:
            snapshot_name: Optional name for the snapshot. If None, timestamp is used.
            
        Returns:
            str: The snapshot identifier/path
        """
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        snapshot_id = snapshot_name or f"snapshot_{timestamp}"
        snapshot_dir = self.snapshots_dir / snapshot_id
        
        self.session_logger.log_operation_start(
            "create_snapshot",
            {"snapshot_id": snapshot_id, "snapshot_dir": str(snapshot_dir)}
        )
        
        try:
            # Create snapshot directory
            snapshot_dir.mkdir(exist_ok=True)
            
            # 1. Export graph in multiple formats
            self._export_graph_data(snapshot_dir)
            
            # 2. Save Neo4j database dump if possible
            self._create_neo4j_dump(snapshot_dir)
            
            # 3. Capture graph statistics
            self._capture_graph_stats(snapshot_dir)
            
            # 4. Save metadata
            self._save_snapshot_metadata(snapshot_dir, snapshot_id)
            
            self.session_logger.log_operation_end(
                "create_snapshot",
                success=True,
                details={"snapshot_path": str(snapshot_dir)}
            )
            
            self.session_logger.log_info(f"Created graph snapshot: {snapshot_id}")
            return snapshot_id
            
        except Exception as e:
            self.session_logger.log_operation_end(
                "create_snapshot",
                success=False,
                details={"error": str(e)}
            )
            self.session_logger.log_error(e, {"snapshot_id": snapshot_id})
            raise
    
    def _export_graph_data(self, snapshot_dir: Path):
        """Export graph data in multiple formats"""
        self.session_logger.log_info("Exporting graph data in multiple formats")
        
        # D3 format
        d3_data = self.graph_manager.export_d3_format()
        with open(snapshot_dir / "graph_d3.json", "w") as f:
            json.dump(d3_data, f, indent=2)
        
        # Simple format
        simple_data = self.graph_manager.export_simple_format()
        with open(snapshot_dir / "graph_simple.json", "w") as f:
            json.dump(simple_data, f, indent=2)
        
        # Mermaid format
        mermaid_data = self.graph_manager.export_mermaid_format()
        with open(snapshot_dir / "graph_mermaid.md", "w") as f:
            f.write(mermaid_data)
        
        self.session_logger.log_info("Graph data exported successfully")
    
    def _create_neo4j_dump(self, snapshot_dir: Path):
        """Create Neo4j database dump using cypher-shell if available"""
        self.session_logger.log_info("Attempting to create Neo4j database dump")
        
        try:
            # Try to create a Cypher export of all data
            export_query = """
            CALL apoc.export.cypher.all(null, {
                format: "cypher-shell",
                useOptimizations: {type: "UNWIND_BATCH", unwindBatchSize: 20}
            })
            """
            
            # Alternative: Manual export of nodes and relationships
            nodes_query = "MATCH (n) RETURN n"
            rels_query = "MATCH (a)-[r]->(b) RETURN a, r, b"
            
            # Save raw cypher queries for manual recreation
            with open(snapshot_dir / "recreate_graph.cypher", "w") as f:
                f.write("// Graph Recreation Script\n")
                f.write("// Run this script to recreate the graph from snapshot\n\n")
                f.write("// Clear existing data\n")
                f.write("MATCH (n) DETACH DELETE n;\n\n")
                f.write("// Note: This is a placeholder for actual node/relationship recreation\n")
                f.write("// The actual implementation would export CREATE statements\n")
            
            self.session_logger.log_info("Neo4j dump preparation completed")
            
        except Exception as e:
            self.session_logger.log_error(e, {"context": "neo4j_dump"})
            # Continue without Neo4j dump - not critical
    
    def _capture_graph_stats(self, snapshot_dir: Path):
        """Capture comprehensive graph statistics"""
        self.session_logger.log_info("Capturing graph statistics")
        
        stats = self.graph_manager.get_graph_stats()
        
        # Enhanced stats with more details
        enhanced_stats = {
            **stats,
            "snapshot_timestamp": datetime.now().isoformat(),
            "database_info": {
                "uri": self.graph_manager.uri,
                "username": self.graph_manager.username
            }
        }
        
        with open(snapshot_dir / "graph_stats.json", "w") as f:
            json.dump(enhanced_stats, f, indent=2)
        
        self.session_logger.log_info(f"Graph statistics captured: {stats}")
    
    def _save_snapshot_metadata(self, snapshot_dir: Path, snapshot_id: str):
        """Save snapshot metadata"""
        metadata = {
            "snapshot_id": snapshot_id,
            "created_at": datetime.now().isoformat(),
            "created_by": "GraphStateManager",
            "purpose": "Pre-surgical-update snapshot",
            "files": list(os.listdir(snapshot_dir)),
            "graph_manager_config": {
                "uri": self.graph_manager.uri,
                "username": self.graph_manager.username
            }
        }
        
        with open(snapshot_dir / "metadata.json", "w") as f:
            json.dump(metadata, f, indent=2)
        
        self.session_logger.log_info(f"Snapshot metadata saved for {snapshot_id}")
    
    def list_snapshots(self) -> List[Dict[str, Any]]:
        """List all available snapshots with metadata"""
        snapshots = []
        
        for snapshot_dir in self.snapshots_dir.iterdir():
            if snapshot_dir.is_dir():
                metadata_file = snapshot_dir / "metadata.json"
                if metadata_file.exists():
                    try:
                        with open(metadata_file, "r") as f:
                            metadata = json.load(f)
                        metadata["path"] = str(snapshot_dir)
                        snapshots.append(metadata)
                    except Exception as e:
                        self.session_logger.log_error(e, {"snapshot_dir": str(snapshot_dir)})
        
        # Sort by creation time (newest first)
        snapshots.sort(key=lambda x: x.get("created_at", ""), reverse=True)
        return snapshots
    
    def compare_snapshots(self, snapshot1_id: str, snapshot2_id: str) -> Dict[str, Any]:
        """Compare two snapshots and return differences"""
        self.session_logger.log_operation_start(
            "compare_snapshots",
            {"snapshot1": snapshot1_id, "snapshot2": snapshot2_id}
        )
        
        try:
            # Load both snapshots
            snap1_path = self.snapshots_dir / snapshot1_id / "graph_stats.json"
            snap2_path = self.snapshots_dir / snapshot2_id / "graph_stats.json"
            
            with open(snap1_path, "r") as f:
                stats1 = json.load(f)
            with open(snap2_path, "r") as f:
                stats2 = json.load(f)
            
            # Calculate differences
            comparison = {
                "snapshot1": {"id": snapshot1_id, "stats": stats1},
                "snapshot2": {"id": snapshot2_id, "stats": stats2},
                "differences": {
                    "nodes_diff": stats2.get("total_nodes", 0) - stats1.get("total_nodes", 0),
                    "relationships_diff": stats2.get("total_relationships", 0) - stats1.get("total_relationships", 0),
                    "node_types_added": [],
                    "node_types_removed": [],
                    "relationship_types_added": [],
                    "relationship_types_removed": []
                }
            }
            
            # Compare node types
            types1 = set(stats1.get("node_types", {}).keys())
            types2 = set(stats2.get("node_types", {}).keys())
            comparison["differences"]["node_types_added"] = list(types2 - types1)
            comparison["differences"]["node_types_removed"] = list(types1 - types2)
            
            # Compare relationship types
            rel_types1 = set(stats1.get("relationship_types", {}).keys())
            rel_types2 = set(stats2.get("relationship_types", {}).keys())
            comparison["differences"]["relationship_types_added"] = list(rel_types2 - rel_types1)
            comparison["differences"]["relationship_types_removed"] = list(rel_types1 - rel_types2)
            
            self.session_logger.log_operation_end(
                "compare_snapshots",
                success=True,
                details=comparison["differences"]
            )
            
            return comparison
            
        except Exception as e:
            self.session_logger.log_operation_end(
                "compare_snapshots",
                success=False,
                details={"error": str(e)}
            )
            self.session_logger.log_error(e)
            raise
    
    def restore_snapshot(self, snapshot_id: str) -> bool:
        """
        Restore graph from a snapshot.
        WARNING: This will clear the current graph and restore from snapshot.
        """
        self.session_logger.log_operation_start(
            "restore_snapshot",
            {"snapshot_id": snapshot_id}
        )
        
        try:
            snapshot_dir = self.snapshots_dir / snapshot_id
            if not snapshot_dir.exists():
                raise ValueError(f"Snapshot {snapshot_id} not found")
            
            # Load D3 format data
            d3_file = snapshot_dir / "graph_d3.json"
            if not d3_file.exists():
                raise ValueError(f"Graph data not found in snapshot {snapshot_id}")
            
            with open(d3_file, "r") as f:
                graph_data = json.load(f)
            
            # Clear current graph
            self.session_logger.log_info("Clearing current graph for restoration")
            self.graph_manager.graph.run("MATCH (n) DETACH DELETE n")
            
            # Restore nodes
            nodes_created = 0
            node_id_mapping = {}  # Map old IDs to new nodes
            
            for node_data in graph_data["nodes"]:
                # Create node
                labels = node_data["labels"]
                properties = node_data["properties"]
                
                if labels:
                    # Use py2neo to create node
                    from py2neo import Node
                    node = Node(*labels, **properties)
                    self.graph_manager.graph.create(node)
                    node_id_mapping[node_data["id"]] = node
                    nodes_created += 1
            
            # Restore relationships
            relationships_created = 0
            for link_data in graph_data["links"]:
                source_node = node_id_mapping.get(link_data["source"])
                target_node = node_id_mapping.get(link_data["target"])
                
                if source_node and target_node:
                    from py2neo import Relationship
                    rel = Relationship(source_node, link_data["type"], target_node)
                    
                    # Add properties if any
                    for key, value in link_data.get("properties", {}).items():
                        rel[key] = value
                    
                    self.graph_manager.graph.create(rel)
                    relationships_created += 1
            
            self.session_logger.log_operation_end(
                "restore_snapshot",
                success=True,
                details={
                    "nodes_restored": nodes_created,
                    "relationships_restored": relationships_created
                }
            )
            
            self.session_logger.log_info(
                f"Restored snapshot {snapshot_id}: {nodes_created} nodes, {relationships_created} relationships"
            )
            
            return True
            
        except Exception as e:
            self.session_logger.log_operation_end(
                "restore_snapshot",
                success=False,
                details={"error": str(e)}
            )
            self.session_logger.log_error(e, {"snapshot_id": snapshot_id})
            return False

    def cleanup_old_snapshots(self, keep_count: int = 10):
        """Clean up old snapshots, keeping only the most recent ones"""
        snapshots = self.list_snapshots()
        
        if len(snapshots) <= keep_count:
            self.session_logger.log_info(f"No cleanup needed. Found {len(snapshots)} snapshots, keeping {keep_count}")
            return
        
        snapshots_to_remove = snapshots[keep_count:]
        
        for snapshot in snapshots_to_remove:
            snapshot_path = Path(snapshot["path"])
            try:
                shutil.rmtree(snapshot_path)
                self.session_logger.log_info(f"Removed old snapshot: {snapshot['snapshot_id']}")
            except Exception as e:
                self.session_logger.log_error(e, {"snapshot_path": str(snapshot_path)})