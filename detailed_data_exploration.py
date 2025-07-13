#!/usr/bin/env python3
"""
Detailed Neo4j Data Exploration

This script provides a deeper analysis of the existing data in the Neo4j database.
Following CLAUDE.md verbose logging requirements for development phase.

Usage: python detailed_data_exploration.py
"""

import os
import json
import time
from datetime import datetime
from typing import Dict, List, Any, Optional
from dotenv import load_dotenv

try:
    from neo4j import GraphDatabase
    NEO4J_AVAILABLE = True
except ImportError as e:
    print(f"ERROR: Neo4j package not available: {e}")
    NEO4J_AVAILABLE = False
    exit(1)

# Load environment variables
load_dotenv()

class DetailedDataExplorer:
    """Detailed exploration of Neo4j data with comprehensive analysis"""
    
    def __init__(self):
        self.start_time = datetime.now()
        self.neo4j_url = os.getenv('NEO4J_URL', 'neo4j://100.83.40.11:7687')
        self.driver = None
        
        # Connect using discovered credentials
        self.log("Connecting to Neo4j database...")
        self.driver = GraphDatabase.driver(self.neo4j_url, auth=("neo4j", "password"))
        self.driver.verify_connectivity()
        self.log("✓ Connected successfully")
    
    def log(self, message: str, level: str = "INFO"):
        """Log with timestamp"""
        timestamp = datetime.now().strftime("[%Y-%m-%d %H:%M:%S]")
        print(f"{timestamp} [DATA_EXPLORER] [{level}] {message}")
    
    def analyze_memory_nodes(self) -> Dict[str, Any]:
        """Analyze Memory nodes in detail"""
        self.log("Analyzing Memory nodes...")
        
        analysis = {}
        
        with self.driver.session() as session:
            # Get all Memory nodes
            result = session.run("""
                MATCH (m:Memory)
                RETURN m
                ORDER BY m.timestamp DESC
            """)
            
            memories = []
            for record in result:
                memory = dict(record["m"])
                memories.append(memory)
            
            analysis["total_memories"] = len(memories)
            self.log(f"Found {len(memories)} Memory nodes")
            
            # Analyze memory content patterns
            if memories:
                # Sample content analysis
                sample_content = []
                metadata_patterns = {}
                
                for i, memory in enumerate(memories[:10]):  # Look at first 10
                    content = memory.get("content", "")
                    metadata_json = memory.get("metadata_json", "{}")
                    
                    try:
                        metadata = json.loads(metadata_json) if metadata_json else {}
                    except:
                        metadata = {}
                    
                    sample_content.append({
                        "id": memory.get("id"),
                        "content_length": len(content),
                        "content_preview": content[:100] + "..." if len(content) > 100 else content,
                        "metadata_keys": list(metadata.keys()) if metadata else [],
                        "timestamp": memory.get("timestamp")
                    })
                    
                    # Track metadata patterns
                    for key in metadata.keys():
                        metadata_patterns[key] = metadata_patterns.get(key, 0) + 1
                
                analysis["sample_memories"] = sample_content
                analysis["metadata_patterns"] = metadata_patterns
                
                # Analyze timestamp distribution
                timestamps = [m.get("timestamp") for m in memories if m.get("timestamp")]
                if timestamps:
                    analysis["timestamp_range"] = {
                        "earliest": min(timestamps),
                        "latest": max(timestamps),
                        "count": len(timestamps)
                    }
                
                self.log(f"Memory content analysis:")
                self.log(f"  Average content length: {sum(len(m.get('content', '')) for m in memories) / len(memories):.1f} chars")
                self.log(f"  Metadata patterns: {metadata_patterns}")
        
        return analysis
    
    def analyze_entity_nodes(self) -> Dict[str, Any]:
        """Analyze Entity nodes in detail"""
        self.log("Analyzing Entity nodes...")
        
        analysis = {}
        
        with self.driver.session() as session:
            # Get all Entity nodes
            result = session.run("""
                MATCH (e:Entity)
                RETURN e.name as name, e.type as type
                ORDER BY e.name
            """)
            
            entities = []
            entity_types = {}
            
            for record in result:
                name = record["name"]
                entity_type = record["type"]
                entities.append({"name": name, "type": entity_type})
                
                entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
            
            analysis["total_entities"] = len(entities)
            analysis["entity_types"] = entity_types
            analysis["all_entities"] = entities
            
            self.log(f"Found {len(entities)} Entity nodes")
            self.log(f"Entity type distribution: {entity_types}")
            
            # Show all entity names grouped by type
            entities_by_type = {}
            for entity in entities:
                etype = entity["type"]
                if etype not in entities_by_type:
                    entities_by_type[etype] = []
                entities_by_type[etype].append(entity["name"])
            
            analysis["entities_by_type"] = entities_by_type
            
            self.log("Entities by type:")
            for etype, names in entities_by_type.items():
                self.log(f"  {etype}: {', '.join(names)}")
        
        return analysis
    
    def analyze_world_nodes(self) -> Dict[str, Any]:
        """Analyze World nodes in detail"""
        self.log("Analyzing World nodes...")
        
        analysis = {}
        
        with self.driver.session() as session:
            result = session.run("""
                MATCH (w:World)
                RETURN w
            """)
            
            worlds = []
            for record in result:
                world = dict(record["w"])
                worlds.append(world)
            
            analysis["total_worlds"] = len(worlds)
            analysis["worlds"] = worlds
            
            if worlds:
                self.log(f"Found {len(worlds)} World node(s):")
                for world in worlds:
                    self.log(f"  World: {world.get('name', 'unnamed')} - {world.get('description', 'no description')}")
                    self.log(f"    Created: {world.get('createdAt')}")
                    self.log(f"    ID: {world.get('worldId')}")
        
        return analysis
    
    def analyze_relationship_patterns(self) -> Dict[str, Any]:
        """Analyze relationship patterns in detail"""
        self.log("Analyzing relationship patterns...")
        
        analysis = {}
        
        with self.driver.session() as session:
            # Analyze MENTIONS relationships
            result = session.run("""
                MATCH (m:Memory)-[r:MENTIONS]->(e:Entity)
                RETURN m.id as memory_id, 
                       m.content as memory_content,
                       e.name as entity_name,
                       e.type as entity_type,
                       m.timestamp as timestamp
                ORDER BY m.timestamp DESC
                LIMIT 20
            """)
            
            mentions = []
            memory_entity_map = {}
            
            for record in result:
                mention = {
                    "memory_id": record["memory_id"],
                    "memory_content_preview": record["memory_content"][:100] + "..." if len(record["memory_content"]) > 100 else record["memory_content"],
                    "entity_name": record["entity_name"],
                    "entity_type": record["entity_type"],
                    "timestamp": record["timestamp"]
                }
                mentions.append(mention)
                
                # Track which entities are mentioned by which memories
                memory_id = record["memory_id"]
                entity_name = record["entity_name"]
                
                if memory_id not in memory_entity_map:
                    memory_entity_map[memory_id] = []
                memory_entity_map[memory_id].append(entity_name)
            
            analysis["recent_mentions"] = mentions
            analysis["memory_entity_connections"] = memory_entity_map
            
            self.log(f"Found {len(mentions)} recent memory-entity mentions")
            
            # Show some example mentions
            self.log("Recent memory-entity mentions:")
            for i, mention in enumerate(mentions[:5]):
                self.log(f"  {i+1}. Memory '{mention['memory_id']}' mentions {mention['entity_type']} '{mention['entity_name']}'")
                self.log(f"     Content preview: {mention['memory_content_preview']}")
            
            # Analyze entity mention frequency
            entity_mention_count = {}
            for record in session.run("""
                MATCH (e:Entity)<-[r:MENTIONS]-(m:Memory)
                RETURN e.name as entity_name, count(r) as mention_count
                ORDER BY mention_count DESC
            """):
                entity_mention_count[record["entity_name"]] = record["mention_count"]
            
            analysis["entity_mention_frequency"] = entity_mention_count
            
            self.log("Most mentioned entities:")
            for entity, count in list(entity_mention_count.items())[:10]:
                self.log(f"  {entity}: {count} mentions")
        
        return analysis
    
    def analyze_data_quality(self) -> Dict[str, Any]:
        """Analyze data quality and completeness"""
        self.log("Analyzing data quality...")
        
        quality_analysis = {}
        
        with self.driver.session() as session:
            # Check for nodes missing key properties
            memory_issues = session.run("""
                MATCH (m:Memory)
                WHERE m.content IS NULL OR m.content = ""
                RETURN count(m) as empty_content_count
            """).single()["empty_content_count"]
            
            entity_issues = session.run("""
                MATCH (e:Entity)
                WHERE e.name IS NULL OR e.name = "" OR e.type IS NULL OR e.type = ""
                RETURN count(e) as incomplete_entity_count
            """).single()["incomplete_entity_count"]
            
            # Check for orphaned nodes (nodes with no relationships)
            orphaned_memories = session.run("""
                MATCH (m:Memory)
                WHERE NOT (m)-[:MENTIONS]->()
                RETURN count(m) as orphaned_count
            """).single()["orphaned_count"]
            
            orphaned_entities = session.run("""
                MATCH (e:Entity)
                WHERE NOT ()<-[:MENTIONS]-(e)
                RETURN count(e) as orphaned_count
            """).single()["orphaned_count"]
            
            quality_analysis = {
                "memory_empty_content": memory_issues,
                "incomplete_entities": entity_issues,
                "orphaned_memories": orphaned_memories,
                "orphaned_entities": orphaned_entities
            }
            
            self.log(f"Data quality analysis:")
            self.log(f"  Memories with empty content: {memory_issues}")
            self.log(f"  Incomplete entities: {entity_issues}")
            self.log(f"  Orphaned memories (no mentions): {orphaned_memories}")
            self.log(f"  Orphaned entities (not mentioned): {orphaned_entities}")
        
        return quality_analysis
    
    def identify_data_patterns(self) -> Dict[str, Any]:
        """Identify interesting patterns in the data"""
        self.log("Identifying data patterns...")
        
        patterns = {}
        
        with self.driver.session() as session:
            # Find memories that mention multiple entities
            multi_entity_memories = session.run("""
                MATCH (m:Memory)-[:MENTIONS]->(e:Entity)
                WITH m, count(e) as entity_count, collect(e.name) as entities
                WHERE entity_count > 1
                RETURN m.id as memory_id, 
                       m.content as content,
                       entity_count,
                       entities
                ORDER BY entity_count DESC
                LIMIT 10
            """).data()
            
            patterns["multi_entity_memories"] = multi_entity_memories
            
            self.log(f"Found {len(multi_entity_memories)} memories mentioning multiple entities")
            
            # Find entities that appear together frequently
            co_occurrence = session.run("""
                MATCH (m:Memory)-[:MENTIONS]->(e1:Entity),
                      (m)-[:MENTIONS]->(e2:Entity)
                WHERE e1.name < e2.name
                WITH e1.name as entity1, e2.name as entity2, count(m) as co_count
                WHERE co_count > 1
                RETURN entity1, entity2, co_count
                ORDER BY co_count DESC
                LIMIT 10
            """).data()
            
            patterns["entity_co_occurrence"] = co_occurrence
            
            self.log("Entity co-occurrence patterns:")
            for record in co_occurrence:
                self.log(f"  {record['entity1']} + {record['entity2']}: {record['co_count']} times")
            
            # Analyze temporal patterns if timestamps are available
            temporal_patterns = session.run("""
                MATCH (m:Memory)
                WHERE m.timestamp IS NOT NULL
                WITH m.timestamp as ts
                ORDER BY ts
                RETURN min(ts) as earliest, max(ts) as latest, count(*) as total
            """).single()
            
            if temporal_patterns:
                patterns["temporal_range"] = {
                    "earliest": temporal_patterns["earliest"],
                    "latest": temporal_patterns["latest"],
                    "total_with_timestamps": temporal_patterns["total"]
                }
        
        return patterns
    
    def generate_comprehensive_report(self) -> Dict[str, Any]:
        """Generate a comprehensive report of the database contents"""
        self.log("=== Generating Comprehensive Database Report ===")
        
        report = {
            "generated_at": datetime.now().isoformat(),
            "database_url": self.neo4j_url,
            "analysis_summary": {}
        }
        
        # Run all analyses
        start_time = time.time()
        
        report["memory_analysis"] = self.analyze_memory_nodes()
        report["entity_analysis"] = self.analyze_entity_nodes()
        report["world_analysis"] = self.analyze_world_nodes()
        report["relationship_analysis"] = self.analyze_relationship_patterns()
        report["quality_analysis"] = self.analyze_data_quality()
        report["pattern_analysis"] = self.identify_data_patterns()
        
        analysis_time = time.time() - start_time
        
        # Create summary
        report["analysis_summary"] = {
            "total_nodes": (
                report["memory_analysis"]["total_memories"] +
                report["entity_analysis"]["total_entities"] +
                report["world_analysis"]["total_worlds"]
            ),
            "total_memories": report["memory_analysis"]["total_memories"],
            "total_entities": report["entity_analysis"]["total_entities"],
            "total_worlds": report["world_analysis"]["total_worlds"],
            "entity_types": report["entity_analysis"]["entity_types"],
            "analysis_duration": analysis_time
        }
        
        self.log(f"=== Report generated in {analysis_time:.3f}s ===")
        
        return report
    
    def close(self):
        """Close database connection"""
        if self.driver:
            self.driver.close()
            self.log("Database connection closed")


def main():
    """Main function"""
    print("Detailed Neo4j Data Exploration")
    print("=" * 50)
    
    explorer = DetailedDataExplorer()
    
    try:
        # Generate comprehensive report
        report = explorer.generate_comprehensive_report()
        
        # Print executive summary
        print("\n" + "=" * 50)
        print("EXECUTIVE SUMMARY")
        print("=" * 50)
        
        summary = report["analysis_summary"]
        print(f"Database URL: {report['database_url']}")
        print(f"Total Nodes: {summary['total_nodes']}")
        print(f"  - Memories: {summary['total_memories']}")
        print(f"  - Entities: {summary['total_entities']}")
        print(f"  - Worlds: {summary['total_worlds']}")
        print(f"Entity Types: {summary['entity_types']}")
        print(f"Analysis Duration: {summary['analysis_duration']:.3f}s")
        
        # Save detailed report to file
        report_filename = f"neo4j_database_report_{datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        with open(report_filename, 'w') as f:
            json.dump(report, f, indent=2, default=str)
        
        print(f"\nDetailed report saved to: {report_filename}")
        
        return 0
        
    except Exception as e:
        print(f"Error during analysis: {e}")
        return 1
    finally:
        explorer.close()


if __name__ == "__main__":
    exit(main())