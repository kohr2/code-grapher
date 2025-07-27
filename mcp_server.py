#!/usr/bin/env python3
"""
Code Grapher MCP Server

🚀 CLAUDE USAGE GUIDE: Complete workflow for intelligent codebase analysis

═══════════════════════════════════════════════════════════════════
📋 COMPLETE WORKFLOW FOR CLAUDE
═══════════════════════════════════════════════════════════════════

🔄 TYPICAL WORKFLOW:
1. create_code_graph(project_path="/path/to/code") 
   → Build initial knowledge graph (ALWAYS DO THIS FIRST)

2. Check resources to understand what's available:
   → Read graph://stats for overview
   → Read graph://entity-types to see what kinds of code elements exist

3. query_code_graph(query="your functionality question")
   → Find relevant code using natural language

4. get_related_entities(entity_name="FoundEntity", max_depth=2)
   → Explore relationships and architectural connections

═══════════════════════════════════════════════════════════════════
⚡ QUICK DECISION GUIDE FOR CLAUDE
═══════════════════════════════════════════════════════════════════

🎯 TASK: "Understand how authentication works"
→ query_code_graph(query="authentication flow", use_hybrid=true)
→ get_related_entities for key classes/functions found

🎯 TASK: "Find all validation functions" 
→ query_code_graph(query="input validation", max_results=15)

🎯 TASK: "Understand impact of changing UserManager"
→ get_related_entities(entity_name="UserManager", max_depth=2)

🎯 TASK: "Code has been updated"
→ update_graph_from_diff() (if incremental changes)
→ create_code_graph() (if major restructuring)

═══════════════════════════════════════════════════════════════════
💡 QUERY OPTIMIZATION FOR CLAUDE
═══════════════════════════════════════════════════════════════════

✅ EFFECTIVE QUERIES (focus on BEHAVIOR/FUNCTIONALITY):
- "authentication flow"
- "user input validation" 
- "database connection management"
- "error handling patterns"
- "configuration loading"
- "API endpoint definitions"

❌ INEFFECTIVE QUERIES (avoid SYNTAX/FILE NAMES):
- "functions" (too broad)
- "config.py" (use file paths instead)
- "import requests" (too syntactic)
- "class definitions" (too generic)

═══════════════════════════════════════════════════════════════════

Tools Available:
- create_code_graph: Parse project and build knowledge graph
- update_graph_from_diff: Incremental updates from git changes  
- query_code_graph: Semantic + structural code retrieval
- get_related_entities: Traverse relationships to specified depth

Resources Available:
- Graph statistics and health metrics
- Entity types and relationship patterns
- Update history and performance stats
"""

import os
import sys
import json
import asyncio
import argparse
from typing import Any, Dict, List, Optional, Sequence
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

# MCP imports
from mcp.server import Server
from mcp.server.models import InitializationOptions
from mcp.server.stdio import stdio_server
from mcp.server.lowlevel.server import NotificationOptions
from mcp.types import (
    Resource, 
    Tool, 
    TextContent, 
    ImageContent, 
    EmbeddedResource, 
    CallToolRequest, 
    ListResourcesRequest, 
    ListToolsRequest, 
    ReadResourceRequest
)

# Code Grapher imports
from core_pipeline import run_enhanced_pipeline, parse_and_extract_entities, create_enhanced_graph_with_entities, extract_enhanced_relationships, generate_code_descriptions
from graph_manager import CodeGraphManager
from surgical_update_coordinator import SurgicalUpdateCoordinator
from rag_pipeline import CodeRAGPipeline
from logger import logger


class CodeGrapherMCPServer:
    """MCP Server for Code Grapher functionality"""
    
    def __init__(self):
        self.server = Server("code-grapher")
        self.graph_manager = None
        self.rag_pipeline = None
        self.surgical_coordinator = None
        self.session_logger = logger.create_session_logger("MCPServer")
        
        # Register handlers
        self._register_tools()
        self._register_resources()
        
        self.session_logger.log_info("Code Grapher MCP Server initialized")
    
    def _register_tools(self):
        """Register MCP tools"""
        
        @self.server.list_tools()
        async def list_tools() -> List[Tool]:
            return [
                Tool(
                    name="create_code_graph",
                    description="""
🔧 TOOL PURPOSE: Build comprehensive knowledge graph from codebase using AST analysis

⚡ WHEN TO USE:
- First time analyzing a codebase (always run this first)
- When you need complete understanding of project structure
- After major code changes that affect multiple files
- When existing graph is stale or corrupted

🎯 WHAT IT DOES:
- Parses ALL code files using AST for 100% accuracy
- Extracts 25+ entity types (functions, classes, imports, decorators, etc.)
- Detects relationships: INHERITS, DECORATES, CALLS across files
- Creates semantic embeddings for intelligent search
- Processes typical codebase in 30-60 seconds

📋 WORKFLOW:
1. Use this FIRST before any queries
2. Check results show reasonable entity/relationship counts
3. Then use query_code_graph for specific questions

⚠️ IMPORTANT NOTES:
- Always clear_existing=true for fresh analysis
- use_ai_descriptions=true adds semantic richness but requires Ollama
- Results are stored in Neo4j + ChromaDB for persistent querying
- Only needs to run once per codebase (unless major changes)
                    """,
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "project_path": {
                                "type": "string",
                                "description": "Absolute path to project root directory (where main code files are)"
                            },
                            "use_ai_descriptions": {
                                "type": "boolean", 
                                "description": "Generate AI-powered entity descriptions using Ollama (enhances semantic search quality)",
                                "default": True
                            },
                            "clear_existing": {
                                "type": "boolean",
                                "description": "Clear existing graph data before analysis (recommended: true for fresh start)", 
                                "default": True
                            }
                        },
                        "required": ["project_path"]
                    }
                ),
                Tool(
                    name="update_graph_from_diff",
                    description="""
🔄 TOOL PURPOSE: Efficiently update existing graph based on specific git changes (surgical updates)

⚡ WHEN TO USE:
- After code changes when you have an existing graph
- When you want incremental updates instead of full reanalysis
- For large codebases where full reanalysis is slow
- When working with recent commits that changed specific files

🎯 WHAT IT DOES:
- Analyzes git diff to identify changed files
- Updates only affected entities and relationships
- Much faster than full reanalysis (seconds vs minutes)
- Maintains graph consistency and relationships

📋 WORKFLOW:
1. Must have existing graph (run create_code_graph first)
2. Make code changes and commit them
3. Use this tool to update graph incrementally
4. Query updated graph normally

⚠️ IMPORTANT NOTES:
- Requires existing graph (fails if no graph exists)
- Best for incremental changes, not major restructuring
- create_snapshot=true recommended for safety
- For major changes, use create_code_graph instead
                    """,
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "commit_hash": {
                                "type": "string",
                                "description": "Git commit hash to analyze changes from (default: HEAD = latest commit)"
                            },
                            "create_snapshot": {
                                "type": "boolean",
                                "description": "Create backup snapshot before updating (recommended: true for safety)",
                                "default": True
                            }
                        }
                    }
                ),
                Tool(
                    name="query_code_graph", 
                    description="""
🔍 TOOL PURPOSE: Find relevant code using natural language queries with semantic + structural understanding

⚡ WHEN TO USE:
- Finding code by functionality: "authentication flow", "user validation", "database connections"
- Understanding how features work: "How does X work?", "What handles Y?"
- Finding patterns: "error handling", "configuration management", "API endpoints"
- Initial exploration of unknown codebases

🎯 WHAT IT DOES:
- Combines vector similarity (meaning) + graph traversal (structure)
- Returns actual code snippets with context
- Understands business logic, not just keywords
- Provides relevance-ranked results

📋 PARAMETER GUIDE:
• use_hybrid=true: Best for "how does X work?" questions (combines semantic + structural)
• use_hybrid=false: Better for finding specific patterns or entity types
• max_results=5-10: For focused results and token efficiency
• max_results=15-20: For comprehensive discovery
• include_relationships=true: Shows connections between found entities

🎯 EFFECTIVE QUERY EXAMPLES:
✅ GOOD QUERIES:
- "authentication flow" (functionality)
- "user input validation" (behavior)
- "database connection management" (system concern)
- "error handling patterns" (code pattern)
- "API route definitions" (structural pattern)
- "configuration loading" (feature)

❌ AVOID THESE:
- "functions" (too broad)
- "config.py" (use file paths, not queries)
- "import requests" (too specific/syntactic)
- "def " (syntax, not behavior)

📋 WORKFLOW:
1. Start with broad functionality queries
2. If too many results, add context: "user authentication flow"
3. Follow up with get_related_entities for deeper context
4. Use include_relationships=true to see connections

⚠️ IMPORTANT NOTES:
- Requires existing graph (run create_code_graph first)
- Results are token-efficient and context-aware
- Follow up with get_related_entities for full architectural understanding
                    """,
                    inputSchema={
                        "type": "object",
                        "properties": {
                            "query": {
                                "type": "string",
                                "description": "Natural language description of functionality or behavior you're looking for (NOT file names or syntax)"
                            },
                            "max_results": {
                                "type": "integer",
                                "description": "Number of results to return. Use 5-10 for focused results, 15-20 for discovery",
                                "default": 10
                            },
                            "use_hybrid": {
                                "type": "boolean",
                                "description": "true=semantic+structural search (best for 'how does X work'), false=semantic only (better for finding patterns)",
                                "default": True
                            },
                            "include_relationships": {
                                "type": "boolean", 
                                "description": "true=show how found entities connect to each other (useful for understanding flow)",
                                "default": False
                            }
                        },
                        "required": ["query"]
                    }
                ),
                Tool(
                    name="get_related_entities",
                    description="""
🕸️ TOOL PURPOSE: Traverse code relationships from a specific entity to understand architectural connections

⚡ WHEN TO USE:
- After query_code_graph finds interesting entities, use this to explore connections
- Understanding impact: "What depends on this function?"
- Tracing execution flow: "What does this class call?"
- Architecture analysis: "What's connected to this component?"
- Building context for code changes

🎯 WHAT IT DOES:
- Follows graph relationships (INHERITS, DECORATES, CALLS) in both directions
- Finds entities that call/inherit/use the target entity
- Discovers what the target entity calls/inherits/uses
- Maps out architectural neighborhoods around key components

📋 DEPTH STRATEGY:
• max_depth=1: Direct relationships only (immediate callers/callees)
• max_depth=2: Include second-degree connections (most useful)
• max_depth=3: Broader architectural context (can be noisy)
• max_depth=4+: Very broad, use sparingly

🎯 RELATIONSHIP TYPES:
- "CALLS": Function/method invocations
- "INHERITS": Class inheritance relationships  
- "DECORATES": Decorator usage patterns
- (Leave empty for all types - recommended)

📋 TYPICAL WORKFLOW:
1. Use query_code_graph to find entities of interest
2. Pick key entities from results (classes, functions)
3. Use this tool to explore their connections
4. Build understanding of how components interact

💡 PRACTICAL EXAMPLES:
- Found "UserManager" class → get_related_entities to see what uses it
- Found "authenticate" function → trace what it calls and what calls it
- Found "DatabaseConnection" → understand the connection patterns
- Planning to modify a function → see what would be impacted

⚠️ IMPORTANT NOTES:
- Use exact entity names from query_code_graph results
- entity_type helps when names are ambiguous
- Start with max_depth=2 for balanced context
- Combine with query results for complete understanding
                    """,
                    inputSchema={
                        "type": "object", 
                        "properties": {
                            "entity_name": {
                                "type": "string",
                                "description": "Exact name of entity from query_code_graph results (function name, class name, etc.)"
                            },
                            "entity_type": {
                                "type": "string",
                                "description": "Type of entity: 'function', 'class', 'method', 'variable' (helps resolve ambiguity)"
                            },
                            "max_depth": {
                                "type": "integer",
                                "description": "Relationship depth: 1=direct only, 2=recommended default, 3=broader context",
                                "default": 2
                            },
                            "relationship_types": {
                                "type": "array",
                                "items": {"type": "string"},
                                "description": "Specific types: ['CALLS', 'INHERITS', 'DECORATES'] or leave empty for all (recommended)"
                            }
                        },
                        "required": ["entity_name"]
                    }
                )
            ]
        
        @self.server.call_tool()
        async def call_tool(name: str, arguments: dict) -> List[TextContent]:
            try:
                if name == "create_code_graph":
                    return await self._create_code_graph(arguments)
                elif name == "update_graph_from_diff":
                    return await self._update_graph_from_diff(arguments)
                elif name == "query_code_graph":
                    return await self._query_code_graph(arguments)
                elif name == "get_related_entities":
                    return await self._get_related_entities(arguments)
                else:
                    return [TextContent(type="text", text=f"Unknown tool: {name}")]
                    
            except Exception as e:
                self.session_logger.log_error(e, {"tool": name, "arguments": arguments})
                return [TextContent(type="text", text=f"Error executing {name}: {str(e)}")]
    
    def _register_resources(self):
        """Register MCP resources"""
        
        @self.server.list_resources()
        async def list_resources() -> List[Resource]:
            return [
                Resource(
                    uri="graph://stats",
                    name="Graph Health & Statistics",
                    description="📊 Overall graph status: entity counts, relationship counts, database health. Use this to verify graph exists and get overview before querying.",
                    mimeType="application/json"
                ),
                Resource(
                    uri="graph://entity-types",
                    name="Code Entity Classifications", 
                    description="🏷️ Discover available entity types (functions, classes, decorators, etc.) and their counts. Useful for understanding what kinds of code elements the graph contains.",
                    mimeType="application/json"
                ),
                Resource(
                    uri="graph://relationship-types", 
                    name="Code Relationship Patterns",
                    description="🔗 Available relationship types (CALLS, INHERITS, DECORATES) and their frequencies. Shows how code components connect to each other.",
                    mimeType="application/json"
                ),
                Resource(
                    uri="graph://performance",
                    name="Update History & Performance",
                    description="⚡ Recent graph operations, processing times, and update history. Useful for understanding graph freshness and system performance.",
                    mimeType="application/json"
                )
            ]
        
        @self.server.read_resource()
        async def read_resource(uri: str) -> str:
            try:
                await self._ensure_graph_manager()
                
                if uri == "graph://stats":
                    stats = self.graph_manager.get_graph_stats()
                    return json.dumps(stats, indent=2)
                
                elif uri == "graph://entity-types":
                    stats = self.graph_manager.get_graph_stats()
                    return json.dumps({
                        "entity_types": stats.get("node_types", {}),
                        "total_entities": stats.get("total_nodes", 0)
                    }, indent=2)
                
                elif uri == "graph://relationship-types":
                    stats = self.graph_manager.get_graph_stats()
                    return json.dumps({
                        "relationship_types": stats.get("relationship_types", {}),
                        "total_relationships": stats.get("total_relationships", 0)
                    }, indent=2)
                
                elif uri == "graph://performance":
                    if self.surgical_coordinator:
                        performance = self.surgical_coordinator.get_performance_report()
                        return json.dumps(performance, indent=2)
                    else:
                        return json.dumps({"message": "No performance data available"}, indent=2)
                
                else:
                    return json.dumps({"error": f"Unknown resource URI: {uri}"})
                    
            except Exception as e:
                self.session_logger.log_error(e, {"resource_uri": uri})
                return json.dumps({"error": str(e)})
    
    async def _ensure_graph_manager(self):
        """Ensure graph manager is initialized"""
        if not self.graph_manager:
            self.graph_manager = CodeGraphManager()
            self.session_logger.log_info("Graph manager initialized")
    
    async def _ensure_rag_pipeline(self):
        """Ensure RAG pipeline is initialized"""
        await self._ensure_graph_manager()
        if not self.rag_pipeline:
            self.rag_pipeline = CodeRAGPipeline(self.graph_manager)
            self.session_logger.log_info("RAG pipeline initialized")
    
    async def _ensure_surgical_coordinator(self):
        """Ensure surgical coordinator is initialized"""
        await self._ensure_graph_manager()
        if not self.surgical_coordinator:
            self.surgical_coordinator = SurgicalUpdateCoordinator(graph_manager=self.graph_manager)
            self.session_logger.log_info("Surgical coordinator initialized")
    
    async def _create_code_graph(self, arguments: dict) -> List[TextContent]:
        """Create code graph from project"""
        project_path = arguments.get("project_path", ".")
        use_ai = arguments.get("use_ai_descriptions", True)
        clear_existing = arguments.get("clear_existing", True)
        
        self.session_logger.log_operation_start(
            "create_code_graph",
            {"project_path": project_path, "use_ai": use_ai, "clear_existing": clear_existing}
        )
        
        try:
            await self._ensure_graph_manager()
            
            # Clear existing graph if requested
            if clear_existing:
                clear_query = "MATCH (n) DETACH DELETE n"
                self.graph_manager.graph.run(clear_query)
            
            # Find Python files
            python_files = []
            for root, dirs, files in os.walk(project_path):
                dirs[:] = [d for d in dirs if not d.startswith('.') and d not in ['__pycache__', 'node_modules', '.git']]
                for file in files:
                    if file.endswith('.py') and not file.startswith('.'):
                        python_files.append(os.path.join(root, file))
            
            if not python_files:
                return [TextContent(type="text", text=f"No Python files found in {project_path}")]
            
            # Parse files
            parsed_files = parse_and_extract_entities(python_files)
            successful_files = [f for f in parsed_files if f["success"]]
            
            # Generate descriptions if requested
            code_descriptions = {}
            if use_ai:
                code_descriptions = generate_code_descriptions(parsed_files, project_path)
            
            # Extract relationships  
            relationships = extract_enhanced_relationships(parsed_files, use_ai=use_ai)
            
            # Create graph
            stats = create_enhanced_graph_with_entities(parsed_files, relationships, code_descriptions)
            
            result = {
                "success": True,
                "project_path": project_path,
                "files_processed": len(successful_files),
                "total_files_found": len(python_files),
                "graph_statistics": stats,
                "ai_descriptions_generated": use_ai and len(code_descriptions) > 0
            }
            
            self.session_logger.log_operation_end("create_code_graph", success=True, details=result)
            
            return [TextContent(
                type="text", 
                text=f"✅ Code graph created successfully!\n\n"
                     f"📁 Files processed: {result['files_processed']}/{result['total_files_found']}\n"
                     f"🏷️ Entities: {stats.get('total_entities', 0)}\n" 
                     f"🔗 Relationships: {stats.get('total_relationships', 0)}\n"
                     f"🤖 AI descriptions: {'Yes' if result['ai_descriptions_generated'] else 'No'}\n\n"
                     f"Graph is ready for querying!"
            )]
            
        except Exception as e:
            self.session_logger.log_operation_end("create_code_graph", success=False, details={"error": str(e)})
            return [TextContent(type="text", text=f"❌ Failed to create code graph: {str(e)}")]
    
    async def _update_graph_from_diff(self, arguments: dict) -> List[TextContent]:
        """Update graph from git diff"""
        commit_hash = arguments.get("commit_hash", "HEAD")
        create_snapshot = arguments.get("create_snapshot", True)
        
        self.session_logger.log_operation_start(
            "update_graph_from_diff", 
            {"commit_hash": commit_hash, "create_snapshot": create_snapshot}
        )
        
        try:
            await self._ensure_surgical_coordinator()
            
            result = self.surgical_coordinator.process_commit(
                commit_hash=commit_hash,
                create_snapshot=create_snapshot
            )
            
            if result["success"]:
                return [TextContent(
                    type="text",
                    text=f"✅ Graph updated successfully from commit {commit_hash[:8]}!\n\n"
                         f"📝 Updates executed: {result['updates_executed']}\n"
                         f"❌ Updates failed: {result['updates_failed']}\n"
                         f"⏱️ Execution time: {result['execution_time']:.2f}s\n"
                         f"💾 Snapshot created: {'Yes' if result['snapshot_id'] else 'No'}\n"
                         f"⚡ Time saved vs full reparse: {result.get('time_saved', 0):.2f}s"
                )]
            else:
                return [TextContent(
                    type="text", 
                    text=f"❌ Failed to update graph from commit {commit_hash[:8]}: {result.get('error', 'Unknown error')}\n"
                         f"🔄 Rollback available: {'Yes' if result.get('rollback_available') else 'No'}"
                )]
                
        except Exception as e:
            self.session_logger.log_operation_end("update_graph_from_diff", success=False, details={"error": str(e)})
            return [TextContent(type="text", text=f"❌ Failed to update graph: {str(e)}")]
    
    async def _query_code_graph(self, arguments: dict) -> List[TextContent]:
        """Query code graph with semantic search"""
        query = arguments["query"]
        max_results = arguments.get("max_results", 10)
        use_hybrid = arguments.get("use_hybrid", True)
        include_relationships = arguments.get("include_relationships", False)
        
        self.session_logger.log_operation_start(
            "query_code_graph",
            {"query": query, "max_results": max_results, "use_hybrid": use_hybrid}
        )
        
        try:
            await self._ensure_rag_pipeline()
            
            # Get results using RAG pipeline
            if use_hybrid:
                results = self.rag_pipeline.hybrid_retrieve(query, k=max_results)
            else:
                results = self.rag_pipeline.retrieve_relevant_content(query, k=max_results)
            
            if not results:
                return [TextContent(type="text", text=f"No relevant code found for query: '{query}'")]
            
            # Format results for display
            response_parts = [f"🔍 Found {len(results)} relevant code snippets for: '{query}'\n"]
            
            for i, result in enumerate(results, 1):
                metadata = result.get("metadata", {})
                relevance = result.get("relevance_score", 0.0)
                source = result.get("source", "unknown")
                
                response_parts.append(f"\n{'='*50}")
                response_parts.append(f"Result #{i} (Relevance: {relevance:.3f}, Source: {source})")
                response_parts.append(f"{'='*50}")
                
                if metadata.get("file_path"):
                    file_path = metadata["file_path"]
                    line_num = metadata.get("line_number", "?")
                    response_parts.append(f"📁 File: {file_path}:{line_num}")
                
                if metadata.get("name"):
                    entity_type = metadata.get("type", "entity")
                    response_parts.append(f"🏷️ {entity_type}: {metadata['name']}")
                
                # Add content
                content = result.get("content", "")
                if len(content) > 500:
                    content = content[:500] + "..."
                response_parts.append(f"\n```python\n{content}\n```")
                
                # Add relationships if requested
                if include_relationships and metadata.get("name"):
                    related = await self._get_entity_relationships(metadata["name"], metadata.get("type"))
                    if related:
                        response_parts.append(f"\n🔗 Related: {', '.join(related[:3])}")
            
            response_text = "\n".join(response_parts)
            
            self.session_logger.log_operation_end("query_code_graph", success=True, details={"results_count": len(results)})
            
            return [TextContent(type="text", text=response_text)]
            
        except Exception as e:
            self.session_logger.log_operation_end("query_code_graph", success=False, details={"error": str(e)})
            return [TextContent(type="text", text=f"❌ Query failed: {str(e)}")]
    
    async def _get_related_entities(self, arguments: dict) -> List[TextContent]:
        """Get entities related to a specific entity"""
        entity_name = arguments["entity_name"]
        entity_type = arguments.get("entity_type")
        max_depth = arguments.get("max_depth", 2)
        relationship_types = arguments.get("relationship_types")
        
        self.session_logger.log_operation_start(
            "get_related_entities",
            {"entity_name": entity_name, "entity_type": entity_type, "max_depth": max_depth}
        )
        
        try:
            await self._ensure_graph_manager()
            
            # Build Cypher query for relationship traversal
            type_filter = f":{entity_type}" if entity_type else ""
            rel_filter = ""
            if relationship_types:
                rel_types = "|".join(relationship_types)
                rel_filter = f"[r:{rel_types}]"
            else:
                rel_filter = "[r]"
            
            query = f"""
            MATCH (start{type_filter} {{name: $entity_name}})
            MATCH path = (start)-{rel_filter}*1..{max_depth}-(related)
            WHERE start <> related
            RETURN related.name as name, 
                   labels(related) as labels,
                   related.file_path as file_path,
                   length(path) as distance,
                   [rel in relationships(path) | type(rel)] as relationship_path
            ORDER BY distance, name
            LIMIT 20
            """
            
            result = self.graph_manager.graph.run(query, entity_name=entity_name)
            records = list(result)
            
            if not records:
                return [TextContent(
                    type="text",
                    text=f"No related entities found for '{entity_name}'{' (' + entity_type + ')' if entity_type else ''}"
                )]
            
            # Format results
            response_parts = [
                f"🔗 Found {len(records)} entities related to '{entity_name}' (max depth: {max_depth})\n"
            ]
            
            current_depth = 0
            for record in records:
                distance = record["distance"]
                
                if distance != current_depth:
                    current_depth = distance
                    response_parts.append(f"\n📏 Distance {distance}:")
                
                name = record["name"]
                labels = record["labels"]
                file_path = record["file_path"] or "unknown"
                rel_path = " → ".join(record["relationship_path"])
                
                entity_label = labels[0] if labels else "Entity"
                response_parts.append(f"  • {name} ({entity_label}) in {Path(file_path).name}")
                response_parts.append(f"    Path: {rel_path}")
            
            response_text = "\n".join(response_parts)
            
            self.session_logger.log_operation_end("get_related_entities", success=True, details={"results_count": len(records)})
            
            return [TextContent(type="text", text=response_text)]
            
        except Exception as e:
            self.session_logger.log_operation_end("get_related_entities", success=False, details={"error": str(e)})
            return [TextContent(type="text", text=f"❌ Failed to get related entities: {str(e)}")]
    
    async def _get_entity_relationships(self, entity_name: str, entity_type: str = None) -> List[str]:
        """Helper to get related entity names"""
        try:
            type_filter = f":{entity_type}" if entity_type else ""
            query = f"""
            MATCH (start{type_filter} {{name: $entity_name}})-[r]-(related)
            RETURN DISTINCT related.name as name
            LIMIT 5
            """
            
            result = self.graph_manager.graph.run(query, entity_name=entity_name)
            return [record["name"] for record in result if record["name"]]
            
        except Exception:
            return []
    
    async def run(self):
        """Run the MCP server"""
        async with stdio_server() as (read_stream, write_stream):
            await self.server.run(
                read_stream, 
                write_stream, 
                InitializationOptions(
                    server_name="code-grapher",
                    server_version="1.0.0",
                    capabilities=self.server.get_capabilities(
                        notification_options=NotificationOptions(
                            prompts_changed=False,
                            resources_changed=True,
                            tools_changed=False
                        ),
                        experimental_capabilities={}
                    )
                )
            )


async def main():
    """Main entry point"""
    parser = argparse.ArgumentParser(description="Code Grapher MCP Server")
    parser.add_argument("--debug", action="store_true", help="Enable debug logging")
    args = parser.parse_args()
    
    if args.debug:
        import logging
        logging.basicConfig(level=logging.DEBUG)
    
    server = CodeGrapherMCPServer()
    await server.run()


if __name__ == "__main__":
    asyncio.run(main())