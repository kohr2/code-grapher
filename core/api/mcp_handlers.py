"""
MCP API Handlers

Refactored MCP server handlers using dependency injection.
Phase 5 verticalization - service-based MCP handlers.
"""

import os
import json
from typing import Dict, Any, List, Optional
import asyncio

from mcp.server.models import Resource, Tool
from mcp.types import (
    TextContent,
    ImageContent,
    EmbeddedResource,
)

from shared.interfaces.logger_interface import LoggerInterface
from shared.services.service_registry import ServiceRegistry
from core.interfaces.pipeline_interface import PipelineInterface
from core.interfaces.rag_service_interface import RAGServiceInterface, RAGQuery
from shared.interfaces.graph_operations_interface import GraphOperationsInterface

# Import service interfaces from Phase 2-4
try:
    from ai_services.interfaces.ai_services_interface import AIServicesInterface
except ImportError:
    from interfaces.ai_services_interface import AIServicesInterface

try:
    from update_agents.interfaces.update_coordinator_interface import UpdateIntelligenceInterface
except ImportError:
    # Fallback interface
    from abc import ABC, abstractmethod
    
    class UpdateIntelligenceInterface(ABC):
        @abstractmethod
        async def analyze_git_diff(self, commit_hash: str) -> Dict[str, Any]:
            pass
        
        @abstractmethod
        async def execute_surgical_update(self, diff_analysis: Dict[str, Any]) -> Dict[str, Any]:
            pass


class MCPHandlers:
    """
    MCP handlers using dependency injection instead of direct imports
    
    Refactored from monolithic mcp_server.py to use service interfaces.
    """
    
    def __init__(self, 
                 services: ServiceRegistry,
                 logger: LoggerInterface):
        self.services = services
        self.logger = logger
        
        # Service instances (lazy loaded)
        self._pipeline_service = None
        self._graph_service = None
        self._rag_service = None
        self._ai_service = None
        self._update_service = None
    
    def _get_pipeline_service(self) -> PipelineInterface:
        """Get pipeline service with lazy initialization"""
        if self._pipeline_service is None:
            self._pipeline_service = self.services.get(PipelineInterface)
        return self._pipeline_service
    
    def _get_graph_service(self) -> GraphOperationsInterface:
        """Get graph service with lazy initialization"""
        if self._graph_service is None:
            self._graph_service = self.services.get(GraphOperationsInterface)
        return self._graph_service
    
    def _get_rag_service(self) -> RAGServiceInterface:
        """Get RAG service with lazy initialization"""
        if self._rag_service is None:
            self._rag_service = self.services.get(RAGServiceInterface)
        return self._rag_service
    
    def _get_ai_service(self) -> Optional[AIServicesInterface]:
        """Get AI service with lazy initialization (optional)"""
        if self._ai_service is None:
            try:
                self._ai_service = self.services.get(AIServicesInterface)
            except Exception:
                return None
        return self._ai_service
    
    def _get_update_service(self) -> Optional[UpdateIntelligenceInterface]:
        """Get update service with lazy initialization (optional)"""
        if self._update_service is None:
            try:
                self._update_service = self.services.get(UpdateIntelligenceInterface)
            except Exception:
                return None
        return self._update_service
    
    def get_tools(self) -> List[Tool]:
        """Get list of available MCP tools"""
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
- use_ai_descriptions=true adds semantic richness but requires AI service
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
                            "description": "Generate AI-powered entity descriptions using AI service (enhances semantic search quality)",
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
- Maintains graph consistency during updates

📋 WORKFLOW:
1. Make sure you have an existing graph (run create_code_graph first)
2. Use this after code changes
3. Check results to ensure updates were applied correctly

⚠️ IMPORTANT NOTES:
- Requires existing graph to update
- Works best with small to medium changes
- For major restructuring, consider full reanalysis
- Automatically handles file additions, deletions, and modifications
                """,
                inputSchema={
                    "type": "object",
                    "properties": {
                        "commit_hash": {
                            "type": "string",
                            "description": "Git commit hash to analyze for changes (optional - defaults to latest changes)",
                            "default": "HEAD"
                        },
                        "project_path": {
                            "type": "string",
                            "description": "Path to project root (defaults to current directory)",
                            "default": "."
                        }
                    }
                }
            ),
            Tool(
                name="query_code_graph",
                description="""
🔍 TOOL PURPOSE: Intelligent code search using hybrid graph + semantic approach

⚡ WHEN TO USE:
- Find specific functionality or patterns in code
- Understand code behavior and relationships
- Locate relevant code for debugging or enhancement
- Explore architectural patterns and dependencies

🎯 WHAT IT DOES:
- Combines graph traversal with semantic similarity
- Searches across entity names, types, and relationships
- Returns relevant code entities with context
- Provides relationship information for understanding

📋 EFFECTIVE QUERY PATTERNS:
✅ GOOD: "authentication flow", "user input validation", "database queries"
✅ GOOD: "error handling patterns", "configuration management", "API endpoints"
❌ AVOID: "def login", "class User" (too specific)
❌ AVOID: "code that", "function which" (too generic)

🎯 OPTIMIZATION TIPS:
- Focus on BEHAVIOR and FUNCTIONALITY rather than exact names
- Use domain terms that describe what code DOES
- Combine related concepts: "user authentication and session management"
- Ask about patterns: "logging and error handling patterns"

⚠️ IMPORTANT NOTES:
- Requires existing graph (run create_code_graph first)
- Results include both direct matches and related entities
- Higher max_results for broader exploration
- Use context_depth > 0 to see relationships
                """,
                inputSchema={
                    "type": "object",
                    "properties": {
                        "query": {
                            "type": "string",
                            "description": "Natural language query describing functionality or code patterns to find"
                        },
                        "max_results": {
                            "type": "integer",
                            "description": "Maximum number of results to return (default: 10, max: 50)",
                            "default": 10,
                            "minimum": 1,
                            "maximum": 50
                        },
                        "context_depth": {
                            "type": "integer", 
                            "description": "Depth of relationship context to include (0=no context, 1=immediate relations, 2=extended relations)",
                            "default": 1,
                            "minimum": 0,
                            "maximum": 3
                        },
                        "use_hybrid": {
                            "type": "boolean",
                            "description": "Use hybrid search combining graph structure + semantic similarity (recommended: true)",
                            "default": True
                        }
                    },
                    "required": ["query"]
                }
            ),
            Tool(
                name="get_related_entities",
                description="""
🕸️ TOOL PURPOSE: Explore relationships and dependencies around specific code entities

⚡ WHEN TO USE:
- Understand impact of changing a specific function/class
- Map dependencies and relationships
- Explore architectural connections
- Analyze code coupling and cohesion

🎯 WHAT IT DOES:
- Traverses graph relationships from a starting entity
- Shows direct and indirect connections
- Includes relationship types (INHERITS, CALLS, DECORATES, etc.)
- Provides relationship strength and direction

📋 WORKFLOW:
1. First, find entity name using query_code_graph
2. Use exact entity name from results
3. Adjust max_depth based on how far you want to explore
4. Review relationship types to understand connections

🎯 RELATIONSHIP TYPES:
- INHERITS: Class inheritance relationships
- CALLS: Function call relationships  
- DECORATES: Decorator applications
- IMPORTS: Module import dependencies
- CONTAINS: File/module containment

⚠️ IMPORTANT NOTES:
- Entity name must match exactly (case-sensitive)
- Higher max_depth shows broader impact but more results
- Use relationship_types filter to focus on specific connections
- Results show both incoming and outgoing relationships
                """,
                inputSchema={
                    "type": "object",
                    "properties": {
                        "entity_name": {
                            "type": "string",
                            "description": "Exact name of the entity to explore (function name, class name, etc.)"
                        },
                        "max_depth": {
                            "type": "integer",
                            "description": "Maximum relationship traversal depth (1=direct connections, 2=connections of connections, etc.)",
                            "default": 2,
                            "minimum": 1,
                            "maximum": 4
                        },
                        "relationship_types": {
                            "type": "array",
                            "items": {"type": "string"},
                            "description": "Filter by specific relationship types (INHERITS, CALLS, DECORATES, IMPORTS, CONTAINS)",
                            "default": []
                        },
                        "max_results": {
                            "type": "integer",
                            "description": "Maximum number of related entities to return",
                            "default": 20,
                            "minimum": 1,
                            "maximum": 100
                        }
                    },
                    "required": ["entity_name"]
                }
            )
        ]
    
    async def handle_create_code_graph(self, arguments: Dict[str, Any]) -> List[TextContent]:
        """Handle create_code_graph tool call using pipeline service"""
        try:
            project_path = arguments.get("project_path")
            use_ai_descriptions = arguments.get("use_ai_descriptions", True)
            clear_existing = arguments.get("clear_existing", True)
            
            self.logger.log_info(f"Creating code graph for: {project_path}")
            
            # Validate project path
            if not os.path.exists(project_path):
                return [TextContent(
                    type="text",
                    text=f"❌ Error: Project path does not exist: {project_path}"
                )]
            
            # Use pipeline service for graph creation
            pipeline_service = self._get_pipeline_service()
            
            result = await pipeline_service.run_enhanced_pipeline(
                target_directory=project_path,
                use_ai=use_ai_descriptions,
                clear_existing=clear_existing
            )
            
            if result.status.value == "success":
                data = result.data or {}
                
                response = f"""
✅ **Code Graph Created Successfully**

📊 **Analysis Results:**
• **Files Processed:** {data.get('files_processed', 0)}/{data.get('total_files', 0)}
• **Entities Extracted:** {data.get('graph_stats', {}).get('total_entities', 0)}
• **Relationships Created:** {data.get('graph_stats', {}).get('total_relationships', 0)}
• **Execution Time:** {result.execution_time:.2f} seconds

🤖 **AI Features:**
• **AI Descriptions:** {'✅ Enabled' if use_ai_descriptions else '❌ Disabled'}
• **Embeddings Created:** {data.get('embeddings_created', 0)}

🎯 **Next Steps:**
1. Use `query_code_graph` to search for specific functionality
2. Use `get_related_entities` to explore relationships
3. Check the graph resources for statistics and health info

💾 **Data Storage:**
• Neo4j: Graph structure and relationships
• ChromaDB: Semantic embeddings for search
• Results: enhanced_pipeline_results.json
                """
                
                return [TextContent(type="text", text=response.strip())]
            else:
                error_msg = f"❌ Pipeline failed: {result.message}"
                if result.errors:
                    error_msg += f"\nErrors: {', '.join(result.errors)}"
                
                return [TextContent(type="text", text=error_msg)]
                
        except Exception as e:
            self.logger.log_error(f"create_code_graph failed: {e}")
            return [TextContent(
                type="text",
                text=f"❌ Error creating code graph: {str(e)}"
            )]
    
    async def handle_update_graph_from_diff(self, arguments: Dict[str, Any]) -> List[TextContent]:
        """Handle update_graph_from_diff tool call using update service"""
        try:
            commit_hash = arguments.get("commit_hash", "HEAD")
            project_path = arguments.get("project_path", ".")
            
            self.logger.log_info(f"Updating graph from diff: {commit_hash}")
            
            # Use update service for surgical updates
            update_service = self._get_update_service()
            
            if not update_service:
                return [TextContent(
                    type="text",
                    text="❌ Update service not available. Surgical updates require the update intelligence service."
                )]
            
            # Analyze git diff
            diff_analysis = await update_service.analyze_git_diff(commit_hash)
            
            if not diff_analysis.get('success', False):
                return [TextContent(
                    type="text",
                    text=f"❌ Failed to analyze git diff: {diff_analysis.get('error', 'Unknown error')}"
                )]
            
            # Execute surgical update
            update_result = await update_service.execute_surgical_update(diff_analysis)
            
            if update_result.get('success', False):
                stats = update_result.get('statistics', {})
                
                response = f"""
✅ **Graph Updated Successfully**

🔄 **Update Results:**
• **Files Modified:** {stats.get('files_modified', 0)}
• **Entities Updated:** {stats.get('entities_updated', 0)}
• **Relationships Updated:** {stats.get('relationships_updated', 0)}
• **Execution Time:** {stats.get('execution_time', 0):.2f} seconds

📝 **Changes Processed:**
• **Commit:** {commit_hash}
• **Change Type:** {diff_analysis.get('change_type', 'Unknown')}

🎯 **Next Steps:**
1. Verify updates with `query_code_graph`
2. Check affected entities with `get_related_entities`
3. Monitor graph health through resources
                """
                
                return [TextContent(type="text", text=response.strip())]
            else:
                return [TextContent(
                    type="text",
                    text=f"❌ Update failed: {update_result.get('error', 'Unknown error')}"
                )]
                
        except Exception as e:
            self.logger.log_error(f"update_graph_from_diff failed: {e}")
            return [TextContent(
                type="text",
                text=f"❌ Error updating graph: {str(e)}"
            )]
    
    async def handle_query_code_graph(self, arguments: Dict[str, Any]) -> List[TextContent]:
        """Handle query_code_graph tool call using RAG service"""
        try:
            query_text = arguments.get("query")
            max_results = arguments.get("max_results", 10)
            context_depth = arguments.get("context_depth", 1)
            use_hybrid = arguments.get("use_hybrid", True)
            
            self.logger.log_info(f"Querying code graph: {query_text}")
            
            # Use RAG service for querying
            rag_service = self._get_rag_service()
            
            query = RAGQuery(
                query=query_text,
                max_results=max_results,
                context_depth=context_depth,
                use_hybrid=use_hybrid
            )
            
            result = await rag_service.query_code_graph(query)
            
            if result.result_count == 0:
                return [TextContent(
                    type="text",
                    text=f"🔍 No results found for query: '{query_text}'\n\nTry:\n• More general terms\n• Different keywords\n• Check if graph exists (run create_code_graph first)"
                )]
            
            # Format results
            response_parts = [
                f"🔍 **Query Results for:** '{query_text}'",
                f"📊 **Found {result.result_count} results in {result.execution_time:.2f} seconds**",
                f"🔧 **Search Type:** {'Hybrid (Graph + Semantic)' if result.hybrid_search_used else 'Vector Only'}",
                ""
            ]
            
            for i, item in enumerate(result.results[:max_results], 1):
                metadata = item.get('metadata', {})
                entity_name = metadata.get('entity_name', 'Unknown')
                entity_type = metadata.get('entity_type', 'unknown')
                file_path = metadata.get('file_path', 'Unknown')
                similarity = item.get('similarity_score', 0)
                
                response_parts.append(f"**{i}. {entity_name}** ({entity_type})")
                response_parts.append(f"   📄 File: `{file_path}`")
                response_parts.append(f"   🎯 Relevance: {similarity:.2f}")
                
                if context_depth > 0 and item.get('context'):
                    context_info = item['context']
                    related = context_info.get('related_entities', [])
                    if related:
                        related_names = [e.get('name', 'Unknown') for e in related[:3]]
                        response_parts.append(f"   🔗 Related: {', '.join(related_names)}")
                
                response_parts.append("")
            
            response_parts.extend([
                "🎯 **Next Steps:**",
                "• Use `get_related_entities` on specific entities for deeper exploration",
                "• Refine query with more specific terms if too many results",
                "• Increase context_depth for more relationship information"
            ])
            
            return [TextContent(type="text", text="\n".join(response_parts))]
            
        except Exception as e:
            self.logger.log_error(f"query_code_graph failed: {e}")
            return [TextContent(
                type="text",
                text=f"❌ Error querying code graph: {str(e)}"
            )]
    
    async def handle_get_related_entities(self, arguments: Dict[str, Any]) -> List[TextContent]:
        """Handle get_related_entities tool call using graph service"""
        try:
            entity_name = arguments.get("entity_name")
            max_depth = arguments.get("max_depth", 2)
            relationship_types = arguments.get("relationship_types", [])
            max_results = arguments.get("max_results", 20)
            
            self.logger.log_info(f"Getting related entities for: {entity_name}")
            
            # Use graph service for relationship traversal
            graph_service = self._get_graph_service()
            
            related_entities = await graph_service.get_related_entities(
                entity_name, 
                max_depth,
                relationship_types,
                max_results
            )
            
            if not related_entities:
                return [TextContent(
                    type="text",
                    text=f"🔍 No related entities found for: '{entity_name}'\n\nTry:\n• Check exact entity name spelling\n• Use query_code_graph first to find entity names\n• Ensure graph exists (run create_code_graph first)"
                )]
            
            # Format results
            response_parts = [
                f"🕸️ **Related Entities for:** '{entity_name}'",
                f"📊 **Found {len(related_entities)} related entities (depth: {max_depth})**",
                ""
            ]
            
            # Group by relationship type
            relationships_by_type = {}
            for entity in related_entities:
                rel_type = entity.get('relationship_type', 'UNKNOWN')
                if rel_type not in relationships_by_type:
                    relationships_by_type[rel_type] = []
                relationships_by_type[rel_type].append(entity)
            
            for rel_type, entities in relationships_by_type.items():
                response_parts.append(f"**{rel_type} ({len(entities)}):**")
                
                for entity in entities[:10]:  # Limit per type
                    name = entity.get('name', 'Unknown')
                    entity_type = entity.get('type', 'unknown')
                    file_path = entity.get('file_path', 'Unknown')
                    depth = entity.get('depth', 0)
                    
                    response_parts.append(f"  • **{name}** ({entity_type}) [depth: {depth}]")
                    response_parts.append(f"    📄 `{file_path}`")
                
                if len(entities) > 10:
                    response_parts.append(f"  ... and {len(entities) - 10} more")
                
                response_parts.append("")
            
            response_parts.extend([
                "🎯 **Analysis:**",
                f"• **Direct dependencies:** {len([e for e in related_entities if e.get('depth', 0) == 1])}",
                f"• **Indirect dependencies:** {len([e for e in related_entities if e.get('depth', 0) > 1])}",
                "",
                "💡 **Impact Assessment:**",
                "• Changes to this entity may affect all listed related entities",
                "• Consider relationship types when planning modifications",
                "• Use lower max_depth for focused analysis, higher for broad impact"
            ])
            
            return [TextContent(type="text", text="\n".join(response_parts))]
            
        except Exception as e:
            self.logger.log_error(f"get_related_entities failed: {e}")
            return [TextContent(
                type="text",
                text=f"❌ Error getting related entities: {str(e)}"
            )]
    
    def get_resources(self) -> List[Resource]:
        """Get list of available MCP resources"""
        return [
            Resource(
                uri="graph://stats",
                name="Graph Statistics",
                description="Current graph database statistics and health metrics",
                mimeType="application/json"
            ),
            Resource(
                uri="graph://entity-types", 
                name="Entity Types",
                description="Available entity types and their counts",
                mimeType="application/json"
            ),
            Resource(
                uri="graph://relationship-types",
                name="Relationship Types", 
                description="Available relationship types and their counts",
                mimeType="application/json"
            ),
            Resource(
                uri="rag://collection-info",
                name="RAG Collection Info",
                description="ChromaDB collection information and embedding statistics",
                mimeType="application/json"
            ),
            Resource(
                uri="system://service-health",
                name="Service Health",
                description="Health status of all registered services",
                mimeType="application/json"
            )
        ]
    
    async def handle_read_resource(self, uri: str) -> str:
        """Handle resource read requests"""
        try:
            if uri == "graph://stats":
                graph_service = self._get_graph_service()
                stats = await graph_service.get_graph_statistics()
                return json.dumps(stats, indent=2)
            
            elif uri == "graph://entity-types":
                graph_service = self._get_graph_service()
                entity_types = await graph_service.get_entity_types()
                return json.dumps(entity_types, indent=2)
            
            elif uri == "graph://relationship-types":
                graph_service = self._get_graph_service()
                rel_types = await graph_service.get_relationship_types()
                return json.dumps(rel_types, indent=2)
            
            elif uri == "rag://collection-info":
                rag_service = self._get_rag_service()
                # Implement collection info method
                info = {"status": "RAG service available", "collection": "code_embeddings"}
                return json.dumps(info, indent=2)
            
            elif uri == "system://service-health":
                health_info = {}
                
                # Check all services
                try:
                    pipeline_service = self._get_pipeline_service()
                    health_info['pipeline'] = await pipeline_service.validate_pipeline_config({})
                except Exception as e:
                    health_info['pipeline'] = f"Error: {e}"
                
                try:
                    graph_service = self._get_graph_service()
                    health_info['graph'] = "Available"
                except Exception as e:
                    health_info['graph'] = f"Error: {e}"
                
                try:
                    rag_service = self._get_rag_service()
                    health_info['rag'] = await rag_service.health_check()
                except Exception as e:
                    health_info['rag'] = f"Error: {e}"
                    
                return json.dumps(health_info, indent=2)
            
            else:
                return json.dumps({"error": f"Unknown resource URI: {uri}"})
                
        except Exception as e:
            self.logger.log_error(f"Resource read failed for {uri}: {e}")
            return json.dumps({"error": str(e)})