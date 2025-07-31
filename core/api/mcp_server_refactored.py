"""
Refactored MCP Server

Phase 5 verticalization - MCP server using dependency injection instead of direct imports.
This replaces the monolithic mcp_server.py with service-based architecture.
"""

import asyncio
import json
from typing import List, Dict, Any

from mcp.server import Server
from mcp.server.models import Resource, Tool
from mcp.types import (
    TextContent,
    ImageContent,
    EmbeddedResource,
)
from mcp.server.stdio import stdio_server

from shared.interfaces.logger_interface import LoggerInterface
from shared.services.service_registry import ServiceRegistry
from core.api.mcp_handlers import MCPHandlers


class CodeGrapherMCPServerRefactored:
    """
    Refactored MCP Server using dependency injection
    
    Phase 5 verticalization - uses service interfaces instead of direct imports.
    This replaces the monolithic mcp_server.py with a cleaner, service-based architecture.
    """
    
    def __init__(self, services: ServiceRegistry, logger: LoggerInterface):
        self.services = services
        self.logger = logger
        
        # Initialize MCP server
        self.server = Server("code-grapher")
        
        # Initialize handlers with dependency injection
        self.handlers = MCPHandlers(services, logger)
        
        # Register MCP endpoints
        self._register_tools()
        self._register_resources()
        
        self.logger.log_info("Code Grapher MCP Server (Refactored) initialized")
    
    def _register_tools(self):
        """Register MCP tools using handlers"""
        
        @self.server.list_tools()
        async def list_tools() -> List[Tool]:
            return self.handlers.get_tools()
        
        @self.server.call_tool()
        async def call_tool(name: str, arguments: Dict[str, Any]) -> List[TextContent]:
            """Route tool calls to appropriate handlers"""
            try:
                self.logger.log_info(f"Tool called: {name} with args: {arguments}")
                
                if name == "create_code_graph":
                    return await self.handlers.handle_create_code_graph(arguments)
                
                elif name == "update_graph_from_diff":
                    return await self.handlers.handle_update_graph_from_diff(arguments)
                
                elif name == "query_code_graph":
                    return await self.handlers.handle_query_code_graph(arguments)
                
                elif name == "get_related_entities":
                    return await self.handlers.handle_get_related_entities(arguments)
                
                else:
                    return [TextContent(
                        type="text",
                        text=f"❌ Unknown tool: {name}"
                    )]
                    
            except Exception as e:
                self.logger.log_error(f"Tool call failed for {name}: {e}")
                return [TextContent(
                    type="text",
                    text=f"❌ Tool execution failed: {str(e)}"
                )]
    
    def _register_resources(self):
        """Register MCP resources using handlers"""
        
        @self.server.list_resources()
        async def list_resources() -> List[Resource]:
            return self.handlers.get_resources()
        
        @self.server.read_resource()
        async def read_resource(uri: str) -> str:
            """Route resource reads to appropriate handlers"""
            try:
                self.logger.log_info(f"Resource requested: {uri}")
                return await self.handlers.handle_read_resource(uri)
                
            except Exception as e:
                self.logger.log_error(f"Resource read failed for {uri}: {e}")
                return json.dumps({"error": str(e)})
    
    async def run(self):
        """Run the MCP server"""
        self.logger.log_info("Starting Code Grapher MCP Server (Refactored)")
        
        try:
            # Run with stdio transport
            async with stdio_server() as (read_stream, write_stream):
                await self.server.run(
                    read_stream,
                    write_stream,
                    self.server.create_initialization_options()
                )
        except Exception as e:
            self.logger.log_error(f"MCP Server failed: {e}")
            raise
    
    async def health_check(self) -> Dict[str, Any]:
        """Perform server health check"""
        try:
            health_status = {
                'server': 'healthy',
                'services': {},
                'tools': len(self.handlers.get_tools()),
                'resources': len(self.handlers.get_resources())
            }
            
            # Check service health through handlers
            try:
                service_health = await self.handlers.handle_read_resource("system://service-health")
                health_status['services'] = json.loads(service_health)
            except Exception as e:
                health_status['services'] = {'error': str(e)}
            
            return health_status
            
        except Exception as e:
            self.logger.log_error(f"Health check failed: {e}")
            return {
                'server': 'unhealthy',
                'error': str(e)
            }


# Main entry point for the refactored MCP server
async def main():
    """
    Main entry point for the refactored MCP server
    
    This should be called from the application bootstrap after services are initialized.
    """
    # Note: In the actual implementation, services would be injected from the application bootstrap
    # This is a placeholder for demonstration
    
    from shared.services.service_registry import ServiceRegistry
    from shared.services.service_locator import ServiceLocator
    
    try:
        # Get services from service locator (temporary solution)
        services = ServiceLocator.get_registry()
        logger = ServiceLocator.get_logger("mcp_server_refactored")
        
        # Create and run refactored server
        server = CodeGrapherMCPServerRefactored(services, logger)
        
        logger.log_info("🚀 Code Grapher MCP Server (Refactored) starting...")
        
        await server.run()
        
    except Exception as e:
        print(f"❌ Failed to start MCP server: {e}")
        raise


if __name__ == "__main__":
    asyncio.run(main())