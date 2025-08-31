#!/usr/bin/env python3
"""
Test script to verify MCP server functionality
"""
import asyncio
import sys
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

async def test_mcp_server():
    """Test the MCP server initialization"""
    try:
        from core.bootstrap.application import create_application
        from core.api.mcp_server_refactored import CodeGrapherMCPServerRefactored
        
        print("✅ MCP dependencies available")
        
        # Test application creation
        app = await create_application()
        print("✅ Application created successfully")
        
        # Test MCP server creation
        mcp_server = CodeGrapherMCPServerRefactored(
            services=app.get_service_registry(),
            logger=app.get_logger()
        )
        print("✅ MCP server created successfully")
        
        # Get available tools
        tools = mcp_server.get_tools()
        print(f"✅ Available MCP tools: {len(tools)}")
        for tool in tools:
            print(f"   - {tool.name}: {tool.description}")
        
        await app.shutdown()
        print("✅ Test completed successfully")
        
    except Exception as e:
        print(f"❌ Test failed: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_mcp_server())

