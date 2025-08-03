#!/opt/homebrew/bin/python3.10
"""
Main Entry Point

Phase 5 verticalization - main application entry point with proper dependency injection.
This replaces the monolithic approach with a clean, service-based architecture.

Requires Python 3.10+ for MCP server compatibility and modern async features.
"""

import asyncio
import sys
import argparse
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent))

from core.bootstrap.application import create_application

# Optional MCP server import
try:
    from core.api.mcp_server_refactored import CodeGrapherMCPServerRefactored
    MCP_AVAILABLE = True
except ImportError:
    CodeGrapherMCPServerRefactored = None
    MCP_AVAILABLE = False


async def run_mcp_server():
    """Run the MCP server with proper service initialization"""
    if not MCP_AVAILABLE:
        print("❌ MCP server not available - missing dependencies (mcp package)")
        print("Install with: pip install mcp")
        return
    
    print("🚀 Starting Code Grapher MCP Server (Phase 5)")
    
    try:
        # Create and initialize application with all services
        app = await create_application()
        
        # Create and run MCP server with dependency injection
        mcp_server = CodeGrapherMCPServerRefactored(
            services=app.get_service_registry(),
            logger=app.get_logger()
        )
        
        # Run the MCP server
        await mcp_server.run()
        
    except KeyboardInterrupt:
        print("\n🔄 Shutting down gracefully...")
        if 'app' in locals():
            await app.shutdown()
    except Exception as e:
        print(f"❌ MCP Server failed: {e}")
        raise


async def run_pipeline(directory: str, use_ai: bool = True):
    """Run the pipeline directly with service architecture"""
    print(f"🚀 Running Code Grapher Pipeline (Phase 5) on: {directory}")
    
    try:
        # Create and initialize application
        app = await create_application()
        
        # Get pipeline service
        try:
            from core.interfaces.pipeline_interface import PipelineInterface
            pipeline_service = app.get_service_registry().get(PipelineInterface)
        except Exception as e:
            print(f"❌ Pipeline service not available: {e}")
            print("Note: Some services may be disabled due to missing dependencies")
            await app.shutdown()
            return
        
        # Run pipeline
        result = await pipeline_service.run_enhanced_pipeline(
            target_directory=directory,
            use_ai=use_ai
        )
        
        # Display results
        if result.status.value == "success":
            data = result.data or {}
            print(f"\n✅ Pipeline completed successfully!")
            print(f"   Files processed: {data.get('files_processed', 0)}")
            print(f"   Entities extracted: {data.get('graph_stats', {}).get('total_entities', 0)}")
            print(f"   Relationships created: {data.get('graph_stats', {}).get('total_relationships', 0)}")
            print(f"   Execution time: {result.execution_time:.2f} seconds")
        else:
            print(f"\n❌ Pipeline failed: {result.message}")
            if result.errors:
                for error in result.errors:
                    print(f"   Error: {error}")
        
        # Shutdown gracefully
        await app.shutdown()
        
    except Exception as e:
        print(f"❌ Pipeline execution failed: {e}")
        raise


async def run_health_check():
    """Run application health check"""
    print("🏥 Running Code Grapher Health Check (Phase 5)")
    
    try:
        # Create and initialize application
        app = await create_application()
        
        # Get application info
        info = app.get_application_info()
        
        print(f"\n📊 Application Status:")
        print(f"   Name: {info['name']}")
        print(f"   Version: {info['version']}")
        print(f"   Phase: {info['phase']}")
        print(f"   Initialized: {'✅ Yes' if info['is_initialized'] else '❌ No'}")
        print(f"   Services: {len(info['registered_services'])}")
        
        print(f"\n🔧 Registered Services:")
        for service_name in info['registered_services']:
            print(f"   • {service_name}")
        
        print(f"\n🏥 Service Health:")
        for service_name, health in info['services_health'].items():
            status = health.get('status', 'unknown')
            emoji = '✅' if status == 'healthy' else ('❌' if status == 'error' else '⚠️')
            print(f"   {emoji} {service_name}: {status}")
            
            if 'error' in health:
                print(f"      Error: {health['error']}")
        
        # Shutdown gracefully
        await app.shutdown()
        
        return info['is_initialized'] and all(
            h.get('status') in ['healthy', 'unknown'] 
            for h in info['services_health'].values()
        )
        
    except Exception as e:
        print(f"❌ Health check failed: {e}")
        return False


def main():
    """Main CLI entry point"""
    parser = argparse.ArgumentParser(
        description="Code Grapher - Phase 5 Core Orchestration Refactoring",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  python main.py mcp                           # Run MCP server
  python main.py pipeline /path/to/code       # Run pipeline on directory
  python main.py pipeline . --no-ai           # Run pipeline without AI
  python main.py health                       # Check application health

Phase 5 Features:
  • Service-based architecture with dependency injection
  • Refactored MCP server using service interfaces
  • Pipeline orchestrator replacing monolithic core_pipeline.py
  • RAG service extracted from rag_pipeline.py
  • Comprehensive health monitoring and error handling
        """
    )
    
    subparsers = parser.add_subparsers(dest='command', help='Available commands')
    
    # MCP Server command
    mcp_parser = subparsers.add_parser(
        'mcp', 
        help='Run MCP server with service architecture'
    )
    
    # Pipeline command
    pipeline_parser = subparsers.add_parser(
        'pipeline',
        help='Run pipeline directly on a directory'
    )
    pipeline_parser.add_argument(
        'directory',
        help='Directory to analyze'
    )
    pipeline_parser.add_argument(
        '--no-ai',
        action='store_true',
        help='Disable AI descriptions and relationship extraction'
    )
    
    # Health check command
    health_parser = subparsers.add_parser(
        'health',
        help='Run application health check'
    )
    
    args = parser.parse_args()
    
    if not args.command:
        parser.print_help()
        return
    
    try:
        if args.command == 'mcp':
            asyncio.run(run_mcp_server())
        
        elif args.command == 'pipeline':
            use_ai = not args.no_ai
            asyncio.run(run_pipeline(args.directory, use_ai))
        
        elif args.command == 'health':
            success = asyncio.run(run_health_check())
            sys.exit(0 if success else 1)
        
        else:
            print(f"❌ Unknown command: {args.command}")
            parser.print_help()
            sys.exit(1)
            
    except KeyboardInterrupt:
        print("\n🔄 Interrupted by user")
        sys.exit(0)
    except Exception as e:
        print(f"❌ Execution failed: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()