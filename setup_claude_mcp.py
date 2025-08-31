#!/usr/bin/env python3
"""
Setup script for Claude MCP integration
"""
import json
import os
from pathlib import Path

def create_claude_config():
    """Create Claude MCP configuration"""
    
    # Get current directory
    current_dir = Path(__file__).parent.absolute()
    
    # Get Neo4j password from user
    print("🔧 Setting up Claude MCP Configuration")
    print("=" * 50)
    
    neo4j_password = input("Enter your Neo4j password (or press Enter to use default 'password'): ").strip()
    if not neo4j_password:
        neo4j_password = "password"
    
    # Create configuration
    config = {
        "mcpServers": {
            "code-grapher": {
                "command": "python",
                "args": [
                    str(current_dir / "main.py"),
                    "mcp"
                ],
                "env": {
                    "NEO4J_URL": "bolt://localhost:7687",
                    "NEO4J_USERNAME": "neo4j",
                    "NEO4J_PASSWORD": neo4j_password,
                    "AI_PROVIDER": "ollama",
                    "OLLAMA_URL": "http://localhost:11434"
                },
                "cwd": str(current_dir)
            }
        }
    }
    
    # Write configuration
    config_file = current_dir / "claude_mcp_config.json"
    with open(config_file, 'w') as f:
        json.dump(config, f, indent=2)
    
    print(f"✅ Configuration saved to: {config_file}")
    
    # Create environment file
    env_file = current_dir / ".env"
    env_content = f"""# Code Grapher Environment Configuration
NEO4J_URL=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD={neo4j_password}
AI_PROVIDER=ollama
OLLAMA_URL=http://localhost:11434
"""
    
    with open(env_file, 'w') as f:
        f.write(env_content)
    
    print(f"✅ Environment file created: {env_file}")
    
    return config_file

def print_instructions(config_file):
    """Print setup instructions"""
    print("\n" + "=" * 50)
    print("🚀 CLAUDE MCP SETUP COMPLETE!")
    print("=" * 50)
    
    print("\n📋 Next Steps:")
    print("1. Copy this configuration file to Claude's MCP directory:")
    print(f"   cp {config_file} ~/.claude/mcp/")
    print("   (or wherever Claude expects MCP configurations)")
    
    print("\n2. Restart Claude Desktop/Web")
    
    print("\n3. Claude should now have access to these Code Grapher tools:")
    print("   • Code analysis and relationship extraction")
    print("   • Graph database operations")
    print("   • RAG (Retrieval-Augmented Generation) services")
    print("   • Pipeline orchestration")
    
    print("\n🔧 Available MCP Tools:")
    print("   • analyze_code: Analyze code files and extract relationships")
    print("   • query_graph: Query the Neo4j graph database")
    print("   • run_pipeline: Execute the full code analysis pipeline")
    print("   • health_check: Check system health and status")
    
    print("\n💡 Usage Example:")
    print("   'Claude, analyze the code in my project and create a graph of relationships'")
    print("   'Show me the current graph statistics'")
    print("   'Run the code analysis pipeline on my project'")

if __name__ == "__main__":
    try:
        config_file = create_claude_config()
        print_instructions(config_file)
    except Exception as e:
        print(f"❌ Setup failed: {e}")
        import traceback
        traceback.print_exc()

