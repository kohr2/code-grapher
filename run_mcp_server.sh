#!/bin/bash
# MCP Server Wrapper Script
# This script properly activates the virtual environment and runs the MCP server

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Change to the project directory
cd "$SCRIPT_DIR"

# Activate the virtual environment
source venv/bin/activate

# Set environment variables
export NEO4J_URL="bolt://localhost:7687"
export NEO4J_USERNAME="neo4j"
export NEO4J_PASSWORD="codegrapher"
export AI_PROVIDER="ollama"
export OLLAMA_URL="http://localhost:11434"
export PYTHONPATH="$SCRIPT_DIR"
export VIRTUAL_ENV="$SCRIPT_DIR/venv"

# Run the MCP server
exec python main.py mcp