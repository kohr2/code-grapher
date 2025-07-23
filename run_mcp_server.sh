#!/bin/bash
# MCP Server wrapper script to ensure correct working directory

# Change to the code-grapher directory
cd "$(dirname "$0")"

# Run the MCP server with python3.10
exec python3.10 mcp_server.py "$@"