# Code Grapher

Ultra-fast codebase analysis system that creates knowledge graphs from code using AST parsing and enables intelligent querying through RAG.

## Project Architecture

**Core Pipeline System:**
- **Entry Point**: `core_pipeline.py` - Lightning-fast AST analysis engine
- **Graph Database**: Neo4j for code relationships and structure
- **Vector Database**: ChromaDB for semantic search
- **Relationship Engine**: Deterministic AST-based relationship extraction
- **RAG System**: Hybrid retrieval combining graph + vector search
- **Primer System**: Business context injection for AI-powered semantic understanding

**Key Components:**
- `graph_manager.py` - Neo4j operations and graph building
- `ast_relationship_extractor.py` - Fast deterministic relationship detection (INHERITS, DECORATES, CALLS)
- `rag_pipeline.py` - Question answering with ChromaDB + Neo4j
- `entity_classifier.py` - 25+ specialized entity classifications
- `surgical_update_coordinator.py` - Git-based incremental updates
- `mcp_server.py` - Model Context Protocol server for Claude integration

## Development Setup

**Prerequisites:**
- Python 3.10+
- Neo4j database (bolt://localhost:7687)
- Ollama server (optional - for AI descriptions only)

**Installation:**
```bash
pip install -r requirements.txt
```

**Environment (.env file):**
```bash
NEO4J_URL=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=your_password

# AI Provider Selection
AI_PROVIDER=ollama  # Options: "ollama" or "gemini"

# Ollama Configuration (if using ollama)
OLLAMA_URL=http://localhost:11434

# Gemini Configuration (if using gemini)
GEMINI_API_KEY=your_gemini_api_key

# Optional: Custom primer file path
PRIMER_FILE_PATH=/path/to/custom/primer.md
```

**Business Context Primer:**
Create a `PRIMER.md` file in the **target project's root directory** (the project being analyzed, not the code-grapher MCP server directory) to provide business context for AI-powered semantic understanding:

```markdown
# Project Business Context

## Project Overview
Brief description of what this codebase does

## Business Purpose  
- Primary goals and objectives
- Target users and use cases
- Core value proposition

## Key Business Concepts
- Domain-specific terminology
- Important business rules
- Integration patterns

## Quality Standards
- Performance requirements
- Scalability needs
- Integration requirements
```

The primer context is automatically injected into AI entity descriptions to provide more accurate, business-aware semantic understanding.

**Start services:**
```bash
# Required: Neo4j
neo4j start

# Optional: Ollama for AI descriptions
ollama serve
ollama pull gemma3:4b
```

## Development Workflow

**Main Commands:**
```bash
# Lightning-fast codebase analysis (primary workflow)
python core_pipeline.py

# Clear databases before fresh analysis  
python clear_databases.py

# Test RAG capabilities
python experiments/test_retrieval.py

# Incremental updates from git commits
python surgical_update_coordinator.py

# Run MCP server for Claude Desktop integration
python mcp_server.py
```

**Testing:**
- Tests in `experiments/` directory
- Results output to `experiments/outputs/`
- Both JSON and Markdown reports generated

**Monitoring:**
- Daily logs in `logs/` directory
- Performance metrics tracked per operation
- Graph snapshots saved in `graph_snapshots/`

## Technology Stack

**Code Analysis:**
- Python AST parsing (deterministic, fast)
- tree-sitter for multi-language support
- Entity classification system

**Databases:**
- Neo4j 5.19.0 (graph storage)
- ChromaDB 0.4.22 (vector storage)

**Optional AI:**
- Ollama (local LLM serving for descriptions)
- sentence-transformers (text embeddings)
- google-generativeai (Gemini integration)

**Development:**
- pytest for testing
- black/flake8/mypy for code quality
- Comprehensive logging and performance tracking
- GitPython for incremental updates

## MCP Server Integration

**MCP Server:** `mcp_server.py` - Model Context Protocol server for Claude integration

**Tools Available:**
- `create_code_graph` - Parse project and build knowledge graph
- `update_graph_from_diff` - Incremental updates from git changes
- `query_code_graph` - Semantic + structural code retrieval  
- `get_related_entities` - Traverse relationships to specified depth

**Resources:**
- Graph statistics and health metrics
- Entity types and relationship patterns  
- Update history and performance stats

**Setup:**
```bash
# Install MCP dependencies
pip install mcp>=1.0.0

# Configure Claude Desktop (add to mcp_servers config)
{
  "mcpServers": {
    "code-grapher": {
      "command": "python3.10",
      "args": ["/path/to/code-grapher/mcp_server.py"],
      "env": {
        "NEO4J_URL": "bolt://localhost:7687",
        "NEO4J_USERNAME": "neo4j",
        "OLLAMA_URL": "http://localhost:11434"
      }
    }
  }
}

# Configure Claude Code MCP (command line)
claude mcp add code-grapher -e NEO4J_URL=bolt://localhost:7687 -e NEO4J_USERNAME=neo4j -e AI_PROVIDER=ollama -e OLLAMA_URL=http://localhost:11434 -- python3.10 /path/to/code-grapher/mcp_server.py

# Or via JSON configuration:
{
  "code-grapher": {
    "command": "python3.10",
    "args": ["/path/to/code-grapher/mcp_server.py"],
    "env": {
      "NEO4J_URL": "bolt://localhost:7687",
      "NEO4J_USERNAME": "neo4j",
      "OLLAMA_URL": "http://localhost:11434"
    }
  }
}
```

**Usage with Claude:**
- Automatically provides intelligent code context without over-fetching
- Enables precise entity relationship traversal
- Supports natural language code behavior queries
- Maintains context boundaries for optimal token usage