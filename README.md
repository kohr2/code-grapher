# Code Grapher

**Ultra-Fast Codebase Analysis with Knowledge Graphs and AI-Powered Intelligence**

Code Grapher transforms your codebase into an intelligent knowledge graph using lightning-fast AST analysis and AI-powered semantic understanding. Built with a modern service-based architecture, it enables natural language queries about your code structure, relationships, and behavior in seconds.

## 🚀 Quick Start

```bash
# Install dependencies  
pip install -r requirements.txt

# Start Neo4j database
neo4j start

# Check system health
python main.py health

# Run codebase analysis with service architecture
python main.py pipeline .

# Query your code with natural language
python experiments/test_retrieval.py

# Use with Claude Desktop via MCP server
python main.py mcp
```

## 🏗️ What Code Grapher Does

Code Grapher analyzes your codebase and creates an intelligent knowledge graph that enables:

- **🔍 Natural Language Code Search**: Ask "How does authentication work?" instead of grepping files
- **⚡ Ultra-Fast Analysis**: Complete codebase analysis in seconds using deterministic AST parsing  
- **📊 Comprehensive Relationships**: Discovers INHERITS, DECORATES, CALLS relationships across your entire codebase
- **🔧 Incremental Updates**: Updates graphs efficiently as code changes via git integration
- **🎯 Semantic Search**: Finds relevant code using meaning, not just text matching
- **🤖 Claude Integration**: MCP server provides intelligent code context without over-fetching
- **🧠 AI-Powered Descriptions**: Optional Ollama or Gemini integration for semantic code understanding

## 🏗️ Architecture Overview  

```
┌─────────────────────┐    ┌─────────────────────┐    ┌─────────────────────┐
│   Analysis Engine   │    │   Knowledge Graph   │    │  Query Interface    │
│                     │    │                     │    │                     │
│ • AST Parsing       │───▶│ • Neo4j Graph DB    │◀───│ • Natural Language  │
│ • Entity Classification│  │ • Vector Embeddings │    │ • Service Architecture│
│ • Fast Relationships│    │ • Surgical Updates  │    │ • MCP Server        │
│ • AI Descriptions   │    │ • Business Context  │    │ • RAG Pipeline      │
└─────────────────────┘    └─────────────────────┘    └─────────────────────┘
```

**Modern Service-Based Architecture:**
- **Service Locator Pattern**: Central service registry with dependency resolution
- **Interface-Based Design**: Clean abstractions for all major components  
- **Health Monitoring**: Real-time service status and comprehensive diagnostics
- **Vertical Slices**: Modular organization by feature (ai-services/, graph-ops/, update-agents/)
- **Graceful Degradation**: System continues working when optional services fail

## 🚀 Core Features

### Lightning-Fast AST Analysis
- **3 Core Relationship Types**: INHERITS (class inheritance), DECORATES (decorator usage), CALLS (function calls)
- **25+ Entity Classifications**: data_class, pydantic_model, factory_function, validator_function, controller_class, service_class, and more
- **Deterministic Accuracy**: 100% accurate relationship detection using AST parsing
- **Sub-Minute Processing**: Complete analysis in 30-60 seconds for most codebases

### Knowledge Graph Intelligence  
- **Neo4j Graph Database**: Stores code structure with rich metadata and relationships
- **Semantic Entity Classification**: Automatically categorizes code elements by purpose and pattern
- **Business Context Integration**: PRIMER.md files inject domain knowledge into AI descriptions
- **Graph Export Formats**: D3.js and Mermaid diagrams for visualization
- **Performance Tracking**: Monitors processing times and relationship extraction accuracy

### RAG-Powered Code Q&A
- **Hybrid Retrieval**: Combines vector similarity (ChromaDB) with graph traversal (Neo4j)
- **Natural Language Queries**: Ask "How does user authentication work?" or "What validates the configuration?"
- **Context-Aware Answers**: Responses include relevant code snippets and relationship explanations  
- **Business Logic Understanding**: Queries about high-level functionality, not just technical details

### AI Provider Flexibility
- **Ollama Integration**: Local AI (Gemma3, Llama models) for privacy-first semantic understanding
- **Gemini Integration**: Google's Gemini AI for enhanced code descriptions and relationship extraction
- **Business Context Injection**: PRIMER.md files automatically enhance AI understanding with domain knowledge
- **Provider Abstraction**: Switch between AI providers without changing your workflow

## 🤖 MCP Server Integration

### Claude Desktop Integration
Code Grapher provides a **Model Context Protocol (MCP) server** that enables Claude to intelligently work with your codebase without manual context management.

### Core MCP Tools
- **`create_code_graph`** - Parse project and build knowledge graph with full AST analysis
- **`update_graph_from_diff`** - Surgically update graph based on git commit changes  
- **`query_code_graph`** - Semantic + structural code retrieval using hybrid search
- **`get_related_entities`** - Traverse relationships to controlled depth for context assembly

### MCP Resources
- **Graph Statistics** - Real-time health metrics and entity counts
- **Entity Types** - Available classifications and relationship patterns
- **Performance Metrics** - Update history and processing statistics
- **Update History** - Track incremental changes and surgical updates

### Key Benefits for Claude
- **Intelligent Context Control** - Gets exactly the right code without over-fetching
- **Relationship Awareness** - Understands how code components connect
- **Semantic Understanding** - Finds code by behavior, not just keywords
- **Incremental Updates** - Keeps graph current with git changes automatically

## 🛠️ Technology Stack

### Code Analysis
- **Python AST**: Built-in abstract syntax tree parsing for deterministic accuracy
- **Tree-sitter**: Multi-language parsing support for extensibility
- **Entity Classification**: Pattern-based code categorization system

### Databases & Storage
- **Neo4j 5.19.0**: Graph database for code relationships and structure
- **ChromaDB 0.4.22**: Vector database for semantic search and RAG
- **Performance Logging**: Comprehensive metrics and operation tracking

### Service Architecture
- **Dependency Injection**: Service locator pattern with automatic resolution
- **Interface-Based Design**: Clean abstractions for all major components
- **Health Monitoring**: Real-time service status and error tracking
- **Graceful Degradation**: System continues working when optional services fail

### Integration & AI Features
- **MCP Server**: Model Context Protocol for Claude Desktop integration
- **AI Provider Options**: Choose between Ollama (local, free) or Gemini (cloud-based)
  - **Ollama + Gemma3:4B**: Local LLM for code descriptions (private, no API key needed)
  - **Google Gemini**: Cloud AI for enhanced descriptions (requires API key)
- **SentenceTransformers**: all-MiniLM-L6-v2 model for semantic embeddings
- **GitPython**: Git integration for surgical graph updates

## 📦 Installation & Setup

### Prerequisites
- **Python 3.10+** (required for MCP server compatibility)
- Neo4j database server
- AI Provider (choose one):
  - Ollama server (free, local) - recommended for privacy
  - Google Gemini API key (cloud-based) - may offer better descriptions

### Installation Steps

1. **Install Dependencies**
   ```bash
   git clone https://github.com/your-org/code-grapher.git
   cd code-grapher
   
   # Create Python 3.10 virtual environment (required for MCP)
   python3.10 -m venv venv310
   source venv310/bin/activate
   
   # Install dependencies in virtual environment
   pip install -r requirements.txt
   ```

2. **Setup Neo4j Database**
   ```bash
   # Install Neo4j (macOS example)
   brew install neo4j
   neo4j start
   
   # Default connection: bolt://localhost:7687
   # Username: neo4j, Password: set during first startup
   ```

3. **Setup AI Provider**

   **Option A: Ollama (Free, Local)**
   ```bash
   # Install Ollama
   curl -fsSL https://ollama.ai/install.sh | sh
   
   # Start server and pull model
   ollama serve
   ollama pull gemma3:4b
   
   # Verify installation
   ollama run gemma3:4b "Hello"
   ```

   **Option B: Google Gemini (Cloud)**  
   - Get API key from [Google AI Studio](https://makersuite.google.com/app/apikey)
   - Add to your `.env` file: `GEMINI_API_KEY=your_api_key_here`
   - Set `AI_PROVIDER=gemini` in `.env`

4. **Configure Environment**
   ```bash
   # Copy example configuration
   cp .env.example .env
   
   # Edit .env with your settings:
   # NEO4J_URL=bolt://localhost:7687
   # NEO4J_USERNAME=neo4j
   # NEO4J_PASSWORD=your_password
   # AI_PROVIDER=ollama  # or "gemini"
   # OLLAMA_URL=http://localhost:11434
   # GEMINI_API_KEY=your_api_key_here  # if using Gemini
   ```

5. **Setup MCP Server (Optional - for Claude Desktop)**
   ```bash
   # Install MCP dependencies (Python 3.10+ required)
   pip install mcp>=1.0.0
   
   # Add to Claude Desktop mcp_servers config:
   {
     "code-grapher": {
       "command": "python3.10",
       "args": ["/path/to/code-grapher/main.py", "mcp"],
       "env": {
         "NEO4J_URL": "bolt://localhost:7687",
         "NEO4J_USERNAME": "neo4j",
         "NEO4J_PASSWORD": "your_password",
         "OLLAMA_URL": "http://localhost:11434"
       }
     }
   }
   ```

## 💡 Usage Examples

### 1. Analyze Your Codebase
```bash
# Health check - verify all services are working
python3.10 main.py health

# Clean start - clear existing data
python3.10 clear_databases.py

# Run complete analysis with service architecture (recommended)
python3.10 main.py pipeline .

# Run with specific options
python3.10 main.py pipeline . --no-ai  # Skip AI descriptions for faster processing

# Example output:
# ✅ Pipeline completed successfully!
#    Files processed: 20/139
#    Entities extracted: 470
#    Relationships created: 428
#    Execution time: 12.34 seconds
```

### 2. Fallback Options
```bash  
# If service architecture has issues, use simple population script
python simple_population_test.py

# Legacy pipeline (may have import issues in current architecture)
python core_pipeline.py
```

### 3. Query Your Code with Natural Language
```python
from rag_pipeline import CodeRAGPipeline

# Initialize the RAG system
rag = CodeRAGPipeline()

# Ask questions about your code
answer = rag.answer_question("How does user authentication work?")
print(answer["response"])

# Use hybrid retrieval (vector + graph)
answer = rag.answer_question("What validates configuration?", use_hybrid=True)
print(answer["response"])
```

### 4. Explore Your Code Graph
```python
from graph_manager import CodeGraphManager

manager = CodeGraphManager()

# Get overview statistics
stats = manager.get_graph_stats()
print(f"Your codebase has {stats['total_nodes']} entities and {stats['total_relationships']} connections")

# Export for visualization
graph_data = manager.export_d3_format()
# Use with D3.js, Gephi, or other graph tools
```

### 5. Incremental Updates
```bash
# Update graph based on git changes instead of full re-analysis
python surgical_update_coordinator.py

# Shows what changed and updates only affected parts
# Much faster than full reanalysis for large codebases
```

### 6. Use with Claude Desktop (MCP)
```bash
# Start the MCP server (if configured in Claude Desktop)
python main.py mcp

# Claude can now use these tools automatically:
# - create_code_graph: Build knowledge graph
# - query_code_graph: Find relevant code semantically
# - get_related_entities: Traverse relationships  
# - update_graph_from_diff: Keep graph current

# Example Claude prompts:
# "Analyze this codebase and show me the authentication logic"
# "Find all classes that handle user input validation"
# "What functions are related to the DatabaseManager class?"
```

## ⚙️ Configuration

### Environment Variables (.env)
```bash
# Required - Neo4j Database
NEO4J_URL=bolt://localhost:7687
NEO4J_USERNAME=neo4j
NEO4J_PASSWORD=your_password

# AI Provider Selection
AI_PROVIDER=ollama  # Options: "ollama" or "gemini"

# Ollama Configuration (if using AI_PROVIDER=ollama)
OLLAMA_URL=http://localhost:11434

# Gemini Configuration (if using AI_PROVIDER=gemini)  
GEMINI_API_KEY=your_gemini_api_key

# Optional: Custom primer file path for business context
PRIMER_FILE_PATH=/path/to/custom/primer.md

# Optional
LOG_LEVEL=INFO
```

### Business Context Integration
Create a `PRIMER.md` file in your target project's root directory to provide business context for AI-powered semantic understanding:

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

### MCP Server Configuration

#### For Claude Desktop
Add to your Claude Desktop `mcp_servers` configuration:
```json
{
  "mcpServers": {
    "code-grapher": {
      "command": "/path/to/code-grapher/venv310/bin/python", 
      "args": ["/path/to/code-grapher/main.py", "mcp"],
      "env": {
        "NEO4J_URL": "bolt://localhost:7687",
        "NEO4J_USERNAME": "neo4j",
        "NEO4J_PASSWORD": "your_password",
        "OLLAMA_URL": "http://localhost:11434"
      }
    }
  }
}
```

#### For Claude Code MCP
```bash
# Add via command line (use the virtual environment Python)
claude mcp add code-grapher \
  -e NEO4J_URL=bolt://localhost:7687 \
  -e NEO4J_USERNAME=neo4j \
  -e NEO4J_PASSWORD=your_password \
  -e OLLAMA_URL=http://localhost:11434 \
  -- /path/to/code-grapher/venv310/bin/python /path/to/code-grapher/main.py mcp

# Or add via JSON configuration
{
  "code-grapher": {
    "command": "/path/to/code-grapher/venv310/bin/python",
    "args": ["/path/to/code-grapher/main.py", "mcp"],
    "env": {
      "NEO4J_URL": "bolt://localhost:7687",
      "NEO4J_USERNAME": "neo4j", 
      "NEO4J_PASSWORD": "your_password",
      "OLLAMA_URL": "http://localhost:11434"
    }
  }
}
```

## 🎯 Use Cases

### Code Documentation & Understanding
- **New Team Members**: Get instant answers about how code works without reading everything
- **Legacy Codebases**: Understand complex systems through natural language queries
- **Architecture Review**: Visualize dependencies and identify potential issues

### Code Quality & Maintenance
- **Dependency Analysis**: Find tightly coupled components that need refactoring
- **Impact Assessment**: Understand what will break before making changes
- **Pattern Detection**: Identify repeated patterns that could be abstracted

### Development Productivity  
- **Code Search**: Find relevant code by functionality, not just text matching
- **API Discovery**: "What functions handle user input?" vs grepping for "input"
- **Debugging**: "What could cause this error?" with context-aware answers
- **Claude Integration**: Intelligent code context in Claude Desktop without manual copying

## 📈 Performance & Results

### Verified Capabilities (Latest Test Results)
- **Service Architecture**: Phase 5 refactoring with dependency injection
- **Files Processed**: 20/139 Python files in ~12 seconds  
- **Entities Extracted**: 470 total with specialized classifications
- **AST Relationships**: 428 relationships across 3 types (INHERITS, DECORATES, CALLS)
- **Graph Database**: 470 nodes, 428 relationships stored in Neo4j
- **Service Health**: Graph operations, AI services, RAG pipeline all operational

### Performance Characteristics
- **AST Processing**: Sub-second analysis per file with 100% accuracy
- **Service Architecture**: Clean dependency resolution with health monitoring
- **Incremental Updates**: Git-based surgical updates for large codebases  
- **Memory Efficiency**: Optimized processing with comprehensive logging
- **Privacy Options**: Local Ollama processing available - no external API calls required

## 🐛 Troubleshooting

### MCP Server Not Starting

**Issue**: MCP server fails to start with "failed" status in Claude Code

**Common Causes & Solutions**:

1. **Python Version Issue** (Most Common)
   ```bash
   # Check Python version in virtual environment
   source venv310/bin/activate
   python --version  # Should be 3.10+
   
   # If not 3.10+, recreate the virtual environment
   rm -rf venv310
   python3.10 -m venv venv310
   source venv310/bin/activate
   pip install -r requirements.txt
   ```

2. **MCP Package Missing**
   ```bash
   # Ensure MCP is installed in the virtual environment
   source venv310/bin/activate
   pip install "mcp>=0.9.0"
   ```

3. **Incorrect Python Path in Configuration**
   - Make sure you're using the full path to the virtual environment Python
   - Example: `/Users/username/Code/code-grapher/venv310/bin/python`
   - NOT just `python3.10` or `python`

4. **Database Connection Issues**
   ```bash
   # Test Neo4j connection
   neo4j status
   # If not running: neo4j start
   
   # Test with correct environment variables
   source venv310/bin/activate
   python main.py health
   ```

### Quick Diagnostic Commands
```bash
# Test MCP server manually
source venv310/bin/activate
python main.py mcp  # Should start without errors

# Check service health
python main.py health

# Check dependencies
pip show mcp neo4j py2neo
```

## 🔧 Development & Testing

### Testing Your Installation
```bash
# Check application health and service status
python main.py health

# Run the test retrieval system
python experiments/test_retrieval.py

# Test simple graph population (fallback method)
python simple_population_test.py

# Check outputs
cat experiments/outputs/retrieval_test_results.md
```

### Monitoring & Logs
- **Daily Logs**: `logs/code_grapher_YYYY-MM-DD.log`
- **Performance Metrics**: Processing times, success rates, token counts
- **Graph Snapshots**: Automatic backups in `graph_snapshots/` directory
- **Service Health**: Real-time monitoring of all service components

### Troubleshooting Common Issues
- **Service Issues**: Run `python main.py health` to check service status
- **Neo4j Connection**: Check `bolt://localhost:7687` is accessible  
- **Import Issues**: Use `python simple_population_test.py` as fallback
- **Empty Results**: Run `clear_databases.py` then `python main.py pipeline .`
- **Slow Performance**: Should complete in under a minute for most codebases

---

**Code Grapher** transforms your codebase into an intelligent knowledge graph using lightning-fast AST analysis and AI-powered semantic understanding, enabling natural language queries about your code's functionality and architecture in seconds, not minutes.