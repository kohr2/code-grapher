# Code Grapher - Technical Architecture Guide

Ultra-fast codebase analysis system that creates knowledge graphs from code using AST parsing and enables intelligent querying through RAG. Built with modern service-based architecture using dependency injection and health monitoring. This guide helps junior developers quickly understand the architecture and become productive.

## 🏗️ Project Architecture Overview

**Modern Service-Based Architecture:**
```
core/
├── bootstrap/          # Application initialization and dependency injection
├── api/               # MCP server handlers and API layer
├── interfaces/        # Core service contracts (Pipeline, RAG, Orchestrator)
├── orchestration/     # Service dependency resolution and coordination
└── services/          # Core business logic (Pipeline, RAG, Classification)

shared/
├── interfaces/        # Cross-cutting service interfaces (Logger, Graph, Config)
├── services/          # Service locator pattern and infrastructure
├── config/           # Configuration management and service setup
└── exceptions/       # Custom exception types and error handling

Vertical Slices (Feature-Organized):
├── ai-services/      # AI provider abstraction (Ollama, Gemini, Mock)
├── graph-ops/        # Neo4j operations and graph database management
├── update-agents/    # Git-based surgical update coordination
├── agents/           # Git diff analysis and coordination logic
└── coordinator/      # High-level agent and workflow coordination
```

**Key Design Patterns:**
- **Service Locator Pattern**: Central registry for service discovery and registration
- **Dependency Injection**: Automatic resolution of service dependencies through interfaces
- **Interface Segregation**: Clean abstractions for all major components
- **Health Monitoring**: Real-time service status checks and graceful degradation
- **Vertical Slice Architecture**: Feature-based organization for better modularity

## 🔧 Entry Points and Core Components

### Main Entry Points
1. **`main.py`** - Primary service-based entry point with modern architecture
   - `python main.py pipeline .` - Run analysis with full service orchestration
   - `python main.py mcp` - Start MCP server with dependency injection
   - `python main.py health` - Comprehensive service health diagnostics

2. **`simple_population_test.py`** - Lightweight fallback script
   - Bypasses service layer for basic graph population
   - Useful for testing when service architecture has issues
   - Direct graph operations without dependency resolution

3. **`core_pipeline.py`** - Legacy monolithic pipeline
   - Direct AST analysis without service layer
   - May have import issues in current modular architecture
   - Retained for backward compatibility

### Core Service Components

**Graph Operations (`shared/services/graph_manager_facade.py`):**
- Neo4j connection management and operations
- Entity creation and relationship building
- Performance monitoring and health checks

**AI Services (`ai-services/`):**
- Provider abstraction for Ollama and Gemini
- Business context injection via PRIMER.md files
- Relationship extraction and entity description generation

**Pipeline Orchestration (`core/orchestration/`):**
- Service dependency resolution
- Pipeline workflow coordination
- Error handling and recovery strategies

**RAG System (`core/services/rag_service.py`):**
- Hybrid retrieval combining Neo4j graph traversal and ChromaDB vector search
- Natural language query processing
- Context-aware response generation

## 🚀 Development Setup

### Prerequisites
- **Python 3.10+** (required for MCP server compatibility and modern async features)
- **Neo4j database** running on bolt://localhost:7687 (for graph storage)
- **AI Provider** (choose one):
  - Ollama server (free, local, privacy-first) - recommended for development
  - Google Gemini API key (cloud-based, enhanced capabilities)

### Environment Configuration (.env)
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

# Optional: Logging and Performance
LOG_LEVEL=INFO
```

### Service Dependencies
The application uses a service locator pattern with automatic dependency resolution:

```python
# Service registration happens in core/bootstrap/application.py
from core.bootstrap.application import create_application

# Services are resolved automatically based on interfaces
app = await create_application()
service_registry = app.get_service_registry()

# Get services by interface
graph_ops = service_registry.get(GraphOperationsInterface)
rag_service = service_registry.get(RAGServiceInterface)
```

## 🔄 Development Workflow

### Starting Development
```bash
# 1. Check service health and dependency status
python main.py health

# 2. Clear databases for fresh start
python clear_databases.py

# 3. Run analysis with modern service architecture (recommended)
python main.py pipeline .

# 4. Test RAG functionality and query system
python experiments/test_retrieval.py

# 5. If service architecture has issues, use fallback
python simple_population_test.py
```

### Common Development Tasks

**Adding a New Service:**
1. Create interface in `shared/interfaces/` or `core/interfaces/`
2. Implement service in appropriate vertical slice
3. Register in `core/bootstrap/application.py`
4. Add health check implementation

**Modifying AI Providers:**
- Work in `ai-services/providers/`
- All providers inherit from `BaseAIProvider` 
- Support both Ollama and Gemini through provider abstraction

**Extending Graph Operations:**
- Modify `shared/services/graph_manager_facade.py`
- Add new entity types in `entity_classifier.py`
- Update relationship extraction in `ast_relationship_extractor.py`

**MCP Server Development:**
- Core handlers in `core/api/mcp_handlers.py`
- Tool definitions and resource management
- Service integration for graph operations

## 📁 Repository Navigation

### Core Architecture Files
```
main.py                          # Primary service-based entry point
simple_population_test.py        # Lightweight fallback testing script
core_pipeline.py                 # Legacy monolithic pipeline (compatibility)

core/
├── bootstrap/application.py     # Application initialization with dependency injection
├── api/mcp_handlers.py         # MCP server tool implementations
├── orchestration/
│   ├── dependency_resolver.py  # Automatic service dependency resolution
│   └── pipeline_orchestrator.py # Pipeline workflow coordination
└── services/
    ├── pipeline_service.py     # Main pipeline orchestration service
    ├── rag_service.py          # RAG query processing and retrieval
    └── classification_service.py # Entity classification logic

shared/
├── services/
│   ├── service_locator.py      # Service discovery and locator pattern
│   ├── service_registry.py     # Central service registration system
│   └── graph_manager_facade.py # Graph operations facade and abstraction
└── interfaces/
    ├── logger_interface.py     # Logging service abstraction
    ├── graph_operations_interface.py # Graph operations contract
    └── service_interface.py    # Base service interface pattern
```

### Vertical Slice Organization
```
ai-services/                     # AI provider abstraction and services
├── providers/
│   ├── ollama_provider.py      # Local Ollama LLM integration
│   ├── gemini_provider.py      # Google Gemini cloud integration
│   ├── mock_provider.py        # Testing and development mock
│   └── base_provider.py        # Common provider interface
├── services/
│   ├── ai_service.py           # Core AI service orchestration
│   ├── description_service.py  # Entity description generation
│   └── relationship_service.py # AI-powered relationship extraction
└── interfaces/
    └── ai_services_interface.py # AI service contracts and abstractions

graph-ops/                       # Neo4j graph database operations
├── services/
│   ├── graph_service.py        # Core graph CRUD operations
│   └── export_service.py       # Graph export and visualization
├── repositories/
│   └── neo4j_repository.py     # Neo4j data access layer
└── interfaces/
    └── graph_repository_interface.py # Repository abstractions

update-agents/                   # Git-based surgical update system
├── services/
│   └── surgical_update_service.py # Git diff-based incremental updates
├── agents/
│   └── coordination_agent.py   # Update workflow coordination
└── interfaces/
    └── surgical_updater_interface.py # Update service contracts
```

### Key Legacy Files
```
graph_manager.py                 # Legacy graph operations (still used)
ast_relationship_extractor.py   # AST-based relationship detection
entity_classifier.py            # Entity type classification system
rag_pipeline.py                 # RAG query pipeline
surgical_update_coordinator.py  # Git-based update coordination
mcp_server.py                   # Legacy MCP server (still functional)
```

## 🛠️ Technology Stack Deep Dive

### Code Analysis Engine
- **Python AST**: Deterministic parsing with 100% accuracy for Python code
- **Tree-sitter**: Multi-language support (extensible beyond Python)
- **Entity Classification**: 25+ specialized classifications (data_class, service_class, factory_function, etc.)
- **Relationship Extraction**: INHERITS, DECORATES, CALLS with AST precision and completeness

### Database Architecture
- **Neo4j 5.19.0**: Primary graph storage for code relationships and structure
  - Nodes: Files, Classes, Functions with comprehensive metadata
  - Relationships: Typed connections (CONTAINS, CALLS, INHERITS, DECORATES, etc.)
  - Indexing: Name-based and type-based indexes for fast traversal
  - Performance: Optimized queries for relationship discovery

- **ChromaDB 0.4.22**: Vector storage for semantic search and RAG
  - Embeddings: SentenceTransformers all-MiniLM-L6-v2 model
  - Collections: Code entities with business context metadata
  - Similarity Search: Cosine similarity for semantic code retrieval
  - Hybrid Queries: Combined with Neo4j graph traversal

### Service Architecture
- **Service Locator Pattern**: Centralized service discovery and registration system
- **Dependency Injection**: Automatic interface resolution at runtime
- **Health Monitoring**: Comprehensive service status and error tracking
- **Graceful Degradation**: System continues operation when optional services fail
- **Interface-Based Design**: Clean abstractions for all major components

### AI Integration
- **Multi-Provider Support**: Pluggable AI providers (Ollama, Gemini, Mock)
- **Business Context Injection**: PRIMER.md files enhance AI understanding with domain knowledge
- **Relationship Extraction**: AI-powered discovery of complex semantic code relationships
- **Entity Description**: AI-generated semantic documentation for code components

## 🧪 Testing Architecture

### Test Organization
```
experiments/                     # Integration and system tests
├── test_retrieval.py           # RAG system and query testing
├── performance_baseline.py     # Performance benchmarking
├── outputs/                    # Test results and performance metrics
└── phase4_integration_tests/   # Legacy integration tests

tests/                          # Unit and integration tests
├── integration/
│   ├── test_phase1_functionality.py # Core functionality tests
│   └── test_phase5_architecture.py  # Service architecture tests
└── ai-services/tests/          # AI service specific unit tests

# Testing Commands
python main.py health                 # Service health and dependency check
python experiments/test_retrieval.py # RAG functionality and query testing
python simple_population_test.py     # Basic graph population test
python experiments/performance_baseline.py # Performance benchmarking
```

### Performance Monitoring
- **Daily Logs**: `logs/code_grapher_YYYY-MM-DD.log` with comprehensive operation tracking
- **Performance Metrics**: Processing times, token counts, success rates, memory usage
- **Graph Snapshots**: Automatic backups in `graph_snapshots/` with metadata
- **Service Metrics**: Health status, dependency resolution times, error tracking
- **AI Performance**: Provider response times, relationship extraction accuracy

## 🔌 MCP Server Integration

### MCP Architecture
The MCP server provides Claude Desktop integration through a modern service-based API with dependency injection:

```python
# MCP Server Entry Points
python main.py mcp              # Modern service-based MCP server with DI
python mcp_server.py           # Legacy MCP server (compatibility)

# Core MCP Tools
create_code_graph              # Build comprehensive knowledge graph
query_code_graph               # Hybrid semantic and structural search
get_related_entities           # Multi-depth relationship traversal
update_graph_from_diff         # Git-based surgical updates
```

### MCP Service Integration
- **Handler Layer**: `core/api/mcp_handlers.py` - Clean tool implementations using services
- **Service Layer**: Full dependency injection for graph operations, RAG, AI services
- **Resource Management**: Real-time graph statistics, entity types, performance metrics
- **Health Integration**: Service status reporting through MCP resources

## 🐛 Troubleshooting Guide

### Common Issues and Solutions

**Service Architecture Issues:**
```bash
# Check service health
python main.py health

# Look for service registration errors
grep "Service.*not registered" logs/code_grapher_*.log

# Use fallback if services fail
python simple_population_test.py
```

**Import/Dependency Issues:**
```bash
# Legacy pipeline import issues (ai-services relative imports)
python core_pipeline.py  # May fail

# Service architecture handles imports cleanly
python main.py pipeline .  # Preferred approach

# Simple fallback bypasses complex imports
python simple_population_test.py  # Always works
```

**Database Connection Issues:**
```bash
# Verify Neo4j is running
neo4j status

# Check connection in logs
grep "Neo4j" logs/code_grapher_*.log

# Test direct connection
python -c "from graph_manager import CodeGraphManager; gm = CodeGraphManager(); print('Connected')"
```

**AI Provider Issues:**
```bash
# Ollama connection
curl http://localhost:11434/api/tags

# Gemini API key
python -c "import os; print('GEMINI_API_KEY' in os.environ)"

# Service provider status
python main.py health | grep -i ai
```

## 🔄 Development Patterns

### Adding New Features
1. **Define Interface**: Create contract in appropriate `interfaces/` directory
2. **Implement Service**: Build concrete implementation in vertical slice
3. **Register Service**: Add to service registry in `core/bootstrap/application.py`
4. **Add Health Check**: Implement health monitoring for service
5. **Update MCP**: Add MCP tools/resources if needed for Claude integration

### Code Quality Standards
- **Interface Segregation**: Keep interfaces focused and minimal
- **Service Isolation**: Services should be independently testable
- **Error Handling**: Always provide graceful degradation
- **Logging**: Comprehensive operation logging for debugging
- **Performance**: Monitor and log processing times

### Testing Practices
- **Service Testing**: Mock dependencies using interfaces
- **Integration Testing**: Test service interactions
- **Performance Testing**: Monitor processing times and memory usage
- **Fallback Testing**: Ensure system works when services fail

---

This architecture enables rapid development while maintaining clean separation of concerns. New developers should start by running `python main.py health` to understand service status, then explore the vertical slices based on their area of interest.