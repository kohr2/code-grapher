# Code Grapher - AI Agent Instructions

## Project Overview
This project focuses on taking a codebase and creating/maintaining a graph database, then using it for RAG (Retrieval-Augmented Generation).

## Development Phase Instructions

### CRITICAL: Verbose Logging Requirements
- **ALWAYS LOG EXTENSIVELY** during development phase
- Log every major decision, action, and observation
- Include timestamps in all logs
- Log both successes AND failures with full context
- When analyzing code, log what patterns you find
- When creating graph relationships, log why you made those connections
- Log performance metrics (time taken, memory usage if relevant)

### Logging Format
```
[TIMESTAMP] [COMPONENT] [LEVEL] Message
Example: [2025-07-11 10:30:45] [GRAPH_BUILDER] [INFO] Analyzing file structure...
```

### AI Lego Bricks Evaluation Tracking
Track and document every interaction with AI Lego Bricks:
1. **What worked well** - Document successful patterns and approaches
2. **What didn't work** - Document failures, limitations, and pain points
3. **Performance observations** - Speed, accuracy, resource usage
4. **Suggestions for improvement** - Ideas for enhancing the system

### Core Project Components

#### 1. Codebase Analysis
- Parse and understand code structure
- Extract entities (functions, classes, modules, etc.)
- Identify relationships and dependencies
- Log every discovery with reasoning

#### 2. Graph Database Management
- Create nodes for code entities
- Establish edges for relationships
- Maintain consistency as code changes
- Track all graph modifications in logs

#### 3. RAG Pipeline
- Index code knowledge in graph
- Implement efficient retrieval mechanisms
- Generate contextual responses using graph data
- Log retrieval performance and accuracy

### Development Guidelines
1. **Be Verbose**: This is development - we need to see everything
2. **Track Everything**: Every decision matters for improving the system
3. **Question Assumptions**: If something seems off about AI Lego Bricks, document it
4. **Measure Performance**: Time operations, count iterations, track resource usage
5. **Document Patterns**: When you find recurring patterns, log them for future optimization

### Evaluation Metrics to Track
- Graph construction time per file/module
- Relationship accuracy (false positives/negatives)
- RAG retrieval relevance scores
- Query response times
- Memory usage patterns
- Error rates and types

### Daily Development Tasks
1. Start each session by reviewing previous logs
2. Document session goals in logs
3. Track progress against goals
4. End session with summary of findings
5. Note any AI Lego Bricks issues for updates

### AI Agent Strategy
- Make liberal use of sub-agents

Remember: We're in development mode - more logging is always better than less!