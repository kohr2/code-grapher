# Code Grapher - Business Context Primer

## Project Overview
Code Grapher is an enterprise-grade codebase analysis system designed to create comprehensive knowledge graphs from source code using AST parsing and semantic analysis.

## Business Purpose
- **Primary Goal**: Enable intelligent code discovery and understanding at scale
- **Target Users**: Senior engineers, technical leads, and AI-assisted development workflows
- **Core Value**: Transform static codebases into queryable knowledge graphs

## Key Business Concepts
- **Graph Creation**: Parse codebases into structured knowledge representations
- **Semantic Analysis**: Use AI to understand code behavior and purpose beyond syntax
- **Relationship Detection**: Identify how code components interact and depend on each other
- **RAG Integration**: Enable natural language queries against code knowledge

## Domain-Specific Terms
- **Entity Classification**: Categorizing code elements (functions, classes, etc.) with business context
- **Surgical Updates**: Incremental graph updates based on git changes to avoid full re-analysis
- **Primer Context**: Business context injection for AI-powered semantic understanding
- **MCP Integration**: Model Context Protocol for Claude Desktop integration

## Quality Standards
- **Performance**: Sub-minute analysis for typical codebases
- **Accuracy**: Deterministic AST-based relationship extraction
- **Scalability**: Handle large enterprise codebases efficiently
- **Integration**: Seamless workflow integration via MCP and APIs