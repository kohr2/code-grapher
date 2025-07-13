#!/usr/bin/env python3
"""
Agent Flow Visualizer for Code Grapher
Creates visual diagrams showing agent pipeline flow and retrieval flow
"""

import json
import os
from datetime import datetime
from typing import Dict, List, Any
import mlflow
from pathlib import Path

class AgentFlowVisualizer:
    """Creates visual diagrams of agent pipeline flow"""
    
    def __init__(self):
        self.config_path = "/Users/danielbeach/Code/code-grapher/config/agent_pipeline_config.json"
        self.mlflow_db = "sqlite:///mlflow.db"
        
    def load_agent_config(self) -> Dict[str, Any]:
        """Load agent configuration"""
        with open(self.config_path, 'r') as f:
            return json.load(f)
    
    def get_mlflow_data(self) -> Dict[str, Any]:
        """Get current MLflow tracking data"""
        mlflow.set_tracking_uri(self.mlflow_db)
        client = mlflow.tracking.MlflowClient()
        
        experiment = client.get_experiment_by_name('CodeGrapher-Agent-Pipeline')
        if not experiment:
            return {"runs": [], "agents": []}
            
        runs = client.search_runs([experiment.experiment_id])
        
        # Separate pipeline runs from agent runs
        pipeline_runs = [run for run in runs if "pipeline_" in run.info.run_name]
        agent_runs = [run for run in runs if "agent_" in run.info.run_name]
        
        return {
            "pipeline_runs": len(pipeline_runs),
            "agent_runs": len(agent_runs),
            "recent_agents": [run.info.run_name.split('_')[1] for run in agent_runs[:5]],
            "last_run": pipeline_runs[0].info.run_name if pipeline_runs else None
        }
    
    def create_mermaid_flow_diagram(self) -> str:
        """Create Mermaid diagram showing agent flow"""
        config = self.load_agent_config()
        mlflow_data = self.get_mlflow_data()
        
        # Get agent sequence from config
        workers = config['agents']['supervisor']['config']['workers']
        
        mermaid = "```mermaid\n"
        mermaid += "graph TD\n"
        mermaid += "    %% Code Grapher Agent Pipeline Flow\n"
        mermaid += "    START([Start: Source Code]) --> PARSE[Code Parser Agent]\n"
        mermaid += "    PARSE --> EXTRACT[Entity Extraction Agent]\n" 
        mermaid += "    EXTRACT --> RELATE[Relationship Analysis Agent]\n"
        mermaid += "    RELATE --> GRAPH[Graph Builder Agent]\n"
        mermaid += "    GRAPH --> RAG[RAG Indexer Agent]\n"
        mermaid += "    RAG --> END([End: Knowledge Graph + Vector Index])\n"
        mermaid += "\n"
        mermaid += "    %% Data Flow\n"
        mermaid += "    PARSE -.->|AST Objects| EXTRACT\n"
        mermaid += "    EXTRACT -.->|Entities| RELATE\n"
        mermaid += "    RELATE -.->|Relationships| GRAPH\n"
        mermaid += "    GRAPH -.->|Graph Nodes| RAG\n"
        mermaid += "\n"
        mermaid += "    %% Storage Systems\n"
        mermaid += "    GRAPH --> NEO4J[(Neo4j Database)]\n"
        mermaid += "    RAG --> CHROMA[(ChromaDB Vector Store)]\n"
        mermaid += "\n"
        mermaid += "    %% Retrieval Flow\n"
        mermaid += "    QUERY([User Query]) --> HYBRID{Hybrid Retrieval}\n"
        mermaid += "    HYBRID --> GRAPH_SEARCH[Graph Query]\n"
        mermaid += "    HYBRID --> VECTOR_SEARCH[Vector Search]\n"
        mermaid += "    GRAPH_SEARCH --> NEO4J\n"
        mermaid += "    VECTOR_SEARCH --> CHROMA\n"
        mermaid += "    NEO4J --> COMBINE[Combine Results]\n"
        mermaid += "    CHROMA --> COMBINE\n"
        mermaid += "    COMBINE --> LLM[Gemini Flash LLM]\n"
        mermaid += "    LLM --> ANSWER([Generated Answer])\n"
        mermaid += "\n"
        mermaid += "    %% Styling\n"
        mermaid += "    classDef agent fill:#e1f5fe,stroke:#01579b,stroke-width:2px\n"
        mermaid += "    classDef storage fill:#f3e5f5,stroke:#4a148c,stroke-width:2px\n"
        mermaid += "    classDef process fill:#fff3e0,stroke:#e65100,stroke-width:2px\n"
        mermaid += "    classDef endpoint fill:#e8f5e8,stroke:#2e7d32,stroke-width:2px\n"
        mermaid += "\n"
        mermaid += "    class PARSE,EXTRACT,RELATE,GRAPH,RAG agent\n"
        mermaid += "    class NEO4J,CHROMA storage\n"
        mermaid += "    class HYBRID,COMBINE,LLM process\n"
        mermaid += "    class START,END,QUERY,ANSWER endpoint\n"
        mermaid += "```\n"
        
        return mermaid
    
    def create_ascii_flow_diagram(self) -> str:
        """Create ASCII diagram for terminal viewing"""
        config = self.load_agent_config()
        mlflow_data = self.get_mlflow_data()
        
        ascii_diagram = f"""
╔═══════════════════════════════════════════════════════════════════════════════╗
║                        CODE GRAPHER AGENT PIPELINE FLOW                      ║
║                            Last Run: {mlflow_data.get('last_run', 'None')[:30]:<30} ║
╠═══════════════════════════════════════════════════════════════════════════════╣

📁 SOURCE CODE
    │
    ▼
┌─────────────────────────┐
│   1. CODE PARSER AGENT  │ ──────► AST Objects
│   • Parse source files  │         (syntax trees)
│   • Extract syntax tree │
└─────────────────────────┘
    │
    ▼
┌─────────────────────────┐
│ 2. ENTITY EXTRACT AGENT │ ──────► Code Entities
│   • Find classes/funcs  │         (classes, functions)
│   • Extract variables   │
└─────────────────────────┘
    │
    ▼
┌─────────────────────────┐
│ 3. RELATIONSHIP AGENT   │ ──────► Relationships
│   • Analyze deps        │         (calls, inherits)
│   • Find connections    │
└─────────────────────────┘
    │
    ▼
┌─────────────────────────┐      ┌─────────────────┐
│  4. GRAPH BUILDER AGENT │ ────►│   Neo4j Graph   │
│   • Create graph nodes  │      │   Database      │
│   • Build relationships │      └─────────────────┘
└─────────────────────────┘
    │
    ▼
┌─────────────────────────┐      ┌─────────────────┐
│   5. RAG INDEXER AGENT  │ ────►│  ChromaDB       │
│   • Create embeddings   │      │  Vector Store   │
│   • Index for search    │      └─────────────────┘
└─────────────────────────┘

╔═══════════════════════════════════════════════════════════════════════════════╗
║                              RETRIEVAL FLOW                                  ║
╚═══════════════════════════════════════════════════════════════════════════════╝

🔍 USER QUERY ────────────────┐
                              │
                              ▼
                    ┌─────────────────────┐
                    │   HYBRID RETRIEVAL  │
                    │   (Graph + Vector)  │
                    └─────────────────────┘
                              │
                ┌─────────────┼─────────────┐
                ▼             ▼             ▼
    ┌─────────────────┐ ┌─────────────┐ ┌─────────────────┐
    │  Graph Query    │ │   Vector    │ │  Combine        │
    │  (Neo4j)        │ │   Search    │ │  Results        │
    │                 │ │  (ChromaDB) │ │                 │
    └─────────────────┘ └─────────────┘ └─────────────────┘
                              │
                              ▼
                    ┌─────────────────────┐
                    │   GEMINI FLASH LLM  │
                    │   Generate Answer   │
                    └─────────────────────┘
                              │
                              ▼
                         💬 RESPONSE

📊 CURRENT STATUS:
  • Pipeline Runs: {mlflow_data.get('pipeline_runs', 0)}
  • Agent Runs: {mlflow_data.get('agent_runs', 0)}
  • Recent Agents: {', '.join(mlflow_data.get('recent_agents', [])[:3])}
  • MLflow Tracking: Active (sqlite:///mlflow.db)
  • Dashboard: http://localhost:5002

        """
        return ascii_diagram
    
    def create_html_interactive_diagram(self) -> str:
        """Create interactive HTML diagram"""
        config = self.load_agent_config()
        mlflow_data = self.get_mlflow_data()
        
        html = f"""
<!DOCTYPE html>
<html>
<head>
    <title>Code Grapher Agent Flow</title>
    <script src="https://unpkg.com/mermaid@10/dist/mermaid.min.js"></script>
    <style>
        body {{ font-family: Arial, sans-serif; margin: 20px; }}
        .container {{ max-width: 1200px; margin: 0 auto; }}
        .status {{ background: #f0f8ff; padding: 15px; border-radius: 5px; margin: 20px 0; }}
        .diagram {{ border: 1px solid #ddd; padding: 20px; margin: 20px 0; }}
    </style>
</head>
<body>
    <div class="container">
        <h1>🤖 Code Grapher Agent Pipeline Flow</h1>
        
        <div class="status">
            <h3>📊 Current Status</h3>
            <ul>
                <li><strong>Pipeline Runs:</strong> {mlflow_data.get('pipeline_runs', 0)}</li>
                <li><strong>Agent Runs:</strong> {mlflow_data.get('agent_runs', 0)}</li>
                <li><strong>Recent Agents:</strong> {', '.join(mlflow_data.get('recent_agents', [])[:3])}</li>
                <li><strong>MLflow Dashboard:</strong> <a href="http://localhost:5002">http://localhost:5002</a></li>
            </ul>
        </div>
        
        <div class="diagram">
            <h3>🔄 Agent Pipeline Flow</h3>
            <div class="mermaid">
                {self.create_mermaid_flow_diagram().replace('```mermaid', '').replace('```', '')}
            </div>
        </div>
        
        <div class="status">
            <h3>ℹ️ Flow Explanation</h3>
            <ol>
                <li><strong>Code Parser:</strong> Reads source files and creates AST representations</li>
                <li><strong>Entity Extractor:</strong> Identifies classes, functions, variables from AST</li>
                <li><strong>Relationship Analyzer:</strong> Finds connections between code entities</li>
                <li><strong>Graph Builder:</strong> Creates knowledge graph in Neo4j database</li>
                <li><strong>RAG Indexer:</strong> Creates vector embeddings for semantic search</li>
            </ol>
            
            <h4>🔍 Retrieval Process:</h4>
            <p>When you query the system, it uses <strong>hybrid retrieval</strong> combining:</p>
            <ul>
                <li><strong>Graph queries</strong> for structural relationships</li>
                <li><strong>Vector search</strong> for semantic similarity</li>
                <li><strong>LLM generation</strong> for human-readable answers</li>
            </ul>
        </div>
    </div>
    
    <script>
        mermaid.initialize({{ startOnLoad: true }});
    </script>
</body>
</html>
        """
        return html
    
    def generate_all_diagrams(self):
        """Generate all diagram formats"""
        print("🎨 Generating Agent Flow Visualizations...")
        
        # ASCII diagram for terminal
        ascii_diagram = self.create_ascii_flow_diagram()
        print(ascii_diagram)
        
        # Save Mermaid diagram
        mermaid_diagram = self.create_mermaid_flow_diagram()
        with open("agent_flow_diagram.md", "w") as f:
            f.write("# Code Grapher Agent Flow\n\n")
            f.write(mermaid_diagram)
        print("📄 Mermaid diagram saved to: agent_flow_diagram.md")
        
        # Save HTML interactive diagram
        html_diagram = self.create_html_interactive_diagram()
        with open("agent_flow_interactive.html", "w") as f:
            f.write(html_diagram)
        print("🌐 Interactive HTML diagram saved to: agent_flow_interactive.html")
        
        print("\n🎯 Open agent_flow_interactive.html in your browser to see the interactive flow!")

if __name__ == "__main__":
    visualizer = AgentFlowVisualizer()
    visualizer.generate_all_diagrams()