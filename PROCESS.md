# Code Grapher Process Documentation

## Core Workflows

### Workflow 1: Codebase Knowledge Graph Initialization Protocol

**Objective**: To create a comprehensive and queryable knowledge graph from an existing, complex codebase. This process is divided into two phases: a high-level structural scan to create a foundational graph, followed by a deep, iterative enrichment process to add semantic context.

#### Phase 1: High-Level Graph Seeding
The goal of this phase is to rapidly construct a structural "skeleton" of the entire codebase. This initial graph will not be semantically rich, but it will provide the essential architecture upon which deep context can be layered.

**Step 1: Establish a Core Graph Schema**
Before any analysis begins, a clear and adaptable schema must be defined for the Neo4j graph. This schema dictates what constitutes an entity (a node) and a relationship (an edge). A robust starting schema should include:

*Node Labels:*
- **File**: Represents a source code file
- **Class**: Represents a class definition
- **Function** (or Method): Represents a function or method
- **Attribute** (or Variable): Represents a class member or global variable

*Relationship Types:*
- **CONTAINS**: A File contains Class(es) and Function(s); a Class contains Method(s) and Attribute(s)
- **CALLS**: A Function calls another Function
- **INHERITS_FROM**: A Class inherits from another Class
- **IMPLEMENTS**: A Class implements an Interface
- **USES**: A Function or Method uses a Class or Attribute

**Step 2: Perform Initial Static Analysis**
LLMs are not efficient at parsing an entire codebase from scratch. Therefore, the initial population should be performed by a dedicated static analysis tool. Tools capable of parsing the source code to generate an Abstract Syntax Tree (AST) or a basic dependency graph are ideal for this step.

*Action*: Configure and run a static analyzer across the entire repository.

*LLM Prompt (for configuration, if needed)*: "Given the programming language [language], what open-source static analysis tools can extract basic code structures like class definitions, function calls, and inheritance relationships? Provide the command-line instructions to run the tool and output the results in a structured format like JSON."

**Step 3: Populate the Initial Graph**
The structured output from the static analysis tool is used to perform a bulk ingestion into Neo4j. This creates the foundational graph representing the codebase's architecture.

*Action*: Write a script to parse the analyzer's output and generate Cypher CREATE statements to build the nodes and relationships defined in the schema.

**Step 4: Generate High-Level Semantic Summaries**
With the structural skeleton in place, the LLM's first task is to add a layer of human-readable meaning.

*LLM Prompt (Agentic Loop)*: "For each File node in the graph that lacks a summary property, retrieve its contained Class and Function nodes. Based on their names and relationships, generate a concise, one-paragraph summary of this file's primary purpose and role in the application. Add this summary as the summary property to the File node."

#### Phase 2: Deep Contextual Enrichment (Graph-RAG Loop)
This phase uses an iterative, agentic loop to analyze each part of the codebase in detail, using the already-populated graph to provide essential context for the LLM.

**Step 1: Set Up the Iteration**
The agent will iterate through each File or Class node in the graph to perform a deep analysis.

**Step 2: Contextual Retrieval (The "RAG" in Graph-RAG)**
For each entity being analyzed, the agent must first retrieve its local neighborhood from the graph. This provides the LLM with crucial context that is impossible to obtain from viewing the file in isolation.

*LLM Prompt (Query Generation)*: "I am analyzing the function [function_name] in file [file_name]. Generate a Cypher query for Neo4j to retrieve a subgraph showing: (1) The Class that contains this function. (2) Any functions that call this function. (3) Any functions this function calls. (4) The full inheritance chain of its parent class."

**Step 3: Deep Analysis with Retrieved Context**
The LLM is now provided with both the source code of the entity and the contextual subgraph retrieved in the previous step. This enables a much deeper level of understanding.

*LLM Prompt (Analysis)*: "Given the source code for [function_name] and the provided contextual subgraph, perform a detailed analysis. Identify and extract the following: (1) The specific business logic or rule implemented. (2) The data flow, tracing how input variables are transformed into outputs. (3) Any non-obvious dependencies or side effects. Structure the output as a set of new potential nodes and relationships to enrich the graph."

**Step 4: Graph Enrichment and Iteration**
The structured output from the LLM is translated into Cypher MERGE statements to add the new, richer semantic information to the graph. This could involve adding properties to existing nodes (e.g., business_rule: 'Calculates user tax based on location') or creating new, more descriptive relationship types (e.g., MODIFIES_STATE_OF). The graph becomes progressively more intelligent with each file processed, improving the context for all subsequent analyses.

### Workflow 2: Protocol for Surgical Graph Updates on Code Change

**Objective**: To efficiently and accurately update the codebase knowledge graph in response to code modifications (e.g., within a pull request) without requiring a full re-scan of the repository.

This process should be integrated into the CI/CD pipeline and triggered by version control events.

**Step 1: Detect Code Changes**
The process begins by identifying the set of files that have been added, modified, or deleted in a given commit or pull request. This "delta" is the sole focus of the update process.

**Step 2: Probe the Graph for Impact Analysis**
Before analyzing the content of the change, the agent must understand its potential impact. For each modified file, it queries the existing graph to find all entities that are directly connected to it.

*LLM Prompt (Impact Query Generation)*: "The file [file_name] has been modified. Generate a Cypher query to find all nodes that have a CALLS, USES, or INHERITS_FROM relationship pointing to any Class or Function node contained within [file_name]. Also, find all nodes that entities in this file call or use. Return this as a complete impact-radius subgraph."

**Step 3: Analyze the Delta with Context**
The LLM is provided with the "before" and "after" versions of the code, along with the impact-radius subgraph retrieved from the graph. This context is critical for understanding the full implications of the change.

*LLM Prompt (Delta Analysis)*: "Analyze the differences between the old and new versions of [file_name], using the provided impact subgraph for context. Identify all created, deleted, or modified entities (nodes) and relationships (edges) according to the graph schema. For modifications, specify the node/edge identifier and the changed properties."

**Step 4: Generate Surgical Cypher Queries**
Based on its analysis, the LLM generates a series of precise Cypher queries to update the graph. This approach avoids altering any unchanged parts of the graph.

*For Deletions (Pruning)*:
LLM Prompt: "The function [function_name] was deleted. Generate a Cypher query to find the corresponding Function node and perform a DETACH DELETE to remove it and all its relationships."

*For Modifications*:
LLM Prompt: "The method [old_method_name] was renamed to [new_method_name]. Generate a Cypher query to find the Method node by its old name and SET its name property to the new name."

*For Additions*:
LLM Prompt: "A new class [class_name] was added to [file_name]. It inherits from [parent_class]. Generate a Cypher query using MERGE to create the new Class node and establish the CONTAINS relationship from the File node and the INHERITS_FROM relationship from the parent Class node."

**Step 5: Vet and Execute Queries**
Executing LLM-generated code directly against a production database carries risk. A validation step is essential to prevent graph corruption.

*Action*: The generated Cypher queries should be passed to a "validator" agent or flagged for human review before execution.

*LLM Prompt (Validator Agent)*: "Review the following Cypher query intended to update the code graph: [Generated Cypher Query]. Verify that: (1) The syntax is valid. (2) It accurately reflects the described code change. (3) It adheres to the established graph schema. Respond with 'Approved' or 'Rejected' with a reason."

Once approved, the queries are executed, bringing the knowledge graph into perfect alignment with the latest version of the codebase, ready for the next development cycle.

## Relevant Research: Semantic Code Graph (SCG)

As codebases have grown to unmanageable sizes, the primary bottleneck has shifted again—this time to developer cognition. The Semantic Code Graph (SCG) approach provides guidance for building more intuitive graph representations.

**Key Paper**: [Semantic Code Graph -- an information model to facilitate software comprehension](https://arxiv.org/html/2310.02128v2)
- arXiv: https://arxiv.org/abs/2310.02128

The SCG captures dependency types at multiple granularity levels (classes to local variables) and focuses on developer comprehension rather than just analysis. This aligns with our Graph-RAG approach where contextual understanding is critical.

---

*Documented: 2025-07-11*
*Context: Development phase setup with comprehensive monitoring*