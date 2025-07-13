#!/usr/bin/env python3
"""
Intelligent Graph-Aware Retrieval System
Combines vector search with graph traversal and agentic decision making
"""

import chromadb
import json
import sys
from pathlib import Path
from typing import Dict, List, Any, Set, Tuple
from dataclasses import dataclass
from enum import Enum

class RetrievalStrategy(Enum):
    VECTOR_ONLY = "vector_only"
    GRAPH_ONLY = "graph_only" 
    HYBRID = "hybrid"
    INTELLIGENT = "intelligent"

class InformationSufficiency(Enum):
    INSUFFICIENT = "insufficient"
    PARTIAL = "partial"
    SUFFICIENT = "sufficient"
    COMPREHENSIVE = "comprehensive"

@dataclass
class RetrievalResult:
    content: str
    metadata: Dict[str, Any]
    relevance_score: float
    source_type: str  # "vector", "graph", "hybrid"
    connections: List[str] = None
    expansion_reason: str = None

@dataclass
class RetrievalContext:
    query: str
    initial_results: List[RetrievalResult]
    sufficiency_assessment: InformationSufficiency
    expansion_needed: bool
    max_results: int
    current_count: int
    connected_entities: Set[str]

class IntelligentGraphRetriever:
    """
    Agentic retrieval system that combines vector search with graph traversal
    and makes intelligent decisions about information sufficiency
    """
    
    def __init__(self, vector_collection_name="code_knowledge", max_results=20):
        self.max_results = max_results
        self.vector_collection_name = vector_collection_name
        self.chroma_client = chromadb.PersistentClient(path="./chroma_db")
        
        # Simulated graph connections (normally would come from Neo4j)
        self.graph_connections = self._build_simulated_graph()
        
        # Decision thresholds
        self.sufficiency_thresholds = {
            "min_results": 2,
            "sufficient_results": 5,
            "high_relevance_threshold": 0.7,
            "connection_expansion_threshold": 0.5
        }
    
    def _build_simulated_graph(self) -> Dict[str, List[str]]:
        """
        Build simulated graph connections (normally would query Neo4j)
        In real implementation, this would be: MATCH (a)-[r]->(b) WHERE...
        """
        return {
            # Error handling ecosystem
            "handle_error": ["log_operation_end", "notify_completion", "error_recovery_system"],
            "log_operation_end": ["handle_error", "audit_logger", "performance_tracker"],
            
            # Response generation pipeline
            "generate_response": ["answer_question", "ResponseFormatter", "output_processor"],
            "answer_question": ["generate_response", "retrieve_relevant_content", "query_processor"],
            "ResponseFormatter": ["generate_response", "template_engine", "content_formatter"],
            
            # UI and presentation layer
            "CodeAnalysisUI": ["display_results", "user_interface_manager", "visualization_engine"],
            "display_results": ["CodeAnalysisUI", "result_formatter", "output_renderer"],
            
            # Analysis and reporting
            "create_analysis_report": ["generate_pipeline_summary", "report_formatter", "data_aggregator"],
            "generate_pipeline_summary": ["create_analysis_report", "metrics_collector", "summary_engine"],
            
            # Notification system
            "notify_completion": ["handle_error", "notification_manager", "user_alerting"],
            
            # Configuration and validation
            "validate_configuration": ["config_manager", "system_validator", "setup_checker"],
            
            # Core analysis functions
            "analyze_code_entities": ["entity_extractor", "code_parser", "relationship_analyzer"],
        }
    
    def intelligent_retrieve(self, query: str, strategy: RetrievalStrategy = RetrievalStrategy.INTELLIGENT) -> List[RetrievalResult]:
        """
        Main intelligent retrieval method with agentic decision making
        """
        print(f"\n🧠 Intelligent Retrieval: '{query}'")
        print(f"Strategy: {strategy.value} | Max results: {self.max_results}")
        
        # Stage 1: Initial vector search
        initial_results = self._vector_search(query, n_results=min(5, self.max_results))
        
        # Stage 2: Assess information sufficiency
        context = RetrievalContext(
            query=query,
            initial_results=initial_results,
            sufficiency_assessment=self._assess_sufficiency(query, initial_results),
            expansion_needed=False,
            max_results=self.max_results,
            current_count=len(initial_results),
            connected_entities=set()
        )
        
        print(f"   📊 Initial results: {len(initial_results)} items")
        print(f"   🎯 Sufficiency: {context.sufficiency_assessment.value}")
        
        # Stage 3: Agentic decision making
        if strategy == RetrievalStrategy.INTELLIGENT:
            context = self._make_expansion_decision(context)
        
        # Stage 4: Graph expansion if needed
        final_results = initial_results.copy()
        if context.expansion_needed and len(final_results) < self.max_results:
            expanded_results = self._expand_with_graph_traversal(context)
            final_results.extend(expanded_results)
            
            # Stage 5: Consolidation
            final_results = self._consolidate_results(final_results, context)
        
        # Stage 6: Final ranking and limiting
        final_results = self._rank_and_limit_results(final_results, context)
        
        print(f"   ✅ Final results: {len(final_results)} items")
        return final_results
    
    def _vector_search(self, query: str, n_results: int = 5) -> List[RetrievalResult]:
        """Perform initial vector similarity search"""
        try:
            collection = self.chroma_client.get_collection(self.vector_collection_name)
            
            results = collection.query(
                query_texts=[query],
                n_results=n_results
            )
            
            retrieval_results = []
            if results['documents'] and results['documents'][0]:
                for i, doc in enumerate(results['documents'][0]):
                    distance = results['distances'][0][i]
                    metadata = results['metadatas'][0][i]
                    relevance = max(0, 1 - distance)
                    
                    retrieval_results.append(RetrievalResult(
                        content=doc,
                        metadata=metadata,
                        relevance_score=relevance,
                        source_type="vector"
                    ))
            
            return retrieval_results
            
        except Exception as e:
            print(f"   ❌ Vector search failed: {e}")
            return []
    
    def _assess_sufficiency(self, query: str, results: List[RetrievalResult]) -> InformationSufficiency:
        """
        Agentic assessment of whether the current results are sufficient
        """
        if not results:
            return InformationSufficiency.INSUFFICIENT
        
        num_results = len(results)
        avg_relevance = sum(r.relevance_score for r in results) / num_results
        high_relevance_count = sum(1 for r in results if r.relevance_score > self.sufficiency_thresholds["high_relevance_threshold"])
        
        # Check for query-specific patterns
        query_lower = query.lower()
        is_complex_query = any(word in query_lower for word in ["how", "what", "where", "integration", "workflow", "system"])
        is_specific_query = any(word in query_lower for word in ["function", "class", "method", "error"])
        
        # Decision logic
        if num_results < self.sufficiency_thresholds["min_results"]:
            return InformationSufficiency.INSUFFICIENT
        
        if high_relevance_count >= 2 and avg_relevance > 0.6:
            if num_results >= self.sufficiency_thresholds["sufficient_results"] or not is_complex_query:
                return InformationSufficiency.SUFFICIENT
            else:
                return InformationSufficiency.PARTIAL
        
        if is_specific_query and high_relevance_count >= 1:
            return InformationSufficiency.SUFFICIENT
        
        if num_results >= 3 and avg_relevance > 0.4:
            return InformationSufficiency.PARTIAL
        
        return InformationSufficiency.INSUFFICIENT
    
    def _make_expansion_decision(self, context: RetrievalContext) -> RetrievalContext:
        """
        Agentic decision making about whether to expand the search
        """
        print(f"   🤔 Making expansion decision...")
        
        # Decision factors
        needs_expansion = False
        expansion_reasons = []
        
        # Factor 1: Information sufficiency
        if context.sufficiency_assessment in [InformationSufficiency.INSUFFICIENT, InformationSufficiency.PARTIAL]:
            needs_expansion = True
            expansion_reasons.append(f"Information {context.sufficiency_assessment.value}")
        
        # Factor 2: Query complexity
        query_lower = context.query.lower()
        is_integration_query = any(word in query_lower for word in ["integration", "connect", "workflow", "system", "together"])
        if is_integration_query and context.current_count < 8:
            needs_expansion = True
            expansion_reasons.append("Integration query needs context")
        
        # Factor 3: High-value results that might have connections
        connected_entities = set()
        for result in context.initial_results:
            entity_name = result.metadata.get('entity', '')
            if entity_name in self.graph_connections:
                connected_entities.update(self.graph_connections[entity_name])
                if result.relevance_score > self.sufficiency_thresholds["connection_expansion_threshold"]:
                    needs_expansion = True
                    expansion_reasons.append(f"High-relevance result '{entity_name}' has graph connections")
        
        # Factor 4: Available capacity
        if context.current_count >= context.max_results * 0.8:
            needs_expansion = False
            expansion_reasons = ["Already near max capacity"]
        
        context.expansion_needed = needs_expansion
        context.connected_entities = connected_entities
        
        print(f"   📝 Expansion decision: {needs_expansion}")
        if expansion_reasons:
            print(f"      Reasons: {'; '.join(expansion_reasons)}")
        
        return context
    
    def _expand_with_graph_traversal(self, context: RetrievalContext) -> List[RetrievalResult]:
        """
        Expand results using graph traversal to find connected entities
        """
        print(f"   🕸️  Expanding with graph traversal...")
        
        expanded_results = []
        entities_to_explore = []
        
        # Identify entities from initial results that have graph connections
        for result in context.initial_results:
            entity_name = result.metadata.get('entity', '')
            if entity_name in self.graph_connections:
                entities_to_explore.append((entity_name, result.relevance_score))
        
        print(f"      📍 Found {len(entities_to_explore)} entities with connections")
        
        # Explore connections for high-relevance entities
        for entity_name, parent_relevance in sorted(entities_to_explore, key=lambda x: x[1], reverse=True):
            if len(expanded_results) + context.current_count >= context.max_results:
                break
                
            connections = self.graph_connections[entity_name]
            print(f"      🔗 Exploring {entity_name} -> {len(connections)} connections")
            
            for connected_entity in connections:
                if len(expanded_results) + context.current_count >= context.max_results:
                    break
                
                # Try to find this connected entity in our vector store
                connected_results = self._search_for_specific_entity(connected_entity)
                
                for connected_result in connected_results:
                    # Calculate connection-based relevance
                    connection_relevance = parent_relevance * 0.7  # Inherit but reduce relevance
                    connected_result.relevance_score = max(connected_result.relevance_score, connection_relevance)
                    connected_result.source_type = "graph"
                    connected_result.connections = [entity_name]
                    connected_result.expansion_reason = f"Connected to high-relevance entity '{entity_name}'"
                    
                    expanded_results.append(connected_result)
                    print(f"         ➕ Added connected entity: {connected_entity} (relevance: {connected_result.relevance_score:.3f})")
        
        print(f"      ✅ Graph expansion added {len(expanded_results)} results")
        return expanded_results
    
    def _search_for_specific_entity(self, entity_name: str) -> List[RetrievalResult]:
        """Search for a specific entity by name"""
        try:
            collection = self.chroma_client.get_collection(self.vector_collection_name)
            
            # Search by entity name in metadata
            results = collection.query(
                query_texts=[entity_name],
                n_results=2,
                where={"entity": entity_name}
            )
            
            retrieval_results = []
            if results['documents'] and results['documents'][0]:
                for i, doc in enumerate(results['documents'][0]):
                    distance = results['distances'][0][i]
                    metadata = results['metadatas'][0][i]
                    relevance = max(0, 1 - distance)
                    
                    retrieval_results.append(RetrievalResult(
                        content=doc,
                        metadata=metadata,
                        relevance_score=relevance,
                        source_type="graph"
                    ))
            
            return retrieval_results
            
        except Exception as e:
            # If specific search fails, create a placeholder result
            return [RetrievalResult(
                content=f"Connected entity: {entity_name} (details not available in current index)",
                metadata={"entity": entity_name, "type": "connected", "file": "unknown"},
                relevance_score=0.3,
                source_type="graph"
            )]
    
    def _consolidate_results(self, results: List[RetrievalResult], context: RetrievalContext) -> List[RetrievalResult]:
        """
        Consolidate and deduplicate results
        """
        print(f"   🔄 Consolidating {len(results)} results...")
        
        # Remove duplicates based on entity name
        seen_entities = set()
        consolidated = []
        
        for result in results:
            entity_name = result.metadata.get('entity', '')
            entity_key = f"{entity_name}_{result.metadata.get('file', '')}"
            
            if entity_key not in seen_entities:
                seen_entities.add(entity_key)
                consolidated.append(result)
            else:
                # If duplicate, keep the one with higher relevance
                existing_idx = next(i for i, r in enumerate(consolidated) 
                                  if f"{r.metadata.get('entity', '')}_{r.metadata.get('file', '')}" == entity_key)
                if result.relevance_score > consolidated[existing_idx].relevance_score:
                    consolidated[existing_idx] = result
        
        print(f"      ✅ Consolidated to {len(consolidated)} unique results")
        return consolidated
    
    def _rank_and_limit_results(self, results: List[RetrievalResult], context: RetrievalContext) -> List[RetrievalResult]:
        """
        Final ranking and limiting of results
        """
        # Sort by relevance score (descending)
        ranked_results = sorted(results, key=lambda x: x.relevance_score, reverse=True)
        
        # Apply limit
        limited_results = ranked_results[:context.max_results]
        
        return limited_results
    
    def explain_retrieval_strategy(self, query: str, results: List[RetrievalResult]) -> Dict[str, Any]:
        """
        Explain the retrieval strategy and decisions made
        """
        vector_results = [r for r in results if r.source_type == "vector"]
        graph_results = [r for r in results if r.source_type == "graph"]
        
        strategy_explanation = {
            "query": query,
            "total_results": len(results),
            "vector_results": len(vector_results),
            "graph_results": len(graph_results),
            "avg_relevance": sum(r.relevance_score for r in results) / len(results) if results else 0,
            "expansion_decisions": [r.expansion_reason for r in results if r.expansion_reason],
            "connected_entities": list(set(conn for r in results if r.connections for conn in r.connections)),
            "retrieval_sources": {
                "vector": len(vector_results),
                "graph": len(graph_results)
            }
        }
        
        return strategy_explanation

def test_intelligent_retrieval():
    """Test the intelligent retrieval system"""
    print("🚀 Testing Intelligent Graph-Aware Retrieval")
    print("=" * 60)
    
    retriever = IntelligentGraphRetriever(max_results=15)
    
    # Test queries that should benefit from graph expansion
    test_queries = [
        {
            "query": "How do error handling and notifications work together in the system?",
            "description": "Complex integration query requiring connected context",
            "expected_expansion": True
        },
        {
            "query": "What is the complete response generation workflow?", 
            "description": "Workflow query needing pipeline understanding",
            "expected_expansion": True
        },
        {
            "query": "Where can I add TTS to error messages?",
            "description": "Specific integration question",
            "expected_expansion": True
        },
        {
            "query": "What does the handle_error function do?",
            "description": "Specific function query",
            "expected_expansion": False
        }
    ]
    
    results_summary = []
    
    for i, test_case in enumerate(test_queries):
        query = test_case["query"]
        description = test_case["description"]
        expected_expansion = test_case["expected_expansion"]
        
        print(f"\n{'='*60}")
        print(f"🧪 Test Case {i+1}: {description}")
        print(f"Query: '{query}'")
        
        # Perform intelligent retrieval
        results = retriever.intelligent_retrieve(query, RetrievalStrategy.INTELLIGENT)
        
        # Get explanation
        explanation = retriever.explain_retrieval_strategy(query, results)
        
        # Analyze results
        had_expansion = explanation["graph_results"] > 0
        expansion_correct = had_expansion == expected_expansion
        
        print(f"\n📊 Results Analysis:")
        print(f"   • Total results: {explanation['total_results']}")
        print(f"   • Vector results: {explanation['vector_results']}")
        print(f"   • Graph results: {explanation['graph_results']}")
        print(f"   • Average relevance: {explanation['avg_relevance']:.3f}")
        print(f"   • Had expansion: {had_expansion} (expected: {expected_expansion}) {'✅' if expansion_correct else '❌'}")
        
        if explanation["connected_entities"]:
            print(f"   • Connected entities: {', '.join(explanation['connected_entities'])}")
        
        if explanation["expansion_decisions"]:
            print(f"   • Expansion reasons: {'; '.join(set(explanation['expansion_decisions']))}")
        
        # Show top results
        print(f"\n📝 Top Results:")
        for j, result in enumerate(results[:3]):
            print(f"   {j+1}. {result.metadata.get('entity', 'unknown')} ({result.source_type})")
            print(f"      Relevance: {result.relevance_score:.3f}")
            print(f"      File: {result.metadata.get('file', 'unknown')}")
            if result.expansion_reason:
                print(f"      Expansion: {result.expansion_reason}")
            print(f"      Content: {result.content[:80]}...")
        
        results_summary.append({
            "query": query,
            "description": description,
            "explanation": explanation,
            "expansion_correct": expansion_correct,
            "results": len(results)
        })
    
    # Overall summary
    print(f"\n{'='*60}")
    print(f"🎯 Overall Test Summary")
    print(f"{'='*60}")
    
    correct_predictions = sum(1 for r in results_summary if r["expansion_correct"])
    total_tests = len(results_summary)
    
    print(f"✅ Expansion prediction accuracy: {correct_predictions}/{total_tests} ({correct_predictions/total_tests*100:.1f}%)")
    print(f"📊 Average results per query: {sum(r['results'] for r in results_summary)/total_tests:.1f}")
    
    # Show which queries got expanded
    expanded_queries = [r for r in results_summary if r["explanation"]["graph_results"] > 0]
    if expanded_queries:
        print(f"\n🕸️  Queries that used graph expansion:")
        for result in expanded_queries:
            print(f"   • '{result['query'][:50]}...' -> {result['explanation']['graph_results']} graph results")
    
    return results_summary

if __name__ == "__main__":
    test_intelligent_retrieval()