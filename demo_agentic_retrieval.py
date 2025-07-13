#!/usr/bin/env python3
"""
Demonstration of Agentic Retrieval for TTS Integration
Shows how the system makes intelligent decisions about information sufficiency
"""

from intelligent_graph_retrieval import IntelligentGraphRetriever, RetrievalStrategy
import json

def demo_tts_integration_with_agentic_retrieval():
    """
    Demonstrate how the agentic retrieval system would help with TTS integration
    """
    print("🎤 TTS Integration with Agentic Graph Retrieval")
    print("=" * 60)
    
    retriever = IntelligentGraphRetriever(max_results=12)
    
    # Real-world TTS integration scenario
    tts_scenario = {
        "scenario": "Adding Text-to-Speech to Code Analysis System",
        "queries": [
            {
                "query": "What are all the places where the system outputs text that users see?",
                "context": "Need to find every user-facing output point for TTS integration",
                "expected_behavior": "Should expand to find connected UI, logging, and notification systems"
            },
            {
                "query": "How are error messages handled and where do they get displayed?",
                "context": "TTS should announce errors immediately for accessibility",
                "expected_behavior": "Should traverse error handling → logging → notification connections"
            },
            {
                "query": "What is the complete flow from user query to final response output?",
                "context": "Need to understand the pipeline to add TTS at the right points",
                "expected_behavior": "Should follow the complete RAG pipeline with all connections"
            }
        ]
    }
    
    print(f"📋 Scenario: {tts_scenario['scenario']}")
    
    all_integration_points = set()
    scenario_results = []
    
    for i, query_info in enumerate(tts_scenario["queries"]):
        query = query_info["query"]
        context = query_info["context"]
        expected_behavior = query_info["expected_behavior"]
        
        print(f"\n{'='*60}")
        print(f"🎯 Query {i+1}: {context}")
        print(f"❓ Question: '{query}'")
        print(f"🎲 Expected: {expected_behavior}")
        
        # Perform intelligent retrieval
        results = retriever.intelligent_retrieve(query, RetrievalStrategy.INTELLIGENT)
        explanation = retriever.explain_retrieval_strategy(query, results)
        
        # Analyze the agentic decisions
        print(f"\n🧠 Agentic Analysis:")
        print(f"   📊 Decision outcome: Retrieved {explanation['total_results']} results")
        print(f"   🎯 Information sources: {explanation['vector_results']} vector + {explanation['graph_results']} graph")
        print(f"   ⚡ Average relevance: {explanation['avg_relevance']:.3f}")
        
        if explanation["expansion_decisions"]:
            print(f"   🤔 Expansion reasoning:")
            for reason in set(explanation["expansion_decisions"]):
                print(f"      • {reason}")
        
        # Extract integration points
        integration_points = []
        for result in results:
            entity = result.metadata.get('entity', 'unknown')
            file = result.metadata.get('file', 'unknown')
            is_user_facing = result.metadata.get('user_facing', False)
            has_output = result.metadata.get('output_type') == 'text' or result.metadata.get('has_print', False)
            
            if entity != 'unknown':
                integration_point = {
                    "entity": entity,
                    "file": file,
                    "relevance": result.relevance_score,
                    "source": result.source_type,
                    "user_facing": is_user_facing,
                    "has_output": has_output,
                    "content_preview": result.content[:80] + "..."
                }
                integration_points.append(integration_point)
                all_integration_points.add(f"{entity}:{file}")
        
        # Show discovered integration points
        print(f"\n📍 TTS Integration Points Discovered:")
        for j, point in enumerate(sorted(integration_points, key=lambda x: x['relevance'], reverse=True)[:5]):
            user_icon = "👤" if point['user_facing'] else "🔧"
            output_icon = "📢" if point['has_output'] else "📝"
            
            print(f"   {j+1}. {user_icon}{output_icon} {point['entity']} (relevance: {point['relevance']:.3f})")
            print(f"      File: {point['file']} | Source: {point['source']}")
            print(f"      Preview: {point['content_preview']}")
        
        scenario_results.append({
            "query": query,
            "context": context,
            "results": len(results),
            "explanation": explanation,
            "integration_points": integration_points
        })
    
    # Consolidate findings across all queries
    print(f"\n{'='*60}")
    print(f"🎯 Consolidated TTS Integration Analysis")
    print(f"{'='*60}")
    
    print(f"📊 Discovery Summary:")
    print(f"   • Total queries analyzed: {len(scenario_results)}")
    print(f"   • Unique integration points found: {len(all_integration_points)}")
    print(f"   • Average results per query: {sum(r['results'] for r in scenario_results) / len(scenario_results):.1f}")
    
    # Show agentic decision patterns
    total_expansions = sum(1 for r in scenario_results if r['explanation']['graph_results'] > 0)
    print(f"   • Queries that triggered expansion: {total_expansions}/{len(scenario_results)}")
    
    # Show most important integration points across all queries
    all_points = []
    for result in scenario_results:
        all_points.extend(result['integration_points'])
    
    # Group by entity and aggregate relevance
    entity_relevance = {}
    for point in all_points:
        entity_key = f"{point['entity']}:{point['file']}"
        if entity_key not in entity_relevance:
            entity_relevance[entity_key] = {
                "entity": point['entity'],
                "file": point['file'],
                "max_relevance": point['relevance'],
                "total_relevance": point['relevance'],
                "mentions": 1,
                "user_facing": point['user_facing'],
                "has_output": point['has_output']
            }
        else:
            entity_relevance[entity_key]["total_relevance"] += point['relevance']
            entity_relevance[entity_key]["max_relevance"] = max(entity_relevance[entity_key]["max_relevance"], point['relevance'])
            entity_relevance[entity_key]["mentions"] += 1
    
    # Rank by combined score (max relevance + frequency)
    ranked_entities = sorted(
        entity_relevance.values(),
        key=lambda x: x['max_relevance'] + (x['mentions'] * 0.1),
        reverse=True
    )
    
    print(f"\n🎯 Top TTS Integration Candidates (Ranked by AI):")
    for i, entity in enumerate(ranked_entities[:8]):
        priority = "🔥 HIGH" if entity['max_relevance'] > 0.5 and entity['user_facing'] else "📋 MED" if entity['has_output'] else "📝 LOW"
        
        print(f"   {i+1}. {priority} {entity['entity']}")
        print(f"      File: {entity['file']}")
        print(f"      Max relevance: {entity['max_relevance']:.3f} | Mentions: {entity['mentions']}")
        print(f"      User-facing: {entity['user_facing']} | Has output: {entity['has_output']}")
    
    # Generate implementation recommendations
    print(f"\n🛠️  AI-Generated Implementation Recommendations:")
    
    high_priority = [e for e in ranked_entities if e['max_relevance'] > 0.5 and e['user_facing']]
    medium_priority = [e for e in ranked_entities if e['has_output'] and e not in high_priority]
    
    print(f"   Phase 1 - Critical TTS Integration ({len(high_priority)} items):")
    for entity in high_priority[:3]:
        print(f"      • Modify {entity['entity']} in {entity['file']}")
        print(f"        Reason: High relevance ({entity['max_relevance']:.3f}) + user-facing")
    
    print(f"   Phase 2 - Secondary TTS Integration ({len(medium_priority)} items):")  
    for entity in medium_priority[:3]:
        print(f"      • Enhance {entity['entity']} in {entity['file']}")
        print(f"        Reason: Has text output, moderate relevance ({entity['max_relevance']:.3f})")
    
    # Show agentic decision insights
    print(f"\n🧠 Agentic Decision Insights:")
    expansion_reasons = []
    for result in scenario_results:
        expansion_reasons.extend(result['explanation']['expansion_decisions'])
    
    unique_reasons = list(set(expansion_reasons))
    if unique_reasons:
        print(f"   • AI expansion triggers identified:")
        for reason in unique_reasons:
            count = expansion_reasons.count(reason)
            print(f"     - {reason} (triggered {count} times)")
    
    print(f"   • Graph traversal discovered {sum(r['explanation']['graph_results'] for r in scenario_results)} connected entities")
    print(f"   • System autonomously decided information sufficiency for each query")
    print(f"   • Agentic ranking prioritized user-facing functions for TTS integration")
    
    return {
        "scenario": tts_scenario,
        "results": scenario_results,
        "top_candidates": ranked_entities[:10],
        "implementation_phases": {
            "phase_1": [e['entity'] for e in high_priority[:3]],
            "phase_2": [e['entity'] for e in medium_priority[:3]]
        }
    }

def compare_retrieval_strategies():
    """
    Compare different retrieval strategies to show the value of agentic decisions
    """
    print(f"\n🔬 Retrieval Strategy Comparison")
    print("=" * 60)
    
    retriever = IntelligentGraphRetriever(max_results=10)
    
    test_query = "How should I integrate TTS with error handling and user notifications?"
    
    strategies = [
        (RetrievalStrategy.VECTOR_ONLY, "Vector similarity only"),
        (RetrievalStrategy.INTELLIGENT, "Intelligent agentic expansion")
    ]
    
    print(f"🧪 Test Query: '{test_query}'")
    
    comparison_results = {}
    
    for strategy, description in strategies:
        print(f"\n📊 Testing: {description}")
        
        if strategy == RetrievalStrategy.VECTOR_ONLY:
            # Simulate vector-only by limiting to initial results
            results = retriever._vector_search(test_query, n_results=5)
            explanation = {"vector_results": len(results), "graph_results": 0, "expansion_decisions": []}
        else:
            results = retriever.intelligent_retrieve(test_query, strategy)
            explanation = retriever.explain_retrieval_strategy(test_query, results)
        
        print(f"   Results: {len(results)} items")
        print(f"   Vector: {explanation['vector_results']} | Graph: {explanation['graph_results']}")
        
        if explanation.get('expansion_decisions'):
            print(f"   Expansion reasons: {', '.join(set(explanation['expansion_decisions']))}")
        
        # Count user-facing and high-relevance results
        user_facing_count = sum(1 for r in results if r.metadata.get('user_facing', False))
        high_relevance_count = sum(1 for r in results if r.relevance_score > 0.5)
        
        print(f"   Quality: {user_facing_count} user-facing, {high_relevance_count} high-relevance")
        
        comparison_results[strategy.value] = {
            "total_results": len(results),
            "user_facing": user_facing_count,
            "high_relevance": high_relevance_count,
            "explanation": explanation
        }
    
    # Show comparison summary
    print(f"\n📈 Strategy Comparison Summary:")
    vector_results = comparison_results["vector_only"]
    intelligent_results = comparison_results["intelligent"]
    
    print(f"   Vector Only:    {vector_results['total_results']} total, {vector_results['user_facing']} user-facing")
    print(f"   Intelligent:    {intelligent_results['total_results']} total, {intelligent_results['user_facing']} user-facing")
    
    improvement = {
        "total": intelligent_results['total_results'] - vector_results['total_results'],
        "user_facing": intelligent_results['user_facing'] - vector_results['user_facing']
    }
    
    print(f"   Improvement:    +{improvement['total']} total results, +{improvement['user_facing']} user-facing results")
    print(f"   Value: Agentic expansion found {improvement['user_facing']} more relevant integration points")
    
    return comparison_results

def main():
    """Run the full agentic retrieval demonstration"""
    print("🚀 Agentic Graph Retrieval Demonstration")
    print("=" * 60)
    
    # Main TTS integration analysis
    tts_analysis = demo_tts_integration_with_agentic_retrieval()
    
    # Strategy comparison
    strategy_comparison = compare_retrieval_strategies()
    
    # Save comprehensive results
    full_results = {
        "tts_integration_analysis": tts_analysis,
        "strategy_comparison": strategy_comparison,
        "summary": {
            "top_candidates": len(tts_analysis["top_candidates"]),
            "implementation_phases": tts_analysis["implementation_phases"],
            "agentic_value": "Autonomous expansion decisions + graph traversal + relevance ranking"
        }
    }
    
    with open("agentic_retrieval_demo_results.json", "w") as f:
        json.dump(full_results, f, indent=2, default=str)
    
    print(f"\n💾 Full analysis saved to: agentic_retrieval_demo_results.json")
    
    return full_results

if __name__ == "__main__":
    main()