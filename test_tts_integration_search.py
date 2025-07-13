#!/usr/bin/env python3
"""
Test retrieval for TTS integration - finding output points and integration opportunities
"""

import chromadb
import json
import sys
from pathlib import Path

def add_realistic_code_data():
    """Add more realistic code data that would exist in a real codebase"""
    print("🛠️  Adding Realistic Code Data for TTS Integration Search")
    print("=" * 60)
    
    try:
        client = chromadb.PersistentClient(path="./chroma_db")
        
        # Get or create collection
        try:
            collection = client.get_collection("code_knowledge")
        except:
            collection = client.create_collection("code_knowledge")
        
        # Add realistic code snippets that would be relevant for TTS integration
        tts_relevant_data = [
            {
                "id": "output_handler_1",
                "document": "def generate_response(self, query_result): \"\"\"Generate final response from query results\"\"\" response_text = self.format_answer(query_result) self.logger.info(f'Generated response: {response_text}') return {'answer': response_text, 'sources': query_result.sources}",
                "metadata": {"type": "function", "file": "rag_pipeline.py", "entity": "generate_response", "output_type": "text", "returns": "response_text"}
            },
            {
                "id": "answer_formatter_1", 
                "document": "class ResponseFormatter: def format_code_explanation(self, entities, relationships): \"\"\"Format code analysis into human readable explanation\"\"\" explanation = f'Found {len(entities)} code entities with {len(relationships)} relationships.' return explanation",
                "metadata": {"type": "class", "file": "response_formatter.py", "entity": "ResponseFormatter", "output_type": "text", "returns": "explanation"}
            },
            {
                "id": "query_processor_1",
                "document": "def answer_question(self, question): \"\"\"Answer user question using RAG\"\"\" retrieved_content = self.retrieve_relevant_content(question) answer = self.llm_client.generate_answer(question, retrieved_content) print(f'Answer: {answer}') return answer",
                "metadata": {"type": "function", "file": "rag_pipeline.py", "entity": "answer_question", "output_type": "text", "returns": "answer", "has_print": True}
            },
            {
                "id": "logging_output_1",
                "document": "def log_operation_end(self, operation, success, details): \"\"\"Log operation completion\"\"\" message = f'Operation {operation} completed with status: {\"SUCCESS\" if success else \"FAILED\"}' self.logger.info(message) return message",
                "metadata": {"type": "function", "file": "logger.py", "entity": "log_operation_end", "output_type": "text", "returns": "message"}
            },
            {
                "id": "error_handler_1",
                "document": "def handle_error(self, error, context): \"\"\"Handle and report errors\"\"\" error_message = f'Error in {context}: {str(error)}' print(f'ERROR: {error_message}') self.notify_user(error_message) return error_message",
                "metadata": {"type": "function", "file": "error_handler.py", "entity": "handle_error", "output_type": "text", "has_print": True, "user_facing": True}
            },
            {
                "id": "summary_generator_1",
                "document": "def generate_pipeline_summary(self, results): \"\"\"Generate summary of pipeline execution\"\"\" summary = f'Pipeline processed {results.files_count} files, extracted {results.entities_count} entities, created {results.relationships_count} relationships.' return summary",
                "metadata": {"type": "function", "file": "pipeline_summary.py", "entity": "generate_pipeline_summary", "output_type": "text", "returns": "summary", "user_facing": True}
            },
            {
                "id": "user_interface_1",
                "document": "class CodeAnalysisUI: def display_results(self, analysis_results): \"\"\"Display analysis results to user\"\"\" for entity in analysis_results.entities: print(f'Found {entity.type}: {entity.name} in {entity.file}') self.render_results(analysis_results)",
                "metadata": {"type": "class", "file": "ui_components.py", "entity": "CodeAnalysisUI", "output_type": "text", "has_print": True, "user_facing": True}
            },
            {
                "id": "report_generator_1",
                "document": "def create_analysis_report(self, graph_data): \"\"\"Create detailed analysis report\"\"\" report = f'Code Analysis Report:\\nTotal classes: {graph_data.class_count}\\nTotal functions: {graph_data.function_count}\\nComplexity score: {graph_data.complexity}' self.save_report(report) return report",
                "metadata": {"type": "function", "file": "report_generator.py", "entity": "create_analysis_report", "output_type": "text", "returns": "report", "user_facing": True}
            },
            {
                "id": "notification_system_1",
                "document": "def notify_completion(self, task_name, duration): \"\"\"Notify user of task completion\"\"\" message = f'Task {task_name} completed successfully in {duration:.2f} seconds' self.send_notification(message) return message",
                "metadata": {"type": "function", "file": "notification_system.py", "entity": "notify_completion", "output_type": "text", "returns": "message", "user_facing": True}
            },
            {
                "id": "config_validator_1",
                "document": "def validate_configuration(self, config): \"\"\"Validate system configuration\"\"\" issues = [] if not config.neo4j_url: issues.append('Neo4j URL not configured') validation_result = f'Configuration validation: {len(issues)} issues found' return validation_result",
                "metadata": {"type": "function", "file": "config_validator.py", "entity": "validate_configuration", "output_type": "text", "returns": "validation_result"}
            }
        ]
        
        # Check existing count
        existing_count = collection.count()
        
        # Add the new documents
        documents = [item["document"] for item in tts_relevant_data]
        metadatas = [item["metadata"] for item in tts_relevant_data]
        ids = [item["id"] for item in tts_relevant_data]
        
        collection.add(
            documents=documents,
            metadatas=metadatas,
            ids=ids
        )
        
        new_count = collection.count()
        print(f"   ✅ Added {len(tts_relevant_data)} TTS-relevant code snippets")
        print(f"   📚 Total items in collection: {new_count} (was {existing_count})")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Failed to add realistic data: {e}")
        return False

def search_for_tts_integration_points():
    """Search for places where TTS could be integrated"""
    print(f"\n🔍 Searching for TTS Integration Points")
    print("=" * 60)
    
    try:
        client = chromadb.PersistentClient(path="./chroma_db")
        collection = client.get_collection("code_knowledge")
        
        # TTS integration queries - looking for output points
        tts_queries = [
            {
                "query": "Where does the system generate text output for users?",
                "purpose": "Find user-facing text generation points",
                "keywords": ["output", "text", "user", "display", "print"]
            },
            {
                "query": "What functions return text messages or responses?", 
                "purpose": "Find functions that return text suitable for TTS",
                "keywords": ["return", "message", "response", "text"]
            },
            {
                "query": "Where are answers and results presented to users?",
                "purpose": "Find presentation layer for TTS integration",
                "keywords": ["answer", "result", "display", "present"]
            },
            {
                "query": "What error messages and notifications are shown?",
                "purpose": "Find error handling for TTS alerts", 
                "keywords": ["error", "notification", "message", "alert"]
            },
            {
                "query": "Where does logging output important information?",
                "purpose": "Find logging points that could be spoken",
                "keywords": ["log", "info", "output", "status"]
            },
            {
                "query": "What functions generate summaries or reports?",
                "purpose": "Find summary generation for TTS readouts",
                "keywords": ["summary", "report", "generate", "analysis"]
            }
        ]
        
        integration_opportunities = {}
        
        for i, query_info in enumerate(tts_queries):
            query = query_info["query"]
            purpose = query_info["purpose"] 
            expected_keywords = query_info["keywords"]
            
            print(f"\n📝 Query {i+1}: {query}")
            print(f"   Purpose: {purpose}")
            
            # Search for relevant code
            results = collection.query(
                query_texts=[query],
                n_results=5,
                where={"output_type": "text"}  # Filter for text outputs
            )
            
            if results['documents'] and results['documents'][0]:
                print(f"   ✅ Found {len(results['documents'][0])} potential integration points:")
                
                query_results = []
                for j, doc in enumerate(results['documents'][0]):
                    distance = results['distances'][0][j]
                    metadata = results['metadatas'][0][j]
                    relevance = max(0, 1 - distance)
                    
                    # Analyze integration potential
                    integration_score = calculate_tts_integration_score(doc, metadata, expected_keywords)
                    
                    print(f"\n      {j+1}. Integration Score: {integration_score:.2f}/10 (Relevance: {relevance:.3f})")
                    print(f"         Function: {metadata.get('entity', 'unknown')}")
                    print(f"         File: {metadata.get('file', 'unknown')}")
                    print(f"         Returns: {metadata.get('returns', 'unknown')}")
                    print(f"         User-facing: {metadata.get('user_facing', False)}")
                    print(f"         Has print: {metadata.get('has_print', False)}")
                    print(f"         Code: {doc[:100]}...")
                    
                    # TTS integration suggestions
                    suggestions = generate_tts_suggestions(metadata, doc, integration_score)
                    if suggestions:
                        print(f"         🎤 TTS Integration Ideas:")
                        for suggestion in suggestions:
                            print(f"            • {suggestion}")
                    
                    query_results.append({
                        "entity": metadata.get('entity'),
                        "file": metadata.get('file'),
                        "integration_score": integration_score,
                        "relevance": relevance,
                        "suggestions": suggestions,
                        "metadata": metadata
                    })
                
                integration_opportunities[query] = {
                    "purpose": purpose,
                    "results": query_results,
                    "top_score": max(r["integration_score"] for r in query_results) if query_results else 0
                }
            else:
                print(f"   ❌ No integration points found")
                integration_opportunities[query] = {"purpose": purpose, "results": [], "top_score": 0}
        
        return integration_opportunities
        
    except Exception as e:
        print(f"❌ TTS integration search failed: {e}")
        return {}

def calculate_tts_integration_score(code, metadata, expected_keywords):
    """Calculate how suitable a code point is for TTS integration"""
    score = 0
    
    # Base score for returning text
    if metadata.get('returns') and 'text' in str(metadata.get('returns', '')).lower():
        score += 3
    
    # Higher score for user-facing functions
    if metadata.get('user_facing'):
        score += 3
    
    # Score for having print statements (already outputting to user)
    if metadata.get('has_print'):
        score += 2
    
    # Score for containing expected keywords
    code_lower = code.lower()
    keyword_matches = sum(1 for kw in expected_keywords if kw in code_lower)
    score += keyword_matches * 0.5
    
    # Score for specific patterns
    if 'return' in code_lower and ('message' in code_lower or 'response' in code_lower):
        score += 1
    
    if 'generate' in code_lower or 'format' in code_lower:
        score += 1
    
    return min(score, 10)  # Cap at 10

def generate_tts_suggestions(metadata, code, integration_score):
    """Generate specific TTS integration suggestions"""
    suggestions = []
    
    entity = metadata.get('entity', 'unknown')
    file = metadata.get('file', 'unknown')
    
    if integration_score >= 7:
        suggestions.append(f"HIGH PRIORITY: Add TTS call after return statement in {entity}")
        suggestions.append(f"Wrap return value with tts_output() function")
    
    if metadata.get('user_facing'):
        suggestions.append(f"Add TTS option to {entity} for accessibility")
        
    if metadata.get('has_print'):
        suggestions.append(f"Replace or supplement print() with TTS in {entity}")
    
    if 'error' in code.lower():
        suggestions.append(f"Add TTS for error notifications in {entity}")
    
    if 'summary' in code.lower() or 'report' in code.lower():
        suggestions.append(f"Add TTS readout option for {entity} output")
    
    if integration_score >= 5:
        suggestions.append(f"Consider adding tts_enabled parameter to {entity}")
    
    return suggestions

def create_tts_integration_plan():
    """Create a concrete TTS integration plan"""
    print(f"\n🎯 TTS Integration Implementation Plan")
    print("=" * 60)
    
    try:
        client = chromadb.PersistentClient(path="./chroma_db")
        collection = client.get_collection("code_knowledge")
        
        # Find the highest-scoring integration points
        high_priority_query = "functions that return user-facing text messages"
        results = collection.query(
            query_texts=[high_priority_query],
            n_results=10
        )
        
        if results['documents'] and results['documents'][0]:
            # Analyze all results for integration planning
            integration_points = []
            
            for i, doc in enumerate(results['documents'][0]):
                metadata = results['metadatas'][0][i]
                score = calculate_tts_integration_score(doc, metadata, ["text", "output", "return", "message"])
                
                if score >= 5:  # Only high-value integration points
                    integration_points.append({
                        "entity": metadata.get('entity'),
                        "file": metadata.get('file'),
                        "score": score,
                        "type": metadata.get('type'),
                        "user_facing": metadata.get('user_facing', False)
                    })
            
            # Sort by integration score
            integration_points.sort(key=lambda x: x['score'], reverse=True)
            
            print(f"📋 Top TTS Integration Candidates:")
            print(f"   Found {len(integration_points)} high-value integration points")
            
            for i, point in enumerate(integration_points[:5]):
                print(f"\n   {i+1}. {point['entity']} (Score: {point['score']:.1f}/10)")
                print(f"      File: {point['file']}")
                print(f"      Type: {point['type']}")
                print(f"      User-facing: {point['user_facing']}")
            
            # Generate implementation steps
            print(f"\n🛠️  Implementation Steps:")
            print(f"   1. Create TTS service class (tts_service.py)")
            print(f"   2. Add TTS configuration to settings")
            print(f"   3. Modify top {min(3, len(integration_points))} functions:")
            
            for i, point in enumerate(integration_points[:3]):
                print(f"      • {point['entity']} in {point['file']}")
            
            print(f"   4. Add TTS toggle to user interface")
            print(f"   5. Test with error messages and summaries first")
            
            return integration_points
            
    except Exception as e:
        print(f"❌ Failed to create integration plan: {e}")
        return []

def main():
    """Run the TTS integration search"""
    print("🚀 TTS Integration Search and Planning")
    print("=" * 60)
    
    # Add realistic code data
    data_added = add_realistic_code_data()
    
    if not data_added:
        print("❌ Could not add test data, stopping")
        return
    
    # Search for integration points
    integration_opportunities = search_for_tts_integration_points()
    
    # Create implementation plan
    integration_plan = create_tts_integration_plan()
    
    # Summary and recommendations
    print(f"\n📊 TTS Integration Search Summary")
    print("=" * 60)
    
    if integration_opportunities:
        total_opportunities = sum(len(opp.get("results", [])) for opp in integration_opportunities.values())
        high_score_opportunities = sum(1 for opp in integration_opportunities.values() 
                                     if opp.get("top_score", 0) >= 7)
        
        print(f"✅ Integration search: Successful")
        print(f"   • Total opportunities found: {total_opportunities}")
        print(f"   • High-priority opportunities: {high_score_opportunities}")
        print(f"   • Ready for implementation: {len(integration_plan)}")
        
        # Best opportunities
        best_opportunities = [(query, opp["top_score"]) for query, opp in integration_opportunities.items() 
                            if opp.get("top_score", 0) >= 6]
        
        if best_opportunities:
            print(f"\n🎯 Best TTS Integration Opportunities:")
            for query, score in sorted(best_opportunities, key=lambda x: x[1], reverse=True):
                print(f"   • Score {score:.1f}/10: {query[:60]}...")
    else:
        print(f"❌ Integration search: Failed")
    
    # Save results
    results = {
        "integration_opportunities": integration_opportunities,
        "implementation_plan": integration_plan,
        "summary": {
            "total_opportunities": total_opportunities if integration_opportunities else 0,
            "high_priority": high_score_opportunities if integration_opportunities else 0,
            "ready_for_implementation": len(integration_plan) if integration_plan else 0
        }
    }
    
    with open("tts_integration_analysis.json", "w") as f:
        json.dump(results, f, indent=2, default=str)
    
    print(f"\n💾 TTS integration analysis saved to: tts_integration_analysis.json")
    
    return results

if __name__ == "__main__":
    main()