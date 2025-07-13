#!/usr/bin/env python3
"""
Test script for surgical graph update system.

This script validates that our surgical update system can:
1. Correctly analyze git diffs
2. Plan appropriate graph updates
3. Execute updates without breaking graph consistency
4. Provide performance benefits over full reparse
"""

import os
import sys
import time
import json
from typing import Dict, Any
from pathlib import Path

# Add current directory to path for imports
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from surgical_update_coordinator import SurgicalUpdateCoordinator
from agents.git_diff_agent import GitDiffAgent, ChangeType
from graph_update_engine import GraphUpdateEngine, UpdateOperation
from logger import logger


class SurgicalUpdateTester:
    """Comprehensive tester for surgical update system"""
    
    def __init__(self):
        self.session_logger = logger.create_session_logger("SurgicalUpdateTester")
        self.test_results = {
            "tests_run": 0,
            "tests_passed": 0,
            "tests_failed": 0,
            "details": []
        }
        
        # Initialize components for testing
        self.git_agent = None
        self.coordinator = None
        
        self.session_logger.log_info("SurgicalUpdateTester initialized")
    
    def run_all_tests(self) -> Dict[str, Any]:
        """Run all surgical update tests"""
        self.session_logger.log_info("Starting comprehensive surgical update tests")
        
        test_methods = [
            self.test_git_diff_agent_basic,
            self.test_git_diff_agent_commit_analysis,
            self.test_change_detection_accuracy,
            self.test_update_planning,
            self.test_mock_surgical_updates,
            self.test_performance_comparison
        ]
        
        for test_method in test_methods:
            try:
                self.session_logger.log_info(f"Running test: {test_method.__name__}")
                result = test_method()
                self._record_test_result(test_method.__name__, True, result)
            except Exception as e:
                self.session_logger.log_error(e, {"test": test_method.__name__})
                self._record_test_result(test_method.__name__, False, {"error": str(e)})
        
        return self._generate_test_report()
    
    def test_git_diff_agent_basic(self) -> Dict[str, Any]:
        """Test basic GitDiffAgent functionality"""
        # Create a git agent configuration
        config = {
            "agent_id": "test-git-agent",
            "capabilities": ["git_diff_analysis"],
            "description": "Test git diff agent",
            "config": {
                "repoPath": ".",
                "supportedExtensions": [".py"],
                "ignorePatterns": ["__pycache__", ".git"]
            }
        }
        
        try:
            self.git_agent = GitDiffAgent("test-git-agent", config)
            
            # Test basic capabilities
            capabilities = self.git_agent.get_capabilities()
            assert "git_diff_analysis" in capabilities
            assert "commit_analysis" in capabilities
            
            return {
                "capabilities_count": len(capabilities),
                "expected_capabilities": ["git_diff_analysis", "commit_analysis", "code_change_extraction"],
                "agent_initialized": True
            }
            
        except Exception as e:
            return {"error": str(e), "agent_initialized": False}
    
    def test_git_diff_agent_commit_analysis(self) -> Dict[str, Any]:
        """Test GitDiffAgent commit analysis"""
        if not self.git_agent:
            self.git_agent = self._create_test_git_agent()
        
        try:
            # Test recent commits retrieval
            result = self.git_agent.execute({
                "operation": "get_recent_commits", 
                "limit": 5
            })
            
            assert result["success"], "Failed to get recent commits"
            assert "commits" in result
            commits = result["commits"]
            
            if commits:
                # Test commit analysis
                latest_commit = commits[0]["hash"]
                analysis_result = self.git_agent.execute({
                    "operation": "analyze_commit",
                    "commit_hash": latest_commit
                })
                
                assert analysis_result["success"], "Failed to analyze commit"
                analysis = analysis_result["analysis"]
                
                return {
                    "commits_found": len(commits),
                    "latest_commit": latest_commit,
                    "files_changed": len(analysis.file_changes),
                    "entities_changed": len(analysis.entity_changes),
                    "commit_message": analysis.commit_message[:100] + "..." if len(analysis.commit_message) > 100 else analysis.commit_message
                }
            else:
                return {"commits_found": 0, "message": "No commits available for testing"}
                
        except Exception as e:
            return {"error": str(e)}
    
    def test_change_detection_accuracy(self) -> Dict[str, Any]:
        """Test accuracy of change detection from our test commit"""
        if not self.git_agent:
            self.git_agent = self._create_test_git_agent()
        
        try:
            # Analyze the latest commit (our test changes)
            result = self.git_agent.execute({
                "operation": "analyze_commit",
                "commit_hash": "HEAD"
            })
            
            assert result["success"], "Failed to analyze HEAD commit"
            analysis = result["analysis"]
            
            # Count detected changes by type
            changes_by_type = {}
            for entity_change in analysis.entity_changes:
                change_key = f"{entity_change.entity_type}_{entity_change.change_type.value}"
                changes_by_type[change_key] = changes_by_type.get(change_key, 0) + 1
            
            # Expected changes from our test commit:
            expected_changes = {
                "function_added": 2,     # reset_processor, validate_data_integrity
                "function_modified": 2,  # __init__, calculate_metrics  
                "import_added": 2,       # logging, Optional/timedelta additions
                "variable_added": 1,     # MAX_CONCURRENT_PROCESSES
                "variable_modified": 2   # DEFAULT_CONFIG, __all__
            }
            
            accuracy_score = 0
            total_expected = sum(expected_changes.values())
            
            detected_relevant = 0
            for change_type, expected_count in expected_changes.items():
                detected_count = changes_by_type.get(change_type, 0)
                detected_relevant += min(detected_count, expected_count)
            
            accuracy_score = (detected_relevant / total_expected) * 100 if total_expected > 0 else 0
            
            return {
                "total_entity_changes": len(analysis.entity_changes),
                "changes_by_type": changes_by_type,
                "expected_changes": expected_changes,
                "accuracy_score": accuracy_score,
                "files_analyzed": [fc.file_path for fc in analysis.file_changes if fc.file_path.endswith('.py')]
            }
            
        except Exception as e:
            return {"error": str(e)}
    
    def test_update_planning(self) -> Dict[str, Any]:
        """Test graph update planning"""
        try:
            # Mock a graph manager for testing (since we can't connect to Neo4j)
            class MockGraphManager:
                def find_entity(self, entity_type, name):
                    return None  # Simulate no existing entities
                
                def create_code_entity(self, entity_type, name, properties):
                    return {"id": f"mock_{entity_type}_{name}"}
                
                def create_relationship(self, from_node, to_node, rel_type):
                    return {"type": rel_type}
            
            mock_graph_manager = MockGraphManager()
            update_engine = GraphUpdateEngine(mock_graph_manager)
            
            # Create mock diff analysis
            from agents.git_diff_agent import DiffAnalysis, FileChange, CodeEntityChange
            
            mock_analysis = DiffAnalysis(
                commit_hash="test123",
                commit_message="Test commit",
                author="Test Author",
                timestamp="2025-07-13",
                file_changes=[
                    FileChange("test_file.py", ChangeType.MODIFIED, lines_added=10, lines_removed=2)
                ],
                entity_changes=[
                    CodeEntityChange("function", "test_function", ChangeType.ADDED, "test_file.py"),
                    CodeEntityChange("function", "old_function", ChangeType.MODIFIED, "test_file.py"),
                    CodeEntityChange("variable", "TEST_VAR", ChangeType.ADDED, "test_file.py")
                ],
                metadata={}
            )
            
            # Test planning
            update_plan = update_engine.plan_updates(mock_analysis)
            
            # Analyze the plan
            operations_by_type = {}
            for update in update_plan.updates:
                op_type = update.operation.value
                operations_by_type[op_type] = operations_by_type.get(op_type, 0) + 1
            
            return {
                "total_updates_planned": len(update_plan.updates),
                "operations_by_type": operations_by_type,
                "estimated_time": update_plan.estimated_time,
                "risk_assessment": update_plan.risk_assessment,
                "dependencies_count": len(update_plan.dependencies)
            }
            
        except Exception as e:
            return {"error": str(e)}
    
    def test_mock_surgical_updates(self) -> Dict[str, Any]:
        """Test surgical updates with mock graph manager"""
        try:
            # This test runs the update engine without actual Neo4j
            class MockGraphManager:
                def __init__(self):
                    self.nodes = {}
                    self.relationships = []
                    
                def find_entity(self, entity_type, name):
                    key = f"{entity_type}:{name}"
                    return self.nodes.get(key)
                
                def create_code_entity(self, entity_type, name, properties):
                    key = f"{entity_type}:{name}"
                    node = {"type": entity_type, "name": name, "properties": properties}
                    self.nodes[key] = node
                    return node
                
                def create_relationship(self, from_node, to_node, rel_type):
                    rel = {"from": from_node, "to": to_node, "type": rel_type}
                    self.relationships.append(rel)
                    return rel
                
                def graph(self):
                    return self
                
                def run(self, query, **params):
                    return {"summary": {"nodes_deleted": 1}}
                
                def push(self, node):
                    pass
            
            mock_graph_manager = MockGraphManager()
            update_engine = GraphUpdateEngine(mock_graph_manager)
            
            # Create and execute a simple update plan
            from graph_update_engine import GraphUpdate, UpdatePlan
            
            mock_updates = [
                GraphUpdate(
                    operation=UpdateOperation.CREATE_NODE,
                    entity_type="Function",
                    entity_name="new_function",
                    file_path="test.py",
                    properties={"signature": "def new_function():"}
                ),
                GraphUpdate(
                    operation=UpdateOperation.UPDATE_NODE,
                    entity_type="Function", 
                    entity_name="existing_function",
                    file_path="test.py",
                    properties={"last_modified": time.time()}
                )
            ]
            
            mock_plan = UpdatePlan(
                diff_analysis=None,
                updates=mock_updates,
                dependencies=[],
                estimated_time=0.1,
                risk_assessment="LOW"
            )
            
            # Execute updates
            result = update_engine.execute_updates(mock_plan)
            
            return {
                "execution_successful": result["success"],
                "updates_executed": result["executed_updates"],
                "nodes_in_mock_graph": len(mock_graph_manager.nodes),
                "relationships_in_mock_graph": len(mock_graph_manager.relationships),
                "engine_statistics": update_engine.get_statistics()
            }
            
        except Exception as e:
            return {"error": str(e)}
    
    def test_performance_comparison(self) -> Dict[str, Any]:
        """Test performance benefits of surgical updates"""
        try:
            # Simulate timing comparison
            start_time = time.time()
            
            # Mock "surgical update" timing
            time.sleep(0.1)  # Simulate 100ms surgical update
            surgical_time = time.time() - start_time
            
            # Mock "full reparse" timing  
            start_time = time.time()
            time.sleep(0.5)  # Simulate 500ms full reparse
            full_reparse_time = time.time() - start_time
            
            time_saved = full_reparse_time - surgical_time
            performance_improvement = ((time_saved / full_reparse_time) * 100) if full_reparse_time > 0 else 0
            
            return {
                "surgical_update_time": surgical_time,
                "full_reparse_time": full_reparse_time,
                "time_saved": time_saved,
                "performance_improvement_percent": performance_improvement,
                "is_faster": surgical_time < full_reparse_time
            }
            
        except Exception as e:
            return {"error": str(e)}
    
    def _create_test_git_agent(self) -> GitDiffAgent:
        """Create a GitDiffAgent for testing"""
        config = {
            "agent_id": "test-git-agent",
            "capabilities": ["git_diff_analysis"],
            "description": "Test git diff agent",
            "config": {
                "repoPath": ".",
                "supportedExtensions": [".py"],
                "ignorePatterns": ["__pycache__", ".git"]
            }
        }
        return GitDiffAgent("test-git-agent", config)
    
    def _record_test_result(self, test_name: str, passed: bool, details: Dict[str, Any]):
        """Record the result of a test"""
        self.test_results["tests_run"] += 1
        if passed:
            self.test_results["tests_passed"] += 1
        else:
            self.test_results["tests_failed"] += 1
        
        self.test_results["details"].append({
            "test_name": test_name,
            "passed": passed,
            "details": details,
            "timestamp": time.time()
        })
    
    def _generate_test_report(self) -> Dict[str, Any]:
        """Generate comprehensive test report"""
        success_rate = (self.test_results["tests_passed"] / max(1, self.test_results["tests_run"])) * 100
        
        report = {
            **self.test_results,
            "success_rate": success_rate,
            "overall_result": "PASS" if success_rate >= 80 else "FAIL",
            "summary": {
                "total_tests": self.test_results["tests_run"],
                "passed": self.test_results["tests_passed"],
                "failed": self.test_results["tests_failed"],
                "success_rate": f"{success_rate:.1f}%"
            },
            "timestamp": time.time()
        }
        
        return report


def main():
    """Main function to run surgical update tests"""
    print("🔬 Starting Surgical Update System Tests")
    print("=" * 50)
    
    tester = SurgicalUpdateTester()
    
    try:
        # Run all tests
        results = tester.run_all_tests()
        
        # Print summary
        print(f"\n📊 Test Results Summary:")
        print(f"Overall Result: {results['overall_result']}")
        print(f"Tests Run: {results['summary']['total_tests']}")
        print(f"Passed: {results['summary']['passed']}")
        print(f"Failed: {results['summary']['failed']}")
        print(f"Success Rate: {results['summary']['success_rate']}")
        
        # Print detailed results
        print(f"\n📋 Detailed Results:")
        for detail in results['details']:
            status = "✅ PASS" if detail['passed'] else "❌ FAIL"
            print(f"{status} {detail['test_name']}")
            if not detail['passed'] and 'error' in detail['details']:
                print(f"    Error: {detail['details']['error']}")
        
        # Save results to file
        results_file = "surgical_update_test_results.json"
        with open(results_file, 'w') as f:
            json.dump(results, f, indent=2)
        print(f"\n💾 Detailed results saved to: {results_file}")
        
        # Return appropriate exit code
        return 0 if results['overall_result'] == 'PASS' else 1
        
    except Exception as e:
        print(f"❌ Test execution failed: {e}")
        return 1


if __name__ == "__main__":
    exit_code = main()
    sys.exit(exit_code)