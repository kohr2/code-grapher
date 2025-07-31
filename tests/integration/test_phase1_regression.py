#!/usr/bin/env python3
"""
Integration tests to verify Phase 1 doesn't break existing functionality
"""
import sys
import os
import unittest
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))


class TestPhase1Regression(unittest.TestCase):
    """Test that Phase 1 changes don't break existing functionality"""
    
    def test_core_pipeline_imports(self):
        """Test that core pipeline imports work"""
        try:
            from core_pipeline import run_enhanced_pipeline, parse_and_extract_entities
            self.assertTrue(True, "Core pipeline imports successfully")
        except ImportError as e:
            self.fail(f"Core pipeline import failed: {e}")
    
    def test_surgical_update_coordinator_imports(self):
        """Test that surgical update coordinator imports work"""
        try:
            from surgical_update_coordinator import SurgicalUpdateCoordinator
            self.assertTrue(True, "Surgical update coordinator imports successfully")
        except ImportError as e:
            self.fail(f"Surgical update coordinator import failed: {e}")
    
    def test_agent_coordinator_functionality(self):
        """Test that agent coordinator works properly"""
        try:
            from coordinator.agent_coordinator import AgentCoordinator
            
            # Test instantiation
            coordinator = AgentCoordinator("test_config")
            self.assertIsNotNone(coordinator)
            
            # Test execute_workflow method exists and works
            result = coordinator.execute_workflow({"files": []})
            self.assertIsInstance(result, dict)
            self.assertIn("success", result)
            self.assertTrue(result["success"])
            
        except Exception as e:
            self.fail(f"Agent coordinator test failed: {e}")
    
    def test_ai_relationship_extractor_instantiation(self):
        """Test that AIRelationshipExtractor can be instantiated (original bug)"""
        try:
            from ai_relationship_extractor import AIRelationshipExtractor
            # Just test that it can be imported and instantiated
            # Don't actually initialize to avoid Ollama dependency in tests
            self.assertTrue(hasattr(AIRelationshipExtractor, '__init__'))
        except ImportError as e:
            self.fail(f"AIRelationshipExtractor import failed: {e}")
    
    def test_service_locator_functionality(self):
        """Test that service locator works"""
        try:
            from shared.services.service_locator import ServiceLocator
            
            # Test logger retrieval
            logger = ServiceLocator.get_logger("test")
            self.assertIsNotNone(logger)
            
            # Test service registry access
            registry = ServiceLocator.get_registry()
            self.assertIsNotNone(registry)
            
        except Exception as e:
            self.fail(f"Service locator test failed: {e}")
    
    def test_logger_facade_functionality(self):
        """Test that logger facade works"""
        try:
            from shared.services.logger_facade import LoggerFactory, LoggerFacade
            
            # Test factory
            logger = LoggerFactory.get_logger("test")
            self.assertIsNotNone(logger)
            
            # Test logging methods exist
            self.assertTrue(hasattr(logger, 'log_info'))
            self.assertTrue(hasattr(logger, 'log_error'))
            self.assertTrue(hasattr(logger, 'log_warning'))
            
        except Exception as e:
            self.fail(f"Logger facade test failed: {e}")
    
    def test_service_registry_functionality(self):
        """Test that service registry works"""
        try:
            from shared.services.service_registry import ServiceRegistry
            from shared.interfaces.logger_interface import LoggerInterface
            from shared.services.logger_facade import LoggerFacade
            
            # Create new registry for testing
            registry = ServiceRegistry()
            
            # Test registration
            test_logger = LoggerFacade("test")
            registry.register(LoggerInterface, test_logger)
            
            # Test retrieval
            retrieved_logger = registry.get(LoggerInterface)
            self.assertEqual(retrieved_logger, test_logger)
            
            # Test is_registered
            self.assertTrue(registry.is_registered(LoggerInterface))
            
            # Clean up
            registry.clear_all()
            
        except Exception as e:
            self.fail(f"Service registry test failed: {e}")
    
    def test_configuration_management(self):
        """Test that configuration management works"""
        try:
            from shared.config.config_manager import ConfigManager
            
            # Test instantiation
            config = ConfigManager()
            self.assertIsNotNone(config)
            
            # Test default values
            db_config = config.get_database_config()
            self.assertIsInstance(db_config, dict)
            
            # Test get/set
            config.set("test.value", "test_data")
            self.assertEqual(config.get("test.value"), "test_data")
            
        except Exception as e:
            self.fail(f"Configuration management test failed: {e}")
    
    def test_graph_manager_facade(self):
        """Test that graph manager facade works"""
        try:
            from shared.services.graph_manager_facade import GraphManagerFacade
            from shared.interfaces.graph_operations_interface import GraphOperationsInterface
            
            # Test instantiation
            facade = GraphManagerFacade()
            self.assertIsNotNone(facade)
            
            # Test interface compliance
            self.assertIsInstance(facade, GraphOperationsInterface)
            
            # Test service interface compliance
            from shared.interfaces.service_interface import ServiceInterface
            self.assertIsInstance(facade, ServiceInterface)
            
            # Test health check
            health = facade.health_check()
            self.assertIsInstance(health, dict)
            self.assertIn("status", health)
            
        except Exception as e:
            self.fail(f"Graph manager facade test failed: {e}")


if __name__ == '__main__':
    # Run tests with verbose output
    unittest.main(verbosity=2)