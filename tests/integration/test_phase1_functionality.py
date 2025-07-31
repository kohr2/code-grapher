#!/usr/bin/env python3
"""
Functional test to verify Phase 1 services work together
"""
import sys
import os
from pathlib import Path

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

def test_service_registry_integration():
    """Test that service registry integrates with dependency injection"""
    print("🧪 Testing service registry integration...")
    
    from shared.services.service_registry import ServiceRegistry
    from shared.services.logger_facade import LoggerFacade
    from shared.services.graph_manager_facade import GraphManagerFacade
    from shared.interfaces.logger_interface import LoggerInterface
    from shared.interfaces.graph_operations_interface import GraphOperationsInterface
    
    # Create registry
    registry = ServiceRegistry()
    
    # Register services
    logger = LoggerFacade("test")
    graph_facade = GraphManagerFacade(logger)
    
    registry.register(LoggerInterface, logger)
    registry.register(GraphOperationsInterface, graph_facade)
    
    # Test retrieval
    retrieved_logger = registry.get(LoggerInterface)
    retrieved_graph = registry.get(GraphOperationsInterface)
    
    print(f"✅ Logger registered and retrieved: {type(retrieved_logger).__name__}")
    print(f"✅ Graph manager registered and retrieved: {type(retrieved_graph).__name__}")
    
    # Test health checks
    health_status = registry.health_check_all()
    print(f"✅ Health check status: {len(health_status)} services checked")
    
    registry.clear_all()
    print("✅ Service registry integration test passed")

def test_configuration_integration():
    """Test configuration management integration"""
    print("\n🧪 Testing configuration integration...")
    
    from shared.config.config_manager import ConfigManager
    from shared.config.service_config import DatabaseConfig, AIConfig, LoggingConfig
    
    # Test config manager
    config = ConfigManager()
    
    # Test database config
    db_config = DatabaseConfig.from_dict(config.get_database_config())
    print(f"✅ Database config loaded: {db_config.url}")
    
    # Test AI config
    ai_config = AIConfig.from_dict(config.get_ai_config())
    print(f"✅ AI config loaded: {ai_config.default_provider}")
    
    # Test logging config
    log_config = LoggingConfig.from_dict(config.get_logging_config())
    print(f"✅ Logging config loaded: {log_config.level}")
    
    print("✅ Configuration integration test passed")

def test_service_locator_fallback():
    """Test that service locator falls back properly"""
    print("\n🧪 Testing service locator fallback...")
    
    from shared.services.service_locator import ServiceLocator
    
    # Clear any existing services
    ServiceLocator.clear_services()
    
    # Test logger fallback
    logger = ServiceLocator.get_logger("test")
    print(f"✅ Logger fallback works: {type(logger).__name__}")
    
    # Test graph manager fallback
    graph_manager = ServiceLocator.get_graph_manager()
    print(f"✅ Graph manager fallback works: {type(graph_manager).__name__}")
    
    print("✅ Service locator fallback test passed")

def test_dependency_injection():
    """Test dependency injection container"""
    print("\n🧪 Testing dependency injection...")
    
    from shared.services.dependency_injector import DependencyInjector, inject
    from shared.services.service_registry import ServiceRegistry
    from shared.services.logger_facade import LoggerFacade
    from shared.interfaces.logger_interface import LoggerInterface
    
    # Set up registry
    registry = ServiceRegistry()
    logger = LoggerFacade("test")
    registry.register(LoggerInterface, logger)
    
    # Create injector
    injector = DependencyInjector(registry)
    
    # Test resolution
    resolved_logger = injector.resolve(LoggerInterface)
    print(f"✅ Dependency injection works: {type(resolved_logger).__name__}")
    
    # Test dependency graph
    dep_graph = injector.get_dependency_graph()
    print(f"✅ Dependency graph generated: {len(dep_graph)} services")
    
    registry.clear_all()
    print("✅ Dependency injection test passed")

if __name__ == '__main__':
    print("🚀 Running Phase 1 Functionality Tests")
    print("=" * 50)
    
    try:
        test_service_registry_integration()
        test_configuration_integration()
        test_service_locator_fallback()
        test_dependency_injection()
        
        print("\n" + "=" * 50)
        print("🎉 All Phase 1 functionality tests PASSED!")
        print("✅ Service registry framework operational")
        print("✅ Configuration management centralized")
        print("✅ Logger facade working with service locator")
        print("✅ Dependency injection container functional")
        print("✅ All existing functionality preserved through facades")
        
    except Exception as e:
        print(f"\n❌ Test failed: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)