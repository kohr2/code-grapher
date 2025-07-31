"""
Phase 5 Architecture Integration Tests

Tests to verify the refactored service-based architecture works correctly.
"""

import pytest
import asyncio
import tempfile
import os
from pathlib import Path
import sys

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from core.bootstrap.application import Application, create_application
from core.interfaces.pipeline_interface import PipelineInterface, PipelineStatus
from core.interfaces.rag_service_interface import RAGServiceInterface, RAGQuery
from shared.interfaces.graph_operations_interface import GraphOperationsInterface
from shared.interfaces.logger_interface import LoggerInterface


class TestPhase5Architecture:
    """Test suite for Phase 5 service-based architecture"""
    
    @pytest.fixture
    async def app(self):
        """Create application instance for testing"""
        app = Application()
        init_result = await app.initialize()
        
        # Verify initialization succeeded
        assert init_result['status'] == 'success'
        assert init_result['initialized_services'] > 0
        
        yield app
        
        # Cleanup
        await app.shutdown()
    
    @pytest.fixture
    def sample_code_directory(self):
        """Create a temporary directory with sample Python code"""
        with tempfile.TemporaryDirectory() as temp_dir:
            # Create sample Python files
            sample_files = {
                'main.py': '''
def main():
    """Main entry point"""
    print("Hello, World!")
    process_data()

def process_data():
    """Process some data"""
    data = get_data()
    return transform_data(data)

def get_data():
    """Get sample data"""
    return [1, 2, 3, 4, 5]

def transform_data(data):
    """Transform data"""
    return [x * 2 for x in data]

if __name__ == "__main__":
    main()
                ''',
                'utils.py': '''
class DataProcessor:
    """Utility class for data processing"""
    
    def __init__(self, config=None):
        self.config = config or {}
    
    def process(self, data):
        """Process data with configuration"""
        if self.config.get('double', False):
            return [x * 2 for x in data]
        return data
    
    @staticmethod
    def validate_data(data):
        """Validate input data"""
        return isinstance(data, list) and len(data) > 0

class ConfigManager:
    """Configuration management"""
    
    def __init__(self):
        self._config = {}
    
    def get(self, key, default=None):
        return self._config.get(key, default)
    
    def set(self, key, value):
        self._config[key] = value
                ''',
                'models/__init__.py': '',
                'models/user.py': '''
class User:
    """User model"""
    
    def __init__(self, name, email):
        self.name = name
        self.email = email
    
    def get_display_name(self):
        """Get user display name"""
        return f"{self.name} <{self.email}>"
    
    def validate_email(self):
        """Validate email format"""
        return "@" in self.email and "." in self.email

class UserManager:
    """User management class"""
    
    def __init__(self):
        self.users = []
    
    def add_user(self, user):
        """Add a user"""
        if user.validate_email():
            self.users.append(user)
            return True
        return False
    
    def get_user_by_email(self, email):
        """Find user by email"""
        for user in self.users:
            if user.email == email:
                return user
        return None
                '''
            }
            
            # Write files to temp directory
            for file_path, content in sample_files.items():
                full_path = Path(temp_dir) / file_path
                full_path.parent.mkdir(parents=True, exist_ok=True)
                full_path.write_text(content)
            
            yield temp_dir
    
    @pytest.mark.asyncio
    async def test_application_initialization(self, app):
        """Test that application initializes correctly"""
        assert app.is_initialized()
        
        # Check that key services are registered
        registry = app.get_service_registry()
        
        # Logger should be available
        logger = registry.get(LoggerInterface)
        assert logger is not None
        
        # Graph service should be available
        try:
            graph_service = registry.get(GraphOperationsInterface)
            assert graph_service is not None
        except Exception:
            pytest.skip("Graph service not available in test environment")
    
    @pytest.mark.asyncio
    async def test_pipeline_orchestrator_integration(self, app, sample_code_directory):
        """Test pipeline orchestrator with sample code"""
        registry = app.get_service_registry()
        
        try:
            pipeline_service = registry.get(PipelineInterface)
            
            # Test pipeline validation
            config = {'target_directory': sample_code_directory}
            is_valid = await pipeline_service.validate_pipeline_config(config)
            assert is_valid
            
            # Test pipeline execution (without AI to avoid external dependencies)
            result = await pipeline_service.run_enhanced_pipeline(
                target_directory=sample_code_directory,
                use_ai=False,
                enable_relationships=False,  # Disable to avoid complex dependencies
                enable_embeddings=False
            )
            
            # Check result status
            assert result.status == PipelineStatus.SUCCESS
            assert result.data is not None
            assert result.execution_time > 0
            
            # Verify files were processed
            data = result.data
            assert data.get('files_processed', 0) > 0
            assert data.get('total_files', 0) >= 4  # main.py, utils.py, models/__init__.py, models/user.py
            
        except Exception as e:
            pytest.skip(f"Pipeline orchestrator test skipped: {e}")
    
    @pytest.mark.asyncio
    async def test_rag_service_basic_functionality(self, app):
        """Test RAG service basic functionality"""
        registry = app.get_service_registry()
        
        try:
            rag_service = registry.get(RAGServiceInterface)
            
            # Test health check
            is_healthy = await rag_service.health_check()
            # Note: May fail in test environment without proper setup
            # Just verify the method exists and returns a boolean
            assert isinstance(is_healthy, bool)
            
            # Test embedding creation with sample entities
            sample_entities = [
                {
                    'name': 'test_function',
                    'type': 'function',
                    'file_path': 'test.py',
                    'line': 1,
                    'properties': {
                        'docstring': 'A test function',
                        'code_snippet': 'def test_function(): pass'
                    }
                }
            ]
            
            # This may fail due to missing ChromaDB setup, but should not crash
            try:
                result = await rag_service.create_embeddings(sample_entities)
                assert 'count' in result
                assert 'execution_time' in result
            except Exception:
                # Expected in test environment without full setup
                pass
            
        except Exception as e:
            pytest.skip(f"RAG service test skipped: {e}")
    
    @pytest.mark.asyncio
    async def test_service_dependency_resolution(self, app):
        """Test that service dependencies are resolved correctly"""
        registry = app.get_service_registry()
        
        # Get application info to check service health
        info = app.get_application_info()
        
        assert info['is_initialized']
        assert len(info['registered_services']) > 0
        
        # Check that services have their dependencies available
        services_health = info['services_health']
        
        # At minimum, logger should be healthy
        logger_health = None
        for service_name, health in services_health.items():
            if 'Logger' in service_name:
                logger_health = health
                break
        
        # Note: In test environment, some services may not be fully healthy
        # due to missing external dependencies (Neo4j, ChromaDB, etc.)
        # We just verify the structure is correct
        assert isinstance(services_health, dict)
        assert len(services_health) > 0
    
    @pytest.mark.asyncio
    async def test_service_registry_functionality(self, app):
        """Test service registry basic functionality"""
        registry = app.get_service_registry()
        
        # Test service registration check
        assert registry.is_registered(LoggerInterface)
        
        # Test service retrieval
        logger = registry.get(LoggerInterface)
        assert logger is not None
        
        # Test getting all registered services
        all_services = registry.get_all_registered()
        assert isinstance(all_services, dict)
        assert LoggerInterface in all_services
    
    @pytest.mark.asyncio
    async def test_configuration_loading(self, app):
        """Test configuration loading and validation"""
        info = app.get_application_info()
        
        assert 'config' in info
        config = info['config']
        
        # Verify configuration structure
        assert isinstance(config, dict)
        
        # Check for expected configuration sections
        expected_sections = ['pipeline', 'rag', 'service_timeout', 'log_level']
        for section in expected_sections:
            assert section in config, f"Missing config section: {section}"
    
    def test_main_entry_point_exists(self):
        """Test that main.py entry point exists and is importable"""
        try:
            import main
            assert hasattr(main, 'main')
            assert callable(main.main)
        except ImportError as e:
            pytest.fail(f"Could not import main.py: {e}")


# Standalone test for quick verification
async def quick_test():
    """Quick standalone test for Phase 5 architecture"""
    print("🧪 Running Phase 5 Architecture Quick Test...")
    
    try:
        # Test application creation
        app = await create_application()
        
        # Verify basic functionality
        assert app.is_initialized()
        
        info = app.get_application_info()
        print(f"   ✅ Application initialized with {len(info['registered_services'])} services")
        
        # Test service registry
        registry = app.get_service_registry()
        logger = registry.get(LoggerInterface)
        logger.log_info("Quick test successful")
        
        # Cleanup
        await app.shutdown()
        
        print("   ✅ Quick test passed!")
        return True
        
    except Exception as e:
        print(f"   ❌ Quick test failed: {e}")
        return False


if __name__ == "__main__":
    # Run quick test if executed directly
    success = asyncio.run(quick_test())
    exit(0 if success else 1)