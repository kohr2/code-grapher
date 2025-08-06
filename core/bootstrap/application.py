"""
Application Bootstrap

Main application entry point with proper dependency injection and service registration.
Phase 5 verticalization - centralized application bootstrap.
"""

import os
import sys
import asyncio
from pathlib import Path
from typing import Dict, Any, Optional
from dotenv import load_dotenv

# Add project root to path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

# Load environment variables
load_dotenv()

from shared.services.service_registry import ServiceRegistry
from shared.config.config_manager import ConfigManager
from shared.interfaces.logger_interface import LoggerInterface
from shared.services.logger_facade import LoggerFacade
from core.config.orchestration_config import OrchestrationConfig
from core.orchestration.dependency_resolver import DependencyResolver, ServiceDependencyInfo, DependencyStatus


class Application:
    """
    Main application class with service registry and dependency injection

    Bootstrap application with proper service initialization order and health monitoring.
    """

    def __init__(self):
        self.service_registry = ServiceRegistry()
        self.config_manager = None
        self.orchestration_config = None
        self.logger = None
        self.dependency_resolver = None

        # Application state
        self._is_initialized = False
        self._services_health = {}

    async def initialize(self) -> Dict[str, Any]:
        """
        Initialize the application with all services

        Returns:
            Initialization results and status
        """
        print("🚀 Initializing Code Grapher Application...")

        try:
            # 1. Initialize configuration
            await self._initialize_configuration()

            # 2. Initialize logging
            await self._initialize_logging()

            # 3. Initialize dependency resolver
            await self._initialize_dependency_resolver()

            # 4. Register all services with dependencies
            await self._register_services()

            # 5. Resolve dependencies and get initialization order
            initialization_order = self.dependency_resolver.resolve_dependencies()

            # 6. Initialize services in dependency order
            initialization_results = await self._initialize_services_in_order(initialization_order)

            # 7. Perform health checks
            health_results = await self._perform_health_checks()

            self._is_initialized = True

            result = {
                "status": "success",
                "initialized_services": len(initialization_results["successful"]),
                "failed_services": len(initialization_results["failed"]),
                "initialization_order": [s.__name__ for s in initialization_order],
                "health_checks": health_results,
                "config_loaded": True,
            }

            self.logger.log_info(f"Application initialized successfully: {result}")
            print(f"✅ Application initialized: {result['initialized_services']} services ready")

            return result

        except Exception as e:
            error_msg = f"Application initialization failed: {e}"
            if self.logger:
                self.logger.log_error(error_msg)
            else:
                print(f"❌ {error_msg}")

            return {"status": "failed", "error": str(e), "initialized_services": 0}

    async def _initialize_configuration(self):
        """Initialize configuration management"""
        print("   📋 Loading configuration...")

        # Initialize config manager
        self.config_manager = ConfigManager()

        # Load orchestration configuration
        self.orchestration_config = OrchestrationConfig.from_env()

        # Validate configuration
        config_errors = self.orchestration_config.validate()
        if config_errors:
            raise RuntimeError(f"Configuration validation failed: {config_errors}")

        print("   ✅ Configuration loaded successfully")

    async def _initialize_logging(self):
        """Initialize logging system"""
        print("   📝 Initializing logging...")

        # Create logger facade (using existing logger system)
        self.logger = LoggerFacade("application")

        # Register logger in service registry
        self.service_registry.register(LoggerInterface, self.logger)

        print("   ✅ Logging initialized")

    async def _initialize_dependency_resolver(self):
        """Initialize dependency resolver"""
        print("   🔗 Initializing dependency resolver...")

        self.dependency_resolver = DependencyResolver(self.logger)

        print("   ✅ Dependency resolver ready")

    async def _register_services(self):
        """Register all services with their dependencies"""
        print("   🏗️  Registering services...")

        # Import service interfaces
        from shared.interfaces.graph_operations_interface import GraphOperationsInterface
        from core.interfaces.pipeline_interface import PipelineInterface
        from core.interfaces.rag_service_interface import RAGServiceInterface

        # Use simple AI service instead of complex ai-services module
        from core.services.simple_ai_service import AIServicesInterface

        try:
            from update_agents.interfaces.update_coordinator_interface import UpdateIntelligenceInterface
        except ImportError:
            UpdateIntelligenceInterface = None

        # Register core services

        # 1. Graph Operations Service (from Phase 4)
        try:
            from shared.services.graph_manager_facade import GraphManagerFacade

            graph_service = GraphManagerFacade(self.logger)
            self.service_registry.register(GraphOperationsInterface, graph_service)

            self.dependency_resolver.register_service_with_dependencies(
                GraphOperationsInterface, graph_service, dependencies=[LoggerInterface]
            )

            print("     ✅ Graph Operations Service registered")
        except Exception as e:
            print(f"     ⚠️  Graph Operations Service registration failed: {e}")

        # 2. AI Services - Simple implementation
        if AIServicesInterface:
            try:
                from core.services.simple_ai_service import SimpleAIService

                ai_service = SimpleAIService(self.logger)
                ai_service.initialize({})
                self.service_registry.register(AIServicesInterface, ai_service)

                self.dependency_resolver.register_service_with_dependencies(
                    AIServicesInterface, ai_service, dependencies=[LoggerInterface]
                )

                print("     ✅ AI Services registered")
            except Exception as e:
                print(f"     ⚠️  AI Services registration failed: {e}")

        # 3. RAG Service - Register after graph service
        try:
            from core.services.rag_service import RAGService

            # Get the already registered graph service
            graph_service = self.service_registry.get(GraphOperationsInterface)

            rag_service = RAGService(
                graph_service=graph_service,
                ai_service=None,  # Will be injected if available
                config=self.orchestration_config.rag,
                logger=self.logger,
            )
            self.service_registry.register(RAGServiceInterface, rag_service)

            dependencies = [LoggerInterface, GraphOperationsInterface]
            optional_deps = []
            if AIServicesInterface:
                optional_deps.append(AIServicesInterface)

            self.dependency_resolver.register_service_with_dependencies(
                RAGServiceInterface, rag_service, dependencies=dependencies, optional_dependencies=optional_deps
            )

            print("     ✅ RAG Service registered")
        except Exception as e:
            print(f"     ⚠️  RAG Service registration failed: {e}")

        # 4. Classification Service
        try:
            from core.services.classification_service import ClassificationService

            classification_service = ClassificationService(self.logger)
            # Note: Not registering interface since it's used internally by pipeline service

            print("     ✅ Classification Service created")
        except Exception as e:
            print(f"     ⚠️  Classification Service creation failed: {e}")
            classification_service = None

        # 5. Pipeline Service
        try:
            from core.services.pipeline_service import PipelineService

            pipeline_service = PipelineService(
                classification_service=classification_service,
                config=self.orchestration_config.pipeline,
                logger=self.logger,
            )
            # Note: Not registering interface since pipeline orchestrator wraps this

            print("     ✅ Pipeline Service created")
        except Exception as e:
            print(f"     ⚠️  Pipeline Service creation failed: {e}")
            pipeline_service = None

        # 6. Pipeline Orchestrator - Use real implementation
        try:
            from core.orchestration.pipeline_orchestrator import PipelineOrchestrator

            pipeline_orchestrator = PipelineOrchestrator(
                services=self.service_registry, config=self.orchestration_config, logger=self.logger
            )
            self.service_registry.register(PipelineInterface, pipeline_orchestrator)

            dependencies = [LoggerInterface, GraphOperationsInterface, RAGServiceInterface]
            optional_deps = []
            if AIServicesInterface:
                optional_deps.append(AIServicesInterface)

            self.dependency_resolver.register_service_with_dependencies(
                PipelineInterface, pipeline_orchestrator, dependencies=dependencies, optional_dependencies=optional_deps
            )

            print("     ✅ Pipeline Orchestrator registered")
        except Exception as e:
            print(f"     ⚠️  Pipeline Orchestrator registration failed: {e}")

        # 7. Update Intelligence Service (from Phase 3) - Optional
        if UpdateIntelligenceInterface:
            try:
                from update_agents.services.update_service import UpdateService

                update_service = UpdateService(
                    graph_service=None,  # Will be injected
                    ai_service=None,  # Will be injected
                    config=None,  # Will use default config
                    logger=self.logger,
                )
                self.service_registry.register(UpdateIntelligenceInterface, update_service)

                dependencies = [LoggerInterface, GraphOperationsInterface]
                optional_deps = [AIServicesInterface] if AIServicesInterface else []

                self.dependency_resolver.register_service_with_dependencies(
                    UpdateIntelligenceInterface,
                    update_service,
                    dependencies=dependencies,
                    optional_dependencies=optional_deps,
                )

                print("     ✅ Update Intelligence Service registered")
            except Exception as e:
                print(f"     ⚠️  Update Intelligence Service registration failed: {e}")

        print(f"   ✅ Service registration complete")

    async def _initialize_services_in_order(self, initialization_order):
        """Initialize services in dependency-resolved order"""
        print("   🔧 Initializing services in dependency order...")

        successful = []
        failed = []

        for service_interface in initialization_order:
            try:
                service = self.service_registry.get(service_interface)

                # Initialize service if it has an initialize method
                if hasattr(service, "initialize"):
                    # Handle both sync and async initialize methods
                    result = service.initialize({})
                    if asyncio.iscoroutine(result):
                        await result

                # Update dependency resolver status
                self.dependency_resolver.update_service_status(service_interface, DependencyStatus.RESOLVED)

                successful.append(service_interface)
                print(f"     ✅ {service_interface.__name__} initialized")

            except Exception as e:
                self.dependency_resolver.update_service_status(service_interface, DependencyStatus.FAILED, str(e))

                failed.append({"service": service_interface.__name__, "error": str(e)})
                print(f"     ❌ {service_interface.__name__} failed: {e}")

        return {"successful": successful, "failed": failed}

    async def _perform_health_checks(self):
        """Perform health checks on all initialized services"""
        print("   🏥 Performing health checks...")

        health_results = {}

        for interface, service in self.service_registry.get_all_registered().items():
            try:
                if hasattr(service, "health_check"):
                    health_result = service.health_check()
                    # Handle both sync and async health checks
                    if asyncio.iscoroutine(health_result):
                        health_result = await health_result

                    # Handle both boolean and dict return types
                    if isinstance(health_result, dict):
                        health_results[interface.__name__] = health_result
                    else:
                        health_results[interface.__name__] = {
                            "status": "healthy" if health_result else "unhealthy",
                            "has_health_check": True,
                        }
                else:
                    health_results[interface.__name__] = {"status": "unknown", "has_health_check": False}

                print(
                    f"     {'✅' if health_results[interface.__name__]['status'] == 'healthy' else '⚠️ '} {interface.__name__}: {health_results[interface.__name__]['status']}"
                )

            except Exception as e:
                health_results[interface.__name__] = {"status": "error", "error": str(e)}
                print(f"     ❌ {interface.__name__}: error - {e}")

        self._services_health = health_results
        return health_results

    def get_service_registry(self) -> ServiceRegistry:
        """Get the service registry"""
        return self.service_registry

    def get_logger(self) -> LoggerInterface:
        """Get the logger"""
        return self.logger

    def is_initialized(self) -> bool:
        """Check if application is initialized"""
        return self._is_initialized

    async def shutdown(self):
        """Shutdown the application gracefully"""
        if self.logger:
            self.logger.log_info("Shutting down application...")

        print("🔄 Shutting down Code Grapher Application...")

        # Shutdown services in reverse order
        try:
            for interface, service in reversed(list(self.service_registry.get_all_registered().items())):
                try:
                    if hasattr(service, "shutdown"):
                        # Handle both sync and async shutdown methods
                        result = service.shutdown()
                        if asyncio.iscoroutine(result):
                            await result
                    print(f"   ✅ {interface.__name__} shutdown")
                except Exception as e:
                    print(f"   ⚠️  {interface.__name__} shutdown failed: {e}")

            print("✅ Application shutdown complete")

        except Exception as e:
            print(f"❌ Application shutdown failed: {e}")

    def get_application_info(self) -> Dict[str, Any]:
        """Get application information"""
        return {
            "name": "Code Grapher",
            "version": "1.0.0",
            "phase": "Phase 5 - Core Orchestration Refactoring",
            "is_initialized": self._is_initialized,
            "registered_services": [i.__name__ for i in self.service_registry.get_all_registered().keys()],
            "services_health": self._services_health,
            "config": self.orchestration_config.to_dict() if self.orchestration_config else {},
        }


# Convenience function for creating application
async def create_application() -> Application:
    """
    Create and initialize the Code Grapher application

    Returns:
        Initialized Application instance
    """
    app = Application()
    await app.initialize()
    return app


# Main entry point
async def main():
    """Main application entry point"""
    try:
        print("🚀 Starting Code Grapher Application (Phase 5)")
        print("=" * 60)

        # Create and initialize application
        app = await create_application()

        # Display application info
        info = app.get_application_info()
        print(f"\n📊 Application Info:")
        print(f"   Name: {info['name']}")
        print(f"   Version: {info['version']}")
        print(f"   Phase: {info['phase']}")
        print(f"   Services: {len(info['registered_services'])}")
        print(f"   Status: {'✅ Ready' if info['is_initialized'] else '❌ Not Ready'}")

        print("\n🎯 Ready for MCP server or direct usage!")
        print("=" * 60)

        return app

    except Exception as e:
        print(f"❌ Application startup failed: {e}")
        raise


if __name__ == "__main__":
    asyncio.run(main())
