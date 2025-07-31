"""
Main AI service orchestrator that coordinates all AI functionality
"""
import time
from typing import Any, Dict, List, Optional
from ..interfaces.ai_services_interface import AIServicesInterface
from ..interfaces.ai_provider_interface import AIProviderInterface
from .relationship_service import RelationshipService
from .description_service import DescriptionService
from .evaluation_service import EvaluationService
from ..providers.ollama_provider import OllamaProvider
from ..providers.gemini_provider import GeminiProvider
from ..providers.mock_provider import MockProvider
from ..models.provider_models import ProviderConfig, AIProviderType
from ..models.relationship_models import RelationshipExtractionResult
from ..models.evaluation_models import EvaluationCategory
from ..config.ai_config import AIServiceConfig
from shared.interfaces.logger_interface import LoggerInterface
from shared.interfaces.service_interface import ServiceInterface


class AIService(AIServicesInterface, ServiceInterface):
    """Main AI service orchestrator"""
    
    def __init__(self, config: Optional[AIServiceConfig] = None, logger: Optional[LoggerInterface] = None):
        """Initialize AI service"""
        self.config = config or AIServiceConfig()
        self.logger = logger
        self._providers: Dict[AIProviderType, AIProviderInterface] = {}
        self._relationship_service: Optional[RelationshipService] = None
        self._description_service: Optional[DescriptionService] = None
        self._evaluation_service: Optional[EvaluationService] = None
        self._initialized = False
    
    # ServiceInterface implementation
    
    def initialize(self, config: Dict[str, Any]) -> None:
        """Initialize the AI service with configuration"""
        try:
            # Update configuration
            if config:
                self.config = AIServiceConfig.from_dict(config)
            
            # Initialize providers
            self._initialize_providers()
            
            # Initialize sub-services
            self._initialize_services()
            
            self._initialized = True
            
            if self.logger:
                self.logger.log_info(f"AI Service initialized with {len(self._providers)} providers")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize AI service: {e}")
            raise
    
    def shutdown(self) -> None:
        """Clean shutdown of AI service"""
        try:
            # Shutdown all providers
            for provider in self._providers.values():
                try:
                    provider.shutdown()
                except Exception as e:
                    if self.logger:
                        self.logger.log_warning(f"Error shutting down provider: {e}")
            
            self._providers.clear()
            self._initialized = False
            
            if self.logger:
                self.logger.log_info("AI Service shut down successfully")
                
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Error during AI service shutdown: {e}")
    
    def health_check(self) -> Dict[str, Any]:
        """Get health status of AI service"""
        try:
            provider_health = {}
            for provider_type, provider in self._providers.items():
                try:
                    provider_health[provider_type.value] = {
                        "available": provider.is_available(),
                        "model_info": provider.get_model_info()
                    }
                except Exception as e:
                    provider_health[provider_type.value] = {
                        "available": False,
                        "error": str(e)
                    }
            
            return {
                "status": "healthy" if self._initialized else "unhealthy",
                "initialized": self._initialized,
                "default_provider": self.config.default_provider.value,
                "providers": provider_health,
                "services": {
                    "relationship_service": self._relationship_service is not None,
                    "description_service": self._description_service is not None,
                    "evaluation_service": self._evaluation_service is not None
                }
            }
            
        except Exception as e:
            return {
                "status": "unhealthy",
                "error": str(e)
            }
    
    def get_service_name(self) -> str:
        """Return the name of this service"""
        return "AIService"
    
    # AIServicesInterface implementation
    
    def get_provider(self, provider_type: Optional[str] = None) -> AIProviderInterface:
        """Get AI provider instance"""
        if not self._initialized:
            raise RuntimeError("AI service not initialized")
        
        # Determine provider type
        if provider_type:
            try:
                provider_enum = AIProviderType(provider_type.lower())
            except ValueError:
                provider_enum = self.config.default_provider
        else:
            provider_enum = self.config.default_provider
        
        # Get provider
        if provider_enum not in self._providers:
            raise ValueError(f"Provider {provider_enum.value} not available")
        
        return self._providers[provider_enum]
    
    def extract_relationships(self, source_file: str, target_file: str,
                            source_code: str, target_code: str) -> RelationshipExtractionResult:
        """Extract relationships with validation"""
        start_time = time.time()
        
        try:
            if not self._relationship_service:
                raise RuntimeError("Relationship service not initialized")
            
            # Track evaluation
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.RELATIONSHIP_EXTRACTION,
                    {"operation": "start", "files": [source_file, target_file]},
                    {"timestamp": start_time}
                )
            
            result = self._relationship_service.extract_relationships(
                source_file, target_file, source_code, target_code
            )
            
            # Track completion
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.RELATIONSHIP_EXTRACTION,
                    {
                        "success": result.success,
                        "total_found": result.total_found,
                        "total_valid": result.total_valid,
                        "duration": time.time() - start_time
                    },
                    {"files": [source_file, target_file]}
                )
            
            return result
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Relationship extraction failed: {e}")
            
            # Track error
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.RELATIONSHIP_EXTRACTION,
                    {"success": False, "error": str(e), "duration": time.time() - start_time}
                )
            
            return RelationshipExtractionResult(
                relationships=[],
                source_file=source_file,
                target_file=target_file,
                extraction_time=time.time() - start_time,
                total_found=0,
                total_valid=0,
                validation_errors=[str(e)],
                success=False,
                error=str(e)
            )
    
    def generate_description(self, entity: Dict[str, Any], context: Optional[str] = None) -> str:
        """Generate AI-powered description for entity"""
        start_time = time.time()
        
        try:
            if not self._description_service:
                raise RuntimeError("Description service not initialized")
            
            description = self._description_service.generate_entity_description(entity, context)
            
            # Track evaluation
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.DESCRIPTION_GENERATION,
                    {
                        "success": True,
                        "entity_type": entity.get('type', 'unknown'),
                        "description_length": len(description),
                        "duration": time.time() - start_time
                    }
                )
            
            return description
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Description generation failed: {e}")
            
            # Track error
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.DESCRIPTION_GENERATION,
                    {"success": False, "error": str(e), "duration": time.time() - start_time}
                )
            
            return f"Error generating description: {e}"
    
    def generate_descriptions_batch(self, entities: List[Dict[str, Any]], 
                                  context: Optional[str] = None) -> Dict[str, str]:
        """Generate descriptions for multiple entities efficiently"""
        descriptions = {}
        
        for entity in entities:
            entity_id = entity.get('id', entity.get('name', f"entity_{len(descriptions)}"))
            descriptions[entity_id] = self.generate_description(entity, context)
        
        return descriptions
    
    def track_evaluation(self, category: str, result: Any, metadata: Optional[Dict[str, Any]] = None) -> None:
        """Track AI evaluation result"""
        try:
            if not self._evaluation_service:
                if self.logger:
                    self.logger.log_warning("Evaluation service not available for tracking")
                return
            
            # Map string category to enum
            try:
                eval_category = EvaluationCategory(category)
            except ValueError:
                if self.logger:
                    self.logger.log_warning(f"Unknown evaluation category: {category}")
                return
            
            self._evaluation_service.track_evaluation(eval_category, result, metadata)
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to track evaluation: {e}")
    
    def get_evaluation_summary(self) -> Dict[str, Any]:
        """Get summary of AI evaluation metrics"""
        try:
            if not self._evaluation_service:
                return {"error": "Evaluation service not available"}
            
            return self._evaluation_service.get_evaluation_summary()
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to get evaluation summary: {e}")
            return {"error": str(e)}
    
    def semantic_search(self, query: str, limit: int = 5) -> List[Dict[str, Any]]:
        """Perform semantic search using AI embeddings"""
        try:
            # This would integrate with vector database/embeddings
            # For now, return placeholder implementation
            if self.logger:
                self.logger.log_info(f"Semantic search query: {query}")
            
            # Track evaluation
            if self._evaluation_service:
                self._evaluation_service.track_evaluation(
                    EvaluationCategory.SEMANTIC_SEARCH,
                    {"query": query, "limit": limit, "implementation": "placeholder"}
                )
            
            return [
                {
                    "id": f"placeholder_{i}",
                    "content": f"Placeholder result {i} for query: {query}",
                    "score": 0.8 - (i * 0.1)
                }
                for i in range(min(limit, 3))
            ]
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Semantic search failed: {e}")
            return []
    
    # Private methods
    
    def _initialize_providers(self) -> None:
        """Initialize AI providers based on configuration"""
        # Initialize Ollama provider
        try:
            ollama_config = ProviderConfig(
                provider_type=AIProviderType.OLLAMA,
                base_url=self.config.ollama_url,
                model_name=self.config.ollama_model,
                timeout=self.config.timeout_seconds,
                max_retries=self.config.max_retries
            )
            ollama_provider = OllamaProvider(ollama_config, self.logger)
            ollama_provider.initialize({})
            self._providers[AIProviderType.OLLAMA] = ollama_provider
            
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Failed to initialize Ollama provider: {e}")
        
        # Initialize Gemini provider if API key available
        if self.config.gemini_api_key:
            try:
                gemini_config = ProviderConfig(
                    provider_type=AIProviderType.GEMINI,
                    api_key=self.config.gemini_api_key,
                    model_name=self.config.gemini_model,
                    timeout=self.config.timeout_seconds,
                    max_retries=self.config.max_retries
                )
                gemini_provider = GeminiProvider(gemini_config, self.logger)
                gemini_provider.initialize({})
                self._providers[AIProviderType.GEMINI] = gemini_provider
                
            except Exception as e:
                if self.logger:
                    self.logger.log_warning(f"Failed to initialize Gemini provider: {e}")
        
        # Always initialize mock provider for testing
        try:
            mock_provider = MockProvider(logger=self.logger)
            mock_provider.initialize({})
            self._providers[AIProviderType.MOCK] = mock_provider
        except Exception as e:
            if self.logger:
                self.logger.log_warning(f"Failed to initialize Mock provider: {e}")
        
        # Ensure we have at least one provider
        if not self._providers:
            raise RuntimeError("No AI providers could be initialized")
    
    def _initialize_services(self) -> None:
        """Initialize sub-services"""
        try:
            # Get default provider directly without validation check
            provider_enum = self.config.default_provider
            
            # Fall back to any available provider if default not available
            if provider_enum not in self._providers and self._providers:
                provider_enum = next(iter(self._providers.keys()))
            
            if provider_enum not in self._providers:
                raise RuntimeError("No providers available for sub-services")
                
            default_provider = self._providers[provider_enum]
            
            # Initialize relationship service
            self._relationship_service = RelationshipService(default_provider, self.logger)
            
            # Initialize description service
            self._description_service = DescriptionService(default_provider, self.logger)
            
            # Initialize evaluation service
            if self.config.enable_evaluation_tracking:
                self._evaluation_service = EvaluationService(self.logger)
            
        except Exception as e:
            if self.logger:
                self.logger.log_error(f"Failed to initialize sub-services: {e}")
            raise