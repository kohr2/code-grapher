import os
import time
from typing import Optional, Dict, Any, List
from datetime import datetime

try:
    import google.generativeai as genai
    GEMINI_AVAILABLE = True
except ImportError:
    GEMINI_AVAILABLE = False

from logger import logger, CodeGrapherLogger
from ai_evaluation_tracker import ai_tracker, EvaluationCategory, Sentiment


class GeminiClient:
    """Client wrapper for Google Gemini Flash LLM with comprehensive logging and evaluation tracking"""
    
    def __init__(self, api_key: Optional[str] = None, model_name: str = "gemini-1.5-flash"):
        self.model_name = model_name
        self.session_logger = logger.create_session_logger("GeminiClient")
        
        # Log initialization start
        self.session_logger.log_operation_start(
            "GeminiClient.init",
            {
                "model_name": model_name,
                "api_key_provided": api_key is not None or os.getenv("GOOGLE_API_KEY") is not None
            }
        )
        
        start_time = time.time()
        
        try:
            if not GEMINI_AVAILABLE:
                raise ImportError("google-generativeai package not available. Install with: pip install google-generativeai")
            
            # Get API key from parameter or environment
            self.api_key = api_key or os.getenv("GOOGLE_API_KEY")
            if not self.api_key:
                raise ValueError("No API key provided. Set GOOGLE_API_KEY environment variable or pass api_key parameter")
            
            # Configure the client
            genai.configure(api_key=self.api_key)
            
            # Initialize the model
            self.model = genai.GenerativeModel(model_name)
            
            # Test the connection with a simple query
            test_response = self.model.generate_content("Hello, respond with 'OK' if you can understand this.")
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "GeminiClient.init",
                duration=duration,
                success=True,
                details={
                    "model_initialized": True,
                    "test_response": test_response.text[:50] if test_response.text else "No response",
                    "connection_verified": True
                }
            )
            
            # Track successful initialization
            ai_tracker.record_success(
                component="gemini_client_init",
                description=f"Successfully initialized Gemini {model_name} client",
                time_saved=duration
            )
            
            self.session_logger.log_info(f"Gemini client initialized successfully with model: {model_name}")
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "GeminiClient.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Track initialization failure
            ai_tracker.record_failure(
                component="gemini_client_init",
                description=f"Failed to initialize Gemini client: {str(e)}",
                error_type=type(e).__name__,
                workaround="Check API key and internet connection"
            )
            
            self.session_logger.log_error(e, {"model_name": model_name})
            raise
    
    def generate_response(self, prompt: str, context: Optional[List[Dict[str, Any]]] = None, 
                         max_tokens: int = 1000, temperature: float = 0.3) -> Dict[str, Any]:
        """Generate a response using Gemini Flash"""
        self.session_logger.log_operation_start(
            "generate_response",
            {
                "prompt_length": len(prompt),
                "context_items": len(context) if context else 0,
                "max_tokens": max_tokens,
                "temperature": temperature
            }
        )
        
        start_time = time.time()
        
        try:
            # Build the full prompt with context
            full_prompt = self._build_contextual_prompt(prompt, context)
            
            self.session_logger.log_decision(
                decision=f"Generated contextual prompt with {len(full_prompt)} characters",
                reasoning="Including retrieved context to improve response relevance",
                alternatives=["Use prompt only", "Pre-process context differently"]
            )
            
            # Configure generation parameters
            generation_config = genai.types.GenerationConfig(
                max_output_tokens=max_tokens,
                temperature=temperature,
            )
            
            # Generate response
            response = self.model.generate_content(
                full_prompt,
                generation_config=generation_config
            )
            
            duration = time.time() - start_time
            
            # Extract response text
            response_text = response.text if response.text else ""
            
            # Calculate metrics
            tokens_estimated = len(response_text.split())  # Rough token estimate
            tokens_per_second = tokens_estimated / duration if duration > 0 else 0
            
            result = {
                "response": response_text,
                "prompt_tokens": len(full_prompt.split()),  # Estimate
                "completion_tokens": tokens_estimated,
                "total_tokens": len(full_prompt.split()) + tokens_estimated,
                "model": self.model_name,
                "processing_time": duration,
                "tokens_per_second": tokens_per_second,
                "context_used": context is not None and len(context) > 0
            }
            
            # Log RAG operation if context was used
            if context:
                self.session_logger.log_rag_operation(
                    operation="llm_generation",
                    query=prompt,
                    results_count=1,
                    relevance_scores=[0.9]  # Assume high relevance for generated response
                )
            
            self.session_logger.log_operation_end(
                "generate_response",
                duration=duration,
                success=True,
                details={
                    "response_length": len(response_text),
                    "estimated_tokens": tokens_estimated,
                    "tokens_per_second": tokens_per_second
                }
            )
            
            # Log performance metrics
            self.session_logger.log_performance(
                metric="llm_generation_time",
                value=duration * 1000,
                unit="ms",
                context={
                    "model": self.model_name,
                    "prompt_length": len(prompt),
                    "response_length": len(response_text),
                    "context_items": len(context) if context else 0
                }
            )
            
            self.session_logger.log_performance(
                metric="llm_tokens_per_second",
                value=tokens_per_second,
                unit="tokens/sec",
                context={"model": self.model_name}
            )
            
            # Evaluate response quality
            if len(response_text) > 50:  # Reasonable response length
                ai_tracker.record_success(
                    component="llm_generation",
                    description=f"Generated {tokens_estimated} tokens in {duration:.2f}s",
                    time_saved=duration,
                    accuracy=85.0  # Placeholder - would implement actual quality metrics
                )
            else:
                ai_tracker.record_observation(
                    component="llm_generation",
                    observation=f"Short response generated ({len(response_text)} chars) - may indicate issues",
                    category=EvaluationCategory.ACCURACY
                )
            
            return result
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "generate_response",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            self.session_logger.log_error(e, {
                "prompt_length": len(prompt),
                "context_items": len(context) if context else 0
            })
            
            # Track generation failure
            ai_tracker.record_failure(
                component="llm_generation",
                description=f"Failed to generate response: {str(e)}",
                error_type=type(e).__name__,
                workaround="Check API quota and prompt format"
            )
            
            # Return error response
            return {
                "response": "",
                "error": str(e),
                "prompt_tokens": 0,
                "completion_tokens": 0,
                "total_tokens": 0,
                "model": self.model_name,
                "processing_time": duration,
                "tokens_per_second": 0,
                "context_used": context is not None and len(context) > 0
            }
    
    def _build_contextual_prompt(self, prompt: str, context: Optional[List[Dict[str, Any]]] = None) -> str:
        """Build a prompt with relevant context from RAG retrieval"""
        if not context:
            return prompt
        
        # Build context section
        context_parts = []
        context_parts.append("=== RELEVANT CODE CONTEXT ===")
        
        for i, item in enumerate(context[:5]):  # Limit to top 5 context items
            content = item.get("content", "")
            metadata = item.get("metadata", {})
            relevance = item.get("relevance_score", 0.0)
            
            context_parts.append(f"\n--- Context {i+1} (relevance: {relevance:.2f}) ---")
            
            # Add metadata if available
            if metadata:
                file_path = metadata.get("file_path", "unknown")
                line_number = metadata.get("line_number", "unknown")
                context_parts.append(f"Source: {file_path}:{line_number}")
            
            context_parts.append(content)
        
        context_parts.append("\n=== END CONTEXT ===\n")
        
        # Combine context with prompt
        full_prompt = "\n".join(context_parts) + "\n" + prompt
        
        # Log context usage
        self.session_logger.log_decision(
            decision=f"Built contextual prompt with {len(context)} context items",
            reasoning="RAG context should improve response accuracy and relevance",
            alternatives=["Use prompt without context", "Summarize context first"]
        )
        
        return full_prompt
    
    def generate_code_response(self, question: str, retrieved_context: List[Dict[str, Any]]) -> Dict[str, Any]:
        """Generate a response specifically for code-related questions"""
        # Create a specialized prompt for code queries
        code_prompt = f"""
You are a helpful assistant that analyzes code and answers questions based on the provided context.

Question: {question}

Please provide a clear, accurate answer based on the context above. If you reference specific code, 
quote it directly. If the context doesn't contain enough information to answer the question, 
say so explicitly.

Focus on:
- Accuracy and precision
- Code examples when relevant  
- Clear explanations of code behavior
- Relationships between code components

Answer:"""
        
        return self.generate_response(code_prompt, retrieved_context, max_tokens=1500, temperature=0.2)
    
    def is_available(self) -> bool:
        """Check if the Gemini client is available and configured"""
        return GEMINI_AVAILABLE and hasattr(self, 'model') and self.model is not None
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the current model"""
        return {
            "model_name": self.model_name,
            "available": self.is_available(),
            "api_key_configured": self.api_key is not None
        }