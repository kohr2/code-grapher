import os
import time
import requests
from typing import Optional, Dict, Any, List
from datetime import datetime
from dotenv import load_dotenv

# Load environment variables
load_dotenv()

from logger import logger, CodeGrapherLogger
# Removed ai_evaluation_tracker dependency


class OllamaClient:
    """Client wrapper for Ollama LLM with comprehensive logging and evaluation tracking"""
    
    def __init__(self, base_url: Optional[str] = None, model_name: str = "gemma3:4b"):
        self.model_name = model_name
        self.base_url = base_url or os.getenv("OLLAMA_URL", "http://localhost:11434")
        self.session_logger = logger.create_session_logger("OllamaClient")
        
        # Log initialization start
        self.session_logger.log_operation_start(
            "OllamaClient.init",
            {
                "model_name": model_name,
                "base_url": self.base_url
            }
        )
        
        start_time = time.time()
        
        try:
            # Test the connection with a simple query
            test_response = self._make_request("Hello, respond with 'OK' if you can understand this.")
            
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "OllamaClient.init",
                duration=duration,
                success=True,
                details={
                    "model_initialized": True,
                    "test_response": test_response.get("response", "")[:50] if test_response else "No response",
                    "connection_verified": True
                }
            )
            
            # Log successful initialization
            logger.logger.info(f"Successfully initialized Ollama {model_name} client in {duration:.2f}s")
            
            self.session_logger.log_info(f"Ollama client initialized successfully with model: {model_name}")
            
        except Exception as e:
            duration = time.time() - start_time
            
            self.session_logger.log_operation_end(
                "OllamaClient.init",
                duration=duration,
                success=False,
                details={"error": str(e)}
            )
            
            # Log initialization failure
            logger.logger.error(f"Failed to initialize Ollama client: {str(e)}")
            
            self.session_logger.log_error(e, {"model_name": model_name, "base_url": self.base_url})
            raise
    
    def _make_request(self, prompt: str, max_tokens: int = 1000, temperature: float = 0.3) -> Dict[str, Any]:
        """Make a request to the Ollama API"""
        url = f"{self.base_url}/api/generate"
        
        payload = {
            "model": self.model_name,
            "prompt": prompt,
            "stream": False,
            "options": {
                "temperature": temperature,
                "num_predict": max_tokens
            }
        }
        
        try:
            response = requests.post(url, json=payload, timeout=60)
            response.raise_for_status()
            
            return response.json()
            
        except requests.exceptions.RequestException as e:
            raise Exception(f"Ollama API request failed: {str(e)}")
    
    def generate_response(self, prompt: str, context: Optional[List[Dict[str, Any]]] = None, 
                         max_tokens: int = 1000, temperature: float = 0.3) -> Dict[str, Any]:
        """Generate a response using Ollama"""
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
            
            # Generate response
            response = self._make_request(full_prompt, max_tokens, temperature)
            
            duration = time.time() - start_time
            
            # Extract response text
            response_text = response.get("response", "")
            
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
                # Log generation success
                logger.logger.info(f"Generated {tokens_estimated} tokens in {duration:.2f}s")
            else:
                # Log short response warning
                logger.logger.warning(f"Short response generated ({len(response_text)} chars) - may indicate issues")
            
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
            
            # Log generation failure
            logger.logger.error(f"Failed to generate response: {str(e)}")
            
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
        """Check if the Ollama client is available and configured"""
        try:
            response = requests.get(f"{self.base_url}/api/tags", timeout=5)
            return response.status_code == 200
        except:
            return False
    
    def get_model_info(self) -> Dict[str, Any]:
        """Get information about the current model"""
        return {
            "model_name": self.model_name,
            "base_url": self.base_url,
            "available": self.is_available()
        }