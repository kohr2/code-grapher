#!/usr/bin/env python3
"""
Example script demonstrating OpenAI provider usage
"""
import os
import sys
from pathlib import Path

# Add ai-services to path
sys.path.insert(0, str(Path(__file__).parent.parent))

from services.ai_service import AIService
from config.ai_config import AIServiceConfig
from models.provider_models import AIProviderType


def main():
    """Demonstrate OpenAI provider usage"""
    print("OpenAI Provider Example")
    print("=" * 50)
    
    # Load environment variables from .env file
    from dotenv import load_dotenv
    load_dotenv()
    
    # Check if OpenAI API key is available
    api_key = os.getenv('OPENAI_API_KEY')
    if not api_key:
        print("Error: OPENAI_API_KEY not found in environment")
        print("Please add your OpenAI API key to the .env file:")
        print("OPENAI_API_KEY='your-api-key-here'")
        return
    
    # Configure AI service - will automatically load from .env
    config = AIServiceConfig(
        openai_model="gpt-4o-mini",
        enable_evaluation_tracking=True
    )
    
    print(f"Using provider: {config.default_provider.value}")
    print(f"OpenAI API key: {'✓ Set' if config.openai_api_key else '✗ Not set'}")
    
    # Initialize AI service
    ai_service = AIService(config)
    
    try:
        print("Initializing AI service...")
        ai_service.initialize({})
        print("✓ AI service initialized successfully")
        
        # Test health check
        health = ai_service.health_check()
        print(f"✓ Health check: {health['status']}")
        print(f"  Providers available: {list(health['providers'].keys())}")
        
        # Test text generation
        print("\nTesting text generation...")
        response = ai_service.get_provider().generate_text(
            "Explain what a code relationship is in software architecture."
        )
        
        if response.success:
            print("✓ Text generation successful")
            print(f"Response: {response.content[:200]}...")
        else:
            print(f"✗ Text generation failed: {response.error}")
        
        # Test entity description generation
        print("\nTesting entity description generation...")
        entity = {
            "type": "function",
            "name": "calculate_interest",
            "code_snippet": "def calculate_interest(principal, rate, time):\n    return principal * rate * time / 100",
            "properties": {
                "parameters": ["principal", "rate", "time"],
                "return_type": "float"
            }
        }
        
        description = ai_service.generate_description(entity, "Banking system for interest calculations")
        print(f"✓ Entity description: {description}")
        
        # Test relationship extraction
        print("\nTesting relationship extraction...")
        source_code = """
class Calculator:
    def add(self, a, b):
        return self.validate_input(a) + self.validate_input(b)
    
    def validate_input(self, value):
        if not isinstance(value, (int, float)):
            raise ValueError("Invalid input")
        return value
"""
        
        target_code = """
def format_result(result):
    return f"Result: {result}"

class Logger:
    def log(self, message):
        print(f"Log: {message}")
"""
        
        result = ai_service.extract_relationships(
            "calculator.py", "utils.py", source_code, target_code
        )
        
        if result.success:
            print(f"✓ Relationship extraction successful")
            print(f"  Found {result.total_found} relationships, {result.total_valid} valid")
            for rel in result.relationships[:3]:  # Show first 3
                print(f"  - {rel.source_entity} {rel.relationship_type.value} {rel.target_entity} (confidence: {rel.confidence:.2f})")
        else:
            print(f"✗ Relationship extraction failed: {result.error}")
        
        # Test evaluation summary
        print("\nTesting evaluation tracking...")
        summary = ai_service.get_evaluation_summary()
        print(f"✓ Evaluation summary: {summary}")
        
    except Exception as e:
        print(f"✗ Error: {e}")
    
    finally:
        # Cleanup
        print("\nShutting down AI service...")
        ai_service.shutdown()
        print("✓ AI service shut down")


if __name__ == "__main__":
    main()
