#!/usr/bin/env python3
"""
Test script to verify OpenAI integration works with .env file
"""
import os
import sys
from pathlib import Path

# Add parent directory to path for imports
parent_dir = Path(__file__).parent.parent
sys.path.insert(0, str(parent_dir))

# Load environment variables
from dotenv import load_dotenv
load_dotenv()

# Change to parent directory for relative imports
os.chdir(str(parent_dir))

from ai_services.services.ai_service import AIService
from ai_services.config.ai_config import AIServiceConfig
from ai_services.models.provider_models import AIProviderType


def test_openai_integration():
    """Test OpenAI integration with .env configuration"""
    print("Testing OpenAI Integration with .env file")
    print("=" * 50)
    
    # Check environment variables
    openai_key = os.getenv('OPENAI_API_KEY')
    ai_provider = os.getenv('AI_PROVIDER', 'openai')
    
    print(f"AI_PROVIDER: {ai_provider}")
    print(f"OPENAI_API_KEY: {'✓ Set' if openai_key else '✗ Not set'}")
    
    if not openai_key:
        print("Error: OPENAI_API_KEY not found in .env file")
        return False
    
    # Create configuration (will auto-load from .env)
    config = AIServiceConfig()
    print(f"Default provider: {config.default_provider.value}")
    print(f"OpenAI model: {config.openai_model}")
    
    # Initialize AI service
    try:
        ai_service = AIService(config)
        ai_service.initialize({})
        print("✓ AI service initialized successfully")
        
        # Test health check
        health = ai_service.health_check()
        print(f"✓ Health check: {health['status']}")
        
        if 'openai' in health['providers']:
            openai_health = health['providers']['openai']
            print(f"✓ OpenAI provider available: {openai_health['available']}")
            if openai_health['available']:
                print(f"  Model: {openai_health['model_info']['model']}")
            else:
                print(f"  Error: {openai_health.get('error', 'Unknown error')}")
        else:
            print("✗ OpenAI provider not found in health check")
            return False
        
        # Test simple text generation
        print("\nTesting text generation...")
        try:
            response = ai_service.get_provider().generate_text("Say 'Hello from OpenAI!'")
            if response.success:
                print(f"✓ Text generation successful: {response.content}")
            else:
                print(f"✗ Text generation failed: {response.error}")
                return False
        except Exception as e:
            print(f"✗ Text generation error: {e}")
            return False
        
        print("\n✓ All tests passed! OpenAI integration is working correctly.")
        return True
        
    except Exception as e:
        print(f"✗ Error during testing: {e}")
        return False
    
    finally:
        try:
            ai_service.shutdown()
        except:
            pass


if __name__ == "__main__":
    success = test_openai_integration()
    sys.exit(0 if success else 1)
