#!/usr/bin/env python3
"""
Simple test script to verify OpenAI integration works with .env file
"""
import os
import sys
from pathlib import Path

# Add current directory to path
sys.path.insert(0, str(Path(__file__).parent))

# Load environment variables
from dotenv import load_dotenv
load_dotenv()

def test_openai_config():
    """Test OpenAI configuration loading"""
    print("Testing OpenAI Configuration Loading")
    print("=" * 50)
    
    # Check environment variables
    openai_key = os.getenv('OPENAI_API_KEY')
    ai_provider = os.getenv('AI_PROVIDER', 'openai')
    
    print(f"AI_PROVIDER: {ai_provider}")
    print(f"OPENAI_API_KEY: {'✓ Set' if openai_key else '✗ Not set'}")
    
    if not openai_key:
        print("Error: OPENAI_API_KEY not found in .env file")
        return False
    
    # Test configuration loading
    try:
        from ai_services.config.ai_config import AIServiceConfig
        config = AIServiceConfig()
        
        print(f"Default provider: {config.default_provider.value}")
        print(f"OpenAI model: {config.openai_model}")
        print(f"OpenAI API key loaded: {'✓ Yes' if config.openai_api_key else '✗ No'}")
        
        if config.openai_api_key:
            print("✓ Configuration loaded successfully from .env file")
            return True
        else:
            print("✗ OpenAI API key not loaded from .env file")
            return False
            
    except Exception as e:
        print(f"✗ Error loading configuration: {e}")
        return False


def test_openai_provider_import():
    """Test OpenAI provider import"""
    print("\nTesting OpenAI Provider Import")
    print("=" * 50)
    
    try:
        from ai_services.providers.openai_provider import OpenAIProvider
        from ai_services.models.provider_models import AIProviderType, ProviderConfig
        
        # Create provider instance
        config = ProviderConfig(
            provider_type=AIProviderType.OPENAI,
            api_key=os.getenv('OPENAI_API_KEY'),
            model_name="gpt-4o-mini"
        )
        
        provider = OpenAIProvider(config)
        print("✓ OpenAI provider imported and created successfully")
        print(f"Provider name: {provider.get_provider_name()}")
        print(f"Model info: {provider.get_model_info()}")
        
        return True
        
    except Exception as e:
        print(f"✗ Error importing OpenAI provider: {e}")
        return False


def main():
    """Run all tests"""
    print("OpenAI Integration Test")
    print("=" * 50)
    
    success = True
    
    # Test configuration
    if not test_openai_config():
        success = False
    
    # Test provider import
    if not test_openai_provider_import():
        success = False
    
    if success:
        print("\n✓ All tests passed! OpenAI integration is ready.")
    else:
        print("\n✗ Some tests failed. Check the errors above.")
    
    return success


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)

