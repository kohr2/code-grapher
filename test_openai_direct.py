#!/usr/bin/env python3
"""
Direct test of OpenAI integration without package imports
"""
import os
import sys
from pathlib import Path

# Load environment variables
from dotenv import load_dotenv
load_dotenv()

def test_environment():
    """Test environment variables"""
    print("Testing Environment Variables")
    print("=" * 50)
    
    openai_key = os.getenv('OPENAI_API_KEY')
    ai_provider = os.getenv('AI_PROVIDER', 'openai')
    
    print(f"AI_PROVIDER: {ai_provider}")
    print(f"OPENAI_API_KEY: {'✓ Set' if openai_key else '✗ Not set'}")
    
    if openai_key:
        print(f"Key starts with: {openai_key[:10]}...")
        print("✓ Environment variables loaded correctly")
        return True
    else:
        print("✗ OPENAI_API_KEY not found")
        return False


def test_openai_client():
    """Test OpenAI client directly"""
    print("\nTesting OpenAI Client")
    print("=" * 50)
    
    try:
        from openai import OpenAI
        
        # Initialize client
        client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        
        # Test with a simple request
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[{"role": "user", "content": "Say 'Hello from OpenAI!'"}],
            max_tokens=10
        )
        
        content = response.choices[0].message.content
        print(f"✓ OpenAI client working: {content}")
        return True
        
    except ImportError:
        print("✗ OpenAI client not installed. Run: pip install openai")
        return False
    except Exception as e:
        print(f"✗ OpenAI client error: {e}")
        return False


def test_configuration_loading():
    """Test configuration loading logic"""
    print("\nTesting Configuration Logic")
    print("=" * 50)
    
    # Simulate the configuration loading logic
    openai_key = os.getenv('OPENAI_API_KEY')
    ai_provider = os.getenv('AI_PROVIDER', 'openai' if openai_key else 'ollama')
    
    print(f"Detected provider: {ai_provider}")
    print(f"OpenAI key available: {'Yes' if openai_key else 'No'}")
    
    # Test provider selection logic
    if ai_provider == 'openai' and openai_key:
        print("✓ Configuration would select OpenAI provider")
        return True
    elif ai_provider == 'ollama':
        print("✓ Configuration would select Ollama provider")
        return True
    else:
        print("✗ Configuration issue detected")
        return False


def main():
    """Run all tests"""
    print("OpenAI Integration Direct Test")
    print("=" * 50)
    
    success = True
    
    # Test environment
    if not test_environment():
        success = False
    
    # Test OpenAI client
    if not test_openai_client():
        success = False
    
    # Test configuration
    if not test_configuration_loading():
        success = False
    
    if success:
        print("\n✓ All tests passed! OpenAI integration is ready.")
        print("\nTo use the AI services:")
        print("1. The .env file is properly configured")
        print("2. OpenAI client is working")
        print("3. Configuration logic is correct")
        print("\nYou can now use the AI services with OpenAI provider.")
    else:
        print("\n✗ Some tests failed. Check the errors above.")
    
    return success


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)

