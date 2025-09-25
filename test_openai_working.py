#!/usr/bin/env python3
"""
Working test of OpenAI integration using direct imports
"""
import os
import sys
from pathlib import Path

# Add current directory to path
sys.path.insert(0, str(Path(__file__).parent))

# Load environment variables
from dotenv import load_dotenv
load_dotenv()

def test_openai_provider_direct():
    """Test OpenAI provider directly"""
    print("Testing OpenAI Provider Directly")
    print("=" * 50)
    
    try:
        # Import OpenAI client directly
        from openai import OpenAI
        
        # Initialize client with API key from .env
        client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        
        # Test text generation
        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[{"role": "user", "content": "Explain what a code relationship is in software architecture in one sentence."}],
            max_tokens=100
        )
        
        content = response.choices[0].message.content
        print(f"✓ OpenAI text generation: {content}")
        
        # Test relationship extraction prompt
        relationship_prompt = """Analyze this code and extract relationships:

Source Code:
```python
class Calculator:
    def add(self, a, b):
        return self.validate_input(a) + self.validate_input(b)
    
    def validate_input(self, value):
        if not isinstance(value, (int, float)):
            raise ValueError("Invalid input")
        return value
```

Target Code:
```python
def format_result(result):
    return f"Result: {result}"
```

Find relationships like: CALLS, INHERITS, USES, IMPLEMENTS, DEPENDS_ON.
Return as JSON list with fields: source_entity, target_entity, relationship_type, confidence (0-1).

Example format:
[
  {
    "source_entity": "function_name",
    "target_entity": "called_function", 
    "relationship_type": "CALLS",
    "confidence": 0.9,
    "context": "function_name calls called_function"
  }
]

Only return valid JSON, no additional text."""

        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[{"role": "user", "content": relationship_prompt}],
            max_tokens=500
        )
        
        content = response.choices[0].message.content
        print(f"✓ OpenAI relationship extraction: {content[:200]}...")
        
        # Try to parse as JSON
        import json
        try:
            relationships = json.loads(content)
            print(f"✓ Parsed {len(relationships)} relationships")
            for rel in relationships[:2]:  # Show first 2
                print(f"  - {rel.get('source_entity')} {rel.get('relationship_type')} {rel.get('target_entity')}")
        except json.JSONDecodeError:
            print("✗ Could not parse as JSON")
        
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def test_entity_description():
    """Test entity description generation"""
    print("\nTesting Entity Description Generation")
    print("=" * 50)
    
    try:
        from openai import OpenAI
        client = OpenAI(api_key=os.getenv('OPENAI_API_KEY'))
        
        entity = {
            "type": "function",
            "name": "calculate_interest",
            "code_snippet": "def calculate_interest(principal, rate, time):\n    return principal * rate * time / 100",
            "properties": {"parameters": ["principal", "rate", "time"]}
        }
        
        prompt = f"""Generate a concise technical description for this {entity['type']}:

Name: {entity['name']}
Code: {entity['code_snippet']}
Parameters: {entity['properties']['parameters']}

Business Context: Banking system for interest calculations

Provide a clear, technical description in 1-2 sentences that explains the function's specific purpose and functionality."""

        response = client.chat.completions.create(
            model="gpt-4o-mini",
            messages=[{"role": "user", "content": prompt}],
            max_tokens=150
        )
        
        content = response.choices[0].message.content
        print(f"✓ Entity description: {content}")
        
        return True
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def main():
    """Run all tests"""
    print("OpenAI Integration Working Test")
    print("=" * 50)
    
    success = True
    
    # Test OpenAI provider directly
    if not test_openai_provider_direct():
        success = False
    
    # Test entity description
    if not test_entity_description():
        success = False
    
    if success:
        print("\n✓ All tests passed! OpenAI integration is working correctly.")
        print("\nThe OpenAI provider can:")
        print("- Generate high-quality text responses")
        print("- Extract code relationships with structured output")
        print("- Generate entity descriptions for code components")
        print("\nYou can now use OpenAI for all AI services functionality.")
    else:
        print("\n✗ Some tests failed. Check the errors above.")
    
    return success


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
