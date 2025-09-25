# OpenAI Integration

This document describes the OpenAI integration in the AI Services vertical slice.

## Overview

The OpenAI provider allows the AI Services to use OpenAI's GPT models for:
- Text generation
- Entity description generation
- Relationship extraction
- Code analysis

## Features

- **Multiple Model Support**: Supports GPT-4o-mini, GPT-4, and other OpenAI models
- **Custom Endpoints**: Supports custom OpenAI-compatible endpoints
- **Error Handling**: Comprehensive error handling and fallback mechanisms
- **Context Support**: Rich context passing for better AI responses
- **JSON Parsing**: Robust JSON response parsing for structured outputs

## Configuration

### Environment Variables

Set your OpenAI API key as an environment variable:

```bash
export OPENAI_API_KEY="your-api-key-here"
```

### Configuration Options

```python
from ai_services.config.ai_config import AIServiceConfig
from ai_services.models.provider_models import AIProviderType

config = AIServiceConfig(
    default_provider=AIProviderType.OPENAI,
    openai_api_key="your-api-key",  # or use OPENAI_API_KEY env var
    openai_model="gpt-4o-mini",     # Default model
    openai_base_url=None,           # Optional: custom endpoint
    timeout_seconds=120,
    max_retries=3
)
```

### Configuration Fields

| Field | Type | Default | Description |
|-------|------|---------|-------------|
| `openai_api_key` | str | None | OpenAI API key |
| `openai_model` | str | "gpt-4o-mini" | OpenAI model to use |
| `openai_base_url` | str | None | Custom OpenAI-compatible endpoint |

## Usage

### Basic Usage

```python
from ai_services.services.ai_service import AIService
from ai_services.config.ai_config import AIServiceConfig
from ai_services.models.provider_models import AIProviderType

# Configure for OpenAI
config = AIServiceConfig(
    default_provider=AIProviderType.OPENAI,
    openai_api_key="your-api-key"
)

# Initialize service
ai_service = AIService(config)
ai_service.initialize({})

# Use the service
provider = ai_service.get_provider("openai")
response = provider.generate_text("Hello, world!")
print(response.content)
```

### Text Generation

```python
# Simple text generation
response = ai_service.get_provider().generate_text(
    "Explain the concept of dependency injection in software architecture."
)

# With context
context = {"domain": "banking", "language": "Python"}
response = ai_service.get_provider().generate_text(
    "Write a function to calculate compound interest",
    context
)
```

### Entity Description Generation

```python
entity = {
    "type": "function",
    "name": "calculate_interest",
    "code_snippet": "def calculate_interest(principal, rate, time): ...",
    "properties": {"parameters": ["principal", "rate", "time"]}
}

description = ai_service.generate_description(
    entity, 
    "Banking system for interest calculations"
)
```

### Relationship Extraction

```python
source_code = """
class Calculator:
    def add(self, a, b):
        return self.validate_input(a) + self.validate_input(b)
"""

target_code = """
def validate_input(value):
    if not isinstance(value, (int, float)):
        raise ValueError("Invalid input")
    return value
"""

result = ai_service.extract_relationships(
    "calculator.py", "utils.py", source_code, target_code
)

for relationship in result.relationships:
    print(f"{relationship.source_entity} {relationship.relationship_type.value} {relationship.target_entity}")
```

## Model Information

### Supported Models

- **gpt-4o-mini**: Fast, cost-effective model (default)
- **gpt-4o**: More capable but slower
- **gpt-4**: High-quality responses
- **gpt-3.5-turbo**: Legacy model

### Model Selection

Choose based on your needs:

- **Development/Testing**: `gpt-4o-mini` (fast, cheap)
- **Production**: `gpt-4o` (balanced)
- **High Quality**: `gpt-4` (best quality, slower)

## Error Handling

The OpenAI provider includes comprehensive error handling:

### Common Errors

1. **API Key Missing**: `RuntimeError: OpenAI client not installed`
2. **Invalid API Key**: Connection test fails
3. **Rate Limiting**: Automatic retry with exponential backoff
4. **Model Unavailable**: Graceful fallback to available models

### Error Recovery

```python
try:
    response = ai_service.get_provider().generate_text("Hello")
    if not response.success:
        print(f"Error: {response.error}")
except Exception as e:
    print(f"Service error: {e}")
```

## Testing

Run the OpenAI provider tests:

```bash
cd code-grapher/ai-services
python -m pytest tests/test_openai_provider.py -v
```

Run all AI service tests:

```bash
python -m pytest tests/ -v
```

## Example Script

See `examples/openai_example.py` for a complete working example:

```bash
export OPENAI_API_KEY="your-api-key"
python examples/openai_example.py
```

## Performance Considerations

### Token Usage

- Monitor token usage through response metadata
- Use appropriate model for task complexity
- Consider caching for repeated requests

### Rate Limits

- OpenAI has rate limits based on your plan
- The provider includes retry logic
- Consider implementing request queuing for high-volume usage

### Cost Optimization

- Use `gpt-4o-mini` for simple tasks
- Implement caching for repeated queries
- Optimize prompts to reduce token usage

## Troubleshooting

### Common Issues

1. **Import Error**: Install OpenAI client
   ```bash
   pip install openai>=1.0.0
   ```

2. **API Key Issues**: Verify key is set correctly
   ```bash
   echo $OPENAI_API_KEY
   ```

3. **Connection Issues**: Check network and API status

4. **Model Issues**: Verify model name and availability

### Debug Mode

Enable debug logging to see detailed information:

```python
import logging
logging.basicConfig(level=logging.DEBUG)
```

## Integration with Existing Services

The OpenAI provider integrates seamlessly with existing AI services:

- **Relationship Service**: Uses OpenAI for relationship extraction
- **Description Service**: Uses OpenAI for entity descriptions
- **Evaluation Service**: Tracks OpenAI usage and performance

## Security Considerations

- Store API keys securely (environment variables)
- Use least-privilege API keys
- Monitor usage and costs
- Consider data privacy requirements

## Future Enhancements

- Streaming responses for long text generation
- Function calling support
- Fine-tuned model support
- Multi-modal capabilities (images, etc.)
