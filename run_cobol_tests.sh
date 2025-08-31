#!/bin/bash

# COBOL Integration Test Runner
# This script runs the COBOL integration tests for Code Grapher

echo "🚀 COBOL Integration Test Runner"
echo "================================"

# Check if we're in the right directory
if [ ! -f "test_cobol_integration.py" ]; then
    echo "❌ Error: test_cobol_integration.py not found"
    echo "   Please run this script from the project root directory"
    exit 1
fi

# Check if COBOL test file exists
if [ ! -f "test_cobol_banking.cbl" ]; then
    echo "❌ Error: test_cobol_banking.cbl not found"
    echo "   Please ensure the COBOL test file is present"
    exit 1
fi

# Check if virtual environment is activated
if [ -z "$VIRTUAL_ENV" ]; then
    echo "⚠️  Warning: Virtual environment not detected"
    echo "   You may want to activate your virtual environment first:"
    echo "   source venv/bin/activate"
    echo ""
fi

# Install dependencies if needed
echo "📦 Checking dependencies..."
if ! python -c "import jpype" 2>/dev/null; then
    echo "   Installing JPype1..."
    pip install jpype1==1.5.1
fi

if ! python -c "import proleap_cobol_parser" 2>/dev/null; then
    echo "   Installing ProLeap COBOL Parser..."
    pip install proleap-cobol-parser==4.0.0
fi

echo "✅ Dependencies checked"

# Run the tests
echo ""
echo "🧪 Running COBOL Integration Tests..."
echo ""

python test_cobol_integration.py

# Capture exit code
EXIT_CODE=$?

echo ""
echo "================================"

if [ $EXIT_CODE -eq 0 ]; then
    echo "🎉 All tests passed! COBOL integration is working correctly."
else
    echo "⚠️  Some tests failed. Check the output above for details."
    echo ""
    echo "💡 Troubleshooting tips:"
    echo "   - Ensure Java 17+ is installed and JAVA_HOME is set"
    echo "   - Check that JPype1 and ProLeap dependencies are installed"
    echo "   - Verify the COBOL test file is present and readable"
fi

exit $EXIT_CODE
