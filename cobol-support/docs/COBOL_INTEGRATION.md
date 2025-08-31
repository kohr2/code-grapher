# COBOL Integration with Code Grapher

This document describes the integration of COBOL parsing capabilities into Code Grapher using the ProLeap ANTLR4-based COBOL parser.

## 🎯 Overview

Code Grapher now supports COBOL code analysis, enabling you to:

- **Parse COBOL files** (.cbl, .cob, .cobol extensions)
- **Extract COBOL entities** (programs, data items, paragraphs, sections)
- **Discover COBOL relationships** (CALL, PERFORM, COPY statements)
- **Integrate COBOL analysis** into your existing knowledge graph pipeline

## 🏗️ Architecture

### Components

1. **COBOL Parser Service** (`shared/services/cobol_parser.py`)
   - Wraps ProLeap ANTLR4-based COBOL parser
   - Extracts entities and relationships from COBOL AST
   - Handles COBOL-specific concepts (divisions, paragraphs, data descriptions)

2. **Multi-Language Parser Integration** (`shared/services/multi_language_parser.py`)
   - Automatically detects COBOL files by extension
   - Routes COBOL files to the appropriate parser
   - Maintains consistency with other language parsers

3. **COBOL Relationship Extractor** (`ast_relationship_extractor.py`)
   - Extracts COBOL-specific relationships
   - Identifies CALL, PERFORM, and COPY statements
   - Maps data flow between COBOL entities

## 🚀 Quick Start

### 1. Install Dependencies

```bash
# Install COBOL support dependencies
pip install jpype1==1.5.1 proleap-cobol-parser==4.0.0

# Or update requirements.txt and install all
pip install -r requirements.txt
```

### 2. Verify Java Installation

The ProLeap parser requires Java 17+:

```bash
java -version
# Should show Java 17 or higher
```

### 3. Test COBOL Integration

```bash
# Run the comprehensive test suite
./run_cobol_tests.sh

# Or run tests directly
python test_cobol_integration.py
```

## 📁 File Structure

```
code-grapher/
├── shared/services/
│   ├── cobol_parser.py          # COBOL parser service
│   └── multi_language_parser.py # Updated with COBOL support
├── ast_relationship_extractor.py # Updated with COBOL relationships
├── test_cobol_banking.cbl       # Sample COBOL banking program
├── test_cobol_integration.py    # Comprehensive test suite
├── run_cobol_tests.sh           # Test runner script
└── COBOL_INTEGRATION.md         # This documentation
```

## 🔍 COBOL Entity Types

The parser extracts the following COBOL entities:

### Core Entities
- **`cobol_program`** - Main program definition
- **`cobol_data_item`** - Working storage, local storage, linkage items
- **`cobol_paragraph`** - Procedure division paragraphs
- **`cobol_section`** - Procedure division sections
- **`cobol_procedure`** - General procedure entities

### Data Items
- **Working Storage** - Program variables and constants
- **Local Storage** - Local variables within procedures
- **Linkage Section** - Parameters passed to/from programs

### Procedure Elements
- **Paragraphs** - Named code blocks (e.g., `1000-INITIALIZE`)
- **Sections** - Grouped paragraphs (e.g., `MAIN-PROCESSING-SECTION`)
- **Statements** - Individual COBOL statements within paragraphs

## 🔗 COBOL Relationship Types

### Program Relationships
- **`CALLS`** - CALL statements to other programs
- **`PERFORM`** - PERFORM statements to paragraphs/sections
- **`IMPORTS`** - COPY statements for copybooks

### Data Relationships
- **`USES`** - Data item usage in procedures
- **`DEFINES`** - Data item definitions
- **`DATA_FLOW`** - Data movement between entities

### Example Relationships

```cobol
0000-MAIN-LOGIC.
    PERFORM 1000-INITIALIZE        # → CALLS relationship
    PERFORM 2000-VALIDATE-INPUT    # → CALLS relationship

3200-PROCESS-WITHDRAWAL.
    PERFORM 3210-CHECK-SUFFICIENT-FUNDS  # → CALLS relationship
    SUBTRACT WS-TRANS-AMOUNT FROM WS-ACCOUNT-BALANCE  # → USES relationship
```

## 🧪 Testing

### Test Files

1. **`test_cobol_banking.cbl`** - Realistic banking system COBOL program
   - Demonstrates common banking operations
   - Includes all major COBOL divisions
   - Shows typical COBOL patterns and relationships

2. **`test_cobol_integration.py`** - Comprehensive test suite
   - Tests parser import and initialization
   - Verifies file parsing and entity extraction
   - Tests relationship extraction
   - Validates pipeline integration

### Running Tests

```bash
# Run all tests with automatic dependency checking
./run_cobol_tests.sh

# Run specific test functions
python -c "
from test_cobol_integration import test_cobol_file_parsing
test_cobol_file_parsing()
"
```

## 🔧 Usage Examples

### Basic COBOL Parsing

```python
from shared.services.cobol_parser import COBOLParser

# Initialize parser
parser = COBOLParser()

# Parse COBOL file
result = parser.parse_file("banking_system.cbl")

# Access parsed data
if result["parse_success"]:
    entities = result["entities"]
    compilation_units = result["compilation_units"]
    print(f"Found {len(entities)} entities")
```

### Multi-Language Parser Integration

```python
from shared.services.multi_language_parser import MultiLanguageParser

# Initialize multi-language parser
parser = MultiLanguageParser()

# Parse any supported language (including COBOL)
result = parser.parse_file("program.cbl")  # Automatically detected as COBOL

# Check language detection
language = parser.get_language_from_extension("file.cob")
print(f"Detected language: {language}")  # Output: cobol
```

### Relationship Extraction

```python
from ast_relationship_extractor import extract_ast_relationships

# Extract relationships from parsed files
relationships = extract_ast_relationships([parsed_cobol_file])

# Filter COBOL relationships
cobol_relationships = [r for r in relationships if r.source_file.endswith('.cbl')]

# Find specific relationship types
calls = [r for r in cobol_relationships if r.relationship_type.value == "CALLS"]
performs = [r for r in cobol_relationships if "PERFORM" in r.context]
```

## 🚨 Troubleshooting

### Common Issues

#### 1. JPype Import Errors
```
ImportError: No module named 'jpype'
```
**Solution**: Install JPype1
```bash
pip install jpype1==1.5.1
```

#### 2. Java Not Found
```
RuntimeError: No JVM shared library file (jvm.dll) found
```
**Solution**: Install Java 17+ and set JAVA_HOME
```bash
# macOS
brew install openjdk@17
export JAVA_HOME=/opt/homebrew/opt/openjdk@17

# Linux
sudo apt install openjdk-17-jdk
export JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
```

#### 3. ProLeap Parser Not Available
```
Warning: COBOL parser not available - install jpype1
```
**Solution**: Check Java installation and JPype setup
```bash
java -version
python -c "import jpype; print('JPype available')"
```

#### 4. COBOL Files Not Detected
```
Language not detected as COBOL
```
**Solution**: Verify file extensions and parser registration
```python
from shared.services.multi_language_parser import MultiLanguageParser
parser = MultiLanguageParser()
print(parser.languages)  # Should include 'cobol'
```

### Debug Mode

Enable verbose logging for troubleshooting:

```python
import logging
logging.basicConfig(level=logging.DEBUG)

# Test parser availability
from shared.services.cobol_parser import COBOLParser
parser = COBOLParser()
print(f"Parser available: {parser.is_available()}")
```

## 📊 Performance Considerations

### Parser Performance
- **Initialization**: ~2-3 seconds (JVM startup)
- **File Parsing**: ~100-500ms per file (depending on size)
- **Memory Usage**: ~50-100MB additional (JVM overhead)

### Optimization Tips
1. **Reuse Parser Instances** - Don't create new parsers for each file
2. **Batch Processing** - Process multiple COBOL files together
3. **JVM Tuning** - Adjust JVM memory settings if needed

## 🔮 Future Enhancements

### Planned Features
- **COBOL Copybook Analysis** - Parse and analyze COPY statements
- **SQL Integration** - Extract EXEC SQL statements
- **CICS Integration** - Parse EXEC CICS commands
- **Performance Profiling** - COBOL-specific performance metrics

### Extension Points
- **Custom COBOL Dialects** - Support for vendor-specific extensions
- **Relationship Patterns** - Configurable relationship extraction rules
- **AST Visualization** - COBOL-specific AST rendering

## 📚 References

- [ProLeap COBOL Parser](https://github.com/uwol/proleap-cobol-parser) - Main parser library
- [ANTLR4](https://www.antlr.org/) - Parser generator used by ProLeap
- [JPype](https://jpype.readthedocs.io/) - Python-Java bridge
- [COBOL Language Reference](https://www.ibm.com/docs/en/cobol-zos) - IBM COBOL documentation

## 🤝 Contributing

To contribute to COBOL integration:

1. **Test with Real COBOL Code** - Use actual enterprise COBOL programs
2. **Report Issues** - Include COBOL code samples and error messages
3. **Enhance Relationship Extraction** - Add new COBOL relationship types
4. **Performance Optimization** - Improve parsing speed and memory usage

---

**Note**: This integration requires Java 17+ and the ProLeap COBOL parser. Ensure your environment meets these requirements before testing.
