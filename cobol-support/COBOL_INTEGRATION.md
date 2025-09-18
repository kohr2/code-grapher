# COBOL Integration with Code Grapher

This document describes the complete integration of COBOL parsing capabilities into Code Grapher using the ProLeap ANTLR4-based COBOL parser, including the latest line information features.

## 🎯 Overview

Code Grapher now supports comprehensive COBOL code analysis, enabling you to:

- **Parse COBOL files** (.cbl, .cob, .cobol extensions)
- **Extract COBOL entities** (programs, data items, paragraphs, sections)
- **Discover COBOL relationships** (CALL, PERFORM, COPY statements)
- **Navigate with line information** (accurate line ranges and counts)
- **Integrate COBOL analysis** into your existing knowledge graph pipeline

## 🏗️ Architecture

### Components

1. **COBOL Parser Service** (`cobol-support/services/cobol_parser.py`)
   - Wraps ProLeap ANTLR4-based COBOL parser
   - Extracts entities and relationships from COBOL AST
   - Handles COBOL-specific concepts (divisions, paragraphs, data descriptions)
   - **NEW**: Provides accurate line information for all entities

2. **Raw ProLeap Parser** (`cobol-support/services/raw_proleap_parser.py`)
   - Direct integration with ProLeap ANTLR4 parser
   - Extracts line information from AST context
   - Generates Java code for detailed COBOL analysis
   - **NEW**: Enhanced with line range extraction

3. **COBOL Section Extractor** (`cobol-support/services/cobol_section_extractor.py`)
   - Extracts relevant sections from COBOL programs
   - Optimizes content for AI processing
   - **NEW**: Includes line range properties for sections

4. **COBOL Relationship Extractor** (`cobol-support/services/cobol_relationship_extractor.py`)
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
cd cobol-support/tests
python test_cobol_line_information.py

# Or run quick validation
python run_line_info_tests.py
```

## 📁 File Structure

```
code-grapher/
├── cobol-support/
│   ├── services/
│   │   ├── cobol_parser.py                    # Main COBOL parser service
│   │   ├── raw_proleap_parser.py              # ProLeap integration
│   │   ├── cobol_section_extractor.py         # Section extraction with line info
│   │   ├── cobol_relationship_extractor.py    # Relationship extraction
│   │   └── reformat_cobol_for_proleap.py      # COBOL file reformatter
│   └── tests/
│       ├── test_cobol_line_information.py    # Comprehensive unit tests
│       ├── test_line_info_integration.py     # Integration tests
│       ├── run_line_info_tests.py            # Quick validation
│       ├── fixtures/
│       │   ├── test_cobol_banking.cbl        # Sample COBOL program
│       │   └── vasu/
│       │       └── vasu_fraud_management_cobol_reformatted.cbl  # Large test file
│       ├── README_line_info_tests.md         # Test documentation
│       └── TEST_SUMMARY.md                   # Implementation summary
└── COBOL_INTEGRATION.md                      # This documentation
```

## 🔍 COBOL Entity Types

The parser extracts the following COBOL entities with **accurate line information**:

### Core Entities
- **`program`** - Main program definition
- **`compilation_unit`** - Compilation unit entities
- **`paragraph`** - Procedure division paragraphs
- **`data_item`** - Working storage, local storage, linkage items

### Line Information Properties
Each entity now includes:
- **`line`**: Line range in "start-end" format (e.g., "217-221")
- **`line_count`**: Number of lines (calculated as end_line - start_line + 1)
- **`start_line`**: Starting line number
- **`end_line`**: Ending line number

### Example Entity with Line Information

```json
{
  "type": "paragraph",
  "name": "0000-MAIN-PROCESS",
  "properties": {
    "unit": "Vasu_fraud_management_cobol_reformatted",
    "context": "Paragraph in Vasu_fraud_management_cobol_reformatted",
    "line": "217-221",
    "line_count": 5,
    "start_line": 217,
    "end_line": 221
  }
}
```

## 🔗 COBOL Relationship Types

### Program Relationships
- **`CALLS`** - CALL statements to other programs
- **`PERFORM`** - PERFORM statements to paragraphs/sections
- **`IMPORTS`** - COPY statements for copybooks

### Enhanced Data Relationships
- **`DATA_FLOW`** - Data movement between entities (MOVE statements)
- **`MODIFIES`** - Data item modifications
- **`READS`** - Data item reads
- **`WRITES`** - Data item writes

### Control Flow Relationships
- **`CONDITIONAL`** - Variables used in IF/EVALUATE conditions
- **`ARITHMETIC`** - Variables used in arithmetic operations

### File Operations
- **`FILE_ACCESS`** - File read/write operations

## 🧪 Testing

### Test Files

1. **`test_cobol_banking.cbl`** - Realistic banking system COBOL program
   - Demonstrates common banking operations
   - Includes all major COBOL divisions
   - Shows typical COBOL patterns and relationships

2. **`vasu_fraud_management_cobol_reformatted.cbl`** - Large fraud management system
   - 1,240 lines of complex COBOL code
   - 218 entities with complete line information
   - Tests performance and accuracy with real-world code

### Test Suites

#### 1. Unit Tests (`test_cobol_line_information.py`)
Comprehensive test suite with 10 test cases:
- Line range format validation
- Line count calculation accuracy
- Entity type-specific validation
- Consistency checks across properties
- Performance testing with large files
- Edge case handling

#### 2. Integration Tests (`test_line_info_integration.py`)
- System integration validation
- JSON serialization testing
- Performance benchmarks
- End-to-end functionality verification

#### 3. Quick Tests (`run_line_info_tests.py`)
- Rapid validation script
- Basic functionality verification
- Performance metrics

### Running Tests

```bash
# Full unit test suite
cd cobol-support/tests
python test_cobol_line_information.py

# Quick validation
python run_line_info_tests.py

# Integration tests
python test_line_info_integration.py

# All tests with verbose output
python -m unittest test_cobol_line_information -v
```

### Test Results

- **✅ 10/10 unit tests passed**
- **✅ 100% success rate** for line information coverage
- **✅ 100% accuracy** for line count calculations
- **✅ 100% JSON serialization** preservation
- **✅ Performance within limits** (< 30s for large files)

## 🔧 Usage Examples

### Basic COBOL Parsing with Line Information

```python
from cobol_support.services.cobol_parser import COBOLParser

# Initialize parser
parser = COBOLParser()

# Parse COBOL file
result = parser.parse_file("banking_system.cbl")

# Access parsed data with line information
if result["parse_success"]:
    entities = result["entities"]
    print(f"Found {len(entities)} entities")
    
    for entity in entities:
        props = entity.get("properties", {})
        print(f"Entity: {entity['name']}")
        print(f"  Type: {entity['type']}")
        print(f"  Line Range: {props.get('line', 'N/A')}")
        print(f"  Line Count: {props.get('line_count', 'N/A')}")
        print(f"  Location: Lines {props.get('start_line', 'N/A')}-{props.get('end_line', 'N/A')}")
```

### Working with Line Information

```python
# Find entities in specific line ranges
def find_entities_in_range(entities, start_line, end_line):
    return [
        entity for entity in entities
        if (entity.get("properties", {}).get("start_line", 0) >= start_line and
            entity.get("properties", {}).get("end_line", 0) <= end_line)
    ]

# Get paragraph entities with line counts
paragraphs = [
    entity for entity in entities 
    if entity.get("type") == "paragraph"
]

for para in paragraphs:
    props = para.get("properties", {})
    line_count = props.get("line_count", 0)
    if line_count > 10:  # Large paragraphs
        print(f"Large paragraph: {para['name']} ({line_count} lines)")
```

### Section Extraction with Line Information

```python
from cobol_support.services.cobol_section_extractor import COBOLSectionExtractor

# Extract sections with line information
extractor = COBOLSectionExtractor()
sections = extractor.extract_relevant_sections(cobol_content)

for section_name, section in sections.items():
    print(f"Section: {section_name}")
    print(f"  Line Range: {section.line_range}")
    print(f"  Line Count: {section.line_count}")
    print(f"  Content: {section.content[:100]}...")
```

## 📊 Performance Metrics

### Parsing Performance

| File Size | Entities | Parse Time | Entities/sec |
|-----------|----------|------------|--------------|
| Small     | 52       | ~5.4s      | ~9.6/s       |
| Large     | 218      | ~8.2s      | ~26.5/s      |

### Line Information Accuracy

- **100% of entities** have proper line information
- **100% accuracy** for line count calculations
- **Consistent data** across all properties
- **JSON serialization** preserves all line data

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

#### 4. Line Information Missing
```
Entity has line: 0 instead of proper range
```
**Solution**: Ensure you're using the updated parser
```python
from cobol_support.services.cobol_parser import COBOLParser
parser = COBOLParser()
# This should now provide proper line information
```

### Debug Mode

Enable verbose logging for troubleshooting:

```python
import logging
logging.basicConfig(level=logging.DEBUG)

# Test parser availability
from cobol_support.services.cobol_parser import COBOLParser
parser = COBOLParser()
print(f"Parser available: {parser.is_available()}")

# Test line information
result = parser.parse_file("test.cbl")
if result.get("parse_success"):
    entities = result.get("entities", [])
    for entity in entities[:3]:
        props = entity.get("properties", {})
        print(f"Entity: {entity['name']}")
        print(f"  Line: {props.get('line', 'N/A')}")
        print(f"  Count: {props.get('line_count', 'N/A')}")
```

## 🔮 Future Enhancements

### Next Priority Areas
1. **File Operations**: READ, WRITE, OPEN, CLOSE statement analysis
2. **String Operations**: STRING, UNSTRING, INSPECT analysis
3. **Advanced Control Flow**: Nested IF, PERFORM UNTIL/THRU analysis
4. **EXEC Statements**: SQL and CICS statement extraction
5. **Copybook Analysis**: Enhanced COPY/REPLACE statement processing
6. **Nested Programs**: Subprogram hierarchy analysis
7. **Declaratives**: Exception handling structure analysis

### Integration Opportunities
- **AI Analysis**: Enhanced data for AI-powered code analysis
- **Refactoring Support**: Better understanding for automated refactoring
- **Documentation Generation**: Enhanced code documentation capabilities
- **Quality Metrics**: Advanced code quality and complexity analysis

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
5. **Line Information Features** - Enhance line range and count accuracy

## 🎉 Implementation Summary

The COBOL integration with line information represents a significant advancement in COBOL code analysis capabilities:

### Key Achievements
- **Complete line information** for all COBOL entities
- **Accurate line ranges** in "start-end" format
- **Automatic line count calculation** for all entities
- **Comprehensive testing** with 100% success rate
- **Performance optimized** for large files
- **JSON serialization** compatibility

### Technical Impact
- **Line Information**: 100% coverage for all entity types
- **Accuracy**: 100% correct line count calculations
- **Performance**: 9.6-26.5 entities/second processing
- **Testing**: Comprehensive test coverage with unit, integration, and performance tests

### Developer Benefits
- **Precise Navigation**: Jump directly to any COBOL entity
- **Code Understanding**: See exactly how many lines each procedure spans
- **Data Mapping**: Know exactly where each data field is defined
- **Maintenance**: Easily locate specific code sections
- **Documentation**: Generate accurate code documentation with line references

This implementation transforms COBOL entities from having `line: 0` to having accurate line ranges like `line: "217-221"` with proper line counts, making COBOL code navigation and understanding much more effective!

---

**Note**: This integration requires Java 17+ and the ProLeap COBOL parser. Ensure your environment meets these requirements before testing.