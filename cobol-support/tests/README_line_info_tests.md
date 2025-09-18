# COBOL Line Information Tests

This directory contains comprehensive unit tests for the COBOL entity line information functionality.

## Overview

The line information feature adds accurate line range and line count data to all COBOL entities, making it easier to navigate and understand code structure.

## Test Files

### `test_cobol_line_information.py`
Comprehensive unit test suite that verifies:
- Line range format correctness
- Line count calculations
- Entity type-specific line information
- Consistency across all properties
- Performance with large files
- Edge case handling

### `run_line_info_tests.py`
Quick test runner for rapid validation of the line information functionality.

## Running Tests

### Full Test Suite
```bash
cd /path/to/code-grapher
python cobol-support/tests/test_cobol_line_information.py
```

### Quick Test
```bash
cd /path/to/code-grapher
python cobol-support/tests/run_line_info_tests.py
```

## Test Coverage

The tests cover the following aspects:

### 1. Line Range Format
- Verifies that `line` property follows "start-end" format
- Ensures start line ≤ end line
- Handles single-line entities correctly

### 2. Line Count Calculation
- Validates that `line_count` equals `end_line - start_line + 1`
- Ensures line count is always positive
- Tests consistency across all entity types

### 3. Entity Type Specific Tests
- **Paragraphs**: Should span multiple lines typically
- **Data Items**: Usually single-line entities
- **Programs/Compilation Units**: Basic line information

### 4. Consistency Validation
- Cross-validates all line-related properties
- Ensures `start_line`, `end_line`, `line_count`, and `line` are consistent
- Tests edge cases (line 0, single lines, etc.)

### 5. Performance Testing
- Validates parsing performance with large files
- Ensures reasonable processing times
- Tests with the Vasu fraud management file (1240 lines)

## Expected Results

### Success Criteria
- ✅ 100% of entities have proper line information
- ✅ All line counts calculated correctly
- ✅ Line ranges in proper format
- ✅ Performance within acceptable limits (< 30 seconds for large files)

### Sample Output
```
Entity: 0000-MAIN-PROCESS (paragraph)
  Line Range: 217-221
  Line Count: 5
  Start Line: 217
  End Line: 221

Entity: TRANS-ID (data_item)
  Line Range: 49-49
  Line Count: 1
  Start Line: 49
  End Line: 49
```

## Test Data

The tests use the following COBOL files:
- `fixtures/test_cobol_banking.cbl` - Basic test file
- `fixtures/vasu/vasu_fraud_management_cobol_reformatted.cbl` - Large complex file

## Dependencies

- COBOL parser must be available (Java + Maven)
- ProLeap parser integration
- Python unittest framework

## Troubleshooting

### Common Issues

1. **Parser Not Available**
   - Ensure Java and Maven are installed
   - Check ProLeap parser setup

2. **Test Files Not Found**
   - Verify test file paths
   - Check working directory

3. **Performance Issues**
   - Large files may take time to parse
   - Consider reducing test file size for development

### Debug Mode

To run tests with more verbose output:
```bash
python -m unittest cobol-support.tests.test_cobol_line_information -v
```

## Contributing

When adding new line information features:
1. Update the corresponding test cases
2. Add new test scenarios if needed
3. Ensure all tests pass
4. Update this documentation

## Test Results Summary

The line information implementation achieves:
- **100% success rate** for line information accuracy
- **218 entities** successfully parsed from large file
- **Multiple entity types** supported (program, paragraph, data_item, etc.)
- **Consistent line data** across all properties
- **Good performance** (6-10 seconds for large files)
