#!/usr/bin/env python3
"""
Debug the COBOL parser to understand why it's not finding entities
"""

from enhanced_cobol_parser import EnhancedCOBOLParser
import logging

# Enable debug logging
logging.basicConfig(level=logging.DEBUG)

# Create test content
test_cobol = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. FRAUD-MANAGEMENT.
000300 AUTHOR. SYSTEM.
000400
000500 ENVIRONMENT DIVISION.
000600 INPUT-OUTPUT SECTION.
000700
000800 DATA DIVISION.
000900 FILE SECTION.
001000
001100 PROCEDURE DIVISION.
001200 1000-INITIALIZE.
001300     DISPLAY 'HELLO WORLD'.
001400     MOVE 1 TO WS-COUNTER.
001500
001600 2000-PROCESS.
001700     PERFORM 1000-INITIALIZE.
001800     STOP RUN.
"""

with open('debug_test.cbl', 'w') as f:
    f.write(test_cobol)

# Test the parser step by step
parser = EnhancedCOBOLParser()

print("=== Testing _parse_content directly ===")
parser._parse_content(test_cobol)

print(f"Entities found: {len(parser.entities)}")
for entity in parser.entities:
    print(f"  {entity.entity_type.value}: {entity.name} (line {entity.line_number})")

# Test individual extraction methods
print("\n=== Testing individual extraction methods ===")

lines = test_cobol.split('\n')
for line_num, line in enumerate(lines, 1):
    if not line.strip():
        continue
    
    # Extract line number and content
    line_number = line[:6].strip() if len(line) >= 6 else ""
    code_content = line[6:].strip() if len(line) >= 6 else line
    
    print(f"Line {line_num}: '{code_content}'")
    
    # Test division extraction
    division = parser._extract_division(code_content, line_num)
    if division:
        print(f"  Found division: {division.name}")
    
    # Test program extraction
    program = parser._extract_program(code_content, line_num)
    if program:
        print(f"  Found program: {program.name}")

# Clean up
import os
os.remove('debug_test.cbl')