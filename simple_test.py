#!/usr/bin/env python3
"""
Simple test to verify COBOL parser functionality
"""

import re

# Test the regex patterns directly
test_content = "000100 IDENTIFICATION DIVISION."
test_content2 = "000200 PROGRAM-ID. FRAUD-MANAGEMENT."

# Test division pattern
division_pattern = r'^[0-9]{6}\s+(IDENTIFICATION\s+DIVISION)\.'
match = re.search(division_pattern, test_content)
print(f"Division test: {match}")

if match:
    print(f"Found division: {match.group(1)}")

# Test program pattern
program_pattern = r'^[0-9]{6}\s+PROGRAM-ID\.\s+([A-Z0-9-]+)'
match2 = re.search(program_pattern, test_content2)
print(f"Program test: {match2}")

if match2:
    print(f"Found program: {match2.group(1)}")

# Test with enhanced parser
from enhanced_cobol_parser import EnhancedCOBOLParser

# Create a simple test file
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

with open('simple_test.cbl', 'w') as f:
    f.write(test_cobol)

# Test the parser
parser = EnhancedCOBOLParser()
results = parser.parse_file('simple_test.cbl')

print(f"\nParser results:")
print(f"Counts: {results['counts']}")
print(f"Entities found: {len(results['entities'])}")

for entity in results['entities']:
    print(f"  {entity.entity_type.value}: {entity.name} (line {entity.line_number})")

# Clean up
import os
os.remove('simple_test.cbl')