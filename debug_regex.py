#!/usr/bin/env python3
"""
Debug regex patterns to understand why they're not matching
"""

import re

# Test content
test_content = "IDENTIFICATION DIVISION."
test_content2 = "PROGRAM-ID. FRAUD-MANAGEMENT."

print("=== Testing Division Pattern ===")
division_pattern = r'^[0-9]{6}\s+(IDENTIFICATION\s+DIVISION)\.'
print(f"Pattern: {division_pattern}")
print(f"Content: '{test_content}'")
match = re.search(division_pattern, test_content)
print(f"Match: {match}")

# Test without line number prefix
division_pattern_no_prefix = r'^(IDENTIFICATION\s+DIVISION)\.'
match2 = re.search(division_pattern_no_prefix, test_content)
print(f"Match without prefix: {match2}")

print("\n=== Testing Program Pattern ===")
program_pattern = r'^[0-9]{6}\s+PROGRAM-ID\.\s+([A-Z0-9-]+)'
print(f"Pattern: {program_pattern}")
print(f"Content: '{test_content2}'")
match3 = re.search(program_pattern, test_content2)
print(f"Match: {match3}")

# Test without line number prefix
program_pattern_no_prefix = r'^PROGRAM-ID\.\s+([A-Z0-9-]+)'
match4 = re.search(program_pattern_no_prefix, test_content2)
print(f"Match without prefix: {match4}")

print("\n=== Testing with actual COBOL format ===")
cobol_line = "000100 IDENTIFICATION DIVISION."
print(f"COBOL line: '{cobol_line}'")

# Remove line number (first 6 characters)
code_content = cobol_line[6:].strip()
print(f"Code content: '{code_content}'")

# Test division pattern on code content
match5 = re.search(division_pattern_no_prefix, code_content)
print(f"Match on code content: {match5}")

if match5:
    print(f"Found division: {match5.group(1)}")

print("\n=== Testing Enhanced Parser Patterns ===")
from enhanced_cobol_parser import EnhancedCOBOLParser

parser = EnhancedCOBOLParser()
print("Division patterns:")
for pattern in parser.patterns['division']:
    print(f"  {pattern}")

print("\nProgram patterns:")
for pattern in parser.patterns['program']:
    print(f"  {pattern}")

# Test each pattern
print("\n=== Testing Each Pattern ===")
for i, pattern in enumerate(parser.patterns['division']):
    match = re.search(pattern, code_content, re.IGNORECASE)
    print(f"Division pattern {i+1}: {match}")
    if match:
        print(f"  Found: {match.group(1)}")

for i, pattern in enumerate(parser.patterns['program']):
    program_content = "PROGRAM-ID. FRAUD-MANAGEMENT."
    match = re.search(pattern, program_content, re.IGNORECASE)
    print(f"Program pattern {i+1}: {match}")
    if match:
        print(f"  Found: {match.group(1)}")