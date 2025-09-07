#!/usr/bin/env python3
"""Debug COBOL parser output"""

import sys
import os
sys.path.append('.')

# Add the cobol-support directory to the path
sys.path.append('cobol-support')

from services.real_proleap_parser import RealProLeapParser
from services.cobol_relationship_extractor import COBOLRelationshipExtractor

def main():
    print("=== Testing COBOL Parser ===")
    
    # Test ProLeap parser
    parser = RealProLeapParser()
    result = parser.parse_file('cobol-support/tests/fixtures/simple_test.cbl')
    
    print(f"Parse success: {result.get('parse_success', False)}")
    print(f"Keys: {list(result.keys())}")
    
    if 'ast_data' in result:
        print(f"AST data keys: {list(result['ast_data'].keys())}")
        
        # Check for specific COBOL constructs
        for key in ['call_statements', 'perform_statements', 'copy_statements', 'use_statements']:
            if key in result['ast_data']:
                print(f"{key}: {result['ast_data'][key]}")
            else:
                print(f"{key}: NOT FOUND")
    
    print("\n=== Testing COBOL Relationship Extractor ===")
    
    # Test relationship extractor
    extractor = COBOLRelationshipExtractor()
    relationships = extractor.extract_relationships(result)
    
    print(f"Relationships extracted: {len(relationships)}")
    
    # Group by relationship type
    rel_types = {}
    for rel in relationships:
        rel_type = rel.relationship_type.value
        if rel_type not in rel_types:
            rel_types[rel_type] = []
        rel_types[rel_type].append(rel)
    
    for rel_type, rels in rel_types.items():
        print(f"\n{rel_type} relationships ({len(rels)}):")
        for rel in rels[:3]:  # Show first 3
            print(f"  {rel.source_entity} -> {rel.target_entity} ({rel.context})")

if __name__ == "__main__":
    main()
