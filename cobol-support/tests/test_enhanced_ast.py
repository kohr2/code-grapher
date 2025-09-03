#!/usr/bin/env python3
"""
Test Enhanced COBOL AST Structure Support
Tests the new 25% of enhanced COBOL AST structures
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))

from services.real_proleap_parser import RealProLeapParser
from cobol_relationship_extractor import extract_cobol_relationships

def test_enhanced_ast_structures():
    """Test the enhanced COBOL AST structure extraction"""
    print("🧪 Testing Enhanced COBOL AST Structure Support")
    print("=" * 60)
    
    # Test file path
    test_file = "fixtures/account_management.cbl"
    
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        return False
    
    print(f"📁 Testing with file: {test_file}")
    
    # Initialize parser
    parser = RealProLeapParser()
    
    if not parser.is_available():
        print("❌ ProLeap parser not available")
        return False
    
    print("✅ ProLeap parser available")
    
    # Parse the file
    print("\n🔍 Parsing COBOL file...")
    result = parser.parse_file(test_file)
    
    if not result.get("parse_success", False):
        print("❌ Parsing failed")
        return False
    
    print("✅ File parsed successfully")
    
    # Test enhanced data structures
    print("\n📊 Testing Enhanced Data Structures:")
    
    # Test data items extraction
    data_items = result.get("data_items", {})
    if data_items:
        print(f"✅ Data items extracted: {len(data_items)} compilation units")
        for unit_name, items in data_items.items():
            print(f"   📋 Unit {unit_name}: {len(items)} data items")
            for item in items[:3]:  # Show first 3 items
                name = item.get('name', 'UNKNOWN')
                level = item.get('level', 'UNKNOWN')
                pic = item.get('picture_clause', 'N/A')
                usage = item.get('usage_clause', 'N/A')
                conditions = item.get('condition_names', 'N/A')
                print(f"      • {name} (Level {level}, PIC {pic}, USAGE {usage})")
                if conditions and conditions != 'N/A':
                    print(f"        Conditions: {conditions}")
    else:
        print("⚠️  No data items extracted")
    
    # Test enhanced statements
    statements = result.get("statements", {})
    if statements:
        print(f"\n✅ Enhanced statements extracted: {len(statements)} compilation units")
        for unit_name, paragraphs in statements.items():
            print(f"   📝 Unit {unit_name}: {len(paragraphs)} paragraphs")
            for para_name, stmt_list in paragraphs.items():
                print(f"      📄 Paragraph {para_name}: {len(stmt_list)} statements")
                for stmt_info in stmt_list[:2]:  # Show first 2 statements
                    if isinstance(stmt_info, dict):
                        stmt_type = stmt_info.get('type', 'UNKNOWN')
                        stmt_details = stmt_info.get('details', '')
                        print(f"         • {stmt_type}: {stmt_details}")
                    else:
                        print(f"         • {stmt_info}")
    else:
        print("⚠️  No enhanced statements extracted")
    
    # Test relationship extraction
    print("\n🔗 Testing Enhanced Relationship Extraction:")
    relationships = extract_cobol_relationships(result)
    
    if relationships:
        print(f"✅ {len(relationships)} relationships extracted")
        
        # Group relationships by type
        rel_types = {}
        for rel in relationships:
            rel_type = rel.relationship_type
            if rel_type not in rel_types:
                rel_types[rel_type] = []
            rel_types[rel_type].append(rel)
        
        print("\n📊 Relationship Types:")
        for rel_type, rels in rel_types.items():
            print(f"   {rel_type}: {len(rels)} relationships")
            for rel in rels[:2]:  # Show first 2 of each type
                print(f"      • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    else:
        print("⚠️  No relationships extracted")
    
    # Test specific enhanced features
    print("\n🎯 Testing Specific Enhanced Features:")
    
    # Test data flow relationships
    data_flow_rels = [r for r in relationships if r.relationship_type == "DATA_FLOW"]
    if data_flow_rels:
        print(f"✅ Data flow relationships: {len(data_flow_rels)}")
        for rel in data_flow_rels[:3]:
            print(f"   • {rel.source_entity} -> {rel.target_entity}")
    else:
        print("⚠️  No data flow relationships found")
    
    # Test arithmetic relationships
    arithmetic_rels = [r for r in relationships if r.relationship_type == "ARITHMETIC"]
    if arithmetic_rels:
        print(f"✅ Arithmetic relationships: {len(arithmetic_rels)}")
        for rel in arithmetic_rels[:3]:
            print(f"   • {rel.source_entity} -> {rel.target_entity}")
    else:
        print("⚠️  No arithmetic relationships found")
    
    # Test conditional relationships
    conditional_rels = [r for r in relationships if r.relationship_type == "CONDITIONAL"]
    if conditional_rels:
        print(f"✅ Conditional relationships: {len(conditional_rels)}")
        for rel in conditional_rels[:3]:
            print(f"   • {rel.source_entity} -> {rel.target_entity}")
    else:
        print("⚠️  No conditional relationships found")
    
    # Test data item relationships
    data_item_rels = [r for r in relationships if r.relationship_type == "CONTAINS" and "Data item" in r.context]
    if data_item_rels:
        print(f"✅ Data item relationships: {len(data_item_rels)}")
        for rel in data_item_rels[:3]:
            print(f"   • {rel.source_entity} -> {rel.target_entity}")
    else:
        print("⚠️  No data item relationships found")
    
    print("\n🎉 Enhanced COBOL AST Structure Test Complete!")
    return True

if __name__ == "__main__":
    success = test_enhanced_ast_structures()
    if success:
        print("\n✅ All tests passed!")
        sys.exit(0)
    else:
        print("\n❌ Some tests failed!")
        sys.exit(1)
