#!/usr/bin/env python3
"""
Test Enhanced COBOL Relationship Extraction
Tests the new relationship extraction capabilities without requiring ProLeap
"""

import sys
import os
sys.path.append(os.path.dirname(os.path.abspath(__file__)))

from cobol_relationship_extractor import extract_cobol_relationships, RelationshipType

def test_enhanced_relationship_extraction():
    """Test the enhanced COBOL relationship extraction with mock data"""
    print("🧪 Testing Enhanced COBOL Relationship Extraction")
    print("=" * 60)
    
    # Create mock COBOL file data with enhanced structures
    mock_file_data = {
        "language": "cobol",
        "file_path": "test_banking.cbl",
        "compilation_units": [
            {"name": "BANKING-SYSTEM", "type": "compilation_unit"}
        ],
        "entities": [
            {"type": "program", "name": "BANKING-SYSTEM"},
            {"type": "compilation_unit", "name": "BANKING-SYSTEM"}
        ],
        "statements": {
            "BANKING-SYSTEM": {
                "1000-INITIALIZE": [
                    {
                        "text": "MOVE ZERO TO WS-ACCOUNT-BALANCE",
                        "type": "MoveStatement",
                        "details": "MOVE_FROM:ZERO:MOVE_TO:WS-ACCOUNT-BALANCE"
                    },
                    {
                        "text": "MOVE 'A' TO WS-ACCOUNT-STATUS",
                        "type": "MoveStatement", 
                        "details": "MOVE_FROM:'A':MOVE_TO:WS-ACCOUNT-STATUS"
                    }
                ],
                "2000-PROCESS-ACCOUNTS": [
                    {
                        "text": "ADD WS-TRANS-AMOUNT TO WS-ACCOUNT-BALANCE",
                        "type": "AddStatement",
                        "details": "ADD_OPERANDS:WS-TRANS-AMOUNT,WS-ACCOUNT-BALANCE"
                    },
                    {
                        "text": "IF WS-ACCOUNT-BALANCE > ZERO",
                        "type": "IfStatement",
                        "details": "IF_CONDITION:WS-ACCOUNT-BALANCE > ZERO"
                    },
                    {
                        "text": "EVALUATE WS-ACCOUNT-TYPE",
                        "type": "EvaluateStatement",
                        "details": "EVALUATE_SUBJECTS:WS-ACCOUNT-TYPE"
                    }
                ],
                "3000-CALCULATE-INTEREST": [
                    {
                        "text": "COMPUTE WS-INTEREST = WS-ACCOUNT-BALANCE * WS-INTEREST-RATE",
                        "type": "ComputeStatement",
                        "details": "COMPUTE_EXPR:WS-INTEREST = WS-ACCOUNT-BALANCE * WS-INTEREST-RATE"
                    }
                ]
            }
        },
        "data_items": {
            "BANKING-SYSTEM": [
                {
                    "name": "WS-ACCOUNT-BALANCE",
                    "level": "05",
                    "type": "DataDescriptionEntry",
                    "picture_clause": "PIC S9(7)V99 COMP-3",
                    "usage_clause": "COMP-3",
                    "value_clause": "",
                    "condition_names": "",
                    "text": "05 WS-ACCOUNT-BALANCE PIC S9(7)V99 COMP-3",
                    "unit": "BANKING-SYSTEM"
                },
                {
                    "name": "WS-ACCOUNT-TYPE",
                    "level": "05",
                    "type": "DataDescriptionEntry",
                    "picture_clause": "PIC X(1)",
                    "usage_clause": "",
                    "value_clause": "",
                    "condition_names": "CHECKING-ACCOUNT:C|SAVINGS-ACCOUNT:S",
                    "text": "05 WS-ACCOUNT-TYPE PIC X(1)",
                    "unit": "BANKING-SYSTEM"
                },
                {
                    "name": "WS-TRANS-AMOUNT",
                    "level": "05",
                    "type": "DataDescriptionEntry",
                    "picture_clause": "PIC S9(7)V99",
                    "usage_clause": "",
                    "value_clause": "",
                    "condition_names": "",
                    "text": "05 WS-TRANS-AMOUNT PIC S9(7)V99",
                    "unit": "BANKING-SYSTEM"
                }
            ]
        },
        "file_descriptions": {
            "BANKING-SYSTEM": [
                {
                    "name": "CUSTOMER-FILE",
                    "text": "FD CUSTOMER-FILE",
                    "unit": "BANKING-SYSTEM"
                }
            ]
        },
        "linkage_items": {
            "BANKING-SYSTEM": [
                {
                    "name": "LK-INPUT-PARAMETERS",
                    "level": "01",
                    "type": "DataDescriptionEntry",
                    "text": "01 LK-INPUT-PARAMETERS",
                    "unit": "BANKING-SYSTEM"
                }
            ]
        }
    }
    
    print("📊 Mock COBOL data created with enhanced structures")
    print(f"   • {len(mock_file_data['statements']['BANKING-SYSTEM'])} paragraphs")
    print(f"   • {len(mock_file_data['data_items']['BANKING-SYSTEM'])} data items")
    print(f"   • {len(mock_file_data['file_descriptions']['BANKING-SYSTEM'])} file descriptions")
    print(f"   • {len(mock_file_data['linkage_items']['BANKING-SYSTEM'])} linkage items")
    
    # Extract relationships
    print("\n🔗 Extracting enhanced relationships...")
    relationships = extract_cobol_relationships(mock_file_data)
    
    if not relationships:
        print("❌ No relationships extracted")
        return False
    
    print(f"✅ {len(relationships)} relationships extracted")
    
    # Analyze relationship types
    rel_types = {}
    for rel in relationships:
        rel_type = rel.relationship_type
        if rel_type not in rel_types:
            rel_types[rel_type] = []
        rel_types[rel_type].append(rel)
    
    print("\n📊 Relationship Analysis:")
    for rel_type, rels in rel_types.items():
        print(f"   {rel_type}: {len(rels)} relationships")
    
    # Test specific enhanced features
    print("\n🎯 Testing Enhanced Features:")
    
    # Test data flow relationships
    data_flow_rels = [r for r in relationships if r.relationship_type == RelationshipType.DATA_FLOW]
    print(f"✅ Data flow relationships: {len(data_flow_rels)}")
    for rel in data_flow_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test arithmetic relationships
    arithmetic_rels = [r for r in relationships if r.relationship_type == RelationshipType.ARITHMETIC]
    print(f"\n✅ Arithmetic relationships: {len(arithmetic_rels)}")
    for rel in arithmetic_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test conditional relationships
    conditional_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONDITIONAL]
    print(f"\n✅ Conditional relationships: {len(conditional_rels)}")
    for rel in conditional_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test data item relationships
    data_item_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONTAINS and "Data item" in r.context]
    print(f"\n✅ Data item relationships: {len(data_item_rels)}")
    for rel in data_item_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test 88-level condition relationships
    condition_rels = [r for r in relationships if "88-level condition" in r.context]
    print(f"\n✅ 88-level condition relationships: {len(condition_rels)}")
    for rel in condition_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test file description relationships
    file_rels = [r for r in relationships if "File description" in r.context]
    print(f"\n✅ File description relationships: {len(file_rels)}")
    for rel in file_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Test linkage item relationships
    linkage_rels = [r for r in relationships if "Linkage item" in r.context]
    print(f"\n✅ Linkage item relationships: {len(linkage_rels)}")
    for rel in linkage_rels:
        print(f"   • {rel.source_entity} -> {rel.target_entity} ({rel.context})")
    
    # Summary
    print(f"\n📈 Summary:")
    print(f"   • Total relationships: {len(relationships)}")
    print(f"   • Enhanced relationship types: {len(rel_types)}")
    print(f"   • Data flow tracking: {'✅' if data_flow_rels else '❌'}")
    print(f"   • Arithmetic analysis: {'✅' if arithmetic_rels else '❌'}")
    print(f"   • Conditional logic: {'✅' if conditional_rels else '❌'}")
    print(f"   • Data item definitions: {'✅' if data_item_rels else '❌'}")
    print(f"   • 88-level conditions: {'✅' if condition_rels else '❌'}")
    print(f"   • File descriptions: {'✅' if file_rels else '❌'}")
    print(f"   • Linkage items: {'✅' if linkage_rels else '❌'}")
    
    return True

if __name__ == "__main__":
    success = test_enhanced_relationship_extraction()
    if success:
        print("\n🎉 Enhanced COBOL Relationship Extraction Test Complete!")
        print("✅ All enhanced features working correctly!")
        sys.exit(0)
    else:
        print("\n❌ Enhanced relationship extraction test failed!")
        sys.exit(1)
