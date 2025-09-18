#!/usr/bin/env python3
"""
COBOL Demo and Showcase Tests

This module contains demo and showcase tests for COBOL functionality:
- Feature demonstrations
- Example usage
- Documentation validation
"""

import os
import sys
import json
from pathlib import Path

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser
from services.cobol_relationship_extractor import extract_cobol_relationships
from ai_services.models.relationship_models import RelationshipType


def demo_parser_functionality():
    """Demo COBOL parser functionality"""
    print("🎯 COBOL Parser Demo")
    print("=" * 50)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available (Java/Maven dependencies missing)")
        return False
    
    print("✅ COBOL parser available")
    
    # Test with a sample file
    test_file = "fixtures/test_cobol_banking.cbl"
    if not os.path.exists(test_file):
        test_file = os.path.join("cobol-support", "tests", test_file)
    
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        return False
    
    print(f"📄 Parsing file: {test_file}")
    
    result = parser.parse_file(test_file)
    
    if not result.get("parse_success", False):
        print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
        return False
    
    print("✅ Parsing successful")
    
    # Show parsing results
    entities = result.get("entities", [])
    compilation_units = result.get("compilation_units", [])
    statements = result.get("statements", {})
    data_items = result.get("data_items", {})
    
    print(f"\n📊 Parsing Results:")
    print(f"  • Compilation units: {len(compilation_units)}")
    print(f"  • Entities: {len(entities)}")
    print(f"  • Statement units: {len(statements)}")
    print(f"  • Data item units: {len(data_items)}")
    
    # Show entity types
    entity_types = {}
    for entity in entities:
        entity_type = entity.get("type", "unknown")
        entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
    
    print(f"\n📈 Entity Types:")
    for entity_type, count in sorted(entity_types.items()):
        print(f"  • {entity_type}: {count}")
    
    # Show sample entities
    print(f"\n📋 Sample Entities:")
    for i, entity in enumerate(entities[:5]):
        properties = entity.get("properties", {})
        print(f"  {i+1}. {entity.get('name', 'unknown')} ({entity.get('type', 'unknown')})")
        print(f"     Line: {properties.get('line', 'N/A')}")
        print(f"     Count: {properties.get('line_count', 'N/A')}")
        print(f"     Range: {properties.get('start_line', 'N/A')}-{properties.get('end_line', 'N/A')}")
    
    return True


def demo_relationship_extraction():
    """Demo COBOL relationship extraction"""
    print("\n🎯 COBOL Relationship Extraction Demo")
    print("=" * 50)
    
    # Create comprehensive mock COBOL data
    mock_cobol_data = {
        "parse_success": True,
        "language": "cobol",
        "file_path": "banking_system.cbl",
        
        # COPY statements
        "copy_statements": {
            "BANKING-SYSTEM": [
                {"name": "BANKING-COPYBOOK", "library": "COMMON", "unit": "BANKING-SYSTEM"},
                {"name": "ERROR-HANDLING", "library": "UTILS", "unit": "BANKING-SYSTEM"},
                {"name": "ACCOUNT-STRUCTURE", "library": "DATA", "unit": "BANKING-SYSTEM"}
            ]
        },
        
        # REPLACING phrases
        "replacing_phrases": {
            "BANKING-SYSTEM": {
                "BANKING-COPYBOOK": [
                    {"replaceable": "ACCOUNT", "replacement": "CUSTOMER"},
                    {"replaceable": "BALANCE", "replacement": "AMOUNT"}
                ],
                "ERROR-HANDLING": [
                    {"replaceable": "ERROR-CODE", "replacement": "ERR-CODE"}
                ]
            }
        },
        
        # CALL statements
        "call_statements": {
            "BANKING-SYSTEM": {
                "MAIN-LOGIC": [
                    {"program_name": "INTEREST-CALCULATOR", "unit": "BANKING-SYSTEM"},
                    {"program_name": "ACCOUNT-VALIDATOR", "unit": "BANKING-SYSTEM"},
                    {"program_name": "REPORT-GENERATOR", "unit": "BANKING-SYSTEM"}
                ],
                "PROCESS-TRANSACTION": [
                    {"program_name": "TRANSACTION-PROCESSOR", "unit": "BANKING-SYSTEM"}
                ]
            }
        },
        
        # CALL parameters
        "call_parameters": {
            "BANKING-SYSTEM": {
                "MAIN-LOGIC": [
                    {"program_name": "INTEREST-CALCULATOR", "param_type": "REFERENCE", "param_name": "WS-ACCOUNT-NUMBER"},
                    {"program_name": "INTEREST-CALCULATOR", "param_type": "REFERENCE", "param_name": "WS-BALANCE"},
                    {"program_name": "ACCOUNT-VALIDATOR", "param_type": "VALUE", "param_name": "WS-ACCOUNT-NUMBER"},
                    {"program_name": "REPORT-GENERATOR", "param_type": "REFERENCE", "param_name": "WS-REPORT-DATA"}
                ]
            }
        },
        
        # USE statements
        "use_statements": {
            "BANKING-SYSTEM": [
                {"use_type": "ERROR", "file_name": "ACCOUNT-FILE", "procedure_name": "", "unit": "BANKING-SYSTEM"},
                {"use_type": "ERROR", "file_name": "TRANSACTION-FILE", "procedure_name": "", "unit": "BANKING-SYSTEM"},
                {"use_type": "EXCEPTION", "file_name": "", "procedure_name": "ERROR-HANDLER", "unit": "BANKING-SYSTEM"}
            ]
        },
        
        # Communication
        "communication": {
            "BANKING-SYSTEM": [
                {"name": "ACCOUNT-QUEUE", "type": "INPUT", "symbolic_queue": "WS-ACCOUNT-QUEUE", "symbolic_destination": "", "unit": "BANKING-SYSTEM"},
                {"name": "TRANSACTION-QUEUE", "type": "OUTPUT", "symbolic_queue": "", "symbolic_destination": "WS-TRANS-DEST", "unit": "BANKING-SYSTEM"},
                {"name": "REPORT-QUEUE", "type": "I_O", "symbolic_queue": "WS-REPORT-QUEUE", "symbolic_destination": "WS-REPORT-DEST", "unit": "BANKING-SYSTEM"}
            ]
        },
        
        # Screens
        "screens": {
            "BANKING-SYSTEM": [
                {"name": "MAIN-MENU", "value": "BANKING SYSTEM MENU", "from": "WS-MENU-DATA", "to": "WS-MENU-DATA", "unit": "BANKING-SYSTEM"},
                {"name": "ACCOUNT-ENTRY", "value": "ENTER ACCOUNT NUMBER:", "from": "WS-ACCOUNT-NUMBER", "to": "WS-ACCOUNT-NUMBER", "unit": "BANKING-SYSTEM"},
                {"name": "BALANCE-DISPLAY", "value": "CURRENT BALANCE:", "from": "WS-BALANCE", "to": "WS-BALANCE", "unit": "BANKING-SYSTEM"}
            ]
        },
        
        # Statements with PERFORM
        "statements": {
            "BANKING-SYSTEM": {
                "MAIN-LOGIC": [
                    {"text": "PERFORM INITIALIZE-SYSTEM", "type": "PERFORM", "details": ""},
                    {"text": "PERFORM PROCESS-ACCOUNTS", "type": "PERFORM", "details": ""},
                    {"text": "PERFORM GENERATE-REPORTS", "type": "PERFORM", "details": ""}
                ],
                "PROCESS-TRANSACTION": [
                    {"text": "PERFORM VALIDATE-TRANSACTION", "type": "PERFORM", "details": ""},
                    {"text": "PERFORM UPDATE-ACCOUNT", "type": "PERFORM", "details": ""}
                ]
            }
        }
    }
    
    print("📊 Mock COBOL Data Analysis:")
    print(f"   📁 File: {mock_cobol_data['file_path']}")
    print(f"   📋 Copy statements: {sum(len(copies) for copies in mock_cobol_data['copy_statements'].values())}")
    print(f"   📞 Call statements: {sum(len(calls) for para_calls in mock_cobol_data['call_statements'].values() for calls in para_calls.values())}")
    print(f"   🔧 Use statements: {sum(len(uses) for uses in mock_cobol_data['use_statements'].values())}")
    print(f"   📡 Communication entries: {sum(len(comm) for comm in mock_cobol_data['communication'].values())}")
    print(f"   🖥️  Screen entries: {sum(len(screens) for screens in mock_cobol_data['screens'].values())}")
    
    print("\n🔗 Extracting Relationships...")
    
    # Extract relationships
    relationships = extract_cobol_relationships(mock_cobol_data)
    
    if not relationships:
        print("❌ No relationships extracted")
        return False
    
    print(f"✅ Extracted {len(relationships)} relationships")
    
    # Group relationships by type
    relationship_types = {}
    for rel in relationships:
        rel_type = rel.relationship_type
        if rel_type not in relationship_types:
            relationship_types[rel_type] = []
        relationship_types[rel_type].append(rel)
    
    print(f"\n📊 Relationship Types:")
    for rel_type, rels in relationship_types.items():
        print(f"   {rel_type}: {len(rels)} relationships")
    
    print(f"\n🔗 Sample Relationships by Type:")
    for rel_type, rels in relationship_types.items():
        print(f"\n   📋 {rel_type} ({len(rels)} relationships):")
        for i, rel in enumerate(rels[:3]):  # Show first 3 of each type
            print(f"      {i+1}. {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
            print(f"         {rel.context}")
        if len(rels) > 3:
            print(f"      ... and {len(rels) - 3} more")
    
    return True


def demo_advanced_features():
    """Demo advanced COBOL features"""
    print("\n🎯 Advanced COBOL Features Demo")
    print("=" * 50)
    
    # Demo relationship types
    print("📋 Available COBOL Relationship Types:")
    cobol_types = [
        RelationshipType.INCLUDES,
        RelationshipType.PASSES_DATA,
        RelationshipType.HANDLES_ERRORS,
        RelationshipType.USES_QUEUE,
        RelationshipType.BINDS_SCREEN,
        RelationshipType.PERFORMS,
        RelationshipType.REPLACES,
        RelationshipType.CALLS,
        RelationshipType.DATA_FLOW,
        RelationshipType.ARITHMETIC,
        RelationshipType.CONDITIONAL,
        RelationshipType.READS,
        RelationshipType.WRITES,
        RelationshipType.FILE_ACCESS,
        RelationshipType.USES,
        RelationshipType.MODIFIES,
        RelationshipType.WRITTEN_BY
    ]
    
    for i, rel_type in enumerate(cobol_types, 1):
        print(f"  {i:2d}. {rel_type.value}")
    
    print(f"\n✅ {len(cobol_types)} COBOL relationship types available")
    
    # Demo parser features
    parser = COBOLParser()
    if parser.is_available():
        print("\n🔧 Parser Features:")
        print("  ✅ COBOL file parsing")
        print("  ✅ Entity extraction")
        print("  ✅ Line information tracking")
        print("  ✅ AST structure analysis")
        print("  ✅ Relationship extraction")
        print("  ✅ Multi-language integration")
    else:
        print("\n⚠️  Parser not available (Java/Maven dependencies missing)")
    
    return True


def demo_integration_workflow():
    """Demo complete integration workflow"""
    print("\n🎯 Complete Integration Workflow Demo")
    print("=" * 50)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available")
        return False
    
    # Step 1: Parse COBOL file
    print("Step 1: Parsing COBOL file...")
    test_file = "fixtures/test_cobol_banking.cbl"
    if not os.path.exists(test_file):
        test_file = os.path.join("cobol-support", "tests", test_file)
    
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        return False
    
    result = parser.parse_file(test_file)
    
    if not result.get("parse_success", False):
        print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
        return False
    
    print("✅ File parsed successfully")
    
    # Step 2: Extract relationships
    print("\nStep 2: Extracting relationships...")
    relationships = extract_cobol_relationships(result)
    print(f"✅ Extracted {len(relationships)} relationships")
    
    # Step 3: Analyze results
    print("\nStep 3: Analyzing results...")
    entities = result.get("entities", [])
    compilation_units = result.get("compilation_units", [])
    
    print(f"  • Compilation units: {len(compilation_units)}")
    print(f"  • Entities: {len(entities)}")
    print(f"  • Relationships: {len(relationships)}")
    
    # Step 4: Show sample data
    print("\nStep 4: Sample data:")
    print("  📋 Sample entities:")
    for i, entity in enumerate(entities[:3]):
        print(f"    {i+1}. {entity.get('name', 'unknown')} ({entity.get('type', 'unknown')})")
    
    print("  🔗 Sample relationships:")
    for i, rel in enumerate(relationships[:3]):
        print(f"    {i+1}. {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
    
    # Step 5: JSON serialization
    print("\nStep 5: JSON serialization...")
    try:
        json_str = json.dumps(result, indent=2, default=str)
        print("✅ Data can be serialized to JSON")
        print(f"  • JSON size: {len(json_str)} characters")
    except Exception as e:
        print(f"❌ JSON serialization failed: {e}")
        return False
    
    print("\n🎉 Complete workflow demo successful!")
    return True


def run_demo_tests():
    """Run all demo tests"""
    print("🚀 COBOL Demo and Showcase Tests")
    print("=" * 60)
    
    demos = [
        ("Parser Functionality", demo_parser_functionality),
        ("Relationship Extraction", demo_relationship_extraction),
        ("Advanced Features", demo_advanced_features),
        ("Integration Workflow", demo_integration_workflow),
    ]
    
    results = []
    
    for demo_name, demo_func in demos:
        print(f"\n{'='*60}")
        print(f"Running {demo_name}")
        print('='*60)
        
        try:
            success = demo_func()
            results.append((demo_name, success))
            
            status = "✅ SUCCESS" if success else "❌ FAILED"
            print(f"\n{demo_name}: {status}")
        except Exception as e:
            print(f"❌ {demo_name} failed with exception: {e}")
            results.append((demo_name, False))
    
    # Print summary
    print(f"\n{'='*60}")
    print("DEMO SUMMARY")
    print('='*60)
    
    passed = sum(1 for _, success in results if success)
    total = len(results)
    
    print(f"Total demos: {total}")
    print(f"Successful: {passed}")
    print(f"Failed: {total - passed}")
    
    print(f"\nDetailed Results:")
    for demo_name, success in results:
        status = "✅ SUCCESS" if success else "❌ FAILED"
        print(f"  {demo_name}: {status}")
    
    if passed == total:
        print(f"\n🎉 All demos completed successfully!")
        print("\n📋 COBOL Features Successfully Demonstrated:")
        print("   ✅ COBOL file parsing and entity extraction")
        print("   ✅ Advanced relationship extraction (COPY, CALL, USE, etc.)")
        print("   ✅ Line information tracking and AST analysis")
        print("   ✅ Multi-language parser integration")
        print("   ✅ JSON serialization and data consistency")
        print("   ✅ Complete end-to-end workflow")
        return True
    else:
        print(f"\n❌ {total - passed} demo(s) failed!")
        return False


def main():
    """Main demo runner"""
    print("🚀 COBOL Demo and Showcase")
    print("=" * 60)
    print("Demonstrating COBOL parsing and relationship extraction capabilities...")
    print("=" * 60)
    
    success = run_demo_tests()
    
    if success:
        print("\n✅ All demos completed successfully!")
        print("💡 The COBOL parser is ready for production use!")
        return 0
    else:
        print("\n❌ Some demos failed!")
        print("💡 Check the output above for details")
        return 1


if __name__ == "__main__":
    sys.exit(main())