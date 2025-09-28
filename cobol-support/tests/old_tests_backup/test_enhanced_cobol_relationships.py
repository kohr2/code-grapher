"""
Test enhanced COBOL relationship extraction
"""

import os
import sys
import json
from pathlib import Path

# Add the cobol-support directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from services.cobol_parser import COBOLParser
from services.cobol_relationship_extractor import extract_cobol_relationships

# Add the project root to the path for imports
sys.path.append(os.path.join(os.path.dirname(__file__), '..', '..'))
from ai_services.models.relationship_models import RelationshipType


def test_enhanced_cobol_relationships():
    """Test enhanced COBOL relationship extraction"""
    print("🧪 Testing enhanced COBOL relationship extraction...")
    
    try:
        parser = COBOLParser()
        if not parser.is_available():
            print("   ⚠️  Skipping - COBOL parser not available")
            return False
        
        # Create a test COBOL file with advanced features
        test_file = "fixtures/test_enhanced_cobol.cbl"
        create_test_cobol_file(test_file)
        
        # Parse the file
        result = parser.parse_file(test_file)
        
        if not result.get("parse_success", False):
            print(f"   ❌ COBOL parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        print("   ✅ File parsed successfully")
        
        # Check for relationships
        relationships = result.get("relationships", [])
        relationship_count = result.get("relationship_count", 0)
        
        print(f"   📊 Found {relationship_count} relationships")
        
        if relationship_count == 0:
            print("   ⚠️  No relationships extracted")
            return False
        
        # Check for specific relationship types
        relationship_types = set(rel.relationship_type for rel in relationships)
        print(f"   📈 Relationship types: {[rt.value for rt in relationship_types]}")
        
        # Verify we have the expected relationship types
        expected_types = {
            RelationshipType.INCLUDES,
            RelationshipType.CALLS,
            RelationshipType.PASSES_DATA,
            RelationshipType.HANDLES_ERRORS,
            RelationshipType.USES_QUEUE,
            RelationshipType.BINDS_SCREEN,
            RelationshipType.PERFORMS,
            RelationshipType.REPLACES
        }
        
        found_types = relationship_types.intersection(expected_types)
        print(f"   ✅ Found {len(found_types)} expected relationship types: {[rt.value for rt in found_types]}")
        
        # Print sample relationships
        print("   📋 Sample relationships:")
        for i, rel in enumerate(relationships[:5]):  # Show first 5
            print(f"      {i+1}. {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
            print(f"         Context: {rel.context}")
        
        # Clean up test file
        if os.path.exists(test_file):
            os.remove(test_file)
        
        return True
        
    except Exception as e:
        print(f"   ❌ Enhanced COBOL relationship test failed: {e}")
        return False


def create_test_cobol_file(file_path: str):
    """Create a test COBOL file with advanced features"""
    test_content = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ENHANCED-TEST.
       AUTHOR. TEST-AUTHOR.
       DATE-WRITTEN. 2025-01-27.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-370.
       OBJECT-COMPUTER. IBM-370.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-ACCOUNT-NUMBER PIC X(10).
       01 WS-BALANCE PIC 9(13)V99.
       01 WS-INTEREST-RATE PIC 9V9999.
       
       COMMUNICATION SECTION.
       CD COMM-AREA FOR INPUT
          SYMBOLIC QUEUE IS WS-QUEUE
          SYMBOLIC DESTINATION IS WS-DEST.
       
       SCREEN SECTION.
       01 MAIN-SCREEN.
          05 VALUE "ACCOUNT NUMBER:" LINE 5 COL 10.
          05 ACCOUNT-FIELD PIC X(10) FROM WS-ACCOUNT-NUMBER
              TO WS-ACCOUNT-NUMBER.
       
       LINKAGE SECTION.
       01 LK-ACCOUNT-NUMBER PIC X(10).
       01 LK-BALANCE PIC 9(13)V99.
       
       PROCEDURE DIVISION USING LK-ACCOUNT-NUMBER LK-BALANCE.
       
       DECLARATIVES.
       ERROR-HANDLING SECTION.
           USE AFTER ERROR ON ACCOUNT-FILE.
           DISPLAY "ERROR OCCURRED".
       END DECLARATIVES.
       
       MAIN-LOGIC.
           COPY BANKING-COPYBOOK REPLACING ==ACCOUNT== BY ==CUSTOMER==.
           PERFORM INITIALIZE-PROGRAM.
           CALL "INTEREST-CALCULATOR" USING WS-ACCOUNT-NUMBER WS-BALANCE
                GIVING WS-INTEREST-RATE.
           PERFORM DISPLAY-RESULTS.
           STOP RUN.
       
       INITIALIZE-PROGRAM.
           MOVE ZEROS TO WS-BALANCE.
           MOVE 0.05 TO WS-INTEREST-RATE.
       
       DISPLAY-RESULTS.
           DISPLAY "ACCOUNT: " WS-ACCOUNT-NUMBER.
           DISPLAY "BALANCE: " WS-BALANCE.
       
       END PROGRAM ENHANCED-TEST.
    """
    
    # Ensure directory exists
    os.makedirs(os.path.dirname(file_path), exist_ok=True)
    
    with open(file_path, 'w') as f:
        f.write(test_content)


def test_relationship_extractor_directly():
    """Test the relationship extractor directly with mock data"""
    print("🧪 Testing COBOL relationship extractor directly...")
    
    try:
        extractor = COBOLRelationshipExtractor()
        
        # Mock COBOL data with advanced features
        mock_data = {
            "parse_success": True,
            "copy_statements": {
                "ENHANCED-TEST": [
                    {"name": "BANKING-COPYBOOK", "library": "COMMON", "unit": "ENHANCED-TEST"}
                ]
            },
            "replacing_phrases": {
                "ENHANCED-TEST": {
                    "BANKING-COPYBOOK": [
                        {"replaceable": "ACCOUNT", "replacement": "CUSTOMER"}
                    ]
                }
            },
            "call_statements": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "unit": "ENHANCED-TEST"}
                    ]
                }
            },
            "call_parameters": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "param_type": "REFERENCE", "param_name": "WS-ACCOUNT-NUMBER"}
                    ]
                }
            },
            "call_giving": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "giving_param": "WS-INTEREST-RATE"}
                    ]
                }
            },
            "use_statements": {
                "ENHANCED-TEST": [
                    {"use_type": "ERROR", "file_name": "ACCOUNT-FILE", "procedure_name": "", "unit": "ENHANCED-TEST"}
                ]
            },
            "communication": {
                "ENHANCED-TEST": [
                    {"name": "COMM-AREA", "type": "INPUT", "symbolic_queue": "WS-QUEUE", "symbolic_destination": "WS-DEST", "unit": "ENHANCED-TEST"}
                ]
            },
            "screens": {
                "ENHANCED-TEST": [
                    {"name": "MAIN-SCREEN", "value": "ACCOUNT NUMBER:", "from": "WS-ACCOUNT-NUMBER", "to": "WS-ACCOUNT-NUMBER", "unit": "ENHANCED-TEST"}
                ]
            },
            "statements": {
                "ENHANCED-TEST": {
                    "MAIN-LOGIC": [
                        {"text": "PERFORM INITIALIZE-PROGRAM", "type": "PERFORM", "details": ""}
                    ]
                }
            }
        }
        
        relationships = extractor.extract_relationships(mock_data)
        
        print(f"   📊 Extracted {len(relationships)} relationships")
        
        # Check relationship types
        relationship_types = set(rel.relationship_type for rel in relationships)
        print(f"   📈 Relationship types: {[rt.value for rt in relationship_types]}")
        
        # Verify specific relationships
        includes_rels = [r for r in relationships if r.relationship_type == RelationshipType.INCLUDES]
        calls_rels = [r for r in relationships if r.relationship_type == RelationshipType.CALLS]
        passes_data_rels = [r for r in relationships if r.relationship_type == RelationshipType.PASSES_DATA]
        
        print(f"   ✅ INCLUDES relationships: {len(includes_rels)}")
        print(f"   ✅ CALLS relationships: {len(calls_rels)}")
        print(f"   ✅ PASSES_DATA relationships: {len(passes_data_rels)}")
        
        # Print sample relationships
        print("   📋 Sample relationships:")
        for i, rel in enumerate(relationships[:3]):
            print(f"      {i+1}. {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
        
        return len(relationships) > 0
        
    except Exception as e:
        print(f"   ❌ Direct relationship extractor test failed: {e}")
        return False


def main():
    """Run all enhanced COBOL relationship tests"""
    print("🚀 Running Enhanced COBOL Relationship Tests")
    print("=" * 50)
    
    tests = [
        ("Direct Relationship Extractor", test_relationship_extractor_directly),
        ("Enhanced COBOL Relationships", test_enhanced_cobol_relationships),
    ]
    
    passed = 0
    total = len(tests)
    
    for test_name, test_func in tests:
        print(f"\n🧪 {test_name}")
        if test_func():
            print(f"   ✅ {test_name} PASSED")
            passed += 1
        else:
            print(f"   ❌ {test_name} FAILED")
    
    print("\n" + "=" * 50)
    print(f"📊 Results: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All enhanced COBOL relationship tests passed!")
        return True
    else:
        print("⚠️  Some tests failed")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
