#!/usr/bin/env python3
"""
Test COBOL Integration with Code Grapher

This test verifies that COBOL parsing is properly integrated into the system,
including entity extraction, relationship extraction, and pipeline integration.
"""

import os
import sys
import json
from pathlib import Path
from typing import Dict, List, Any

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

def test_cobol_parser_import():
    """Test that COBOL parser can be imported"""
    print("🧪 Testing COBOL parser import...")
    
    try:
        # Try importing from the correct path
        import sys
        import os
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
        from services.cobol_parser import COBOLParser
        print("   ✅ COBOL parser imported successfully")
        return True
    except ImportError as e:
        print(f"   ❌ Failed to import COBOL parser: {e}")
        return False

def test_cobol_parser_initialization():
    """Test COBOL parser initialization"""
    print("🧪 Testing COBOL parser initialization...")
    
    try:
        import sys
        import os
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
        from services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if parser.is_available():
            print("   ✅ COBOL parser initialized successfully")
            return True
        else:
            print("   ⚠️  COBOL parser not available (Java/Maven dependencies missing)")
            return False
    except Exception as e:
        print(f"   ❌ Failed to initialize COBOL parser: {e}")
        return False

def test_cobol_file_parsing():
    """Test parsing of COBOL file"""
    print("🧪 Testing COBOL file parsing...")
    
    try:
        import sys
        import os
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
        from services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if not parser.is_available():
            print("   ⚠️  Skipping - COBOL parser not available")
            return False
        
        # Test file path
        test_file = "cobol-support/tests/fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            print(f"   ❌ Test file not found: {test_file}")
            return False
        
        # Parse the COBOL file
        result = parser.parse_file(test_file)
        
        # Verify parsing was successful
        if not result.get("parse_success", False):
            print(f"   ❌ COBOL parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        print("   ✅ COBOL file parsed successfully")
        
        # Verify language detection
        if result.get("language") != "cobol":
            print(f"   ❌ Language not detected as COBOL: {result.get('language')}")
            return False
        
        print("   ✅ Language correctly detected as COBOL")
        
        # Verify entities were extracted
        entities = result.get("entities", [])
        if not entities:
            print("   ❌ No entities extracted from COBOL file")
            return False
        
        print(f"   ✅ Extracted {len(entities)} entities from COBOL file")
        
        # Verify compilation units
        compilation_units = result.get("compilation_units", [])
        if not compilation_units:
            print("   ❌ No compilation units extracted")
            return False
        
        print(f"   ✅ Extracted {len(compilation_units)} compilation units")
        
        return True
        
    except Exception as e:
        print(f"   ❌ COBOL file parsing test failed: {e}")
        return False

def test_cobol_entity_extraction():
    """Test detailed COBOL entity extraction"""
    print("🧪 Testing COBOL entity extraction details...")
    
    try:
        import sys
        import os
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))
        from services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if not parser.is_available():
            print("   ⚠️  Skipping - COBOL parser not available")
            return False
        
        test_file = "cobol-support/tests/fixtures/test_cobol_banking.cbl"
        result = parser.parse_file(test_file)
        
        if not result.get("parse_success", False):
            print(f"   ❌ COBOL parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        entities = result.get("entities", [])
        
        # Check for different entity types
        entity_types = set(entity.get("type") for entity in entities)
        print(f"   📊 Found entity types: {entity_types}")
        
        # Verify we have at least some entities
        if len(entities) == 0:
            print("   ❌ No entities extracted")
            return False
        
        # Check for program entity
        program_entities = [e for e in entities if e.get("type") == "cobol_program"]
        if not program_entities:
            print("   ⚠️  No program entities found")
        else:
            print(f"   ✅ Found {len(program_entities)} program entities")
        
        # Check for data entities
        data_entities = [e for e in entities if e.get("type") == "cobol_data_item"]
        if not data_entities:
            print("   ⚠️  No data entities found")
        else:
            print(f"   ✅ Found {len(data_entities)} data entities")
        
        # Check for procedure entities
        proc_entities = [e for e in entities if e.get("type") in ["cobol_paragraph", "cobol_section", "cobol_procedure"]]
        if not proc_entities:
            print("   ⚠️  No procedure entities found")
        else:
            print(f"   ✅ Found {len(proc_entities)} procedure entities")
        
        return True
        
    except Exception as e:
        print(f"   ❌ COBOL entity extraction test failed: {e}")
        return False

def test_cobol_relationship_extraction():
    """Test COBOL relationship extraction"""
    print("🧪 Testing COBOL relationship extraction...")
    
    try:
        from ast_relationship_extractor import ASTRelationshipExtractor
        
        extractor = ASTRelationshipExtractor()
        
        # Test with a simple COBOL parsing result
        mock_cobol_result = {
            "parse_success": True,
            "language": "cobol",
            "file_path": "test.cbl",
            "entities": [
                {
                    "type": "cobol_program",
                    "name": "TEST-PROGRAM",
                    "file_path": "test.cbl",
                    "line_number": 1
                }
            ],
            "compilation_units": []
        }
        
        relationships = extractor.extract_relationships(mock_cobol_result)
        
        if not isinstance(relationships, list):
            print("   ❌ Relationships should be a list")
            return False
        
        print(f"   ✅ Extracted {len(relationships)} relationships")
        return True
        
    except Exception as e:
        print(f"   ❌ COBOL relationship extraction test failed: {e}")
        return False

def test_multi_language_parser_integration():
    """Test COBOL integration with multi-language parser"""
    print("🧪 Testing COBOL integration with multi-language parser...")
    
    try:
        from shared.services.multi_language_parser import MultiLanguageParser
        
        parser = MultiLanguageParser()
        
        # Test COBOL file detection
        test_file = "cobol-support/tests/fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            print(f"   ❌ Test file not found: {test_file}")
            return False
        
        # Test language detection
        language = parser.detect_language(test_file)
        if language != "cobol":
            print(f"   ❌ Language not detected as COBOL: {language}")
            return False
        
        print("   ✅ Language correctly detected as COBOL")
        
        # Test parsing through multi-language parser
        result = parser.parse_file(test_file)
        
        if not result:
            print("   ❌ Multi-language parser returned no result")
            return False
        
        if not result.get("parse_success", False):
            print(f"   ❌ Multi-language parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        print("   ✅ Multi-language parser successfully parsed COBOL file")
        return True
        
    except Exception as e:
        print(f"   ❌ Multi-language parser integration test failed: {e}")
        return False

def test_cobol_pipeline_integration():
    """Test COBOL integration with the full pipeline"""
    print("🧪 Testing COBOL pipeline integration...")
    
    try:
        from shared.services.multi_language_parser import MultiLanguageParser
        from ast_relationship_extractor import ASTRelationshipExtractor
        
        # Initialize components
        parser = MultiLanguageParser()
        extractor = ASTRelationshipExtractor()
        
        test_file = "cobol-support/tests/fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            print(f"   ❌ Test file not found: {test_file}")
            return False
        
        # Parse the file
        parse_result = parser.parse_file(test_file)
        
        if not parse_result or not parse_result.get("parse_success", False):
            print(f"   ❌ Parsing failed: {parse_result.get('error', 'Unknown error') if parse_result else 'No result'}")
            return False
        
        print("   ✅ File parsed successfully")
        
        # Extract relationships
        relationships = extractor.extract_relationships(parse_result)
        
        if not isinstance(relationships, list):
            print("   ❌ Relationships should be a list")
            return False
        
        print(f"   ✅ Extracted {len(relationships)} relationships")
        
        # Verify we have entities
        entities = parse_result.get("entities", [])
        if not entities:
            print("   ❌ No entities found in parse result")
            return False
        
        print(f"   ✅ Found {len(entities)} entities")
        
        return True
        
    except Exception as e:
        print(f"   ❌ COBOL pipeline integration test failed: {e}")
        return False

def main():
    """Run all COBOL integration tests"""
    print("\n🚀 Running COBOL Integration Tests")
    print("=" * 50)
    
    tests = [
        ("COBOL Parser Import", test_cobol_parser_import),
        ("COBOL Parser Initialization", test_cobol_parser_initialization),
        ("COBOL File Parsing", test_cobol_file_parsing),
        ("COBOL Entity Extraction", test_cobol_entity_extraction),
        ("COBOL Relationship Extraction", test_cobol_relationship_extraction),
        ("Multi-Language Parser Integration", test_multi_language_parser_integration),
        ("COBOL Pipeline Integration", test_cobol_pipeline_integration),
    ]
    
    results = []
    
    for test_name, test_func in tests:
        print(f"\n{test_name}:")
        try:
            result = test_func()
            results.append((test_name, result))
        except Exception as e:
            print(f"   ❌ Test failed with exception: {e}")
            results.append((test_name, False))
    
    # Print summary
    print("\n" + "=" * 50)
    print("📊 TEST RESULTS SUMMARY")
    print("=" * 50)
    
    passed = 0
    for test_name, result in results:
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{test_name}: {status}")
        if result:
            passed += 1
    
    print(f"\nOverall: {passed}/{len(results)} tests passed")
    
    if passed == len(results):
        print("🎉 All tests passed!")
    else:
        print("⚠️  Some tests failed. Check the output above for details.")
    
    # Print expected output sample
    print("\n📋 Expected COBOL Parsing Output Sample:")
    print("=" * 50)
    expected_output = {
        "parse_success": True,
        "language": "cobol",
        "file_path": "test_cobol_banking.cbl",
        "compilation_units": [
            {
                "name": "BANKING-SYSTEM",
                "program_id": "BANKING-SYSTEM",
                "divisions": {
                    "identification": {
                        "program_id": "BANKING-SYSTEM",
                        "author": "CODE-GRAPHER-TEST",
                        "date_written": "2025-01-27"
                    },
                    "data": {
                        "working_storage": [
                            {
                                "name": "WS-ACCOUNT-NUMBER",
                                "level": 5,
                                "data_type": "alphanumeric",
                                "picture": "X(10)",
                                "line_number": 25
                            }
                        ]
                    },
                    "procedure": {
                        "paragraphs": [
                            {
                                "name": "0000-MAIN-LOGIC",
                                "line_number": 85,
                                "statements": [
                                    "PERFORM 1000-INITIALIZE"
                                ]
                            }
                        ]
                    }
                }
            }
        ],
        "entities": [
            {
                "type": "cobol_program",
                "name": "BANKING-SYSTEM",
                "file_path": "test_cobol_banking.cbl",
                "line_number": 1,
                "context": "COBOL program definition",
                "language": "cobol"
            }
        ]
    }
    print(json.dumps(expected_output, indent=2))

if __name__ == "__main__":
    main()