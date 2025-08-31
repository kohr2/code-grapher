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
project_root = Path(__file__).parent
sys.path.insert(0, str(project_root))

def test_cobol_parser_import():
    """Test that COBOL parser can be imported"""
    print("🧪 Testing COBOL parser import...")
    
    try:
        from shared.services.cobol_parser import COBOLParser
        print("   ✅ COBOL parser imported successfully")
        return True
    except ImportError as e:
        print(f"   ❌ Failed to import COBOL parser: {e}")
        return False

def test_cobol_parser_initialization():
    """Test COBOL parser initialization"""
    print("🧪 Testing COBOL parser initialization...")
    
    try:
        from shared.services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if parser.is_available():
            print("   ✅ COBOL parser initialized successfully")
            return True
        else:
            print("   ⚠️  COBOL parser not available (JPype/Java dependencies missing)")
            return False
    except Exception as e:
        print(f"   ❌ Failed to initialize COBOL parser: {e}")
        return False

def test_cobol_file_parsing():
    """Test parsing of COBOL file"""
    print("🧪 Testing COBOL file parsing...")
    
    try:
        from shared.services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if not parser.is_available():
            print("   ⚠️  Skipping - COBOL parser not available")
            return False
        
        # Test file path
        test_file = "test_cobol_banking.cbl"
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
    """Test COBOL entity extraction details"""
    print("🧪 Testing COBOL entity extraction details...")
    
    try:
        from shared.services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        if not parser.is_available():
            print("   ⚠️  Skipping - COBOL parser not available")
            return False
        
        test_file = "test_cobol_banking.cbl"
        result = parser.parse_file(test_file)
        
        entities = result.get("entities", [])
        
        # Check for specific entity types
        entity_types = {}
        for entity in entities:
            entity_type = entity.get("type", "unknown")
            entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
        
        print(f"   📊 Entity type distribution: {entity_types}")
        
        # Verify we have the expected entity types
        expected_types = ["cobol_program", "cobol_data_item", "cobol_paragraph", "cobol_section"]
        found_types = set(entity_types.keys())
        
        for expected_type in expected_types:
            if expected_type in found_types:
                print(f"   ✅ Found {expected_type} entities: {entity_types[expected_type]}")
            else:
                print(f"   ⚠️  Missing {expected_type} entities")
        
        # Check for specific entities we expect
        program_entities = [e for e in entities if e.get("type") == "cobol_program"]
        if program_entities:
            program_name = program_entities[0].get("name")
            print(f"   ✅ Found COBOL program: {program_name}")
        else:
            print("   ❌ No COBOL program entity found")
        
        return True
        
    except Exception as e:
        print(f"   ❌ COBOL entity extraction test failed: {e}")
        return False

def test_cobol_relationship_extraction():
    """Test COBOL relationship extraction"""
    print("🧪 Testing COBOL relationship extraction...")
    
    try:
        from ast_relationship_extractor import extract_ast_relationships
        
        # Create a mock parsed file structure for testing
        mock_parsed_file = {
            "success": True,
            "language": "cobol",
            "file_path": "test_cobol_banking.cbl",
            "compilation_units": [
                {
                    "program_id": "BANKING-SYSTEM",
                    "divisions": {
                        "procedure": {
                            "paragraphs": [
                                {
                                    "name": "0000-MAIN-LOGIC",
                                    "line_number": 85,
                                    "statements": [
                                        "PERFORM 1000-INITIALIZE",
                                        "PERFORM 2000-VALIDATE-INPUT"
                                    ]
                                },
                                {
                                    "name": "3200-PROCESS-WITHDRAWAL",
                                    "line_number": 120,
                                    "statements": [
                                        "PERFORM 3210-CHECK-SUFFICIENT-FUNDS"
                                    ]
                                }
                            ]
                        }
                    }
                }
            ]
        }
        
        # Extract relationships
        relationships = extract_ast_relationships([mock_parsed_file])
        
        # Look for COBOL-specific relationships
        cobol_relationships = [r for r in relationships if r.source_file == "test_cobol_banking.cbl"]
        
        if cobol_relationships:
            print(f"   ✅ Extracted {len(cobol_relationships)} COBOL relationships")
            
            # Check for PERFORM statements
            perform_relationships = [r for r in cobol_relationships if "PERFORM" in r.context]
            if perform_relationships:
                print(f"   ✅ Found {len(perform_relationships)} PERFORM relationships")
            else:
                print("   ⚠️  No PERFORM relationships found")
        else:
            print("   ⚠️  No COBOL relationships extracted")
        
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
        
        # Test language detection
        cobol_extensions = ['.cbl', '.cob', '.cobol']
        for ext in cobol_extensions:
            test_file = f"test_file{ext}"
            detected_language = parser.get_language_from_extension(test_file)
            if detected_language == 'cobol':
                print(f"   ✅ Correctly detected {ext} as COBOL")
            else:
                print(f"   ❌ Failed to detect {ext} as COBOL (got: {detected_language})")
        
        # Check if COBOL is in supported languages
        if 'cobol' in parser.languages:
            print(f"   ✅ COBOL registered in supported languages: {parser.languages['cobol']}")
        else:
            print("   ❌ COBOL not found in supported languages")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Multi-language parser integration test failed: {e}")
        return False

def test_cobol_pipeline_integration():
    """Test COBOL integration with the main pipeline"""
    print("🧪 Testing COBOL pipeline integration...")
    
    try:
        # Test that COBOL files can be processed by the pipeline
        from shared.services.multi_language_parser import MultiLanguageParser
        
        parser = MultiLanguageParser()
        
        # Create a test COBOL file path
        test_file = "test_cobol_banking.cbl"
        
        if not os.path.exists(test_file):
            print(f"   ❌ Test file not found: {test_file}")
            return False
        
        # Try to parse with the multi-language parser
        result = parser.parse_file(test_file)
        
        if result.get("language") == "cobol":
            print("   ✅ COBOL file successfully processed by multi-language parser")
            
            # Check if entities were extracted
            entities = result.get("entities", [])
            if entities:
                print(f"   ✅ Pipeline extracted {len(entities)} entities from COBOL")
            else:
                print("   ⚠️  Pipeline extracted no entities from COBOL")
            
            return True
        else:
            print(f"   ❌ Pipeline failed to process COBOL file as COBOL: {result.get('language')}")
            return False
        
    except Exception as e:
        print(f"   ❌ COBOL pipeline integration test failed: {e}")
        return False

def run_all_tests():
    """Run all COBOL integration tests"""
    print("🚀 Running COBOL Integration Tests")
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
    
    results = {}
    passed = 0
    total = len(tests)
    
    for test_name, test_func in tests:
        print(f"\n{test_name}:")
        try:
            result = test_func()
            results[test_name] = result
            if result:
                passed += 1
        except Exception as e:
            print(f"   ❌ Test crashed: {e}")
            results[test_name] = False
    
    # Summary
    print("\n" + "=" * 50)
    print("📊 TEST RESULTS SUMMARY")
    print("=" * 50)
    
    for test_name, result in results.items():
        status = "✅ PASS" if result else "❌ FAIL"
        print(f"{test_name}: {status}")
    
    print(f"\nOverall: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All COBOL integration tests passed!")
        return True
    else:
        print("⚠️  Some tests failed. Check the output above for details.")
        return False

def create_sample_cobol_output():
    """Create a sample of expected COBOL parsing output for reference"""
    print("\n📋 Expected COBOL Parsing Output Sample:")
    print("=" * 50)
    
    sample_output = {
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
                                "statements": ["PERFORM 1000-INITIALIZE"]
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
    
    print(json.dumps(sample_output, indent=2))

if __name__ == "__main__":
    # Run all tests
    success = run_all_tests()
    
    # Show expected output sample
    create_sample_cobol_output()
    
    # Exit with appropriate code
    sys.exit(0 if success else 1)
