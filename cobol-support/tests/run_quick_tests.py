#!/usr/bin/env python3
"""
Quick COBOL Test Runner

This script runs a fast subset of critical tests for development and CI.
Focuses on basic functionality verification without comprehensive testing.
"""

import os
import sys
import time
from pathlib import Path

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))


def quick_parser_test():
    """Quick parser availability and basic functionality test"""
    print("🧪 Quick Parser Test")
    print("-" * 30)
    
    try:
        from services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        
        # Test availability
        if not parser.is_available():
            print("⚠️  COBOL parser not available (Java/Maven dependencies missing)")
            return False
        
        print("✅ COBOL parser available")
        
        # Test with a simple file
        test_file = "fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            print(f"❌ Test file not found: {test_file}")
            return False
        
        print(f"📄 Testing with file: {test_file}")
        
        result = parser.parse_file(test_file)
        
        if not result.get("parse_success", False):
            print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        print("✅ Parsing successful")
        
        entities = result.get("entities", [])
        print(f"📊 Found {len(entities)} entities")
        
        # Check line information
        line_info_correct = 0
        for entity in entities:
            properties = entity.get("properties", {})
            if all(key in properties for key in ["line", "line_count", "start_line", "end_line"]):
                start_line = properties["start_line"]
                end_line = properties["end_line"]
                line_count = properties["line_count"]
                expected_count = end_line - start_line + 1
                
                if line_count == expected_count:
                    line_info_correct += 1
        
        success_rate = line_info_correct / len(entities) * 100 if entities else 0
        print(f"📈 Line info success rate: {success_rate:.1f}%")
        
        return success_rate > 90  # Expect at least 90% success rate
        
    except Exception as e:
        print(f"❌ Quick parser test failed: {e}")
        return False


def quick_relationship_test():
    """Quick relationship extraction test"""
    print("🧪 Quick Relationship Test")
    print("-" * 30)
    
    try:
        # Add the cobol-support path
        import sys
        cobol_support_path = os.path.join(os.path.dirname(__file__), '..')
        if cobol_support_path not in sys.path:
            sys.path.insert(0, cobol_support_path)
        
        # Add the project root path for ai-services
        project_root = os.path.join(os.path.dirname(__file__), '..', '..')
        if project_root not in sys.path:
            sys.path.insert(0, project_root)
        
        from services.cobol_relationship_extractor import extract_cobol_relationships
        import sys
        sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', '..', 'ai-services'))
        from models.relationship_models import RelationshipType
        
        # Mock COBOL data
        mock_data = {
            "parse_success": True,
            "language": "cobol",
            "file_path": "test.cbl",
            "compilation_units": [{"name": "TEST-PROGRAM", "type": "program"}],
            "entities": [
                {"type": "program", "name": "TEST-PROGRAM", "file_path": "test.cbl"},
                {"type": "paragraph", "name": "MAIN-LOGIC", "file_path": "test.cbl"}
            ],
            "statements": {
                "TEST-PROGRAM": {
                    "MAIN-LOGIC": [
                        {"type": "PerformStatement", "text": "PERFORM INITIALIZE", "details": "PERFORM INITIALIZE"}
                    ]
                }
            }
        }
        
        relationships = extract_cobol_relationships(mock_data)
        
        if not relationships:
            print("❌ No relationships extracted")
            return False
        
        print(f"✅ Extracted {len(relationships)} relationships")
        
        # Check for basic relationship types
        rel_types = set(rel.relationship_type for rel in relationships)
        print(f"📊 Relationship types: {[rt.value for rt in rel_types]}")
        
        # Should have at least CONTAINS relationships
        contains_rels = [r for r in relationships if r.relationship_type == RelationshipType.CONTAINS]
        if not contains_rels:
            print("❌ No CONTAINS relationships found")
            return False
        
        print("✅ Basic relationships working")
        return True
        
    except Exception as e:
        print(f"❌ Quick relationship test failed: {e}")
        return False


def quick_integration_test():
    """Quick integration test"""
    print("🧪 Quick Integration Test")
    print("-" * 30)
    
    try:
        # Add the cobol-support path
        import sys
        cobol_support_path = os.path.join(os.path.dirname(__file__), '..')
        if cobol_support_path not in sys.path:
            sys.path.insert(0, cobol_support_path)
        
        from services.cobol_parser import COBOLParser
        
        parser = COBOLParser()
        
        if not parser.is_available():
            print("⚠️  Skipping - COBOL parser not available")
            return False
        
        # Test file path
        test_file = "fixtures/test_cobol_banking.cbl"
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            print(f"❌ Test file not found: {test_file}")
            return False
        
        # Parse the file
        result = parser.parse_file(test_file)
        
        if not result.get("parse_success", False):
            print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
            return False
        
        print("✅ File parsed successfully")
        
        # Check basic structure
        required_keys = ["parse_success", "language", "file_path", "entities"]
        for key in required_keys:
            if key not in result:
                print(f"❌ Missing required key: {key}")
                return False
        
        print("✅ Basic structure correct")
        
        # Check language detection
        if result.get("language") != "cobol":
            print(f"❌ Language not detected as COBOL: {result.get('language')}")
            return False
        
        print("✅ Language detection correct")
        
        # Check entities
        entities = result.get("entities", [])
        if not entities:
            print("❌ No entities extracted")
            return False
        
        print(f"✅ Extracted {len(entities)} entities")
        
        return True
        
    except Exception as e:
        print(f"❌ Quick integration test failed: {e}")
        return False


def run_quick_tests():
    """Run all quick tests"""
    print("🚀 Quick COBOL Tests")
    print("=" * 50)
    
    start_time = time.time()
    
    tests = [
        ("Parser", quick_parser_test),
        ("Relationships", quick_relationship_test),
        ("Integration", quick_integration_test),
    ]
    
    results = []
    
    for test_name, test_func in tests:
        print(f"\n{test_name} Test:")
        test_start = time.time()
        success = test_func()
        test_end = time.time()
        
        results.append({
            'name': test_name,
            'success': success,
            'duration': test_end - test_start
        })
        
        status = "✅ PASSED" if success else "❌ FAILED"
        print(f"{test_name}: {status} ({test_end - test_start:.2f}s)")
    
    end_time = time.time()
    total_duration = end_time - start_time
    
    # Print summary
    print(f"\n{'='*50}")
    print("QUICK TEST SUMMARY")
    print('='*50)
    
    passed = sum(1 for r in results if r['success'])
    total = len(results)
    
    print(f"Total tests: {total}")
    print(f"Passed: {passed}")
    print(f"Failed: {total - passed}")
    print(f"Total duration: {total_duration:.2f}s")
    
    print(f"\nDetailed Results:")
    for result in results:
        status = "✅ PASSED" if result['success'] else "❌ FAILED"
        print(f"  {result['name']}: {status} ({result['duration']:.2f}s)")
    
    if passed == total:
        print(f"\n🎉 All quick tests passed!")
        return True
    else:
        print(f"\n❌ {total - passed} test(s) failed!")
        return False


def main():
    """Main quick test runner"""
    print("🚀 COBOL Quick Test Runner")
    print("=" * 50)
    print("Running fast subset of critical tests...")
    print("=" * 50)
    
    success = run_quick_tests()
    
    if success:
        print("\n✅ Quick tests completed successfully!")
        print("💡 For comprehensive testing, run: python run_tests.py --mode all")
        return 0
    else:
        print("\n❌ Some quick tests failed!")
        print("💡 Check the output above for details")
        return 1


if __name__ == "__main__":
    sys.exit(main())
