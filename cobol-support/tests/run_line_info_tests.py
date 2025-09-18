#!/usr/bin/env python3
"""
Quick Test Runner for COBOL Line Information

This script provides a quick way to test the COBOL line information functionality
without running the full test suite.
"""

import os
import sys
from pathlib import Path

# Add the project root to the path
project_root = Path(__file__).parent.parent.parent
sys.path.insert(0, str(project_root))

# Add cobol-support to the path
cobol_support_path = Path(__file__).parent.parent
sys.path.insert(0, str(cobol_support_path))

from services.cobol_parser import COBOLParser


def quick_test():
    """Run a quick test of the line information functionality"""
    print("🧪 Quick Test: COBOL Line Information")
    print("=" * 40)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available")
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
    line_info_incorrect = 0
    
    for entity in entities:
        properties = entity.get("properties", {})
        
        # Check if line information is present and correct
        has_line = "line" in properties
        has_line_count = "line_count" in properties
        has_start_line = "start_line" in properties
        has_end_line = "end_line" in properties
        
        if has_line and has_line_count and has_start_line and has_end_line:
            # Check consistency
            start_line = properties["start_line"]
            end_line = properties["end_line"]
            line_count = properties["line_count"]
            expected_count = end_line - start_line + 1
            
            if line_count == expected_count:
                line_info_correct += 1
            else:
                line_info_incorrect += 1
                print(f"⚠️  Inconsistent line info for {entity.get('name', 'unknown')}: "
                      f"count={line_count}, expected={expected_count}")
        else:
            line_info_incorrect += 1
            print(f"⚠️  Missing line info for {entity.get('name', 'unknown')}")
    
    print(f"✅ Correct line info: {line_info_correct}")
    print(f"❌ Incorrect line info: {line_info_incorrect}")
    
    # Show sample entities
    print("\n📋 Sample entities with line information:")
    for i, entity in enumerate(entities[:5]):
        properties = entity.get("properties", {})
        print(f"  {i+1}. {entity.get('name', 'unknown')} ({entity.get('type', 'unknown')})")
        print(f"     Line: {properties.get('line', 'unknown')}")
        print(f"     Count: {properties.get('line_count', 'unknown')}")
        print(f"     Range: {properties.get('start_line', 'unknown')}-{properties.get('end_line', 'unknown')}")
        print()
    
    success_rate = line_info_correct / len(entities) * 100 if entities else 0
    print(f"📈 Success rate: {success_rate:.1f}%")
    
    return line_info_incorrect == 0


def test_vasu_file():
    """Test with the larger Vasu file"""
    print("\n🧪 Testing with Vasu file")
    print("=" * 40)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available")
        return False
    
    vasu_file = "fixtures/vasu/vasu_fraud_management_cobol_reformatted.cbl"
    if not os.path.exists(vasu_file):
        vasu_file = os.path.join("cobol-support", "tests", vasu_file)
    
    if not os.path.exists(vasu_file):
        print(f"❌ Vasu file not found: {vasu_file}")
        return False
    
    print(f"📄 Testing with file: {vasu_file}")
    
    import time
    start_time = time.time()
    
    result = parser.parse_file(vasu_file)
    
    end_time = time.time()
    parse_time = end_time - start_time
    
    if not result.get("parse_success", False):
        print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
        return False
    
    print(f"✅ Parsing successful in {parse_time:.2f} seconds")
    
    entities = result.get("entities", [])
    print(f"📊 Found {len(entities)} entities")
    
    # Count entity types
    entity_types = {}
    for entity in entities:
        entity_type = entity.get("type", "unknown")
        entity_types[entity_type] = entity_types.get(entity_type, 0) + 1
    
    print("📈 Entity type distribution:")
    for entity_type, count in entity_types.items():
        print(f"  {entity_type}: {count}")
    
    # Check line information quality
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
    
    return success_rate > 95  # Expect at least 95% success rate


def main():
    """Main test runner"""
    print("🚀 COBOL Line Information Quick Test")
    print("=" * 50)
    
    # Run quick test
    quick_success = quick_test()
    
    # Run Vasu file test
    vasu_success = test_vasu_file()
    
    print("\n" + "=" * 50)
    if quick_success and vasu_success:
        print("🎉 All quick tests passed!")
        return True
    else:
        print("❌ Some tests failed")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
