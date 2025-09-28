#!/usr/bin/env python3
"""
Integration Test for COBOL Line Information

This test verifies that the line information functionality integrates
properly with the broader code grapher system.
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


def test_line_info_integration():
    """Test that line information integrates properly with the system"""
    print("🧪 Integration Test: COBOL Line Information")
    print("=" * 50)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available")
        return False
    
    # Test with a known file
    test_file = "fixtures/test_cobol_banking.cbl"
    if not os.path.exists(test_file):
        test_file = os.path.join("cobol-support", "tests", test_file)
    
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        return False
    
    print(f"📄 Testing with: {test_file}")
    
    # Parse the file
    result = parser.parse_file(test_file)
    
    if not result.get("parse_success", False):
        print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
        return False
    
    print("✅ Parsing successful")
    
    entities = result.get("entities", [])
    print(f"📊 Found {len(entities)} entities")
    
    # Verify line information is present and correct
    line_info_stats = {
        "total_entities": len(entities),
        "has_line": 0,
        "has_line_count": 0,
        "has_start_line": 0,
        "has_end_line": 0,
        "correct_calculations": 0,
        "paragraphs": 0,
        "data_items": 0,
        "programs": 0
    }
    
    for entity in entities:
        properties = entity.get("properties", {})
        entity_type = entity.get("type", "unknown")
        
        # Count entity types
        if entity_type == "paragraph":
            line_info_stats["paragraphs"] += 1
        elif entity_type == "data_item":
            line_info_stats["data_items"] += 1
        elif entity_type == "program":
            line_info_stats["programs"] += 1
        
        # Check line information presence
        if "line" in properties:
            line_info_stats["has_line"] += 1
        if "line_count" in properties:
            line_info_stats["has_line_count"] += 1
        if "start_line" in properties:
            line_info_stats["has_start_line"] += 1
        if "end_line" in properties:
            line_info_stats["has_end_line"] += 1
        
        # Check calculation correctness
        if all(key in properties for key in ["start_line", "end_line", "line_count"]):
            start_line = properties["start_line"]
            end_line = properties["end_line"]
            line_count = properties["line_count"]
            expected_count = end_line - start_line + 1
            
            if line_count == expected_count:
                line_info_stats["correct_calculations"] += 1
    
    # Print statistics
    print("\n📈 Line Information Statistics:")
    print(f"  Total entities: {line_info_stats['total_entities']}")
    print(f"  Paragraphs: {line_info_stats['paragraphs']}")
    print(f"  Data items: {line_info_stats['data_items']}")
    print(f"  Programs: {line_info_stats['programs']}")
    print()
    print(f"  Has line property: {line_info_stats['has_line']}/{line_info_stats['total_entities']}")
    print(f"  Has line_count property: {line_info_stats['has_line_count']}/{line_info_stats['total_entities']}")
    print(f"  Has start_line property: {line_info_stats['has_start_line']}/{line_info_stats['total_entities']}")
    print(f"  Has end_line property: {line_info_stats['has_end_line']}/{line_info_stats['total_entities']}")
    print(f"  Correct calculations: {line_info_stats['correct_calculations']}/{line_info_stats['total_entities']}")
    
    # Calculate success rates
    line_success_rate = (line_info_stats["has_line"] / line_info_stats["total_entities"]) * 100
    calculation_success_rate = (line_info_stats["correct_calculations"] / line_info_stats["total_entities"]) * 100
    
    print(f"\n📊 Success Rates:")
    print(f"  Line property coverage: {line_success_rate:.1f}%")
    print(f"  Calculation accuracy: {calculation_success_rate:.1f}%")
    
    # Show sample entities with line information
    print(f"\n📋 Sample Entities with Line Information:")
    for i, entity in enumerate(entities[:5]):
        properties = entity.get("properties", {})
        print(f"  {i+1}. {entity.get('name', 'unknown')} ({entity.get('type', 'unknown')})")
        print(f"     Line: {properties.get('line', 'N/A')}")
        print(f"     Count: {properties.get('line_count', 'N/A')}")
        print(f"     Range: {properties.get('start_line', 'N/A')}-{properties.get('end_line', 'N/A')}")
        if entity.get('type') == 'data_item':
            print(f"     Level: {properties.get('level', 'N/A')}")
        print()
    
    # Test JSON serialization (important for system integration)
    print("🔄 Testing JSON serialization...")
    try:
        # Create a clean copy without non-serializable objects
        clean_result = {
            "parse_success": result.get("parse_success", False),
            "language": result.get("language", ""),
            "file_path": result.get("file_path", ""),
            "entities": result.get("entities", []),
            "ast_data": result.get("ast_data", {}),
            "divisions": result.get("divisions", {}),
            "paragraphs": result.get("paragraphs", {}),
            "statements": result.get("statements", {}),
            "data_items": result.get("data_items", {}),
            "file_descriptions": result.get("file_descriptions", {}),
            "linkage_items": result.get("linkage_items", {}),
            "success": result.get("success", False),
            "using_real_parser": result.get("using_real_parser", False)
        }
        
        json_str = json.dumps(clean_result, indent=2)
        parsed_back = json.loads(json_str)
        print("✅ JSON serialization successful")
        
        # Verify line information is preserved in JSON
        json_entities = parsed_back.get("entities", [])
        json_line_info_preserved = 0
        
        for entity in json_entities:
            properties = entity.get("properties", {})
            if all(key in properties for key in ["line", "line_count", "start_line", "end_line"]):
                json_line_info_preserved += 1
        
        json_success_rate = (json_line_info_preserved / len(json_entities)) * 100 if json_entities else 0
        print(f"✅ JSON line info preservation: {json_success_rate:.1f}%")
        
    except Exception as e:
        print(f"❌ JSON serialization failed: {e}")
        return False
    
    # Overall success criteria
    success = (
        line_success_rate >= 95 and  # At least 95% of entities should have line info
        calculation_success_rate >= 95 and  # At least 95% should have correct calculations
        json_success_rate >= 95  # JSON serialization should preserve line info
    )
    
    if success:
        print("\n🎉 Integration test PASSED!")
        print("✅ Line information is properly integrated with the system")
    else:
        print("\n❌ Integration test FAILED!")
        print("❌ Line information integration needs improvement")
    
    return success


def test_performance_benchmark():
    """Test performance with different file sizes"""
    print("\n🧪 Performance Benchmark Test")
    print("=" * 50)
    
    parser = COBOLParser()
    
    if not parser.is_available():
        print("❌ COBOL parser not available")
        return False
    
    # Test files of different sizes
    test_files = [
        ("Small file", "fixtures/test_cobol_banking.cbl"),
        ("Large file", "fixtures/vasu/vasu_fraud_management_cobol_reformatted.cbl")
    ]
    
    results = []
    
    for test_name, test_file in test_files:
        if not os.path.exists(test_file):
            test_file = os.path.join("cobol-support", "tests", test_file)
        
        if not os.path.exists(test_file):
            print(f"⚠️  Skipping {test_name}: file not found")
            continue
        
        print(f"📄 Testing {test_name}: {test_file}")
        
        import time
        start_time = time.time()
        
        result = parser.parse_file(test_file)
        
        end_time = time.time()
        parse_time = end_time - start_time
        
        if result.get("parse_success", False):
            entities = result.get("entities", [])
            print(f"✅ Parsed {len(entities)} entities in {parse_time:.2f}s")
            
            # Calculate entities per second
            entities_per_second = len(entities) / parse_time if parse_time > 0 else 0
            print(f"📊 Performance: {entities_per_second:.1f} entities/second")
            
            results.append({
                "name": test_name,
                "entities": len(entities),
                "time": parse_time,
                "entities_per_second": entities_per_second
            })
        else:
            print(f"❌ Parsing failed: {result.get('error', 'Unknown error')}")
    
    # Print performance summary
    if results:
        print(f"\n📈 Performance Summary:")
        for result in results:
            print(f"  {result['name']}: {result['entities']} entities in {result['time']:.2f}s "
                  f"({result['entities_per_second']:.1f} entities/s)")
    
    return len(results) > 0


def main():
    """Main integration test runner"""
    print("🚀 COBOL Line Information Integration Test")
    print("=" * 60)
    
    # Run integration test
    integration_success = test_line_info_integration()
    
    # Run performance benchmark
    performance_success = test_performance_benchmark()
    
    print("\n" + "=" * 60)
    if integration_success and performance_success:
        print("🎉 All integration tests PASSED!")
        print("✅ COBOL line information is ready for production use")
        return True
    else:
        print("❌ Some integration tests FAILED!")
        print("❌ COBOL line information needs fixes before production")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
