"""
Simple test for COBOL relationship extraction
"""

import os
import sys

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

from ai_services.models.relationship_models import RelationshipType, RelationshipExtraction


def test_relationship_types():
    """Test that COBOL relationship types are defined"""
    print("🧪 Testing COBOL relationship types...")
    
    try:
        # Check that COBOL-specific relationship types exist
        cobol_types = [
            RelationshipType.INCLUDES,
            RelationshipType.PASSES_DATA,
            RelationshipType.HANDLES_ERRORS,
            RelationshipType.USES_QUEUE,
            RelationshipType.BINDS_SCREEN,
            RelationshipType.PERFORMS,
            RelationshipType.REPLACES
        ]
        
        print(f"   ✅ Found {len(cobol_types)} COBOL relationship types")
        
        for rel_type in cobol_types:
            print(f"      - {rel_type.value}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Relationship types test failed: {e}")
        return False


def test_relationship_creation():
    """Test creating COBOL relationships"""
    print("🧪 Testing COBOL relationship creation...")
    
    try:
        # Create a sample relationship
        rel = RelationshipExtraction(
            source_entity="PROGRAM:TEST-PROGRAM",
            target_entity="COPYBOOK:BANKING-COPYBOOK",
            relationship_type=RelationshipType.INCLUDES,
            confidence=0.9,
            context="COPY statement includes BANKING-COPYBOOK",
            metadata={
                'copy_library': 'COMMON',
                'unit_name': 'TEST-PROGRAM'
            }
        )
        
        print(f"   ✅ Created relationship: {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
        print(f"      Context: {rel.context}")
        print(f"      Confidence: {rel.confidence}")
        
        return True
        
    except Exception as e:
        print(f"   ❌ Relationship creation test failed: {e}")
        return False


def test_mock_relationship_extraction():
    """Test relationship extraction with mock data"""
    print("🧪 Testing mock relationship extraction...")
    
    try:
        # Import the extractor
        from cobol_support.services.cobol_relationship_extractor import COBOLRelationshipExtractor
        
        extractor = COBOLRelationshipExtractor()
        
        # Mock COBOL data
        mock_data = {
            "parse_success": True,
            "copy_statements": {
                "TEST-PROGRAM": [
                    {"name": "BANKING-COPYBOOK", "library": "COMMON", "unit": "TEST-PROGRAM"}
                ]
            },
            "call_statements": {
                "TEST-PROGRAM": {
                    "MAIN-LOGIC": [
                        {"program_name": "INTEREST-CALCULATOR", "unit": "TEST-PROGRAM"}
                    ]
                }
            }
        }
        
        relationships = extractor.extract_relationships(mock_data)
        
        print(f"   ✅ Extracted {len(relationships)} relationships")
        
        for i, rel in enumerate(relationships):
            print(f"      {i+1}. {rel.source_entity} -{rel.relationship_type.value}-> {rel.target_entity}")
        
        return len(relationships) > 0
        
    except Exception as e:
        print(f"   ❌ Mock relationship extraction test failed: {e}")
        return False


def main():
    """Run all simple COBOL relationship tests"""
    print("🚀 Running Simple COBOL Relationship Tests")
    print("=" * 50)
    
    tests = [
        ("Relationship Types", test_relationship_types),
        ("Relationship Creation", test_relationship_creation),
        ("Mock Relationship Extraction", test_mock_relationship_extraction),
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
        print("🎉 All simple COBOL relationship tests passed!")
        return True
    else:
        print("⚠️  Some tests failed")
        return False


if __name__ == "__main__":
    success = main()
    sys.exit(0 if success else 1)
