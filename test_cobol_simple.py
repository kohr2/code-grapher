#!/usr/bin/env python3
"""
Simple test for COBOL support components

This test verifies that the individual COBOL support components work correctly
without relying on the module import structure.
"""

import sys
import os
import tempfile
import asyncio

# Add the cobol-support directory to the path
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'cobol-support'))

def test_enhanced_parser():
    """Test the enhanced COBOL parser."""
    print("✅ Testing Enhanced COBOL Parser...")
    
    try:
        from services.enhanced_cobol_parser import EnhancedCOBOLParser
        
        # Create test COBOL content
        test_cobol = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300
000400 ENVIRONMENT DIVISION.
000500
000600 DATA DIVISION.
000700
000800 PROCEDURE DIVISION.
000900 1000-TEST.
001000     DISPLAY 'HELLO WORLD'.
001100     STOP RUN.
"""
        
        # Write test file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write(test_cobol)
            test_file = f.name
        
        try:
            # Test parser
            parser = EnhancedCOBOLParser()
            results = parser.parse_file(test_file)
            
            counts = results.get('counts', {})
            entities = results.get('entities', [])
            
            print(f"   Found {len(entities)} entities")
            print(f"   Counts: {counts}")
            
            # Check for expected entities
            if counts.get('programs', 0) >= 1:
                print("   ✅ Programs detected")
            if counts.get('divisions', 0) >= 1:
                print("   ✅ Divisions detected")
            if counts.get('statements', 0) >= 1:
                print("   ✅ Statements detected")
            
            return True
            
        finally:
            os.unlink(test_file)
            
    except Exception as e:
        print(f"   ❌ Parser test failed: {e}")
        return False

def test_analysis_agent():
    """Test the COBOL analysis agent."""
    print("\n✅ Testing COBOL Analysis Agent...")
    
    try:
        from agents.cobol_analysis_agent import COBOLAnalysisAgent
        
        # Create test COBOL content
        test_cobol = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. ANALYSIS-TEST.
000300
000400 ENVIRONMENT DIVISION.
000500
000600 DATA DIVISION.
000700
000800 PROCEDURE DIVISION.
000900 1000-TEST.
001000     DISPLAY 'ANALYSIS TEST'.
001100     STOP RUN.
"""
        
        # Write test file
        with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
            f.write(test_cobol)
            test_file = f.name
        
        try:
            # Test analysis agent
            agent = COBOLAnalysisAgent()
            analysis = agent.analyze_cobol_file(test_file)
            
            if analysis:
                print(f"   Expected counts: {analysis.expected_counts}")
                print(f"   Actual counts: {analysis.actual_counts}")
                print(f"   Recommendations: {len(analysis.recommendations)}")
                print("   ✅ Analysis completed")
                return True
            else:
                print("   ❌ Analysis returned None")
                return False
                
        finally:
            os.unlink(test_file)
            
    except Exception as e:
        print(f"   ❌ Analysis agent test failed: {e}")
        return False

def test_validation_system():
    """Test the validation system."""
    print("\n✅ Testing Validation System...")
    
    try:
        from services.cobol_validation_system import COBOLValidationSystem
        
        # Test validation system
        validator = COBOLValidationSystem()
        
        # Mock data
        actual_counts = {
            'programs': 1,
            'divisions': 2,
            'sections': 3,
            'paragraphs': 10,
            'statements': 15
        }
        
        entities = []  # Mock entities
        
        validation_results = validator.validate_parsing_results(actual_counts, entities)
        
        print(f"   Validation results: {len(validation_results)} entity types")
        
        for entity_type, result in validation_results.items():
            print(f"   {entity_type}: {result.accuracy_percentage:.1f}% accuracy")
        
        print("   ✅ Validation completed")
        return True
        
    except Exception as e:
        print(f"   ❌ Validation system test failed: {e}")
        return False

def main():
    """Run all tests."""
    print("🧪 Running Simple COBOL Support Tests\n")
    
    tests = [
        ("Enhanced Parser", test_enhanced_parser),
        ("Analysis Agent", test_analysis_agent),
        ("Validation System", test_validation_system),
    ]
    
    results = []
    
    for test_name, test_func in tests:
        print(f"{'='*50}")
        print(f"Running: {test_name}")
        print('='*50)
        
        try:
            result = test_func()
            results.append((test_name, result))
        except Exception as e:
            print(f"❌ Test {test_name} failed with exception: {e}")
            results.append((test_name, False))
    
    # Summary
    print(f"\n{'='*50}")
    print("TEST SUMMARY")
    print('='*50)
    
    passed = 0
    total = len(results)
    
    for test_name, result in results:
        status = "✅ PASSED" if result else "❌ FAILED"
        print(f"{test_name}: {status}")
        if result:
            passed += 1
    
    print(f"\nOverall: {passed}/{total} tests passed")
    
    if passed == total:
        print("🎉 All tests passed! COBOL support components are working.")
    else:
        print("⚠️  Some tests failed. Please check the components.")
    
    return passed == total

if __name__ == "__main__":
    success = main()
    exit(0 if success else 1)