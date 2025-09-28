#!/usr/bin/env python3
"""
Integration test for COBOL support module

This test verifies that the COBOL support system is properly integrated
and working correctly.
"""

import asyncio
import tempfile
import os
from pathlib import Path

# Test the integrated COBOL support
import sys
sys.path.append('/workspace')

from cobol_support import (
    COBOLIntegration,
    COBOLAnalysisAgent,
    EnhancedCOBOLParser,
    COBOLValidationSystem,
    analyze_cobol_file,
    get_cobol_entity_counts
)

def test_imports():
    """Test that all components can be imported correctly."""
    print("✅ Testing imports...")
    
    try:
        import sys
        sys.path.append('/workspace')
        from cobol_support import COBOLIntegration
        from cobol_support import COBOLAnalysisAgent
        from cobol_support import EnhancedCOBOLParser
        from cobol_support import COBOLValidationSystem
        print("✅ All imports successful")
        return True
    except ImportError as e:
        print(f"❌ Import failed: {e}")
        return False

def test_basic_functionality():
    """Test basic functionality of the COBOL support system."""
    print("\n✅ Testing basic functionality...")
    
    # Create a test COBOL file
    test_cobol = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300 AUTHOR. TEST-SYSTEM.
000400
000500 ENVIRONMENT DIVISION.
000600 INPUT-OUTPUT SECTION.
000700 FILE-CONTROL.
000800     SELECT TEST-FILE ASSIGN TO 'TEST.DAT'.
000900
001000 DATA DIVISION.
001100 FILE SECTION.
001200 FD TEST-FILE.
001300 01 TEST-RECORD.
001400     05 TEST-ID PIC X(10).
001500     05 TEST-AMOUNT PIC 9(10)V99.
001600
001700 WORKING-STORAGE SECTION.
001800 01 WS-COUNTER PIC 9(5) VALUE ZERO.
001900 01 WS-TOTAL PIC 9(12)V99 VALUE ZERO.
002000
002100 PROCEDURE DIVISION.
002200 1000-INITIALIZE.
002300     DISPLAY 'INITIALIZING TEST PROGRAM'.
002400     MOVE ZERO TO WS-COUNTER.
002500     MOVE ZERO TO WS-TOTAL.
002600
002700 2000-PROCESS.
002800     PERFORM 1000-INITIALIZE.
002900     IF WS-COUNTER > 0
003000         DISPLAY 'PROCESSING DATA'
003100     END-IF.
003200
003300 9999-END-PROGRAM.
003400     DISPLAY 'TEST PROGRAM COMPLETE'.
003500     STOP RUN.
"""
    
    # Write test file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
        f.write(test_cobol)
        test_file = f.name
    
    try:
        # Test entity counts
        counts = get_cobol_entity_counts(test_file)
        print(f"✅ Entity counts: {counts}")
        
        # Test enhanced parser
        parser = EnhancedCOBOLParser()
        results = parser.parse_file(test_file)
        print(f"✅ Parser found {len(results['entities'])} entities")
        
        # Test validation system
        validator = COBOLValidationSystem()
        validation_results = validator.validate_parsing_results(
            results['counts'],
            results['entities']
        )
        print(f"✅ Validation completed for {len(validation_results)} entity types")
        
        # Test analysis agent
        analyzer = COBOLAnalysisAgent()
        analysis = analyzer.analyze_cobol_file(test_file)
        print(f"✅ Analysis completed with {len(analysis.recommendations)} recommendations")
        
        return True
        
    except Exception as e:
        print(f"❌ Basic functionality test failed: {e}")
        return False
        
    finally:
        # Clean up test file
        os.unlink(test_file)

async def test_integration():
    """Test the integration layer."""
    print("\n✅ Testing integration layer...")
    
    # Create a test COBOL file
    test_cobol = """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. INTEGRATION-TEST.
000300
000400 ENVIRONMENT DIVISION.
000500
000600 DATA DIVISION.
000700
000800 PROCEDURE DIVISION.
000900 1000-TEST.
001000     DISPLAY 'INTEGRATION TEST'.
001100     STOP RUN.
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False) as f:
        f.write(test_cobol)
        test_file = f.name
    
    try:
        # Test integration
        integration = COBOLIntegration()
        results = await integration.analyze_cobol_file(test_file)
        
        summary = results.get('summary', {})
        print(f"✅ Integration test completed")
        print(f"   Overall accuracy: {summary.get('overall_accuracy', 0):.1f}%")
        print(f"   Expected entities: {summary.get('total_expected_entities', 0)}")
        print(f"   Actual entities: {summary.get('total_actual_entities', 0)}")
        
        return True
        
    except Exception as e:
        print(f"❌ Integration test failed: {e}")
        return False
        
    finally:
        os.unlink(test_file)

async def test_background_processing():
    """Test background processing capabilities."""
    print("\n✅ Testing background processing...")
    
    try:
        # Start background service
        from cobol_support import start_cobol_background_service
        integration = await start_cobol_background_service(max_workers=1)
        
        # Get initial stats
        stats = integration.get_processing_stats()
        print(f"✅ Background service started")
        print(f"   Initial stats: {stats}")
        
        # Stop service
        await integration.stop_background_processing()
        print(f"✅ Background service stopped")
        
        return True
        
    except Exception as e:
        print(f"❌ Background processing test failed: {e}")
        return False

def test_cli_import():
    """Test CLI import."""
    print("\n✅ Testing CLI import...")
    
    try:
        import sys
        sys.path.append('/workspace')
        from cobol_support.cli import main
        print("✅ CLI module imported successfully")
        return True
    except ImportError as e:
        print(f"❌ CLI import failed: {e}")
        return False

async def main():
    """Run all integration tests."""
    print("🧪 Running COBOL Support Integration Tests\n")
    
    tests = [
        ("Import Test", test_imports),
        ("Basic Functionality", test_basic_functionality),
        ("Integration Layer", test_integration),
        ("Background Processing", test_background_processing),
        ("CLI Import", test_cli_import),
    ]
    
    results = []
    
    for test_name, test_func in tests:
        print(f"\n{'='*50}")
        print(f"Running: {test_name}")
        print('='*50)
        
        try:
            if asyncio.iscoroutinefunction(test_func):
                result = await test_func()
            else:
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
        print("🎉 All tests passed! COBOL support is properly integrated.")
    else:
        print("⚠️  Some tests failed. Please check the integration.")
    
    return passed == total

if __name__ == "__main__":
    success = asyncio.run(main())
    exit(0 if success else 1)