#!/usr/bin/env python3
"""
Integration Tests for COBOL Parsing Solution
Tests the merged COBOL parsing capabilities with the main pipeline
"""

import unittest
import tempfile
import json
from pathlib import Path
from unittest.mock import Mock, patch

import sys
sys.path.insert(0, str(Path(__file__).parent.parent))

from cobol_relationship_extractor import COBOLRelationshipExtractor, extract_cobol_relationships
from shared.services.cobol_monitoring_service import COBOLMonitoringService, MonitoringConfig


class TestCOBOLIntegration(unittest.TestCase):
    """Test COBOL integration with the main pipeline"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.extractor = COBOLRelationshipExtractor()
        self.monitoring_service = COBOLMonitoringService()
        
        # Create a temporary COBOL file for testing
        self.temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False)
        self.temp_file.write(self._get_sample_cobol_content())
        self.temp_file.close()
        self.temp_file_path = self.temp_file.name
    
    def tearDown(self):
        """Clean up test fixtures"""
        Path(self.temp_file_path).unlink(missing_ok=True)
    
    def _get_sample_cobol_content(self) -> str:
        """Get sample COBOL content for testing"""
        return """000100 IDENTIFICATION DIVISION.
000200 PROGRAM-ID. TEST-PROGRAM.
000300 AUTHOR. TEST-SYSTEM.
000400 DATE-WRITTEN. 2024-01-01.
000500
000600 ENVIRONMENT DIVISION.
000700 CONFIGURATION SECTION.
000800 SOURCE-COMPUTER. IBM-370.
000900 OBJECT-COMPUTER. IBM-370.
001000 INPUT-OUTPUT SECTION.
001100 FILE-CONTROL.
001200     SELECT TEST-FILE ASSIGN TO TEST01.
001300
001400 DATA DIVISION.
001500 FILE SECTION.
001600 FD  TEST-FILE
001700     LABEL RECORDS ARE STANDARD.
001800 01  TEST-RECORD.
001900     05  RECORD-ID        PIC X(10).
002000     05  RECORD-DATA      PIC X(50).
002100
002200 WORKING-STORAGE SECTION.
002300 01  WS-COUNTERS.
002400     05  WS-RECORD-COUNT    PIC 9(6) VALUE ZERO.
002500 01  WS-FLAGS.
002600     05  WS-EOF-FLAG        PIC X VALUE 'N'.
002700         88  WS-EOF          VALUE 'Y'.
002800
002900 PROCEDURE DIVISION.
003000 1000-MAIN-PROCESSING.
003100     PERFORM 1100-OPEN-FILES
003200     PERFORM 1200-PROCESS-RECORDS
003300     PERFORM 1300-CLOSE-FILES
003400     STOP RUN.
003500
003600 1100-OPEN-FILES.
003700     OPEN INPUT TEST-FILE
003800     DISPLAY 'FILE OPENED SUCCESSFULLY'.
003900
004000 1200-PROCESS-RECORDS.
004100     READ TEST-FILE
004200         AT END SET WS-EOF TO TRUE
004300     END-READ
004400     PERFORM UNTIL WS-EOF
004500         ADD 1 TO WS-RECORD-COUNT
004600         READ TEST-FILE
004700             AT END SET WS-EOF TO TRUE
004800         END-READ
004900     END-PERFORM.
005000
005100 1300-CLOSE-FILES.
005200     CLOSE TEST-FILE
005300     DISPLAY 'FILE CLOSED SUCCESSFULLY'.
"""
    
    def test_cobol_entity_extraction(self):
        """Test COBOL entity extraction"""
        entities = self.extractor.extract_entities_from_file(self.temp_file_path)
        
        # Verify entity extraction
        self.assertIn('programs', entities)
        self.assertIn('divisions', entities)
        self.assertIn('sections', entities)
        self.assertIn('paragraphs', entities)
        self.assertIn('statements', entities)
        self.assertIn('data_items', entities)
        self.assertIn('files', entities)
        
        # Verify specific entities
        self.assertEqual(len(entities['programs']), 1)
        self.assertEqual(entities['programs'][0].name, 'TEST-PROGRAM')
        
        self.assertEqual(len(entities['divisions']), 4)  # IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE
        
        # Verify sections
        section_names = [s.name for s in entities['sections']]
        self.assertIn('CONFIGURATION', section_names)
        self.assertIn('INPUT-OUTPUT', section_names)
        self.assertIn('FILE', section_names)
        self.assertIn('WORKING-STORAGE', section_names)
        
        # Verify paragraphs
        paragraph_names = [p.name for p in entities['paragraphs']]
        self.assertIn('1000-MAIN-PROCESSING', paragraph_names)
        self.assertIn('1100-OPEN-FILES', paragraph_names)
        self.assertIn('1200-PROCESS-RECORDS', paragraph_names)
        self.assertIn('1300-CLOSE-FILES', paragraph_names)
    
    def test_cobol_relationship_extraction(self):
        """Test COBOL relationship extraction"""
        # Mock parsed files data
        mock_parsed_files = [
            {
                "file_path": self.temp_file_path,
                "success": True,
                "entities": []
            }
        ]
        
        relationships = extract_cobol_relationships(mock_parsed_files)
        
        # Verify relationships were extracted
        self.assertIsInstance(relationships, list)
        self.assertGreater(len(relationships), 0)
        
        # Verify relationship types
        relationship_types = {rel.relationship_type for rel in relationships}
        self.assertIn('CONTAINS', relationship_types)
        self.assertIn('DEFINES', relationship_types)
        
        # Verify hierarchical relationships
        contains_rels = [rel for rel in relationships if rel.relationship_type == 'CONTAINS']
        self.assertGreater(len(contains_rels), 0)
        
        # Verify program contains divisions
        program_division_rels = [
            rel for rel in contains_rels 
            if rel.source_type == 'PROGRAM' and rel.target_type == 'DIVISION'
        ]
        self.assertGreater(len(program_division_rels), 0)
    
    def test_entity_counts(self):
        """Test entity counting functionality"""
        counts = self.extractor.get_entity_counts(self.temp_file_path)
        
        # Verify counts
        self.assertEqual(counts['programs'], 1)
        self.assertEqual(counts['divisions'], 4)
        self.assertGreater(counts['sections'], 0)
        self.assertGreater(counts['paragraphs'], 0)
        self.assertGreater(counts['statements'], 0)
        self.assertGreater(counts['data_items'], 0)
        self.assertGreater(counts['files'], 0)
    
    def test_validation_functionality(self):
        """Test validation functionality"""
        expected_counts = {
            'programs': 1,
            'divisions': 4,
            'sections': 4,
            'paragraphs': 4,
            'statements': 10,
            'data_items': 5,
            'files': 1
        }
        
        discrepancies = self.extractor.validate_parsing_results(self.temp_file_path, expected_counts)
        
        # Verify validation results
        self.assertIsInstance(discrepancies, dict)
        
        # Check if discrepancies are within reasonable bounds
        for entity_type, discrepancy in discrepancies.items():
            self.assertIn('expected', discrepancy)
            self.assertIn('actual', discrepancy)
            self.assertIn('severity', discrepancy)
    
    def test_monitoring_service_initialization(self):
        """Test COBOL monitoring service initialization"""
        config = MonitoringConfig(
            watch_directory='/tmp',
            check_interval=10,
            auto_fix_enabled=False
        )
        
        service = COBOLMonitoringService(config)
        
        # Test initialization
        self.assertTrue(service.initialize())
        
        # Test status
        status = service.get_status()
        self.assertEqual(status['service_name'], 'COBOL Monitoring Service')
        self.assertEqual(status['status'], 'stopped')
        
        # Test statistics
        stats = service.get_statistics()
        self.assertIn('files_analyzed', stats)
        self.assertIn('issues_found', stats)
        self.assertIn('issues_fixed', stats)
    
    def test_monitoring_service_file_processing(self):
        """Test monitoring service file processing"""
        service = COBOLMonitoringService()
        service.initialize()
        
        # Test file analysis
        counts = service._analyze_cobol_file(self.temp_file_path)
        
        self.assertIsInstance(counts, dict)
        self.assertIn('programs', counts)
        self.assertIn('divisions', counts)
        self.assertIn('sections', counts)
        self.assertIn('paragraphs', counts)
        self.assertIn('statements', counts)
        self.assertIn('data_items', counts)
        self.assertIn('files', counts)
        
        # Verify counts are reasonable
        self.assertEqual(counts['programs'], 1)
        self.assertEqual(counts['divisions'], 4)
    
    def test_issue_detection(self):
        """Test issue detection functionality"""
        service = COBOLMonitoringService()
        service.initialize()
        
        # Analyze file
        actual_counts = service._analyze_cobol_file(self.temp_file_path)
        
        # Detect issues with unrealistic expected counts
        expected_counts = {
            'programs': 10,  # Unrealistic expectation
            'divisions': 20,  # Unrealistic expectation
            'sections': 50,  # Unrealistic expectation
            'paragraphs': 100,  # Unrealistic expectation
            'statements': 200,  # Unrealistic expectation
            'data_items': 50,  # Unrealistic expectation
            'files': 10  # Unrealistic expectation
        }
        
        issues = service._detect_issues(self.temp_file_path, expected_counts)
        
        # Verify issues were detected
        self.assertGreater(len(issues), 0)
        
        for issue in issues:
            self.assertIn('issue_id', issue.__dict__)
            self.assertIn('severity', issue.__dict__)
            self.assertIn('description', issue.__dict__)
            self.assertIn('expected_count', issue.__dict__)
            self.assertIn('actual_count', issue.__dict__)
    
    def test_pipeline_integration(self):
        """Test integration with the main pipeline"""
        # Mock the pipeline's parsed_files format
        mock_parsed_files = [
            {
                "file_path": self.temp_file_path,
                "success": True,
                "entities": [],
                "file_type": "cobol"
            }
        ]
        
        # Test the integration function
        relationships = extract_cobol_relationships(mock_parsed_files)
        
        # Verify integration works
        self.assertIsInstance(relationships, list)
        
        # Verify relationships have the expected structure
        if relationships:
            rel = relationships[0]
            self.assertHasAttr(rel, 'source_entity')
            self.assertHasAttr(rel, 'target_entity')
            self.assertHasAttr(rel, 'relationship_type')
            self.assertHasAttr(rel, 'confidence')
            self.assertHasAttr(rel, 'source_file')
            self.assertHasAttr(rel, 'target_file')
    
    def test_error_handling(self):
        """Test error handling in various scenarios"""
        # Test with non-existent file
        non_existent_file = "/tmp/non_existent_file.cbl"
        entities = self.extractor.extract_entities_from_file(non_existent_file)
        self.assertEqual(entities, {})
        
        # Test with invalid COBOL file
        invalid_file = tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False)
        invalid_file.write("This is not valid COBOL content")
        invalid_file.close()
        
        try:
            entities = self.extractor.extract_entities_from_file(invalid_file.name)
            # Should handle gracefully
            self.assertIsInstance(entities, dict)
        finally:
            Path(invalid_file.name).unlink(missing_ok=True)
    
    def test_performance(self):
        """Test performance with larger COBOL file"""
        # Create a larger COBOL file
        large_file = tempfile.NamedTemporaryFile(mode='w', suffix='.cbl', delete=False)
        
        # Generate larger content
        content = self._get_sample_cobol_content()
        for i in range(10):  # Repeat content 10 times
            large_file.write(content.replace('TEST-PROGRAM', f'TEST-PROGRAM-{i}'))
        
        large_file.close()
        
        try:
            # Test extraction performance
            import time
            start_time = time.time()
            entities = self.extractor.extract_entities_from_file(large_file.name)
            end_time = time.time()
            
            # Verify performance is reasonable (should complete in under 1 second)
            self.assertLess(end_time - start_time, 1.0)
            
            # Verify entities were extracted
            self.assertGreater(len(entities.get('programs', [])), 0)
            
        finally:
            Path(large_file.name).unlink(missing_ok=True)


class TestCOBOLMonitoringService(unittest.TestCase):
    """Test COBOL monitoring service functionality"""
    
    def setUp(self):
        """Set up test fixtures"""
        self.config = MonitoringConfig(
            watch_directory='/tmp',
            check_interval=1,
            auto_fix_enabled=False,
            notification_enabled=False
        )
        self.service = COBOLMonitoringService(self.config)
    
    def test_service_lifecycle(self):
        """Test service lifecycle management"""
        # Test initialization
        self.assertTrue(self.service.initialize())
        
        # Test start
        self.assertTrue(self.service.start())
        self.assertTrue(self.service.running)
        
        # Test stop
        self.assertTrue(self.service.stop())
        self.assertFalse(self.service.running)
    
    def test_configuration(self):
        """Test configuration handling"""
        self.assertEqual(self.service.config.watch_directory, '/tmp')
        self.assertEqual(self.service.config.check_interval, 1)
        self.assertFalse(self.service.config.auto_fix_enabled)
        self.assertFalse(self.service.config.notification_enabled)
    
    def test_statistics_tracking(self):
        """Test statistics tracking"""
        stats = self.service.get_statistics()
        
        self.assertIn('uptime_seconds', stats)
        self.assertIn('files_analyzed', stats)
        self.assertIn('issues_found', stats)
        self.assertIn('issues_fixed', stats)
        self.assertIn('fix_rate', stats)
        self.assertIn('recent_issues', stats)
        
        # Initial values should be zero
        self.assertEqual(stats['files_analyzed'], 0)
        self.assertEqual(stats['issues_found'], 0)
        self.assertEqual(stats['issues_fixed'], 0)


if __name__ == '__main__':
    # Run the tests
    unittest.main(verbosity=2)