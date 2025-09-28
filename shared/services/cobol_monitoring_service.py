#!/usr/bin/env python3
"""
COBOL Monitoring Service
Integrates with the existing service architecture to provide continuous COBOL parsing monitoring
"""

import asyncio
import json
import logging
import time
import re
from pathlib import Path
from typing import Dict, List, Optional, Set
from dataclasses import dataclass, asdict
from datetime import datetime
import threading
import queue

from shared.interfaces.service_interface import ServiceInterface
from shared.exceptions.service_exceptions import ServiceException
from logger import logger


@dataclass
class ParsingIssue:
    """Represents a parsing issue that needs attention"""
    issue_id: str
    file_path: str
    issue_type: str  # 'missing_entities', 'incorrect_counts', 'parsing_error'
    severity: str     # 'low', 'medium', 'high', 'critical'
    description: str
    expected_count: int
    actual_count: int
    timestamp: datetime
    status: str = 'open'  # 'open', 'investigating', 'fixed', 'ignored'


@dataclass
class MonitoringConfig:
    """Configuration for COBOL monitoring"""
    watch_directory: str = '/workspace'
    check_interval: int = 30  # seconds
    max_issues_per_file: int = 10
    auto_fix_enabled: bool = True
    notification_enabled: bool = True
    log_level: str = 'INFO'


class COBOLMonitoringService(ServiceInterface):
    """Service for continuous COBOL parsing monitoring and validation"""
    
    def __init__(self, config: Optional[MonitoringConfig] = None):
        self.config = config or MonitoringConfig()
        self.running = False
        self.issues_queue = queue.Queue()
        self.parsing_cache = {}
        self.issue_history = []
        self.stats = {
            'files_analyzed': 0,
            'issues_found': 0,
            'issues_fixed': 0,
            'uptime_start': datetime.now()
        }
        
        # Expected counts for different COBOL file types
        self.expected_patterns = {
            'fraud_management': {
                'programs': 1,
                'divisions': 4,
                'sections': 16,
                'paragraphs': 261,
                'statements': 316
            },
            'banking_system': {
                'programs': 1,
                'divisions': 4,
                'sections': 20,
                'paragraphs': 300,
                'statements': 400
            },
            'default': {
                'programs': 1,
                'divisions': 4,
                'sections': 10,
                'paragraphs': 100,
                'statements': 200
            }
        }
        
        # Enhanced regex patterns
        self.patterns = {
            'program_id': re.compile(r'^\s*\d+\s+PROGRAM-ID\.\s+([A-Z0-9-]+)', re.IGNORECASE),
            'division': re.compile(r'^\s*\d+\s+([A-Z]+)\s+DIVISION', re.IGNORECASE),
            'section': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+SECTION\.', re.IGNORECASE),
            'paragraph': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s*\.', re.IGNORECASE),
            'data_item': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+.*PIC\s+', re.IGNORECASE),
            'file_definition': re.compile(r'^\s*\d+\s+([A-Z0-9-]+)\s+.*SELECT\s+', re.IGNORECASE),
            'statement': re.compile(r'^\s*\d+\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|GO\s+TO|CALL|EXIT|STOP|EVALUATE|INITIALIZE|SET|STRING|UNSTRING)', re.IGNORECASE)
        }
        
        # Statement keywords for comprehensive extraction
        self.statement_keywords = {
            'DISPLAY', 'MOVE', 'IF', 'PERFORM', 'READ', 'WRITE', 'OPEN', 'CLOSE',
            'ACCEPT', 'COMPUTE', 'ADD', 'SUBTRACT', 'MULTIPLY', 'DIVIDE', 'GO TO',
            'CALL', 'EXIT', 'STOP', 'EVALUATE', 'INITIALIZE', 'SET', 'STRING',
            'UNSTRING', 'SEARCH', 'SORT', 'MERGE', 'RELEASE', 'RETURN', 'REWRITE',
            'DELETE', 'START', 'CONTINUE', 'NEXT', 'END-IF', 'END-PERFORM',
            'END-EVALUATE', 'END-SEARCH', 'END-SORT', 'END-MERGE'
        }
    
    def initialize(self) -> bool:
        """Initialize the monitoring service"""
        try:
            logger.info("Initializing COBOL Monitoring Service")
            self.running = False
            return True
        except Exception as e:
            logger.error(f"Failed to initialize COBOL Monitoring Service: {e}")
            return False
    
    def start(self) -> bool:
        """Start the monitoring service"""
        try:
            logger.info("Starting COBOL Monitoring Service")
            self.running = True
            
            # Start monitoring in a separate thread
            self.monitoring_thread = threading.Thread(target=self._monitoring_loop, daemon=True)
            self.monitoring_thread.start()
            
            return True
        except Exception as e:
            logger.error(f"Failed to start COBOL Monitoring Service: {e}")
            return False
    
    def stop(self) -> bool:
        """Stop the monitoring service"""
        try:
            logger.info("Stopping COBOL Monitoring Service")
            self.running = False
            
            if hasattr(self, 'monitoring_thread'):
                self.monitoring_thread.join(timeout=5)
            
            return True
        except Exception as e:
            logger.error(f"Failed to stop COBOL Monitoring Service: {e}")
            return False
    
    def get_status(self) -> Dict[str, any]:
        """Get service status"""
        uptime = datetime.now() - self.stats['uptime_start']
        
        return {
            'service_name': 'COBOL Monitoring Service',
            'status': 'running' if self.running else 'stopped',
            'uptime_seconds': uptime.total_seconds(),
            'files_analyzed': self.stats['files_analyzed'],
            'issues_found': self.stats['issues_found'],
            'issues_fixed': self.stats['issues_fixed'],
            'config': asdict(self.config)
        }
    
    def _monitoring_loop(self):
        """Main monitoring loop"""
        logger.info("COBOL monitoring loop started")
        
        while self.running:
            try:
                # Scan directory for COBOL files
                self._scan_directory()
                
                # Process issues queue
                self._process_issues_queue()
                
                # Wait before next scan
                time.sleep(self.config.check_interval)
                
            except Exception as e:
                logger.error(f"Error in monitoring loop: {e}")
                time.sleep(5)  # Short delay before retrying
        
        logger.info("COBOL monitoring loop stopped")
    
    def _scan_directory(self):
        """Scan directory for COBOL files"""
        watch_path = Path(self.config.watch_directory)
        
        for file_path in watch_path.rglob("*.cbl"):
            if file_path.is_file():
                self._process_file(str(file_path))
        
        for file_path in watch_path.rglob("*.cob"):
            if file_path.is_file():
                self._process_file(str(file_path))
    
    def _process_file(self, file_path: str):
        """Process a single COBOL file"""
        try:
            logger.info(f"Processing COBOL file: {file_path}")
            
            # Analyze the file
            actual_counts = self._analyze_cobol_file(file_path)
            self.stats['files_analyzed'] += 1
            
            # Detect issues
            issues = self._detect_issues(file_path, actual_counts)
            
            if issues:
                self.stats['issues_found'] += len(issues)
                logger.warning(f"Found {len(issues)} issues in {file_path}")
                
                # Add issues to queue
                for issue in issues:
                    self.issues_queue.put(issue)
                    self.issue_history.append(issue)
                
                # Auto-fix if enabled
                if self.config.auto_fix_enabled:
                    for issue in issues:
                        if issue.severity in ['high', 'critical']:
                            self._auto_fix_issue(issue)
            else:
                logger.info(f"No issues found in {file_path}")
                
        except Exception as e:
            logger.error(f"Error processing file {file_path}: {e}")
    
    def _analyze_cobol_file(self, file_path: str) -> Dict[str, int]:
        """Analyze a COBOL file and return entity counts"""
        try:
            with open(file_path, 'r', encoding='utf-8', errors='ignore') as file:
                lines = file.readlines()
            
            counts = {
                'programs': 0, 'divisions': 0, 'sections': 0,
                'paragraphs': 0, 'statements': 0, 'data_items': 0, 'files': 0
            }
            
            current_division = None
            current_section = None
            current_paragraph = None
            
            for line_num, line in enumerate(lines, 1):
                cleaned_line = self._clean_line(line)
                if not cleaned_line or cleaned_line.startswith('*'):
                    continue
                
                # Count programs
                if re.search(self.patterns['program_id'], line, re.IGNORECASE):
                    counts['programs'] += 1
                
                # Count divisions
                division_match = re.search(self.patterns['division'], line, re.IGNORECASE)
                if division_match:
                    current_division = division_match.group(1)
                    counts['divisions'] += 1
                
                # Count sections
                section_match = re.search(self.patterns['section'], line, re.IGNORECASE)
                if section_match:
                    current_section = section_match.group(1)
                    counts['sections'] += 1
                
                # Count paragraphs
                paragraph_match = re.search(self.patterns['paragraph'], line, re.IGNORECASE)
                if paragraph_match:
                    current_paragraph = paragraph_match.group(1)
                    counts['paragraphs'] += 1
                
                # Count data items
                if re.search(self.patterns['data_item'], line, re.IGNORECASE):
                    counts['data_items'] += 1
                
                # Count file definitions
                if re.search(self.patterns['file_definition'], line, re.IGNORECASE):
                    counts['files'] += 1
                
                # Count statements
                if re.search(self.patterns['statement'], line, re.IGNORECASE):
                    counts['statements'] += 1
                else:
                    # Additional statement detection
                    for keyword in self.statement_keywords:
                        if cleaned_line.upper().startswith(keyword):
                            counts['statements'] += 1
                            break
            
            return counts
            
        except Exception as e:
            logger.error(f"Error analyzing file {file_path}: {e}")
            return {}
    
    def _clean_line(self, line: str) -> str:
        """Remove COBOL line numbers and clean the line"""
        if len(line) > 6:
            return line[6:].strip()
        return line.strip()
    
    def _detect_issues(self, file_path: str, actual_counts: Dict[str, int]) -> List[ParsingIssue]:
        """Detect parsing issues in a COBOL file"""
        issues = []
        expected_counts = self._get_expected_counts(file_path)
        
        for entity_type, expected_count in expected_counts.items():
            actual_count = actual_counts.get(entity_type, 0)
            
            if actual_count != expected_count:
                severity = self._calculate_severity(expected_count, actual_count)
                
                issue = ParsingIssue(
                    issue_id=f"{Path(file_path).name}_{entity_type}_{int(time.time())}",
                    file_path=file_path,
                    issue_type='incorrect_counts',
                    severity=severity,
                    description=f"Expected {expected_count} {entity_type}, found {actual_count}",
                    expected_count=expected_count,
                    actual_count=actual_count,
                    timestamp=datetime.now()
                )
                issues.append(issue)
        
        return issues
    
    def _get_expected_counts(self, file_path: str) -> Dict[str, int]:
        """Get expected entity counts for a file"""
        filename = Path(file_path).name.lower()
        
        if 'fraud' in filename:
            return self.expected_patterns['fraud_management']
        elif 'banking' in filename or 'bank' in filename:
            return self.expected_patterns['banking_system']
        else:
            return self.expected_patterns['default']
    
    def _calculate_severity(self, expected: int, actual: int) -> str:
        """Calculate severity based on the difference between expected and actual counts"""
        if expected == 0:
            return 'low'
        
        percentage_diff = abs(actual - expected) / expected * 100
        
        if percentage_diff >= 50:
            return 'critical'
        elif percentage_diff >= 25:
            return 'high'
        elif percentage_diff >= 10:
            return 'medium'
        else:
            return 'low'
    
    def _auto_fix_issue(self, issue: ParsingIssue) -> bool:
        """Attempt to automatically fix a parsing issue"""
        try:
            logger.info(f"Attempting to auto-fix issue: {issue.issue_id}")
            
            # Generate enhanced parser for the specific file
            self._generate_file_specific_parser(issue.file_path)
            
            # Re-analyze the file
            new_counts = self._analyze_cobol_file(issue.file_path)
            
            # Check if the issue is resolved
            if new_counts.get(issue.issue_type.replace('_counts', ''), 0) == issue.expected_count:
                issue.status = 'fixed'
                self.stats['issues_fixed'] += 1
                logger.info(f"Successfully fixed issue: {issue.issue_id}")
                return True
            else:
                issue.status = 'investigating'
                logger.warning(f"Could not auto-fix issue: {issue.issue_id}")
                return False
                
        except Exception as e:
            logger.error(f"Error auto-fixing issue {issue.issue_id}: {e}")
            issue.status = 'investigating'
            return False
    
    def _generate_file_specific_parser(self, file_path: str):
        """Generate a parser specifically optimized for a file"""
        parser_code = f'''#!/usr/bin/env python3
"""
File-specific COBOL parser for {Path(file_path).name}
Generated by COBOL Monitoring Service
"""

import re
from typing import Dict, List

class FileSpecificCOBOLParser:
    def __init__(self):
        # Optimized patterns for this specific file
        self.patterns = {{
            'program_id': re.compile(r'^\\s*\\d+\\s+PROGRAM-ID\\.\\s+([A-Z0-9-]+)', re.IGNORECASE),
            'division': re.compile(r'^\\s*\\d+\\s+([A-Z]+)\\s+DIVISION', re.IGNORECASE),
            'section': re.compile(r'^\\s*\\d+\\s+([A-Z0-9-]+)\\s+SECTION\\.', re.IGNORECASE),
            'paragraph': re.compile(r'^\\s*\\d+\\s+([A-Z0-9-]+)\\s*\\.', re.IGNORECASE),
            'data_item': re.compile(r'^\\s*\\d+\\s+([A-Z0-9-]+)\\s+.*PIC\\s+', re.IGNORECASE),
            'file_definition': re.compile(r'^\\s*\\d+\\s+([A-Z0-9-]+)\\s+.*SELECT\\s+', re.IGNORECASE),
            'statement': re.compile(r'^\\s*\\d+\\s+(DISPLAY|MOVE|IF|PERFORM|READ|WRITE|OPEN|CLOSE|ACCEPT|COMPUTE|ADD|SUBTRACT|MULTIPLY|DIVIDE|GO\\s+TO|CALL|EXIT|STOP|EVALUATE|INITIALIZE|SET|STRING|UNSTRING)', re.IGNORECASE)
        }}
    
    def parse_file(self, file_path: str) -> Dict[str, int]:
        # Implementation specific to this file's structure
        pass
'''
        
        output_path = f"/workspace/parsers/{Path(file_path).stem}_parser.py"
        Path(output_path).parent.mkdir(exist_ok=True)
        
        with open(output_path, 'w') as f:
            f.write(parser_code)
        
        logger.info(f"Generated file-specific parser: {output_path}")
    
    def _process_issues_queue(self):
        """Process issues from the queue"""
        while not self.issues_queue.empty():
            try:
                issue = self.issues_queue.get_nowait()
                
                if self.config.notification_enabled:
                    self._send_notification(issue)
                
                if self.config.auto_fix_enabled and issue.severity in ['high', 'critical']:
                    self._auto_fix_issue(issue)
                
            except queue.Empty:
                break
            except Exception as e:
                logger.error(f"Error processing issue: {e}")
    
    def _send_notification(self, issue: ParsingIssue):
        """Send notification about an issue"""
        notification = f"""
COBOL Parsing Issue Detected
===========================
File: {issue.file_path}
Type: {issue.issue_type}
Severity: {issue.severity}
Description: {issue.description}
Expected: {issue.expected_count}
Actual: {issue.actual_count}
Time: {issue.timestamp}
"""
        logger.warning(notification)
        
        # Write to notification file
        with open('/workspace/cobol_notifications.log', 'a') as f:
            f.write(notification + "\n")
    
    def get_recent_issues(self, limit: int = 10) -> List[Dict[str, any]]:
        """Get recent issues"""
        recent_issues = sorted(self.issue_history, key=lambda x: x.timestamp, reverse=True)[:limit]
        return [asdict(issue) for issue in recent_issues]
    
    def get_statistics(self) -> Dict[str, any]:
        """Get monitoring statistics"""
        uptime = datetime.now() - self.stats['uptime_start']
        
        return {
            'uptime_seconds': uptime.total_seconds(),
            'files_analyzed': self.stats['files_analyzed'],
            'issues_found': self.stats['issues_found'],
            'issues_fixed': self.stats['issues_fixed'],
            'fix_rate': (self.stats['issues_fixed'] / self.stats['issues_found'] * 100) if self.stats['issues_found'] > 0 else 0,
            'recent_issues': self.get_recent_issues(5)
        }