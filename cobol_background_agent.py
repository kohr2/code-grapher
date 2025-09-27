#!/usr/bin/env python3
"""
COBOL Background Agent

This background agent coordinates the analysis, parsing, and validation of COBOL files
to identify and fix parsing discrepancies. It runs continuously to monitor and improve
COBOL parsing accuracy.

Key Components:
1. COBOL Analysis Agent - Identifies discrepancies
2. Enhanced COBOL Parser - Improved entity extraction
3. Validation System - Validates results against expected structure
4. Fix Implementation - Applies corrections automatically
"""

import asyncio
import logging
import json
import time
from typing import Dict, List, Optional, Callable
from dataclasses import dataclass
from pathlib import Path
import threading
import queue

# Import our custom components
from cobol_analysis_agent import COBOLAnalysisAgent
from enhanced_cobol_parser import EnhancedCOBOLParser
from cobol_validation_system import COBOLValidationSystem

logger = logging.getLogger(__name__)

@dataclass
class COBOLProcessingTask:
    file_path: str
    priority: int = 1
    callback: Optional[Callable] = None
    metadata: Dict = None

    def __post_init__(self):
        if self.metadata is None:
            self.metadata = {}

@dataclass
class ProcessingResult:
    task: COBOLProcessingTask
    success: bool
    results: Dict
    errors: List[str]
    processing_time: float

class COBOLBackgroundAgent:
    """
    Background agent for continuous COBOL processing and improvement.
    """
    
    def __init__(self, max_workers: int = 4):
        self.max_workers = max_workers
        self.running = False
        self.task_queue = queue.PriorityQueue()
        self.results_queue = queue.Queue()
        self.workers = []
        
        # Initialize components
        self.analysis_agent = COBOLAnalysisAgent()
        self.enhanced_parser = EnhancedCOBOLParser()
        self.validation_system = COBOLValidationSystem()
        
        # Processing statistics
        self.stats = {
            'tasks_processed': 0,
            'successful_tasks': 0,
            'failed_tasks': 0,
            'total_processing_time': 0.0,
            'average_accuracy': 0.0
        }
        
        # Configuration
        self.config = {
            'auto_fix_enabled': True,
            'validation_enabled': True,
            'report_generation': True,
            'continuous_monitoring': True,
            'output_directory': 'cobol_processing_results'
        }
        
        # Create output directory
        Path(self.config['output_directory']).mkdir(exist_ok=True)

    async def start(self):
        """Start the background agent."""
        logger.info("Starting COBOL Background Agent")
        self.running = True
        
        # Start worker threads
        for i in range(self.max_workers):
            worker = threading.Thread(target=self._worker_loop, daemon=True)
            worker.start()
            self.workers.append(worker)
        
        # Start monitoring loop
        monitor_task = asyncio.create_task(self._monitoring_loop())
        
        logger.info(f"Background agent started with {self.max_workers} workers")
        return monitor_task

    async def stop(self):
        """Stop the background agent."""
        logger.info("Stopping COBOL Background Agent")
        self.running = False
        
        # Wait for workers to finish
        for worker in self.workers:
            worker.join(timeout=5.0)
        
        logger.info("Background agent stopped")

    def submit_task(self, task: COBOLProcessingTask):
        """Submit a COBOL processing task."""
        self.task_queue.put((task.priority, time.time(), task))
        logger.info(f"Submitted task: {task.file_path}")

    def _worker_loop(self):
        """Worker loop for processing tasks."""
        while self.running:
            try:
                # Get task from queue (with timeout)
                priority, timestamp, task = self.task_queue.get(timeout=1.0)
                
                # Process the task
                result = self._process_task(task)
                
                # Put result in results queue
                self.results_queue.put(result)
                
                # Update statistics
                self._update_stats(result)
                
            except queue.Empty:
                continue
            except Exception as e:
                logger.error(f"Worker error: {e}")
                if 'task' in locals():
                    error_result = ProcessingResult(
                        task=task,
                        success=False,
                        results={},
                        errors=[str(e)],
                        processing_time=0.0
                    )
                    self.results_queue.put(error_result)

    def _process_task(self, task: COBOLProcessingTask) -> ProcessingResult:
        """Process a single COBOL task."""
        start_time = time.time()
        errors = []
        
        try:
            logger.info(f"Processing COBOL file: {task.file_path}")
            
            # Step 1: Analyze the file
            analysis = self.analysis_agent.analyze_cobol_file(task.file_path)
            if not analysis:
                raise Exception("Analysis failed")
            
            # Step 2: Parse with enhanced parser
            parse_results = self.enhanced_parser.parse_file(task.file_path)
            if not parse_results:
                raise Exception("Enhanced parsing failed")
            
            # Step 3: Validate results
            validation_results = self.validation_system.validate_parsing_results(
                parse_results['counts'], 
                parse_results['entities']
            )
            
            # Step 4: Generate comprehensive results
            comprehensive_results = {
                'analysis': {
                    'expected_counts': analysis.expected_counts,
                    'actual_counts': analysis.actual_counts,
                    'discrepancies': analysis.discrepancies,
                    'recommendations': analysis.recommendations
                },
                'parsing': parse_results,
                'validation': {
                    entity_type: {
                        'expected': result.expected,
                        'actual': result.actual,
                        'difference': result.difference,
                        'accuracy_percentage': result.accuracy_percentage,
                        'issues': result.issues,
                        'recommendations': result.recommendations
                    }
                    for entity_type, result in validation_results.items()
                }
            }
            
            # Step 5: Generate reports if enabled
            if self.config['report_generation']:
                self._generate_reports(task, comprehensive_results)
            
            # Step 6: Apply fixes if enabled
            if self.config['auto_fix_enabled']:
                self._apply_fixes(task, comprehensive_results)
            
            processing_time = time.time() - start_time
            
            result = ProcessingResult(
                task=task,
                success=True,
                results=comprehensive_results,
                errors=errors,
                processing_time=processing_time
            )
            
            logger.info(f"Successfully processed {task.file_path} in {processing_time:.2f}s")
            return result
            
        except Exception as e:
            processing_time = time.time() - start_time
            error_msg = f"Error processing {task.file_path}: {e}"
            logger.error(error_msg)
            errors.append(error_msg)
            
            return ProcessingResult(
                task=task,
                success=False,
                results={},
                errors=errors,
                processing_time=processing_time
            )

    def _generate_reports(self, task: COBOLProcessingTask, results: Dict):
        """Generate comprehensive reports."""
        try:
            # Generate timestamp for unique filenames
            timestamp = int(time.time())
            base_name = Path(task.file_path).stem
            
            # Analysis report
            analysis_report_file = f"{self.config['output_directory']}/{base_name}_analysis_{timestamp}.json"
            with open(analysis_report_file, 'w') as f:
                json.dump(results['analysis'], f, indent=2)
            
            # Parsing report
            parsing_report_file = f"{self.config['output_directory']}/{base_name}_parsing_{timestamp}.json"
            with open(parsing_report_file, 'w') as f:
                json.dump(results['parsing'], f, indent=2)
            
            # Validation report
            validation_report_file = f"{self.config['output_directory']}/{base_name}_validation_{timestamp}.json"
            with open(validation_report_file, 'w') as f:
                json.dump(results['validation'], f, indent=2)
            
            # Summary report
            summary_report = self._generate_summary_report(results)
            summary_file = f"{self.config['output_directory']}/{base_name}_summary_{timestamp}.json"
            with open(summary_file, 'w') as f:
                json.dump(summary_report, f, indent=2)
            
            logger.info(f"Generated reports for {task.file_path}")
            
        except Exception as e:
            logger.error(f"Error generating reports: {e}")

    def _generate_summary_report(self, results: Dict) -> Dict:
        """Generate a summary report."""
        analysis = results['analysis']
        validation = results['validation']
        
        # Calculate overall accuracy
        total_expected = sum(analysis['expected_counts'].values())
        total_actual = sum(analysis['actual_counts'].values())
        overall_accuracy = (total_actual / total_expected * 100) if total_expected > 0 else 0
        
        # Identify critical issues
        critical_issues = []
        for entity_type, result in validation.items():
            if result['accuracy_percentage'] < 50:
                critical_issues.append(f"{entity_type}: {result['accuracy_percentage']:.1f}% accuracy")
        
        return {
            'overall_accuracy': overall_accuracy,
            'total_expected_entities': total_expected,
            'total_actual_entities': total_actual,
            'critical_issues': critical_issues,
            'entity_breakdown': {
                entity_type: {
                    'expected': result['expected'],
                    'actual': result['actual'],
                    'accuracy': result['accuracy_percentage']
                }
                for entity_type, result in validation.items()
            },
            'recommendations': analysis['recommendations'][:5]  # Top 5 recommendations
        }

    def _apply_fixes(self, task: COBOLProcessingTask, results: Dict):
        """Apply automatic fixes based on analysis."""
        try:
            # Generate fix suggestions
            fix_suggestions = self._generate_fix_suggestions(results)
            
            if fix_suggestions:
                # Save fix suggestions
                timestamp = int(time.time())
                base_name = Path(task.file_path).stem
                fix_file = f"{self.config['output_directory']}/{base_name}_fixes_{timestamp}.json"
                
                with open(fix_file, 'w') as f:
                    json.dump(fix_suggestions, f, indent=2)
                
                logger.info(f"Generated fix suggestions for {task.file_path}")
            
        except Exception as e:
            logger.error(f"Error applying fixes: {e}")

    def _generate_fix_suggestions(self, results: Dict) -> Dict:
        """Generate specific fix suggestions."""
        validation = results['validation']
        suggestions = {
            'parser_improvements': [],
            'regex_patterns': {},
            'validation_rules': [],
            'priority_fixes': []
        }
        
        # Analyze each entity type
        for entity_type, result in validation.items():
            if result['difference'] > 0:  # Missing entities
                suggestions['priority_fixes'].append({
                    'entity_type': entity_type,
                    'missing_count': result['difference'],
                    'priority': 'high' if result['accuracy_percentage'] < 50 else 'medium'
                })
                
                # Specific suggestions
                if entity_type == 'divisions':
                    suggestions['parser_improvements'].append(
                        "Add specific regex patterns for each division type"
                    )
                elif entity_type == 'paragraphs':
                    suggestions['parser_improvements'].append(
                        "Review paragraph validation logic to avoid over-filtering"
                    )
        
        return suggestions

    def _update_stats(self, result: ProcessingResult):
        """Update processing statistics."""
        self.stats['tasks_processed'] += 1
        
        if result.success:
            self.stats['successful_tasks'] += 1
        else:
            self.stats['failed_tasks'] += 1
        
        self.stats['total_processing_time'] += result.processing_time
        
        # Update average accuracy
        if result.success and 'validation' in result.results:
            accuracies = [
                info['accuracy_percentage'] 
                for info in result.results['validation'].values()
            ]
            if accuracies:
                self.stats['average_accuracy'] = sum(accuracies) / len(accuracies)

    async def _monitoring_loop(self):
        """Monitoring loop for continuous operation."""
        while self.running:
            try:
                # Process results
                while not self.results_queue.empty():
                    result = self.results_queue.get_nowait()
                    self._handle_result(result)
                
                # Log statistics periodically
                if self.stats['tasks_processed'] > 0:
                    logger.info(f"Processed {self.stats['tasks_processed']} tasks, "
                              f"{self.stats['successful_tasks']} successful, "
                              f"avg accuracy: {self.stats['average_accuracy']:.1f}%")
                
                # Sleep before next iteration
                await asyncio.sleep(10)
                
            except Exception as e:
                logger.error(f"Monitoring loop error: {e}")
                await asyncio.sleep(5)

    def _handle_result(self, result: ProcessingResult):
        """Handle a processing result."""
        if result.task.callback:
            try:
                result.task.callback(result)
            except Exception as e:
                logger.error(f"Callback error: {e}")

    def get_stats(self) -> Dict:
        """Get current processing statistics."""
        return self.stats.copy()

    def get_queue_status(self) -> Dict:
        """Get current queue status."""
        return {
            'pending_tasks': self.task_queue.qsize(),
            'pending_results': self.results_queue.qsize(),
            'active_workers': len([w for w in self.workers if w.is_alive()])
        }

# Example usage and testing
async def main():
    """Main function to demonstrate the background agent."""
    agent = COBOLBackgroundAgent(max_workers=2)
    
    # Start the agent
    monitor_task = await agent.start()
    
    # Submit some example tasks
    example_files = [
        "vasu_fraud_management_cobol_reformatted.cbl",
        "test_cobol_banking.cbl"
    ]
    
    for i, file_path in enumerate(example_files):
        task = COBOLProcessingTask(
            file_path=file_path,
            priority=1,
            metadata={'submitted_by': 'test', 'batch_id': 1}
        )
        agent.submit_task(task)
    
    # Let it run for a bit
    await asyncio.sleep(30)
    
    # Print statistics
    stats = agent.get_stats()
    queue_status = agent.get_queue_status()
    
    print(f"\n=== Background Agent Statistics ===")
    print(f"Tasks processed: {stats['tasks_processed']}")
    print(f"Successful: {stats['successful_tasks']}")
    print(f"Failed: {stats['failed_tasks']}")
    print(f"Average accuracy: {stats['average_accuracy']:.1f}%")
    print(f"Pending tasks: {queue_status['pending_tasks']}")
    
    # Stop the agent
    await agent.stop()

if __name__ == "__main__":
    asyncio.run(main())