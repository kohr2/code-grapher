#!/usr/bin/env python3
"""
COBOL Integration Module

This module provides integration points for the COBOL analysis system
with the main code grapher application.
"""

import asyncio
import logging
from typing import Dict, List, Optional
from pathlib import Path

from cobol_support.agents import COBOLAnalysisAgent, COBOLBackgroundAgent, COBOLProcessingTask
from cobol_support.services import EnhancedCOBOLParser, COBOLValidationSystem

logger = logging.getLogger(__name__)

class COBOLIntegration:
    """
    Integration class for COBOL analysis with the main code grapher system.
    """
    
    def __init__(self, config: Optional[Dict] = None):
        self.config = config or {}
        self.analysis_agent = COBOLAnalysisAgent()
        self.enhanced_parser = EnhancedCOBOLParser()
        self.validation_system = COBOLValidationSystem()
        self.background_agent = None
        
    async def start_background_processing(self, max_workers: int = 2):
        """Start the background agent for continuous COBOL processing."""
        self.background_agent = COBOLBackgroundAgent(max_workers=max_workers)
        await self.background_agent.start()
        logger.info("COBOL background processing started")
        
    async def stop_background_processing(self):
        """Stop the background agent."""
        if self.background_agent:
            await self.background_agent.stop()
            self.background_agent = None
            logger.info("COBOL background processing stopped")
    
    def analyze_cobol_file(self, file_path: str) -> Dict:
        """
        Analyze a COBOL file and return comprehensive results.
        
        Args:
            file_path: Path to the COBOL file
            
        Returns:
            Dictionary containing analysis results
        """
        logger.info(f"Analyzing COBOL file: {file_path}")
        
        # Step 1: Analyze discrepancies
        analysis = self.analysis_agent.analyze_cobol_file(file_path)
        
        # Step 2: Parse with enhanced parser
        parse_results = self.enhanced_parser.parse_file(file_path)
        
        # Step 3: Validate results
        validation_results = self.validation_system.validate_parsing_results(
            parse_results['counts'],
            parse_results['entities']
        )
        
        return {
            'file_path': file_path,
            'analysis': analysis,
            'parsing': parse_results,
            'validation': validation_results,
            'summary': self._generate_summary(analysis, parse_results, validation_results)
        }
    
    def submit_cobol_task(self, file_path: str, priority: int = 1, metadata: Dict = None):
        """
        Submit a COBOL file for background processing.
        
        Args:
            file_path: Path to the COBOL file
            priority: Task priority (higher = more important)
            metadata: Additional metadata for the task
        """
        if not self.background_agent:
            raise RuntimeError("Background agent not started. Call start_background_processing() first.")
        
        task = COBOLProcessingTask(
            file_path=file_path,
            priority=priority,
            metadata=metadata or {}
        )
        
        self.background_agent.submit_task(task)
        logger.info(f"Submitted COBOL task: {file_path}")
    
    def get_processing_stats(self) -> Dict:
        """Get current processing statistics."""
        if not self.background_agent:
            return {}
        
        return self.background_agent.get_stats()
    
    def get_queue_status(self) -> Dict:
        """Get current queue status."""
        if not self.background_agent:
            return {}
        
        return self.background_agent.get_queue_status()
    
    def _generate_summary(self, analysis, parse_results, validation_results) -> Dict:
        """Generate a summary of the analysis results."""
        if not analysis or not parse_results or not validation_results:
            return {}
        
        # Calculate overall accuracy
        total_expected = sum(analysis.expected_counts.values())
        total_actual = sum(parse_results['counts'].values())
        overall_accuracy = (total_actual / total_expected * 100) if total_expected > 0 else 0
        
        # Identify critical issues
        critical_issues = []
        for entity_type, result in validation_results.items():
            if result.accuracy_percentage < 50:
                critical_issues.append(f"{entity_type}: {result.accuracy_percentage:.1f}% accuracy")
        
        return {
            'overall_accuracy': overall_accuracy,
            'total_expected_entities': total_expected,
            'total_actual_entities': total_actual,
            'critical_issues': critical_issues,
            'entity_breakdown': {
                entity_type: {
                    'expected': result.expected,
                    'actual': result.actual,
                    'accuracy': result.accuracy_percentage
                }
                for entity_type, result in validation_results.items()
            },
            'recommendations': analysis.recommendations[:5] if analysis else []
        }

# Convenience functions for easy integration
async def analyze_cobol_file(file_path: str) -> Dict:
    """
    Convenience function to analyze a single COBOL file.
    
    Args:
        file_path: Path to the COBOL file
        
    Returns:
        Analysis results dictionary
    """
    integration = COBOLIntegration()
    return integration.analyze_cobol_file(file_path)

async def start_cobol_background_service(max_workers: int = 2) -> COBOLIntegration:
    """
    Convenience function to start COBOL background processing.
    
    Args:
        max_workers: Number of worker threads
        
    Returns:
        COBOLIntegration instance
    """
    integration = COBOLIntegration()
    await integration.start_background_processing(max_workers)
    return integration

def get_cobol_entity_counts(file_path: str) -> Dict[str, int]:
    """
    Get entity counts for a COBOL file.
    
    Args:
        file_path: Path to the COBOL file
        
    Returns:
        Dictionary of entity counts
    """
    parser = EnhancedCOBOLParser()
    results = parser.parse_file(file_path)
    return results.get('counts', {})

# Example usage
async def main():
    """Example usage of the COBOL integration."""
    # Analyze a single file
    results = await analyze_cobol_file("example.cbl")
    print(f"Analysis results: {results['summary']}")
    
    # Start background service
    integration = await start_cobol_background_service()
    
    # Submit multiple files for processing
    cobol_files = ["file1.cbl", "file2.cbl", "file3.cbl"]
    for file_path in cobol_files:
        if Path(file_path).exists():
            integration.submit_cobol_task(file_path)
    
    # Monitor processing
    await asyncio.sleep(10)
    
    stats = integration.get_processing_stats()
    print(f"Processing stats: {stats}")
    
    # Stop background service
    await integration.stop_background_processing()

if __name__ == "__main__":
    asyncio.run(main())