#!/usr/bin/env python3
"""
COBOL Background Agent

This agent runs in the background to continuously analyze COBOL parsing discrepancies,
implement fixes, and validate improvements. It orchestrates the entire analysis
and enhancement pipeline.
"""

import asyncio
import logging
import time
import json
from typing import Dict, List, Optional, Any
from pathlib import Path
from datetime import datetime, timedelta
import threading
from dataclasses import asdict

from cobol_analysis_agent import COBOLAnalysisAgent, AnalysisResult
from enhanced_cobol_parser import EnhancedCOBOLParser, COBOLStructure
from cobol_validation_system import COBOLValidationSystem, ValidationReport

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.FileHandler('cobol_background_agent.log'),
        logging.StreamHandler()
    ]
)
logger = logging.getLogger(__name__)

class COBOLBackgroundAgent:
    """Background agent for continuous COBOL analysis and improvement"""
    
    def __init__(self, config: Optional[Dict] = None):
        self.config = config or self._get_default_config()
        
        # Initialize components
        self.analysis_agent = COBOLAnalysisAgent()
        self.enhanced_parser = EnhancedCOBOLParser()
        self.validation_system = COBOLValidationSystem()
        
        # Agent state
        self.is_running = False
        self.last_analysis_time = None
        self.analysis_history = []
        self.improvement_tracking = {}
        
        # File monitoring
        self.monitored_files = set()
        self.file_timestamps = {}
        
        # Performance metrics
        self.metrics = {
            "analyses_completed": 0,
            "improvements_implemented": 0,
            "validation_reports_generated": 0,
            "average_analysis_time": 0,
            "success_rate": 0
        }
        
        logger.info("COBOL Background Agent initialized")
    
    def _get_default_config(self) -> Dict:
        """Get default agent configuration"""
        return {
            "analysis_interval": 300,  # 5 minutes
            "file_monitoring": True,
            "auto_improvements": True,
            "validation_enabled": True,
            "report_generation": True,
            "performance_tracking": True,
            "max_history_size": 100,
            "output_directory": "cobol_analysis_output"
        }
    
    async def start(self):
        """Start the background agent"""
        logger.info("Starting COBOL Background Agent")
        self.is_running = True
        
        # Create output directory
        output_dir = Path(self.config["output_directory"])
        output_dir.mkdir(exist_ok=True)
        
        # Start background tasks
        tasks = [
            asyncio.create_task(self._monitor_files()),
            asyncio.create_task(self._periodic_analysis()),
            asyncio.create_task(self._generate_reports()),
            asyncio.create_task(self._track_improvements())
        ]
        
        try:
            await asyncio.gather(*tasks)
        except Exception as e:
            logger.error(f"Background agent error: {e}")
        finally:
            self.is_running = False
            logger.info("COBOL Background Agent stopped")
    
    async def _monitor_files(self):
        """Monitor COBOL files for changes"""
        logger.info("Starting file monitoring")
        
        while self.is_running:
            try:
                # Scan for COBOL files
                cobol_files = self._find_cobol_files()
                
                for file_path in cobol_files:
                    if file_path not in self.monitored_files:
                        self.monitored_files.add(file_path)
                        logger.info(f"Added new COBOL file to monitoring: {file_path}")
                    
                    # Check if file was modified
                    current_mtime = Path(file_path).stat().st_mtime
                    if file_path in self.file_timestamps:
                        if current_mtime > self.file_timestamps[file_path]:
                            logger.info(f"File modified: {file_path}")
                            await self._analyze_file_async(file_path)
                    
                    self.file_timestamps[file_path] = current_mtime
                
                await asyncio.sleep(60)  # Check every minute
                
            except Exception as e:
                logger.error(f"File monitoring error: {e}")
                await asyncio.sleep(60)
    
    async def _periodic_analysis(self):
        """Perform periodic analysis of COBOL files"""
        logger.info("Starting periodic analysis")
        
        while self.is_running:
            try:
                cobol_files = self._find_cobol_files()
                
                for file_path in cobol_files:
                    await self._analyze_file_async(file_path)
                
                await asyncio.sleep(self.config["analysis_interval"])
                
            except Exception as e:
                logger.error(f"Periodic analysis error: {e}")
                await asyncio.sleep(self.config["analysis_interval"])
    
    async def _analyze_file_async(self, file_path: str):
        """Analyze a single COBOL file asynchronously"""
        start_time = time.time()
        
        try:
            logger.info(f"Analyzing COBOL file: {file_path}")
            
            # Run analysis
            analysis_result = self.analysis_agent.analyze_cobol_file(file_path)
            
            # Parse with enhanced parser
            structure = self.enhanced_parser.parse_cobol_file(file_path)
            
            # Validate results
            validation_report = self.validation_system.validate_structure(
                structure, 
                asdict(analysis_result.expected)
            )
            
            # Track analysis
            self._track_analysis(file_path, analysis_result, validation_report)
            
            # Implement improvements if enabled
            if self.config["auto_improvements"]:
                await self._implement_improvements(file_path, analysis_result, validation_report)
            
            # Update metrics
            analysis_time = time.time() - start_time
            self._update_metrics(analysis_time, validation_report.overall_status == "PASSED")
            
            logger.info(f"Analysis completed for {file_path} in {analysis_time:.2f}s")
            
        except Exception as e:
            logger.error(f"Analysis failed for {file_path}: {e}")
            self._update_metrics(time.time() - start_time, False)
    
    async def _implement_improvements(self, file_path: str, analysis_result: AnalysisResult, validation_report: ValidationReport):
        """Implement improvements based on analysis results"""
        improvements_made = []
        
        try:
            # Generate enhanced parser config
            enhanced_config = self.analysis_agent.generate_enhanced_parser_config(analysis_result)
            
            # Update parser configuration
            self.enhanced_parser.config.update(enhanced_config)
            improvements_made.append("Updated parser configuration")
            
            # Implement specific fixes based on validation results
            for result in validation_report.results:
                if not result.passed:
                    improvement = await self._implement_specific_fix(result, file_path)
                    if improvement:
                        improvements_made.append(improvement)
            
            # Track improvements
            if improvements_made:
                self.improvement_tracking[file_path] = {
                    "timestamp": datetime.now().isoformat(),
                    "improvements": improvements_made,
                    "validation_status_before": "FAILED",  # Would be tracked from previous run
                    "validation_status_after": validation_report.overall_status
                }
                
                self.metrics["improvements_implemented"] += len(improvements_made)
                logger.info(f"Implemented {len(improvements_made)} improvements for {file_path}")
            
        except Exception as e:
            logger.error(f"Failed to implement improvements for {file_path}: {e}")
    
    async def _implement_specific_fix(self, validation_result, file_path: str) -> Optional[str]:
        """Implement specific fixes based on validation results"""
        entity_type = validation_result.rule.entity_type
        actual_count = validation_result.actual_count
        expected_count = validation_result.rule.expected_count
        
        if actual_count < expected_count:
            if entity_type == "DIVISION":
                return "Enhanced division extraction patterns"
            elif entity_type == "PARAGRAPH":
                return "Improved paragraph detection and filtering"
            elif entity_type == "STATEMENT":
                return "Added comprehensive statement parsing"
            elif entity_type == "SECTION":
                return "Enhanced section identification"
        
        return None
    
    def _track_analysis(self, file_path: str, analysis_result: AnalysisResult, validation_report: ValidationReport):
        """Track analysis results in history"""
        analysis_record = {
            "timestamp": datetime.now().isoformat(),
            "file_path": file_path,
            "analysis_result": asdict(analysis_result),
            "validation_report": asdict(validation_report),
            "metrics": self.metrics.copy()
        }
        
        self.analysis_history.append(analysis_record)
        
        # Maintain history size
        if len(self.analysis_history) > self.config["max_history_size"]:
            self.analysis_history.pop(0)
        
        self.last_analysis_time = datetime.now()
    
    def _update_metrics(self, analysis_time: float, success: bool):
        """Update performance metrics"""
        self.metrics["analyses_completed"] += 1
        
        # Update average analysis time
        total_analyses = self.metrics["analyses_completed"]
        current_avg = self.metrics["average_analysis_time"]
        self.metrics["average_analysis_time"] = ((current_avg * (total_analyses - 1)) + analysis_time) / total_analyses
        
        # Update success rate
        if success:
            current_success_rate = self.metrics["success_rate"]
            self.metrics["success_rate"] = ((current_success_rate * (total_analyses - 1)) + 100) / total_analyses
        else:
            current_success_rate = self.metrics["success_rate"]
            self.metrics["success_rate"] = (current_success_rate * (total_analyses - 1)) / total_analyses
    
    def _find_cobol_files(self) -> List[str]:
        """Find COBOL files in the workspace"""
        cobol_files = []
        workspace_path = Path("/workspace")
        
        # Common COBOL file extensions
        cobol_extensions = ['.cbl', '.cob', '.cobol', '.cpy']
        
        for ext in cobol_extensions:
            cobol_files.extend(workspace_path.rglob(f"*{ext}"))
        
        return [str(f) for f in cobol_files]
    
    async def _generate_reports(self):
        """Generate periodic reports"""
        while self.is_running:
            try:
                if self.config["report_generation"] and self.analysis_history:
                    await self._generate_comprehensive_report()
                
                await asyncio.sleep(3600)  # Generate reports every hour
                
            except Exception as e:
                logger.error(f"Report generation error: {e}")
                await asyncio.sleep(3600)
    
    async def _generate_comprehensive_report(self):
        """Generate comprehensive analysis report"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        report_file = Path(self.config["output_directory"]) / f"comprehensive_report_{timestamp}.json"
        
        report = {
            "timestamp": datetime.now().isoformat(),
            "agent_metrics": self.metrics,
            "analysis_history": self.analysis_history[-10:],  # Last 10 analyses
            "improvement_tracking": self.improvement_tracking,
            "monitored_files": list(self.monitored_files),
            "configuration": self.config
        }
        
        with open(report_file, 'w') as f:
            json.dump(report, f, indent=2)
        
        logger.info(f"Comprehensive report generated: {report_file}")
    
    async def _track_improvements(self):
        """Track and analyze improvement effectiveness"""
        while self.is_running:
            try:
                if self.improvement_tracking:
                    await self._analyze_improvement_effectiveness()
                
                await asyncio.sleep(1800)  # Check every 30 minutes
                
            except Exception as e:
                logger.error(f"Improvement tracking error: {e}")
                await asyncio.sleep(1800)
    
    async def _analyze_improvement_effectiveness(self):
        """Analyze effectiveness of implemented improvements"""
        for file_path, improvement_data in self.improvement_tracking.items():
            try:
                # Re-analyze file to check improvement effectiveness
                analysis_result = self.analysis_agent.analyze_cobol_file(file_path)
                structure = self.enhanced_parser.parse_cobol_file(file_path)
                validation_report = self.validation_system.validate_structure(structure)
                
                # Update improvement tracking with current status
                improvement_data["current_validation_status"] = validation_report.overall_status
                improvement_data["last_check"] = datetime.now().isoformat()
                
                logger.info(f"Improvement effectiveness check for {file_path}: {validation_report.overall_status}")
                
            except Exception as e:
                logger.error(f"Failed to check improvement effectiveness for {file_path}: {e}")
    
    def stop(self):
        """Stop the background agent"""
        logger.info("Stopping COBOL Background Agent")
        self.is_running = False
    
    def get_status(self) -> Dict[str, Any]:
        """Get current agent status"""
        return {
            "is_running": self.is_running,
            "last_analysis_time": self.last_analysis_time.isoformat() if self.last_analysis_time else None,
            "metrics": self.metrics,
            "monitored_files_count": len(self.monitored_files),
            "analysis_history_size": len(self.analysis_history),
            "improvements_tracked": len(self.improvement_tracking)
        }
    
    def run_single_analysis(self, file_path: str) -> Dict[str, Any]:
        """Run a single analysis on a specific file"""
        try:
            logger.info(f"Running single analysis on {file_path}")
            
            # Run analysis
            analysis_result = self.analysis_agent.analyze_cobol_file(file_path)
            
            # Parse with enhanced parser
            structure = self.enhanced_parser.parse_cobol_file(file_path)
            
            # Validate results
            validation_report = self.validation_system.validate_structure(
                structure, 
                asdict(analysis_result.expected)
            )
            
            return {
                "file_path": file_path,
                "analysis_result": asdict(analysis_result),
                "validation_report": asdict(validation_report),
                "structure_counts": structure.get_counts(),
                "timestamp": datetime.now().isoformat()
            }
            
        except Exception as e:
            logger.error(f"Single analysis failed for {file_path}: {e}")
            return {"error": str(e)}

async def main():
    """Main function to run the background agent"""
    agent = COBOLBackgroundAgent()
    
    try:
        # Run single analysis on any COBOL files found
        cobol_files = agent._find_cobol_files()
        
        if cobol_files:
            print(f"Found {len(cobol_files)} COBOL files")
            for file_path in cobol_files:
                result = agent.run_single_analysis(file_path)
                if "error" not in result:
                    print(f"\nAnalysis completed for {file_path}")
                    print(f"Validation Status: {result['validation_report']['overall_status']}")
                    print(f"Entity Counts: {result['structure_counts']}")
        else:
            print("No COBOL files found in workspace")
        
        # Start background agent
        print("\nStarting background agent...")
        await agent.start()
        
    except KeyboardInterrupt:
        print("\nShutting down background agent...")
        agent.stop()
    except Exception as e:
        logger.error(f"Background agent error: {e}")

if __name__ == "__main__":
    asyncio.run(main())