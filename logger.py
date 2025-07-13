import logging
import os
from datetime import datetime
from typing import Optional, Dict, Any
import json
from pathlib import Path

class CodeGrapherLogger:
    """Comprehensive logging system for Code Grapher development phase"""
    
    def __init__(self, name: str = "CodeGrapher", log_dir: str = "logs"):
        self.name = name
        self.log_dir = Path(log_dir)
        self.log_dir.mkdir(exist_ok=True)
        
        # Create formatters
        self.detailed_formatter = logging.Formatter(
            '[%(asctime)s] [%(name)s] [%(levelname)s] %(message)s',
            datefmt='%Y-%m-%d %H:%M:%S'
        )
        
        # Set up file handlers
        self.setup_handlers()
        
        # Create logger
        self.logger = logging.getLogger(name)
        self.logger.setLevel(logging.DEBUG)  # Capture everything in dev
        
        # Add handlers
        for handler in self.handlers.values():
            self.logger.addHandler(handler)
    
    def setup_handlers(self):
        """Set up different log handlers for different purposes"""
        self.handlers = {}
        
        # Main log file - everything
        main_log = self.log_dir / f"code_grapher_{datetime.now().strftime('%Y%m%d')}.log"
        main_handler = logging.FileHandler(main_log)
        main_handler.setLevel(logging.DEBUG)
        main_handler.setFormatter(self.detailed_formatter)
        self.handlers['main'] = main_handler
        
        # Performance log - timing and metrics
        perf_log = self.log_dir / "performance.log"
        perf_handler = logging.FileHandler(perf_log)
        perf_handler.setLevel(logging.INFO)
        perf_handler.setFormatter(self.detailed_formatter)
        self.handlers['performance'] = perf_handler
        
        # AI Lego Bricks evaluation log
        ai_log = self.log_dir / "ai_lego_bricks_evaluation.log"
        ai_handler = logging.FileHandler(ai_log)
        ai_handler.setLevel(logging.INFO)
        ai_handler.setFormatter(self.detailed_formatter)
        self.handlers['ai_evaluation'] = ai_handler
        
        # Console handler for immediate feedback
        console_handler = logging.StreamHandler()
        console_handler.setLevel(logging.INFO)
        console_handler.setFormatter(self.detailed_formatter)
        self.handlers['console'] = console_handler
    
    def log_operation_start(self, operation: str, details: Optional[Dict[str, Any]] = None):
        """Log the start of an operation with context"""
        msg = f"OPERATION START: {operation}"
        if details:
            msg += f" | Details: {json.dumps(details)}"
        self.logger.info(msg)
    
    def log_operation_end(self, operation: str, duration: float, success: bool, 
                         details: Optional[Dict[str, Any]] = None):
        """Log the end of an operation with results"""
        status = "SUCCESS" if success else "FAILED"
        msg = f"OPERATION END: {operation} | Status: {status} | Duration: {duration:.3f}s"
        if details:
            msg += f" | Details: {json.dumps(details)}"
        self.logger.info(msg)
    
    def log_graph_action(self, action: str, entity_type: str, entity_name: str, 
                        relationships: Optional[list] = None):
        """Log graph database actions"""
        msg = f"GRAPH ACTION: {action} | Type: {entity_type} | Name: {entity_name}"
        if relationships:
            msg += f" | Relationships: {relationships}"
        self.logger.info(msg)
    
    def log_code_analysis(self, file_path: str, findings: Dict[str, Any]):
        """Log code analysis findings"""
        msg = f"CODE ANALYSIS: {file_path} | Findings: {json.dumps(findings, indent=2)}"
        self.logger.debug(msg)
    
    def log_performance(self, metric: str, value: float, unit: str = "ms", 
                       context: Optional[Dict[str, Any]] = None):
        """Log performance metrics"""
        msg = f"PERFORMANCE: {metric} = {value} {unit}"
        if context:
            msg += f" | Context: {json.dumps(context)}"
        self.logger.info(msg)
        # Also write to performance handler
        perf_logger = logging.getLogger(f"{self.name}.performance")
        perf_logger.addHandler(self.handlers['performance'])
        perf_logger.info(msg)
    
    def log_ai_evaluation(self, component: str, evaluation: str, 
                         category: str = "general"):
        """Log AI Lego Bricks evaluation feedback"""
        msg = f"AI EVALUATION [{category.upper()}]: Component: {component} | {evaluation}"
        self.logger.info(msg)
        # Also write to AI evaluation handler
        ai_logger = logging.getLogger(f"{self.name}.ai_evaluation")
        ai_logger.addHandler(self.handlers['ai_evaluation'])
        ai_logger.info(msg)
    
    def log_error(self, error: Exception, context: Optional[Dict[str, Any]] = None):
        """Log errors with full context"""
        msg = f"ERROR: {type(error).__name__}: {str(error)}"
        if context:
            msg += f" | Context: {json.dumps(context)}"
        self.logger.error(msg, exc_info=True)
    
    def log_decision(self, decision: str, reasoning: str, 
                    alternatives: Optional[list] = None):
        """Log architectural/algorithmic decisions"""
        msg = f"DECISION: {decision} | Reasoning: {reasoning}"
        if alternatives:
            msg += f" | Alternatives considered: {alternatives}"
        self.logger.info(msg)
    
    def log_rag_operation(self, operation: str, query: str, 
                         results_count: int, relevance_scores: Optional[list] = None):
        """Log RAG pipeline operations"""
        msg = f"RAG OPERATION: {operation} | Query: '{query}' | Results: {results_count}"
        if relevance_scores:
            avg_score = sum(relevance_scores) / len(relevance_scores) if relevance_scores else 0
            msg += f" | Avg Relevance: {avg_score:.3f}"
        self.logger.info(msg)
    
    def log_info(self, msg: str):
        """Log an info message"""
        self.logger.info(msg)
    
    def log_debug(self, msg: str):
        """Log a debug message"""
        self.logger.debug(msg)
    
    def log_warning(self, msg: str):
        """Log a warning message"""
        self.logger.warning(msg)
    
    def create_session_logger(self, session_name: str) -> 'CodeGrapherLogger':
        """Create a child logger for a specific session"""
        return CodeGrapherLogger(f"{self.name}.{session_name}", self.log_dir)

# Global logger instance
logger = CodeGrapherLogger()

# Convenience functions
def log_info(msg: str):
    logger.logger.info(msg)

def log_debug(msg: str):
    logger.logger.debug(msg)

def log_warning(msg: str):
    logger.logger.warning(msg)

def log_error(msg: str, error: Optional[Exception] = None):
    if error:
        logger.log_error(error)
    else:
        logger.logger.error(msg)