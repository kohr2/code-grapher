"""
COBOL Support Module

This module provides comprehensive COBOL parsing and analysis capabilities
to address discrepancies in COBOL entity extraction.

Key Components:
- COBOL Analysis Agent: Identifies parsing discrepancies
- Enhanced COBOL Parser: Improved entity extraction
- Validation System: Validates results against expected structure
- Background Agent: Continuous processing and monitoring

Expected Structure:
- Programs: 1
- Divisions: 4 (IDENTIFICATION, ENVIRONMENT, DATA, PROCEDURE)
- Sections: 16
- Paragraphs: 261
- Statements: 316
"""

from cobol_support.agents.cobol_analysis_agent import COBOLAnalysisAgent
from cobol_support.agents.cobol_background_agent import COBOLBackgroundAgent, COBOLProcessingTask
from cobol_support.services.enhanced_cobol_parser import EnhancedCOBOLParser
from cobol_support.services.cobol_validation_system import COBOLValidationSystem

__all__ = [
    'COBOLAnalysisAgent',
    'COBOLBackgroundAgent', 
    'COBOLProcessingTask',
    'EnhancedCOBOLParser',
    'COBOLValidationSystem'
]

__version__ = '1.0.0'