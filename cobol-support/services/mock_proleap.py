"""
Mock ProLeap COBOL Parser for Testing

This module provides a mock implementation of the ProLeap COBOL parser
when the real Java-based parser isn't available. It simulates the
expected behavior for testing the COBOL integration.
"""

import re
from typing import Dict, List, Any, Optional
from pathlib import Path


class MockProLeapParser:
    """Mock implementation of ProLeap COBOL parser for testing"""
    
    def __init__(self):
        self.available = True
    
    def analyzeFile(self, file_path, source_format):
        """Mock file analysis that returns a program object"""
        return MockProgram(file_path)


class MockProgram:
    """Mock program object that simulates ProLeap program structure"""
    
    def __init__(self, file_path):
        self.file_path = file_path
        self._compilation_units = [MockCompilationUnit(file_path)]
    
    def getCompilationUnits(self):
        """Return mock compilation units"""
        return self._compilation_units
    
    def getName(self):
        """Return program name"""
        return "MOCK-PROGRAM"


class MockCompilationUnit:
    """Mock compilation unit that simulates ProLeap compilation unit"""
    
    def __init__(self, file_path):
        self.file_path = file_path
        self._program_unit = MockProgramUnit()
    
    def getName(self):
        """Return compilation unit name"""
        return "MOCK-COMPILATION-UNIT"
    
    def getProgramUnit(self):
        """Return mock program unit"""
        return self._program_unit


class MockProgramUnit:
    """Mock program unit that simulates ProLeap program unit"""
    
    def __init__(self):
        self._identification_division = MockIdentificationDivision()
        self._data_division = MockDataDivision()
        self._procedure_division = MockProcedureDivision()
    
    def getProgramId(self):
        """Return mock program ID"""
        return "MOCK-PROGRAM-ID"
    
    def getIdentificationDivision(self):
        """Return mock identification division"""
        return self._identification_division
    
    def getDataDivision(self):
        """Return mock data division"""
        return self._data_division
    
    def getProcedureDivision(self):
        """Return mock procedure division"""
        return self._procedure_division


class MockIdentificationDivision:
    """Mock identification division"""
    
    def getProgramIdParagraph(self):
        """Return mock program ID paragraph"""
        return MockProgramIdParagraph()
    
    def getAuthorParagraph(self):
        """Return mock author paragraph"""
        return MockAuthorParagraph()
    
    def getDateWrittenParagraph(self):
        """Return mock date written paragraph"""
        return MockDateWrittenParagraph()


class MockProgramIdParagraph:
    """Mock program ID paragraph"""
    
    def getProgramName(self):
        """Return mock program name"""
        return MockProgramName()


class MockProgramName:
    """Mock program name"""
    
    def getText(self):
        """Return mock program name text"""
        return "MOCK-PROGRAM"


class MockAuthorParagraph:
    """Mock author paragraph"""
    
    def getAuthor(self):
        """Return mock author"""
        return MockAuthor()


class MockAuthor:
    """Mock author"""
    
    def getText(self):
        """Return mock author text"""
        return "MOCK-AUTHOR"


class MockDateWrittenParagraph:
    """Mock date written paragraph"""
    
    def getDateWritten(self):
        """Return mock date written"""
        return MockDateWritten()


class MockDateWritten:
    """Mock date written"""
    
    def getText(self):
        """Return mock date written text"""
        return "2025-01-27"


class MockDataDivision:
    """Mock data division"""
    
    def getWorkingStorageSection(self):
        """Return mock working storage section"""
        return MockWorkingStorageSection()
    
    def getLocalStorageSection(self):
        """Return mock local storage section"""
        return MockLocalStorageSection()
    
    def getLinkageSection(self):
        """Return mock linkage section"""
        return MockLinkageSection()


class MockWorkingStorageSection:
    """Mock working storage section"""
    
    def getDataDescriptionEntries(self):
        """Return mock data description entries"""
        return [MockDataDescriptionEntry("WS-ACCOUNT-NUMBER", 5, "X(10)")]


class MockLocalStorageSection:
    """Mock local storage section"""
    
    def getDataDescriptionEntries(self):
        """Return mock data description entries"""
        return []


class MockLinkageSection:
    """Mock linkage section"""
    
    def getDataDescriptionEntries(self):
        """Return mock data description entries"""
        return []


class MockDataDescriptionEntry:
    """Mock data description entry"""
    
    def __init__(self, name, level, picture):
        self.name = name
        self.level = level
        self.picture = picture
        self._ctx = MockContext()
    
    def getName(self):
        """Return data item name"""
        return self.name
    
    def getLevelNumber(self):
        """Return level number"""
        return self.level
    
    def getPictureString(self):
        """Return picture string"""
        return MockPictureString(self.picture)
    
    def getValue(self):
        """Return value (none for working storage)"""
        return None
    
    def getCtx(self):
        """Return context for line number extraction"""
        return self._ctx


class MockPictureString:
    """Mock picture string"""
    
    def __init__(self, picture):
        self.picture = picture
    
    def getText(self):
        """Return picture text"""
        return self.picture


class MockContext:
    """Mock context for line number extraction"""
    
    def getStart(self):
        """Return mock start token"""
        return MockStartToken()


class MockStartToken:
    """Mock start token"""
    
    def getLine(self):
        """Return mock line number"""
        return 1


class MockProcedureDivision:
    """Mock procedure division"""
    
    def getParagraphs(self):
        """Return mock paragraphs"""
        return [MockParagraph("0000-MAIN-LOGIC"), MockParagraph("1000-INITIALIZE")]
    
    def getSections(self):
        """Return mock sections"""
        return []


class MockParagraph:
    """Mock paragraph"""
    
    def __init__(self, name):
        self.name = name
        self._ctx = MockContext()
    
    def getName(self):
        """Return paragraph name"""
        return self.name
    
    def getStatements(self):
        """Return mock statements"""
        return [MockStatement(f"PERFORM {self.name}")]
    
    def getCtx(self):
        """Return context for line number extraction"""
        return self._ctx


class MockStatement:
    """Mock statement"""
    
    def __init__(self, text):
        self.text = text
    
    def getText(self):
        """Return statement text"""
        return self.text


# Mock source format enum
class MockSourceFormat:
    """Mock source format enum"""
    TANDEM = "TANDEM"


# Mock preprocessor
class MockPreprocessor:
    """Mock preprocessor"""
    CobolSourceFormatEnum = MockSourceFormat


# Mock parser runner
class MockCobolParserRunnerImpl:
    """Mock COBOL parser runner implementation"""
    
    def __init__(self):
        pass
    
    def analyzeFile(self, file_path, source_format):
        """Analyze file and return mock program"""
        return MockProgram(str(file_path))


# Mock file class
class MockFile:
    """Mock file class for testing"""
    
    def __init__(self, path):
        self.path = path
    
    def __str__(self):
        return self.path
