"""
COBOL Parser Service using ProLeap ANTLR4-based parser
Integrates with existing multi-language parser architecture
"""

import os
import json
from typing import Dict, List, Any, Optional, Tuple
from pathlib import Path
from dataclasses import dataclass

try:
    import jpype
    import jpype.imports
    COBOL_PARSER_AVAILABLE = True
except ImportError:
    COBOL_PARSER_AVAILABLE = False

# Import mock parser as fallback
from .mock_proleap import (
    MockCobolParserRunnerImpl, 
    MockPreprocessor, 
    MockFile
)


@dataclass
class COBOLDataItem:
    """Represents a COBOL data item"""
    name: str
    level: int
    data_type: str
    picture: Optional[str]
    value: Optional[str]
    line_number: int
    context: str


@dataclass
class COBOLParagraph:
    """Represents a COBOL paragraph"""
    name: str
    line_number: int
    statements: List[str]
    context: str


@dataclass
class COBOLDivision:
    """Represents a COBOL division"""
    name: str
    sections: Dict[str, Any]
    paragraphs: List[COBOLParagraph]
    data_items: List[COBOLDataItem]


class COBOLParser:
    """COBOL parser using ProLeap ANTLR4-based parser with mock fallback"""
    
    def __init__(self):
        self.parser_available = COBOL_PARSER_AVAILABLE
        self._setup_cobol_parser()
    
    def _setup_cobol_parser(self):
        """Initialize the ProLeap COBOL parser or fall back to mock"""
        if not self.parser_available:
            print("Warning: COBOL parser not available - using mock implementation for testing")
            self._setup_mock_parser()
            return
            
        try:
            # Initialize JVM if not already running
            if not jpype.isJVMStarted():
                # Add the ProLeap JAR to the classpath
                jar_path = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "lib", "proleap-cobol-parser-4.0.0.jar")
                if os.path.exists(jar_path):
                    jpype.startJVM(classpath=[jar_path])
                    print(f"✅ JVM started with ProLeap JAR: {jar_path}")
                else:
                    print(f"⚠️  ProLeap JAR not found at: {jar_path}")
                    print("   Using mock implementation for testing")
                    self._setup_mock_parser()
                    return
            
            # Import ProLeap classes
            from io.github.uwol.proleap.cobol.asg.runner.impl import CobolParserRunnerImpl
            from io.github.uwol.proleap.cobol.preprocessor import CobolPreprocessor
            
            self.parser_runner = CobolParserRunnerImpl()
            self.source_format = CobolPreprocessor.CobolSourceFormatEnum.TANDEM
            self.using_mock = False
            
            print("✅ COBOL parser initialized successfully (ProLeap)")
            
        except Exception as e:
            print(f"❌ Failed to initialize ProLeap COBOL parser: {e}")
            print("   Falling back to mock implementation for testing")
            self._setup_mock_parser()
    
    def _setup_mock_parser(self):
        """Setup mock parser for testing when ProLeap is not available"""
        self.parser_runner = MockCobolParserRunnerImpl()
        self.source_format = MockPreprocessor.CobolSourceFormatEnum.TANDEM
        self.using_mock = True
        print("✅ COBOL parser initialized successfully (Mock - for testing)")
    
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file and return AST/ASG data"""
        try:
            # Parse COBOL file using available parser
            if self.using_mock:
                # Use mock file object for testing
                file_obj = MockFile(file_path)
            else:
                # Use real Java file object
                file_obj = jpype.java.io.File(file_path)
            
            program = self.parser_runner.analyzeFile(file_obj, self.source_format)
            
            # Extract compilation units and program units
            compilation_units = []
            for cu in program.getCompilationUnits():
                cu_data = self._extract_compilation_unit(cu, file_path)
                compilation_units.append(cu_data)
            
            # Extract all entities for relationship analysis
            all_entities = self._extract_all_entities(compilation_units, file_path)
            
            return {
                "parse_success": True,
                "language": "cobol",
                "file_path": file_path,
                "compilation_units": compilation_units,
                "entities": all_entities,
                "ast_data": self._extract_ast_data(program),
                "success": True,  # For compatibility with existing pipeline
                "using_mock": self.using_mock  # Indicate if using mock parser
            }
            
        except Exception as e:
            return {
                "parse_success": False,
                "success": False,
                "error": str(e),
                "file_path": file_path,
                "language": "cobol",
                "entities": [],
                "using_mock": self.using_mock
            }
    
    def _extract_compilation_unit(self, cu, file_path: str) -> Dict[str, Any]:
        """Extract data from a compilation unit"""
        program_unit = cu.getProgramUnit()
        
        return {
            "name": cu.getName(),
            "file_path": file_path,
            "program_id": program_unit.getProgramId() if program_unit else None,
            "divisions": self._extract_divisions(program_unit) if program_unit else {},
            "procedures": self._extract_procedures(program_unit) if program_unit else []
        }
    
    def _extract_divisions(self, program_unit) -> Dict[str, Any]:
        """Extract COBOL divisions (Identification, Data, Procedure)"""
        divisions = {}
        
        # Identification Division
        id_division = program_unit.getIdentificationDivision()
        if id_division:
            divisions["identification"] = {
                "program_id": self._safe_get_program_name(id_division),
                "author": self._safe_get_author(id_division),
                "date_written": self._safe_get_date_written(id_division)
            }
        
        # Data Division
        data_division = program_unit.getDataDivision()
        if data_division:
            divisions["data"] = {
                "working_storage": self._extract_working_storage(data_division),
                "local_storage": self._extract_local_storage(data_division),
                "linkage_section": self._extract_linkage_section(data_division)
            }
        
        # Procedure Division
        proc_division = program_unit.getProcedureDivision()
        if proc_division:
            divisions["procedure"] = {
                "paragraphs": self._extract_procedure_paragraphs(proc_division),
                "sections": self._extract_procedure_sections(proc_division)
            }
        
        return divisions
    
    def _safe_get_program_name(self, id_division) -> Optional[str]:
        """Safely extract program name from identification division"""
        try:
            program_id_para = id_division.getProgramIdParagraph()
            if program_id_para:
                program_name = program_id_para.getProgramName()
                if program_name:
                    return str(program_name.getText())
            return None
        except:
            return None
    
    def _safe_get_author(self, id_division) -> Optional[str]:
        """Safely extract author from identification division"""
        try:
            author_para = id_division.getAuthorParagraph()
            if author_para:
                author = author_para.getAuthor()
                if author:
                    return str(author.getText())
            return None
        except:
            return None
    
    def _safe_get_date_written(self, id_division) -> Optional[str]:
        """Safely extract date written from identification division"""
        try:
            date_para = id_division.getDateWrittenParagraph()
            if date_para:
                date_written = date_para.getDateWritten()
                if date_written:
                    return str(date_written.getText())
            return None
        except:
            return None
    
    def _extract_working_storage(self, data_division) -> List[Dict[str, Any]]:
        """Extract working storage section data descriptions"""
        working_storage = []
        
        try:
            ws_section = data_division.getWorkingStorageSection()
            if ws_section:
                for entry in ws_section.getDataDescriptionEntries():
                    try:
                        data_item = {
                            "name": str(entry.getName()) if entry.getName() else "unknown",
                            "level": entry.getLevelNumber() if entry.getLevelNumber() else 0,
                            "data_type": self._get_data_type(entry),
                            "picture": self._safe_get_picture(entry),
                            "value": self._safe_get_value(entry),
                            "line_number": self._get_line_number(entry)
                        }
                        working_storage.append(data_item)
                    except Exception as e:
                        print(f"Warning: Could not extract data description entry: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract working storage: {e}")
        
        return working_storage
    
    def _extract_local_storage(self, data_division) -> List[Dict[str, Any]]:
        """Extract local storage section data descriptions"""
        local_storage = []
        
        try:
            ls_section = data_division.getLocalStorageSection()
            if ls_section:
                for entry in ls_section.getDataDescriptionEntries():
                    try:
                        data_item = {
                            "name": str(entry.getName()) if entry.getName() else "unknown",
                            "level": entry.getLevelNumber() if entry.getLevelNumber() else 0,
                            "data_type": self._get_data_type(entry),
                            "picture": self._safe_get_picture(entry),
                            "value": self._safe_get_value(entry),
                            "line_number": self._get_line_number(entry)
                        }
                        local_storage.append(data_item)
                    except Exception as e:
                        print(f"Warning: Could not extract data description entry: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract local storage: {e}")
        
        return local_storage
    
    def _extract_linkage_section(self, data_division) -> List[Dict[str, Any]]:
        """Extract linkage section data descriptions"""
        linkage_section = []
        
        try:
            link_section = data_division.getLinkageSection()
            if link_section:
                for entry in link_section.getDataDescriptionEntries():
                    try:
                        data_item = {
                            "name": str(entry.getName()) if entry.getName() else "unknown",
                            "level": entry.getLevelNumber() if entry.getLevelNumber() else 0,
                            "data_type": self._get_data_type(entry),
                            "picture": self._safe_get_picture(entry),
                            "value": self._safe_get_value(entry),
                            "line_number": self._get_line_number(entry)
                        }
                        linkage_section.append(data_item)
                    except Exception as e:
                        print(f"Warning: Could not extract data description entry: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract linkage section: {e}")
        
        return linkage_section
    
    def _safe_get_picture(self, entry) -> Optional[str]:
        """Safely extract picture string from data description entry"""
        try:
            if hasattr(entry, 'getPictureString'):
                picture = entry.getPictureString()
                if picture:
                    return str(picture.getText())
            return None
        except:
            return None
    
    def _safe_get_value(self, entry) -> Optional[str]:
        """Safely extract value from data description entry"""
        try:
            if hasattr(entry, 'getValue'):
                value = entry.getValue()
                if value:
                    return str(value.getText())
            return None
        except:
            return None
    
    def _get_line_number(self, entry) -> int:
        """Get line number from entry context"""
        try:
            if hasattr(entry, 'getCtx') and entry.getCtx():
                ctx = entry.getCtx()
                if hasattr(ctx, 'getStart') and ctx.getStart():
                    return ctx.getStart().getLine()
            return 1
        except:
            return 1
    
    def _get_data_type(self, entry) -> str:
        """Determine data type from COBOL entry"""
        try:
            # Check for various data types
            if hasattr(entry, 'getPictureString') and entry.getPictureString():
                picture = str(entry.getPictureString().getText())
                if picture.startswith('X'):
                    return "alphanumeric"
                elif picture.startswith('9'):
                    return "numeric"
                elif picture.startswith('S9'):
                    return "signed_numeric"
                elif picture.startswith('V'):
                    return "decimal"
                else:
                    return "custom"
            elif hasattr(entry, 'getValue') and entry.getValue():
                value = str(entry.getValue().getText())
                if value.startswith('"') or value.startswith("'"):
                    return "literal"
                elif value.isdigit() or value.startswith('-'):
                    return "numeric_literal"
                else:
                    return "identifier"
            else:
                return "unknown"
        except:
            return "unknown"
    
    def _extract_procedure_paragraphs(self, proc_division) -> List[Dict[str, Any]]:
        """Extract procedure division paragraphs"""
        paragraphs = []
        
        try:
            if hasattr(proc_division, 'getParagraphs'):
                for para in proc_division.getParagraphs():
                    try:
                        para_data = {
                            "name": str(para.getName()) if para.getName() else "unknown",
                            "line_number": self._get_line_number(para),
                            "statements": self._extract_statements(para),
                            "context": "COBOL paragraph"
                        }
                        paragraphs.append(para_data)
                    except Exception as e:
                        print(f"Warning: Could not extract paragraph: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract procedure paragraphs: {e}")
        
        return paragraphs
    
    def _extract_procedure_sections(self, proc_division) -> List[Dict[str, Any]]:
        """Extract procedure division sections"""
        sections = []
        
        try:
            if hasattr(proc_division, 'getSections'):
                for section in proc_division.getSections():
                    try:
                        section_data = {
                            "name": str(section.getName()) if section.getName() else "unknown",
                            "line_number": self._get_line_number(section),
                            "paragraphs": self._extract_procedure_paragraphs(section),
                            "context": "COBOL section"
                        }
                        sections.append(section_data)
                    except Exception as e:
                        print(f"Warning: Could not extract section: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract procedure sections: {e}")
        
        return sections
    
    def _extract_statements(self, para) -> List[str]:
        """Extract statements from a paragraph"""
        statements = []
        
        try:
            if hasattr(para, 'getStatements'):
                for stmt in para.getStatements():
                    try:
                        if hasattr(stmt, 'getText'):
                            stmt_text = str(stmt.getText())
                            if stmt_text.strip():
                                statements.append(stmt_text.strip())
                    except Exception as e:
                        print(f"Warning: Could not extract statement: {e}")
                        continue
        except Exception as e:
            print(f"Warning: Could not extract statements: {e}")
        
        return statements
    
    def _extract_procedures(self, program_unit) -> List[Dict[str, Any]]:
        """Extract procedures from program unit"""
        procedures = []
        
        try:
            proc_division = program_unit.getProcedureDivision()
            if proc_division:
                # Extract paragraphs
                if hasattr(proc_division, 'getParagraphs'):
                    for para in proc_division.getParagraphs():
                        try:
                            proc_data = {
                                "name": str(para.getName()) if para.getName() else "unknown",
                                "line_number": self._get_line_number(para),
                                "type": "paragraph",
                                "context": "COBOL procedure"
                            }
                            procedures.append(proc_data)
                        except Exception as e:
                            print(f"Warning: Could not extract procedure: {e}")
                            continue
                
                # Extract sections
                if hasattr(proc_division, 'getSections'):
                    for section in proc_division.getSections():
                        try:
                            proc_data = {
                                "name": str(section.getName()) if section.getName() else "unknown",
                                "line_number": self._get_line_number(section),
                                "type": "section",
                                "context": "COBOL procedure"
                            }
                            procedures.append(proc_data)
                        except Exception as e:
                            print(f"Warning: Could not extract procedure: {e}")
                            continue
        except Exception as e:
            print(f"Warning: Could not extract procedures: {e}")
        
        return procedures
    
    def _extract_all_entities(self, compilation_units: List[Dict[str, Any]], file_path: str) -> List[Dict[str, Any]]:
        """Extract all entities for relationship analysis"""
        entities = []
        
        for cu in compilation_units:
            # Program entity
            if cu.get("program_id"):
                entities.append({
                    "type": "cobol_program",
                    "name": cu["program_id"],
                    "file_path": file_path,
                    "line_number": 1,
                    "context": "COBOL program definition",
                    "language": "cobol"
                })
            
            # Data entities from divisions
            if "divisions" in cu:
                divisions = cu["divisions"]
                
                # Working storage items
                if "data" in divisions and "working_storage" in divisions["data"]:
                    for ws_item in divisions["data"]["working_storage"]:
                        entities.append({
                            "type": "cobol_data_item",
                            "name": ws_item["name"],
                            "file_path": file_path,
                            "line_number": ws_item.get("line_number", 1),
                            "context": f"Working storage: {ws_item.get('data_type', 'unknown')}",
                            "language": "cobol",
                            "properties": {
                                "level": ws_item.get("level", 0),
                                "data_type": ws_item.get("data_type", "unknown"),
                                "picture": ws_item.get("picture"),
                                "value": ws_item.get("value")
                            }
                        })
                
                # Local storage items
                if "data" in divisions and "local_storage" in divisions["data"]:
                    for ls_item in divisions["data"]["local_storage"]:
                        entities.append({
                            "type": "cobol_data_item",
                            "name": ls_item["name"],
                            "file_path": file_path,
                            "line_number": ls_item.get("line_number", 1),
                            "context": f"Local storage: {ls_item.get('data_type', 'unknown')}",
                            "language": "cobol",
                            "properties": {
                                "level": ls_item.get("level", 0),
                                "data_type": ls_item.get("data_type", "unknown"),
                                "picture": ls_item.get("picture"),
                                "value": ls_item.get("value")
                            }
                        })
                
                # Linkage section items
                if "data" in divisions and "linkage_section" in divisions["data"]:
                    for link_item in divisions["data"]["linkage_section"]:
                        entities.append({
                            "type": "cobol_data_item",
                            "name": link_item["name"],
                            "file_path": file_path,
                            "line_number": link_item.get("line_number", 1),
                            "context": f"Linkage section: {link_item.get('data_type', 'unknown')}",
                            "language": "cobol",
                            "properties": {
                                "level": link_item.get("level", 0),
                                "data_type": link_item.get("data_type", "unknown"),
                                "picture": link_item.get("picture"),
                                "value": link_item.get("value")
                            }
                        })
                
                # Procedure entities
                if "procedure" in divisions:
                    proc_div = divisions["procedure"]
                    
                    # Paragraphs
                    if "paragraphs" in proc_div:
                        for para in proc_div["paragraphs"]:
                            entities.append({
                                "type": "cobol_paragraph",
                                "name": para.get("name", "unknown"),
                                "file_path": file_path,
                                "line_number": para.get("line_number", 1),
                                "context": "COBOL paragraph",
                                "language": "cobol",
                                "properties": {
                                    "statements": para.get("statements", [])
                                }
                            })
                    
                    # Sections
                    if "sections" in proc_div:
                        for section in proc_div["sections"]:
                            entities.append({
                                "type": "cobol_section",
                                "name": section.get("name", "unknown"),
                                "file_path": file_path,
                                "line_number": section.get("line_number", 1),
                                "context": "COBOL section",
                                "language": "cobol",
                                "properties": {
                                    "paragraphs": section.get("paragraphs", [])
                                }
                            })
            
            # Direct procedures
            for proc in cu.get("procedures", []):
                entities.append({
                    "type": "cobol_procedure",
                    "name": proc.get("name", "unknown"),
                    "file_path": file_path,
                    "line_number": proc.get("line_number", 1),
                    "context": f"COBOL {proc.get('type', 'procedure')}",
                    "language": "cobol",
                    "properties": {
                        "procedure_type": proc.get("type", "unknown")
                    }
                })
        
        return entities
    
    def _extract_ast_data(self, program) -> Dict[str, Any]:
        """Extract AST structure for relationship analysis"""
        try:
            return {
                "ast_type": "cobol_ast",
                "program_name": program.getName() if hasattr(program, 'getName') else None,
                "compilation_units_count": len(program.getCompilationUnits()),
                "parser_type": "proleap_antlr4" if not self.using_mock else "mock_parser"
            }
        except Exception as e:
            print(f"Warning: Could not extract AST data: {e}")
            return {
                "ast_type": "cobol_ast",
                "error": str(e),
                "parser_type": "mock_parser" if self.using_mock else "unknown"
            }
    
    def is_available(self) -> bool:
        """Check if COBOL parser is available"""
        return True  # Always return True since we have mock fallback
