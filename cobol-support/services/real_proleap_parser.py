"""
Real ProLeap COBOL Parser using Maven to run the parser directly
"""

import os
import subprocess
import tempfile
import json
from typing import Dict, List, Any, Optional


class RealProLeapParser:
    """Real COBOL parser using ProLeap through Maven"""
    
    def __init__(self):
        self.java_available = self._check_java()
        self.maven_available = self._check_maven()
        self.proleap_available = self._check_proleap()
        self._setup_parser()
    
    def _check_java(self) -> bool:
        """Check if Java is available"""
        try:
            result = subprocess.run(['java', '-version'], 
                                  capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception:
            return False
    
    def _check_maven(self) -> bool:
        """Check if Maven is available"""
        try:
            result = subprocess.run(['mvn', '-version'], 
                                  capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception:
            return False
    
    def _check_proleap(self) -> bool:
        """Check if ProLeap project is available"""
        # Get the directory where this file is located
        current_dir = os.path.dirname(os.path.abspath(__file__))
        
        # Try relative path from cobol-support directory first
        proleap_dir = os.path.join(current_dir, '..', '..', 'proleap-cobol-parser')
        if os.path.exists(proleap_dir) and os.path.exists(os.path.join(proleap_dir, 'pom.xml')):
            self.proleap_dir = os.path.abspath(proleap_dir)
            return True
        
        # Try relative path from cobol-support directory
        proleap_dir = os.path.join(current_dir, '..', 'proleap-cobol-parser')
        if os.path.exists(proleap_dir) and os.path.exists(os.path.join(proleap_dir, 'pom.xml')):
            self.proleap_dir = os.path.abspath(proleap_dir)
            return True
        
        # Try relative path from project root
        proleap_dir = 'proleap-cobol-parser'
        if os.path.exists(proleap_dir) and os.path.exists(os.path.join(proleap_dir, 'pom.xml')):
            self.proleap_dir = os.path.abspath(proleap_dir)
            return True
            
        return False
    
    def _setup_parser(self):
        """Setup the parser"""
        if not self.java_available:
            print("❌ Java not available")
            self.using_real_parser = False
            return
        
        if not self.maven_available:
            print("❌ Maven not available")
            self.using_real_parser = False
            return
        
        if not self.proleap_available:
            print("❌ ProLeap project not found")
            self.using_real_parser = False
            return
        
        self.using_real_parser = True
        print(f"✅ Real ProLeap parser ready with: {self.proleap_dir}")
    
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """Parse a COBOL file using real ProLeap"""
        if not self.using_real_parser:
            return self._fallback_parse(file_path)
        
        try:
            # Convert to absolute path
            abs_file_path = os.path.abspath(file_path)
            
            # Create a Java program that uses ProLeap directly
            java_code = self._create_proleap_program(abs_file_path)
            
            # Write Java code to temporary file
            temp_dir = tempfile.mkdtemp()
            java_file = os.path.join(temp_dir, 'RealProLeapParser.java')
            
            with open(java_file, 'w') as f:
                f.write(java_code)
            
            # Run the program using Maven
            result = self._run_proleap_parser(java_file, abs_file_path, temp_dir)
            
            # Clean up
            import shutil
            shutil.rmtree(temp_dir)
            
            if result['success']:
                return {
                    "parse_success": True,
                    "language": "cobol",
                    "file_path": file_path,
                    "compilation_units": result.get('compilation_units', []),
                    "entities": result.get('entities', []),
                    "ast_data": result.get('ast_data', {}),
                    "divisions": result.get('divisions', {}),
                    "paragraphs": result.get('paragraphs', {}),
                    "statements": result.get('statements', {}),
                    "data_items": result.get('data_items', {}),
                    "file_descriptions": result.get('file_descriptions', {}),
                    "linkage_items": result.get('linkage_items', {}),
                    "success": True,
                    "using_real_parser": True
                }
            else:
                print(f"⚠️  ProLeap parsing failed: {result.get('error', 'Unknown error')}")
                return self._fallback_parse(file_path)
                
        except Exception as e:
            print(f"❌ ProLeap parsing error: {e}")
            return self._fallback_parse(file_path)
    
    def _create_proleap_program(self, file_path: str) -> str:
        """Create a Java program that uses ProLeap directly"""
        return f"""
import java.io.File;
import java.util.*;
import io.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl;
import io.proleap.cobol.preprocessor.CobolPreprocessor;
import io.proleap.cobol.asg.metamodel.Program;
import io.proleap.cobol.asg.metamodel.CompilationUnit;

public class RealProLeapParser {{
    public static void main(String[] args) {{
        try {{
            System.out.println("Parsing COBOL file with real ProLeap...");
            
            // Create the parser
            CobolParserRunnerImpl parser = new CobolParserRunnerImpl();
            
            // Load the COBOL file
            File cobolFile = new File("{file_path}");
            if (!cobolFile.exists()) {{
                System.out.println("ERROR: COBOL file not found: " + cobolFile.getAbsolutePath());
                System.exit(1);
            }}
            
            System.out.println("✅ COBOL file found: " + cobolFile.getAbsolutePath());
            
            // Parse the file
            Program program = parser.analyzeFile(cobolFile, CobolPreprocessor.CobolSourceFormatEnum.TANDEM);
            
            System.out.println("✅ File parsed successfully");
            
            // Extract program information
            List<CompilationUnit> compilationUnits = program.getCompilationUnits();
            
            System.out.println("SUCCESS");
            System.out.println("PROGRAM_NAME:UNKNOWN");
            System.out.println("COMPILATION_UNITS:" + compilationUnits.size());
            
            // Print compilation unit details
            for (int i = 0; i < compilationUnits.size(); i++) {{
                CompilationUnit unit = compilationUnits.get(i);
                String unitName = unit.getName();
                System.out.println("UNIT_" + i + "_NAME:" + (unitName != null ? unitName : "UNKNOWN"));
            }}
            
            // Extract entities and relationships
            System.out.println("ENTITIES_START");
            
            // Program entity
            System.out.println("ENTITY:PROGRAM:UNKNOWN");
            
            // Compilation units with detailed structure
            for (CompilationUnit unit : compilationUnits) {{
                String unitName = unit.getName();
                System.out.println("ENTITY:COMPILATION_UNIT:" + (unitName != null ? unitName : "UNKNOWN"));
                
                // Extract detailed structure using correct ProLeap API
                var programUnit = unit.getProgramUnit();
                if (programUnit != null) {{
                    // Extract data division
                    var dataDivision = programUnit.getDataDivision();
                    if (dataDivision != null) {{
                        System.out.println("DIVISION:DATA:" + unitName);
                    }}
                    
                    // Extract procedure division with paragraphs and statements
                    var procedureDivision = programUnit.getProcedureDivision();
                    if (procedureDivision != null) {{
                        System.out.println("DIVISION:PROCEDURE:" + unitName);
                        
                        // Extract paragraphs
                        var paragraphs = procedureDivision.getParagraphs();
                        if (paragraphs != null) {{
                            for (var paragraph : paragraphs) {{
                                var paraName = paragraph.getName();
                                if (paraName != null) {{
                                    System.out.println("PARAGRAPH:" + paraName + ":" + unitName);
                                    
                                    // Extract statements from paragraphs with detailed analysis
                                    var statements = paragraph.getStatements();
                                    if (statements != null) {{
                                        for (var statement : statements) {{
                                            // Get the actual COBOL text from the parser context
                                            var ctx = statement.getCtx();
                                            var stmtText = ctx != null ? ctx.getText() : statement.toString();
                                            var stmtType = statement.getClass().getSimpleName();
                                            
                                            // Extract detailed statement information based on type
                                            var stmtDetails = "";
                                            
                                            // Handle IF statements
                                            if (stmtType.equals("IfStatement")) {{
                                                var ifStmt = (io.proleap.cobol.asg.metamodel.procedure.ifstmt.IfStatement) statement;
                                                var condition = ifStmt.getCondition();
                                                if (condition != null) {{
                                                    stmtDetails = "IF_CONDITION:" + (condition.getCtx() != null ? condition.getCtx().getText() : "");
                                                }}
                                            }}
                                            // Handle PERFORM statements
                                            else if (stmtType.equals("PerformStatement")) {{
                                                var performStmt = (io.proleap.cobol.asg.metamodel.procedure.perform.PerformStatement) statement;
                                                var procedureCalls = performStmt.getProcedureCalls();
                                                if (procedureCalls != null && !procedureCalls.isEmpty()) {{
                                                    var calls = new StringBuilder();
                                                    for (var call : procedureCalls) {{
                                                        if (calls.length() > 0) calls.append(",");
                                                        calls.append(call.getName());
                                                    }}
                                                    stmtDetails = "PERFORM_TARGETS:" + calls.toString();
                                                }}
                                            }}
                                            // Handle MOVE statements
                                            else if (stmtType.equals("MoveStatement")) {{
                                                var moveStmt = (io.proleap.cobol.asg.metamodel.procedure.movestmt.MoveStatement) statement;
                                                var sendingArea = moveStmt.getSendingArea();
                                                var receivingArea = moveStmt.getReceivingArea();
                                                if (sendingArea != null && receivingArea != null) {{
                                                    stmtDetails = "MOVE_FROM:" + sendingArea.getName() + ":MOVE_TO:" + receivingArea.getName();
                                                }}
                                            }}
                                            // Handle ADD statements
                                            else if (stmtType.equals("AddStatement")) {{
                                                var addStmt = (io.proleap.cobol.asg.metamodel.procedure.add.AddStatement) statement;
                                                var operands = addStmt.getOperands();
                                                if (operands != null && !operands.isEmpty()) {{
                                                    var ops = new StringBuilder();
                                                    for (var operand : operands) {{
                                                        if (ops.length() > 0) ops.append(",");
                                                        ops.append(operand.getName());
                                                    }}
                                                    stmtDetails = "ADD_OPERANDS:" + ops.toString();
                                                }}
                                            }}
                                            // Handle SUBTRACT statements
                                            else if (stmtType.equals("SubtractStatement")) {{
                                                var subStmt = (io.proleap.cobol.asg.metamodel.procedure.subtract.SubtractStatement) statement;
                                                var operands = subStmt.getOperands();
                                                if (operands != null && !operands.isEmpty()) {{
                                                    var ops = new StringBuilder();
                                                    for (var operand : operands) {{
                                                        if (ops.length() > 0) ops.append(",");
                                                        ops.append(operand.getName());
                                                    }}
                                                    stmtDetails = "SUBTRACT_OPERANDS:" + ops.toString();
                                                }}
                                            }}
                                            // Handle COMPUTE statements
                                            else if (stmtType.equals("ComputeStatement")) {{
                                                var computeStmt = (io.proleap.cobol.asg.metamodel.procedure.compute.ComputeStatement) statement;
                                                var arithmeticExpression = computeStmt.getArithmeticExpression();
                                                if (arithmeticExpression != null) {{
                                                    stmtDetails = "COMPUTE_EXPR:" + (arithmeticExpression.getCtx() != null ? arithmeticExpression.getCtx().getText() : "");
                                                }}
                                            }}
                                            // Handle EVALUATE statements
                                            else if (stmtType.equals("EvaluateStatement")) {{
                                                var evalStmt = (io.proleap.cobol.asg.metamodel.procedure.evaluate.EvaluateStatement) statement;
                                                var subjects = evalStmt.getSubjects();
                                                if (subjects != null && !subjects.isEmpty()) {{
                                                    var subjs = new StringBuilder();
                                                    for (var subject : subjects) {{
                                                        if (subjs.length() > 0) subjs.append(",");
                                                        subjs.append(subject.getName());
                                                    }}
                                                    stmtDetails = "EVALUATE_SUBJECTS:" + subjs.toString();
                                                }}
                                            }}
                                            
                                            System.out.println("STATEMENT:" + paraName + ":" + stmtType + ":" + stmtDetails + ":" + stmtText + ":" + unitName);
                                        }}
                                    }}
                                    
                                    // Extract procedure calls (PERFORM statements)
                                    var calls = paragraph.getCalls();
                                    if (calls != null) {{
                                        for (var call : calls) {{
                                            // Get the actual COBOL text from the parser context
                                            var ctx = call.getCtx();
                                            var callText = ctx != null ? ctx.getText() : call.toString();
                                            System.out.println("CALL:" + paraName + ":" + callText + ":" + unitName);
                                        }}
                                    }}
                                }}
                            }}
                        }}
                    }}
                }}
            }}
            
            // Extract Data Division details with enhanced structure analysis
            try {{
                var dataDivision = unit.getProgramUnit().getDataDivision();
                if (dataDivision != null) {{
                    System.out.println("DATA_DIVISION_FOUND:" + unitName);
                    
                    // Working Storage Section with detailed analysis
                    var workingStorage = dataDivision.getWorkingStorageSection();
                    if (workingStorage != null) {{
                        var dataEntries = workingStorage.getDataDescriptionEntries();
                        if (dataEntries != null) {{
                            for (var entry : dataEntries) {{
                                var entryName = entry.getName();
                                var entryLevel = entry.getLevelNumber();
                                var entryType = entry.getClass().getSimpleName();
                                var entryText = entry.getCtx() != null ? entry.getCtx().getText() : entry.toString();
                                
                                // Extract Picture Clause details
                                var pictureClause = entry.getPictureClause();
                                var pictureText = "";
                                if (pictureClause != null) {{
                                    pictureText = pictureClause.getCtx() != null ? pictureClause.getCtx().getText() : "";
                                }}
                                
                                // Extract Usage Clause
                                var usageClause = entry.getUsageClause();
                                var usageText = "";
                                if (usageClause != null) {{
                                    usageText = usageClause.getCtx() != null ? usageClause.getCtx().getText() : "";
                                }}
                                
                                // Extract Value Clause
                                var valueClause = entry.getValueClause();
                                var valueText = "";
                                if (valueClause != null) {{
                                    valueText = valueClause.getCtx() != null ? valueClause.getCtx().getText() : "";
                                }}
                                
                                // Extract 88-level conditions
                                var conditionNames = entry.getConditionNames();
                                var conditionText = "";
                                if (conditionNames != null && !conditionNames.isEmpty()) {{
                                    var conditions = new StringBuilder();
                                    for (var condition : conditionNames) {{
                                        if (conditions.length() > 0) conditions.append("|");
                                        conditions.append(condition.getName());
                                        var values = condition.getValues();
                                        if (values != null && !values.isEmpty()) {{
                                            conditions.append(":");
                                            for (var value : values) {{
                                                conditions.append(value.getLiteral().getValue());
                                                conditions.append(",");
                                            }}
                                            if (conditions.length() > 0 && conditions.charAt(conditions.length()-1) == ',') {{
                                                conditions.setLength(conditions.length()-1);
                                            }}
                                        }}
                                    }}
                                    conditionText = conditions.toString();
                                }}
                                
                                System.out.println("DATA_ITEM:" + entryName + ":" + entryLevel + ":" + entryType + ":" + 
                                                 pictureText + ":" + usageText + ":" + valueText + ":" + conditionText + ":" + entryText + ":" + unitName);
                            }}
                        }}
                    }}
                    
                    // File Section
                    var fileSection = dataDivision.getFileSection();
                    if (fileSection != null) {{
                        var fileEntries = fileSection.getFileDescriptionEntries();
                        if (fileEntries != null) {{
                            for (var entry : fileEntries) {{
                                var fileName = entry.getName();
                                var fileText = entry.getCtx() != null ? entry.getCtx().getText() : entry.toString();
                                System.out.println("FILE_DESC:" + fileName + ":" + fileText + ":" + unitName);
                            }}
                        }}
                    }}
                    
                    // Linkage Section
                    var linkageSection = dataDivision.getLinkageSection();
                    if (linkageSection != null) {{
                        var linkageEntries = linkageSection.getDataDescriptionEntries();
                        if (linkageEntries != null) {{
                            for (var entry : linkageEntries) {{
                                var entryName = entry.getName();
                                var entryLevel = entry.getLevelNumber();
                                var entryType = entry.getClass().getSimpleName();
                                var entryText = entry.getCtx() != null ? entry.getCtx().getText() : entry.toString();
                                System.out.println("LINKAGE_ITEM:" + entryName + ":" + entryLevel + ":" + entryType + ":" + entryText + ":" + unitName);
                            }}
                        }}
                    }}
                }}
            }} catch (Exception e) {{
                System.out.println("DATA_DIVISION_ERROR:" + e.getMessage());
            }}
            
            // Extract Environment Division details (simplified for now)
            try {{
                var environmentDivision = unit.getProgramUnit().getEnvironmentDivision();
                if (environmentDivision != null) {{
                    System.out.println("ENVIRONMENT_DIVISION_FOUND:" + unitName);
                }}
            }} catch (Exception e) {{
                // Environment division extraction not available yet
            }}
            
            System.out.println("ENTITIES_END");
            
        }} catch (Exception e) {{
            System.out.println("ERROR:" + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }}
    }}
}}
"""
    
    def _run_proleap_parser(self, java_file: str, cobol_file: str, temp_dir: str) -> Dict[str, Any]:
        """Run the ProLeap parser using Maven"""
        target_java_file = None
        try:
            # Copy the Java file to the ProLeap project
            proleap_src_dir = os.path.join(self.proleap_dir, 'src', 'main', 'java')
            if not os.path.exists(proleap_src_dir):
                os.makedirs(proleap_src_dir)
            
            target_java_file = os.path.join(proleap_src_dir, 'RealProLeapParser.java')
            import shutil
            shutil.copy2(java_file, target_java_file)
            # Compile using Maven
            compile_result = subprocess.run([
                'mvn', 'compile', '-q'
            ], capture_output=True, text=True, timeout=60, cwd=self.proleap_dir)
            
            if compile_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Compilation failed: {compile_result.stderr}'
                }
            
            # Add a small delay to ensure file system is ready
            import time
            time.sleep(0.5)
            
            # Run the compiled program directly with java command
            # First, copy dependencies to target/dependency
            copy_deps_result = subprocess.run([
                'mvn', 'dependency:copy-dependencies', '-q'
            ], capture_output=True, text=True, timeout=60, cwd=self.proleap_dir)
            
            if copy_deps_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Failed to copy dependencies: {copy_deps_result.stderr}'
                }
            
            # Build classpath
            classpath_parts = [os.path.join(self.proleap_dir, 'target', 'classes')]
            
            # Add all JAR files from target/dependency
            dependency_dir = os.path.join(self.proleap_dir, 'target', 'dependency')
            if os.path.exists(dependency_dir):
                jar_files = [os.path.join(dependency_dir, f) for f in os.listdir(dependency_dir) if f.endswith('.jar')]
                classpath_parts.extend(jar_files)
            
            classpath = ':'.join(classpath_parts)
            
            run_result = subprocess.run([
                'java', '-cp', classpath, 'RealProLeapParser'
            ], capture_output=True, text=True, timeout=120, cwd=self.proleap_dir)
            
            # Check if the output contains SUCCESS (check both stdout and stderr)
            combined_output = run_result.stdout + run_result.stderr
            
            # Check if the output contains SUCCESS (check both stdout and stderr)
            
            if 'SUCCESS' in combined_output:
                # Parse the output
                result = self._parse_proleap_output(combined_output)
                return result
            else:
                return {
                    'success': False,
                    'error': f'Execution failed: {run_result.stderr}'
                }
            
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'error': 'ProLeap parser timed out'
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
        finally:
            # Don't clean up here - let the Java file persist for debugging
            pass
    
    def _parse_proleap_output(self, output: str) -> Dict[str, Any]:
        """Parse the output from the ProLeap program"""
        lines = output.strip().split('\n')
        
        # Find the SUCCESS line
        success_line_index = -1
        for i, line in enumerate(lines):
            if line.strip() == 'SUCCESS':
                success_line_index = i
                break
        
        if success_line_index == -1:
            return {
                'success': False,
                'error': f'ProLeap program failed: {output}'
            }
        
        result = {
            'success': True,
            'compilation_units': [],
            'entities': [],
            'ast_data': {},
            'divisions': {},
            'paragraphs': {},
            'statements': {}
        }
        
        parsing_entities = False
        
        for line in lines[success_line_index + 1:]:
            # Handle special lines without colons
            if line.strip() == 'ENTITIES_START':
                parsing_entities = True
                continue
            elif line.strip() == 'ENTITIES_END':
                parsing_entities = False
                continue
            
            if ':' in line:
                key, value = line.split(':', 1)
                
                if key == 'PROGRAM_NAME':
                    result['ast_data']['program_name'] = value
                elif key == 'COMPILATION_UNITS':
                    result['ast_data']['compilation_units_count'] = int(value)
                elif key.startswith('UNIT_') and key.endswith('_NAME'):
                    result['compilation_units'].append({
                        'name': value,
                        'type': 'compilation_unit'
                    })
                elif parsing_entities and key == 'ENTITY':
                    parts = value.split(':', 1)
                    if len(parts) == 2:
                        entity_type, entity_name = parts
                        entity = {
                            'type': entity_type.lower(),
                            'name': entity_name
                        }
                        result['entities'].append(entity)
                elif key == 'DIVISION':
                    parts = value.split(':', 1)
                    if len(parts) == 2:
                        div_type, unit_name = parts
                        if unit_name not in result['divisions']:
                            result['divisions'][unit_name] = {}
                        result['divisions'][unit_name][div_type.lower()] = True
                elif key == 'PARAGRAPH':
                    parts = value.split(':', 1)
                    if len(parts) == 2:
                        para_name, unit_name = parts
                        if unit_name not in result['paragraphs']:
                            result['paragraphs'][unit_name] = []
                        result['paragraphs'][unit_name].append({
                            'name': para_name,
                            'unit': unit_name
                        })
                elif key == 'STATEMENT':
                    parts = value.split(':', 4)
                    if len(parts) >= 3:
                        para_name = parts[0]
                        stmt_type = parts[1] if len(parts) > 1 else "UNKNOWN"
                        stmt_details = parts[2] if len(parts) > 2 else ""
                        stmt_text = parts[3] if len(parts) > 3 else parts[2] if len(parts) == 3 else ""
                        unit_name = parts[4] if len(parts) > 4 else parts[3] if len(parts) == 4 else parts[2] if len(parts) == 3 else ""
                        
                        if unit_name not in result['statements']:
                            result['statements'][unit_name] = {}
                        if para_name not in result['statements'][unit_name]:
                            result['statements'][unit_name][para_name] = []
                        
                        # Store enhanced statement information
                        stmt_info = {
                            'text': stmt_text,
                            'type': stmt_type,
                            'details': stmt_details
                        }
                        result['statements'][unit_name][para_name].append(stmt_info)
                elif key == 'DATA_ITEM':
                    parts = value.split(':', 8)
                    if len(parts) >= 6:
                        item_name = parts[0]
                        item_level = parts[1]
                        item_type = parts[2]
                        picture_clause = parts[3] if len(parts) > 3 else ""
                        usage_clause = parts[4] if len(parts) > 4 else ""
                        value_clause = parts[5] if len(parts) > 5 else ""
                        condition_names = parts[6] if len(parts) > 6 else ""
                        item_text = parts[7] if len(parts) > 7 else ""
                        unit_name = parts[8] if len(parts) > 8 else ""
                        
                        if 'data_items' not in result:
                            result['data_items'] = {}
                        if unit_name not in result['data_items']:
                            result['data_items'][unit_name] = []
                        
                        data_item = {
                            'name': item_name,
                            'level': item_level,
                            'type': item_type,
                            'picture_clause': picture_clause,
                            'usage_clause': usage_clause,
                            'value_clause': value_clause,
                            'condition_names': condition_names,
                            'text': item_text,
                            'unit': unit_name
                        }
                        result['data_items'][unit_name].append(data_item)
                elif key == 'FILE_DESC':
                    parts = value.split(':', 2)
                    if len(parts) == 3:
                        file_name, file_text, unit_name = parts
                        if 'file_descriptions' not in result:
                            result['file_descriptions'] = {}
                        if unit_name not in result['file_descriptions']:
                            result['file_descriptions'][unit_name] = []
                        
                        file_desc = {
                            'name': file_name,
                            'text': file_text,
                            'unit': unit_name
                        }
                        result['file_descriptions'][unit_name].append(file_desc)
                elif key == 'LINKAGE_ITEM':
                    parts = value.split(':', 4)
                    if len(parts) >= 4:
                        item_name = parts[0]
                        item_level = parts[1]
                        item_type = parts[2]
                        item_text = parts[3]
                        unit_name = parts[4] if len(parts) > 4 else ""
                        
                        if 'linkage_items' not in result:
                            result['linkage_items'] = {}
                        if unit_name not in result['linkage_items']:
                            result['linkage_items'][unit_name] = []
                        
                        linkage_item = {
                            'name': item_name,
                            'level': item_level,
                            'type': item_type,
                            'text': item_text,
                            'unit': unit_name
                        }
                        result['linkage_items'][unit_name].append(linkage_item)
                elif key == 'CALL':
                    parts = value.split(':', 2)
                    if len(parts) == 3:
                        para_name, call_text, unit_name = parts
                        if unit_name not in result['statements']:
                            result['statements'][unit_name] = {}
                        if para_name not in result['statements'][unit_name]:
                            result['statements'][unit_name][para_name] = []
                        result['statements'][unit_name][para_name].append(call_text)
        return result
    
    def _fallback_parse(self, file_path: str) -> Dict[str, Any]:
        """Fallback parsing when ProLeap is not available"""
        return {
            "parse_success": False,
            "success": False,
            "error": "Real ProLeap parser not available",
            "file_path": file_path,
            "language": "cobol",
            "entities": [],
            "using_real_parser": False
        }
    
    def is_available(self) -> bool:
        """Check if real ProLeap parser is available"""
        return self.using_real_parser

