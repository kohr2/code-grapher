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
            
            // Use visitor pattern to extract COBOL constructs
            System.out.println("DEBUG: Creating visitor...");
            var visitor = new io.proleap.cobol.asg.visitor.impl.CobolProcedureStatementVisitorImpl(program) {{
                @Override
                public Boolean visitCallStatement(io.proleap.cobol.CobolParser.CallStatementContext ctx) {{
                    System.out.println("DEBUG: visitCallStatement called");
                    var scope = findScope(ctx);
                    var compilationUnit = findCompilationUnit(ctx);
                    var programUnit = findProgramUnit(ctx);
                    
                    // Extract program name from identifier or literal
                    String programName = "UNKNOWN";
                    if (ctx.identifier() != null) {{
                        programName = ctx.identifier().getText();
                    }} else if (ctx.literal() != null) {{
                        programName = ctx.literal().getText();
                    }}
                    
                    var unitName = compilationUnit != null ? compilationUnit.getName() : "UNKNOWN";
                    System.out.println("CALL_STATEMENT:" + programName + ":" + unitName);
                    
                    // Extract USING parameters
                    if (ctx.callUsingPhrase() != null) {{
                        var usingCtx = ctx.callUsingPhrase();
                        if (usingCtx.callUsingParameter() != null) {{
                            for (var param : usingCtx.callUsingParameter()) {{
                                String paramName = param.getText();
                                String paramType = "UNKNOWN";
                                if (param.callByReferencePhrase() != null) {{
                                    paramType = "REFERENCE";
                                }} else if (param.callByValuePhrase() != null) {{
                                    paramType = "VALUE";
                                }} else if (param.callByContentPhrase() != null) {{
                                    paramType = "CONTENT";
                                }}
                                System.out.println("CALL_PARAM:" + programName + ":" + paramType + ":" + paramName + ":" + unitName);
                            }}
                        }}
                    }}
                    
                    // Extract GIVING parameter
                    if (ctx.callGivingPhrase() != null) {{
                        var givingCtx = ctx.callGivingPhrase();
                        if (givingCtx.identifier() != null) {{
                            String givingParam = givingCtx.identifier().getText();
                            System.out.println("CALL_GIVING:" + programName + ":" + givingParam + ":" + unitName);
                        }}
                    }}
                    
                    return visitChildren(ctx);
                }}
                
                @Override
                public Boolean visitCopyStatement(io.proleap.cobol.CobolParser.CopyStatementContext ctx) {{
                    var compilationUnit = findCompilationUnit(ctx);
                    var unitName = compilationUnit != null ? compilationUnit.getName() : "UNKNOWN";
                    
                    if (ctx.copySource() != null) {{
                        var copyName = ctx.copySource().getText();
                        var copyLibrary = ctx.libraryName() != null ? ctx.libraryName().getText() : "";
                        System.out.println("COPY_STATEMENT:" + copyName + ":" + copyLibrary + ":" + unitName);
                        
                        // Extract replacing phrases
                        if (ctx.replacingPhrase() != null) {{
                            var replacingCtx = ctx.replacingPhrase();
                            if (replacingCtx.replaceable() != null && replacingCtx.replacement() != null) {{
                                for (int i = 0; i < Math.min(replacingCtx.replaceable().size(), replacingCtx.replacement().size()); i++) {{
                                    var replaceable = replacingCtx.replaceable(i).getText();
                                    var replacement = replacingCtx.replacement(i).getText();
                                    System.out.println("REPLACING:" + copyName + ":" + replaceable + ":" + replacement + ":" + unitName);
                                }}
                            }}
                        }}
                    }}
                    
                    return visitChildren(ctx);
                }}
                
                @Override
                public Boolean visitUseStatement(io.proleap.cobol.CobolParser.UseStatementContext ctx) {{
                    var compilationUnit = findCompilationUnit(ctx);
                    var unitName = compilationUnit != null ? compilationUnit.getName() : "UNKNOWN";
                    
                    var useType = ctx.USE() != null ? ctx.USE().getText() : "UNKNOWN";
                    var fileName = ctx.fileName() != null ? ctx.fileName().getText() : "";
                    var procedureName = ctx.procedureName() != null ? ctx.procedureName().getText() : "";
                    
                    System.out.println("USE_STATEMENT:" + useType + ":" + fileName + ":" + procedureName + ":" + unitName);
                    
                    return visitChildren(ctx);
                }}
            }};
            
            // Visit all compilation units
            System.out.println("DEBUG: Visiting compilation units...");
            for (CompilationUnit unit : compilationUnits) {{
                System.out.println("DEBUG: Visiting unit: " + unit.getName());
                visitor.visit(unit.getCtx());
            }}
            
            // Compilation units with basic structure
            for (CompilationUnit unit : compilationUnits) {{
                String unitName = unit.getName();
                System.out.println("ENTITY:COMPILATION_UNIT:" + (unitName != null ? unitName : "UNKNOWN"));
                
                // Extract basic structure using correct ProLeap API
                var programUnit = unit.getProgramUnit();
                if (programUnit != null) {{
                    // Extract data division
                    var dataDivision = programUnit.getDataDivision();
                    if (dataDivision != null) {{
                        System.out.println("DIVISION:DATA:" + unitName);
                        
                        // Note: Communication and Screen sections require more complex ASG traversal
                        // For now, we'll focus on basic program structure
                    }}
                    
                    // Extract procedure division with paragraphs and statements
                    var procedureDivision = programUnit.getProcedureDivision();
                    if (procedureDivision != null) {{
                        System.out.println("DIVISION:PROCEDURE:" + unitName);
                        
                        // USE statements are now handled by the visitor pattern above
                        
                        // Extract paragraphs
                        var paragraphs = procedureDivision.getParagraphs();
                        if (paragraphs != null) {{
                            for (var paragraph : paragraphs) {{
                                var paraName = paragraph.getName();
                                if (paraName != null) {{
                                    System.out.println("PARAGRAPH:" + paraName + ":" + unitName);
                                    
                                    // Extract statements from paragraphs
                                    var statements = paragraph.getStatements();
                                    if (statements != null) {{
                                        for (var statement : statements) {{
                                            // Get the actual COBOL text from the parser context
                                            var ctx = statement.getCtx();
                                            var stmtText = ctx != null ? ctx.getText() : statement.toString();
                                            var stmtType = statement.getClass().getSimpleName();
                                            
                                            System.out.println("STATEMENT:" + paraName + ":" + stmtType + ":" + stmtText + ":" + unitName);
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
                                    
                                    // CALL statements are now handled by the visitor pattern above
                                }}
                            }}
                        }}
                    }}
                }}
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
                elif key == 'COPY_STATEMENT':
                    parts = value.split(':', 3)
                    if len(parts) >= 3:
                        copy_name, copy_library, unit_name = parts[0], parts[1], parts[2]
                        if 'copy_statements' not in result:
                            result['copy_statements'] = {}
                        if unit_name not in result['copy_statements']:
                            result['copy_statements'][unit_name] = []
                        result['copy_statements'][unit_name].append({
                            'name': copy_name,
                            'library': copy_library,
                            'unit': unit_name
                        })
                elif key == 'REPLACING':
                    parts = value.split(':', 4)
                    if len(parts) >= 4:
                        copy_name, replaceable, replacement, unit_name = parts[0], parts[1], parts[2], parts[3]
                        if 'replacing_phrases' not in result:
                            result['replacing_phrases'] = {}
                        if unit_name not in result['replacing_phrases']:
                            result['replacing_phrases'][unit_name] = {}
                        if copy_name not in result['replacing_phrases'][unit_name]:
                            result['replacing_phrases'][unit_name][copy_name] = []
                        result['replacing_phrases'][unit_name][copy_name].append({
                            'replaceable': replaceable,
                            'replacement': replacement
                        })
                elif key == 'CALL_STATEMENT':
                    parts = value.split(':', 2)
                    if len(parts) >= 2:
                        program_name, unit_name = parts[0], parts[1]
                        if 'call_statements' not in result:
                            result['call_statements'] = {}
                        if unit_name not in result['call_statements']:
                            result['call_statements'][unit_name] = []
                        result['call_statements'][unit_name].append({
                            'program_name': program_name,
                            'unit': unit_name
                        })
                elif key == 'CALL_PARAM':
                    parts = value.split(':', 4)
                    if len(parts) >= 4:
                        program_name, param_type, param_name, unit_name = parts[0], parts[1], parts[2], parts[3]
                        if 'call_parameters' not in result:
                            result['call_parameters'] = {}
                        if unit_name not in result['call_parameters']:
                            result['call_parameters'][unit_name] = []
                        result['call_parameters'][unit_name].append({
                            'program_name': program_name,
                            'param_type': param_type,
                            'param_name': param_name,
                            'unit': unit_name
                        })
                elif key == 'CALL_GIVING':
                    parts = value.split(':', 3)
                    if len(parts) >= 3:
                        program_name, giving_param, unit_name = parts[0], parts[1], parts[2]
                        if 'call_giving' not in result:
                            result['call_giving'] = {}
                        if unit_name not in result['call_giving']:
                            result['call_giving'][unit_name] = []
                        result['call_giving'][unit_name].append({
                            'program_name': program_name,
                            'giving_param': giving_param,
                            'unit': unit_name
                        })
                elif key == 'CALL_PARAM':
                    parts = value.split(':', 5)
                    if len(parts) >= 5:
                        para_name, program_name, param_type, param_name, unit_name = parts[0], parts[1], parts[2], parts[3], parts[4]
                        if 'call_parameters' not in result:
                            result['call_parameters'] = {}
                        if unit_name not in result['call_parameters']:
                            result['call_parameters'][unit_name] = {}
                        if para_name not in result['call_parameters'][unit_name]:
                            result['call_parameters'][unit_name][para_name] = []
                        result['call_parameters'][unit_name][para_name].append({
                            'program_name': program_name,
                            'param_type': param_type,
                            'param_name': param_name
                        })
                elif key == 'CALL_GIVING':
                    parts = value.split(':', 4)
                    if len(parts) >= 4:
                        para_name, program_name, giving_param, unit_name = parts[0], parts[1], parts[2], parts[3]
                        if 'call_giving' not in result:
                            result['call_giving'] = {}
                        if unit_name not in result['call_giving']:
                            result['call_giving'][unit_name] = {}
                        if para_name not in result['call_giving'][unit_name]:
                            result['call_giving'][unit_name][para_name] = []
                        result['call_giving'][unit_name][para_name].append({
                            'program_name': program_name,
                            'giving_param': giving_param
                        })
                elif key == 'USE_STATEMENT':
                    parts = value.split(':', 4)
                    if len(parts) >= 4:
                        use_type, file_name, procedure_name, unit_name = parts[0], parts[1], parts[2], parts[3]
                        if 'use_statements' not in result:
                            result['use_statements'] = {}
                        if unit_name not in result['use_statements']:
                            result['use_statements'][unit_name] = []
                        result['use_statements'][unit_name].append({
                            'use_type': use_type,
                            'file_name': file_name,
                            'procedure_name': procedure_name,
                            'unit': unit_name
                        })
                elif key == 'COMMUNICATION':
                    parts = value.split(':', 5)
                    if len(parts) >= 5:
                        comm_name, comm_type, symbolic_queue, symbolic_destination, unit_name = parts[0], parts[1], parts[2], parts[3], parts[4]
                        if 'communication' not in result:
                            result['communication'] = {}
                        if unit_name not in result['communication']:
                            result['communication'][unit_name] = []
                        result['communication'][unit_name].append({
                            'name': comm_name,
                            'type': comm_type,
                            'symbolic_queue': symbolic_queue,
                            'symbolic_destination': symbolic_destination,
                            'unit': unit_name
                        })
                elif key == 'SCREEN':
                    parts = value.split(':', 5)
                    if len(parts) >= 5:
                        screen_name, screen_value, screen_from, screen_to, unit_name = parts[0], parts[1], parts[2], parts[3], parts[4]
                        if 'screens' not in result:
                            result['screens'] = {}
                        if unit_name not in result['screens']:
                            result['screens'][unit_name] = []
                        result['screens'][unit_name].append({
                            'name': screen_name,
                            'value': screen_value,
                            'from': screen_from,
                            'to': screen_to,
                            'unit': unit_name
                        })
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

