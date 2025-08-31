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
        proleap_dir = 'proleap-cobol-parser'
        if os.path.exists(proleap_dir) and os.path.exists(os.path.join(proleap_dir, 'pom.xml')):
            self.proleap_dir = proleap_dir
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
            
            // Compilation units
            for (CompilationUnit unit : compilationUnits) {{
                String unitName = unit.getName();
                System.out.println("ENTITY:COMPILATION_UNIT:" + (unitName != null ? unitName : "UNKNOWN"));
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
            
            # Run the compiled program
            run_result = subprocess.run([
                'mvn', 'exec:java', '-Dexec.mainClass="RealProLeapParser"', '-q'
            ], capture_output=True, text=True, timeout=120, cwd=self.proleap_dir)
            
            # Clean up the copied file
            if os.path.exists(target_java_file):
                os.remove(target_java_file)
            
            # Check if the output contains SUCCESS (ignore warnings)
            if 'SUCCESS' in run_result.stdout:
                # Parse the output
                return self._parse_proleap_output(run_result.stdout)
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
            'ast_data': {}
        }
        
        parsing_entities = False
        
        for line in lines[success_line_index + 1:]:
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
                elif key == 'ENTITIES_START':
                    parsing_entities = True
                elif key == 'ENTITIES_END':
                    parsing_entities = False
                elif parsing_entities and key == 'ENTITY':
                    parts = value.split(':', 1)
                    if len(parts) == 2:
                        entity_type, entity_name = parts
                        result['entities'].append({
                            'type': entity_type.lower(),
                            'name': entity_name
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

