"""
Simple test to verify ProLeap classes can be used
"""

import os
import subprocess
import tempfile
from typing import Dict, List, Any, Optional


class SimpleProLeapTest:
    """Simple test to verify ProLeap classes can be used"""
    
    def __init__(self):
        self.java_available = self._check_java()
        self.maven_available = self._check_maven()
        self.proleap_available = self._check_proleap()
        self._setup_test()
    
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
    
    def _setup_test(self):
        """Setup the test"""
        if not self.java_available:
            print("❌ Java not available")
            self.using_proleap = False
            return
        
        if not self.maven_available:
            print("❌ Maven not available")
            self.using_proleap = False
            return
        
        if not self.proleap_available:
            print("❌ ProLeap project not found")
            self.using_proleap = False
            return
        
        self.using_proleap = True
        print(f"✅ Simple ProLeap test ready with: {self.proleap_dir}")
    
    def test_proleap_classes(self) -> Dict[str, Any]:
        """Test if ProLeap classes can be used"""
        if not self.using_proleap:
            return {"success": False, "error": "ProLeap not available"}
        
        try:
            # Create a simple Java program that tests ProLeap classes
            java_code = """
import java.io.File;
import java.util.*;

public class SimpleProLeapTest {
    public static void main(String[] args) {
        try {
            System.out.println("Testing ProLeap classes...");
            
            // Test if we can load the classes
            try {
                Class<?> runnerClass = Class.forName("io.github.uwol.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl");
                System.out.println("✅ CobolParserRunnerImpl loaded");
                
                Class<?> preprocessorClass = Class.forName("io.github.uwol.proleap.cobol.preprocessor.CobolPreprocessor");
                System.out.println("✅ CobolPreprocessor loaded");
                
                Class<?> programClass = Class.forName("io.github.uwol.proleap.cobol.asg.metamodel.Program");
                System.out.println("✅ Program loaded");
                
                // Try to create an instance
                Object parser = runnerClass.getDeclaredConstructor().newInstance();
                System.out.println("✅ Parser instance created");
                
                System.out.println("SUCCESS");
                System.out.println("CLASSES_LOADED:true");
                
            } catch (ClassNotFoundException e) {
                System.out.println("ERROR: Class not found: " + e.getMessage());
                e.printStackTrace();
            } catch (Exception e) {
                System.out.println("ERROR: " + e.getMessage());
                e.printStackTrace();
            }
            
        } catch (Exception e) {
            System.out.println("ERROR: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
"""
            
            # Write Java code to temporary file
            temp_dir = tempfile.mkdtemp()
            java_file = os.path.join(temp_dir, 'SimpleProLeapTest.java')
            
            with open(java_file, 'w') as f:
                f.write(java_code)
            
            # Copy to ProLeap project and run
            result = self._run_proleap_test(java_file, temp_dir)
            
            # Clean up
            import shutil
            shutil.rmtree(temp_dir)
            
            return result
                
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _run_proleap_test(self, java_file: str, temp_dir: str) -> Dict[str, Any]:
        """Run the ProLeap test"""
        try:
            # Copy the Java file to the ProLeap project
            proleap_src_dir = os.path.join(self.proleap_dir, 'src', 'main', 'java')
            if not os.path.exists(proleap_src_dir):
                os.makedirs(proleap_src_dir)
            
            target_java_file = os.path.join(proleap_src_dir, 'SimpleProLeapTest.java')
            import shutil
            shutil.copy2(java_file, target_java_file)
            
            # Compile and run using Maven
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
                'mvn', 'exec:java', '-Dexec.mainClass="SimpleProLeapTest"', '-q'
            ], capture_output=True, text=True, timeout=60, cwd=self.proleap_dir)
            
            # Clean up the copied file
            if os.path.exists(target_java_file):
                os.remove(target_java_file)
            
            if run_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Execution failed: {run_result.stderr}'
                }
            
            # Parse the output
            return self._parse_test_output(run_result.stdout)
            
        except subprocess.TimeoutExpired:
            return {
                'success': False,
                'error': 'ProLeap test timed out'
            }
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def _parse_test_output(self, output: str) -> Dict[str, Any]:
        """Parse the output from the test program"""
        lines = output.strip().split('\n')
        
        if not lines or lines[0] != 'SUCCESS':
            return {
                'success': False,
                'error': f'Test program failed: {output}'
            }
        
        result = {'success': True}
        
        for line in lines[1:]:
            if ':' in line:
                key, value = line.split(':', 1)
                result[key.lower()] = value
        
        return result
