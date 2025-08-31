"""
Simple test parser to verify Java setup
"""

import os
import subprocess
import tempfile
from typing import Dict, List, Any, Optional


class SimpleTestParser:
    """Simple test parser to verify Java setup"""
    
    def __init__(self):
        self.java_available = self._check_java()
        self._setup_parser()
    
    def _check_java(self) -> bool:
        """Check if Java is available"""
        try:
            result = subprocess.run(['java', '-version'], 
                                  capture_output=True, text=True, timeout=10)
            return result.returncode == 0
        except Exception:
            return False
    
    def _setup_parser(self):
        """Setup the parser"""
        if not self.java_available:
            print("❌ Java not available")
            self.using_java = False
            return
        
        self.using_java = True
        print("✅ Simple test parser ready")
    
    def test_java_setup(self) -> Dict[str, Any]:
        """Test basic Java setup"""
        if not self.using_java:
            return {"success": False, "error": "Java not available"}
        
        try:
            # Create a simple Java program
            java_code = """
public class SimpleTest {
    public static void main(String[] args) {
        System.out.println("SUCCESS");
        System.out.println("JAVA_VERSION:" + System.getProperty("java.version"));
        System.out.println("JAVA_HOME:" + System.getProperty("java.home"));
        System.out.println("CLASS_PATH:" + System.getProperty("java.class.path"));
    }
}
"""
            
            # Write Java code to temporary file
            temp_dir = tempfile.mkdtemp()
            java_file = os.path.join(temp_dir, 'SimpleTest.java')
            
            with open(java_file, 'w') as f:
                f.write(java_code)
            
            # Compile and run
            compile_result = subprocess.run([
                'javac', java_file
            ], capture_output=True, text=True, timeout=30, cwd=temp_dir)
            
            if compile_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Compilation failed: {compile_result.stderr}'
                }
            
            run_result = subprocess.run([
                'java', '-cp', temp_dir, 'SimpleTest'
            ], capture_output=True, text=True, timeout=30)
            
            # Clean up
            import shutil
            shutil.rmtree(temp_dir)
            
            if run_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Execution failed: {run_result.stderr}'
                }
            
            # Parse output
            lines = run_result.stdout.strip().split('\n')
            if lines and lines[0] == 'SUCCESS':
                result = {'success': True}
                for line in lines[1:]:
                    if ':' in line:
                        key, value = line.split(':', 1)
                        result[key.lower()] = value
                return result
            else:
                return {
                    'success': False,
                    'error': f'Unexpected output: {run_result.stdout}'
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
    
    def test_jar_loading(self) -> Dict[str, Any]:
        """Test if we can load JARs"""
        if not self.using_java:
            return {"success": False, "error": "Java not available"}
        
        try:
            # Test with a simple JAR
            java_code = """
import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

public class JarTest {
    public static void main(String[] args) {
        try {
            System.out.println("SUCCESS");
            
            // Test if we can create a class loader
            File jarFile = new File("cobol-support/lib/proleap-cobol-parser-4.0.0.jar");
            if (jarFile.exists()) {
                System.out.println("JAR_EXISTS:true");
                System.out.println("JAR_SIZE:" + jarFile.length());
                
                URLClassLoader classLoader = new URLClassLoader(
                    new URL[]{jarFile.toURI().toURL()},
                    JarTest.class.getClassLoader()
                );
                System.out.println("CLASSLOADER_CREATED:true");
                
                // Try to list some classes
                try {
                    Class<?> testClass = classLoader.loadClass("io.proleap.cobol.CobolParser");
                    System.out.println("CLASS_LOADED:true");
                    System.out.println("CLASS_NAME:" + testClass.getName());
                } catch (ClassNotFoundException e) {
                    System.out.println("CLASS_LOADED:false");
                    System.out.println("ERROR:" + e.getMessage());
                }
            } else {
                System.out.println("JAR_EXISTS:false");
            }
            
        } catch (Exception e) {
            System.out.println("ERROR:" + e.getMessage());
            e.printStackTrace();
        }
    }
}
"""
            
            # Write Java code to temporary file
            temp_dir = tempfile.mkdtemp()
            java_file = os.path.join(temp_dir, 'JarTest.java')
            
            with open(java_file, 'w') as f:
                f.write(java_code)
            
            # Compile and run
            compile_result = subprocess.run([
                'javac', java_file
            ], capture_output=True, text=True, timeout=30, cwd=temp_dir)
            
            if compile_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Compilation failed: {compile_result.stderr}'
                }
            
            run_result = subprocess.run([
                'java', '-cp', temp_dir, 'JarTest'
            ], capture_output=True, text=True, timeout=30)
            
            # Clean up
            import shutil
            shutil.rmtree(temp_dir)
            
            if run_result.returncode != 0:
                return {
                    'success': False,
                    'error': f'Execution failed: {run_result.stderr}'
                }
            
            # Parse output
            lines = run_result.stdout.strip().split('\n')
            if lines and lines[0] == 'SUCCESS':
                result = {'success': True}
                for line in lines[1:]:
                    if ':' in line:
                        key, value = line.split(':', 1)
                        result[key.lower()] = value
                return result
            else:
                return {
                    'success': False,
                    'error': f'Unexpected output: {run_result.stdout}'
                }
                
        except Exception as e:
            return {
                'success': False,
                'error': str(e)
            }
