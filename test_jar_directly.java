import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;

public class TestJarDirectly {
    public static void main(String[] args) {
        try {
            System.out.println("Testing JAR loading directly...");
            
            // Load the JAR file
            File jarFile = new File("cobol-support/lib/proleap-cobol-parser-fat.jar");
            if (!jarFile.exists()) {
                System.out.println("ERROR: JAR file not found: " + jarFile.getAbsolutePath());
                System.exit(1);
            }
            
            System.out.println("✅ JAR file found: " + jarFile.getAbsolutePath());
            System.out.println("JAR size: " + jarFile.length() + " bytes");
            
            // Create a class loader for the JAR
            URLClassLoader classLoader = new URLClassLoader(
                new URL[]{jarFile.toURI().toURL()},
                TestJarDirectly.class.getClassLoader()
            );
            
            System.out.println("✅ ClassLoader created");
            
            // Try to load the main classes
            try {
                Class<?> runnerClass = classLoader.loadClass("io.github.uwol.proleap.cobol.asg.runner.impl.CobolParserRunnerImpl");
                System.out.println("✅ CobolParserRunnerImpl loaded successfully");
                
                Class<?> preprocessorClass = classLoader.loadClass("io.github.uwol.proleap.cobol.preprocessor.CobolPreprocessor");
                System.out.println("✅ CobolPreprocessor loaded successfully");
                
                Class<?> programClass = classLoader.loadClass("io.github.uwol.proleap.cobol.asg.metamodel.Program");
                System.out.println("✅ Program loaded successfully");
                
                System.out.println("\\n🎉 All classes loaded successfully!");
                
            } catch (ClassNotFoundException e) {
                System.out.println("❌ Class not found: " + e.getMessage());
                e.printStackTrace();
            }
            
        } catch (Exception e) {
            System.out.println("❌ Error: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
