import java.io.File;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;

/**
 * MSGSample launcher for OpenASP AX
 * This wrapper loads and executes the MSGSample program from the JAR file
 */
public class MSGSample {
    public static void main(String[] args) {
        try {
            // Get the current directory
            File jarFile = new File("MSGSample.jar");
            if (!jarFile.exists()) {
                System.err.println("Error: MSGSample.jar not found in current directory");
                System.exit(1);
            }
            
            // Create a class loader for the JAR
            URL[] urls = { jarFile.toURI().toURL() };
            URLClassLoader classLoader = new URLClassLoader(urls);
            
            // Load the main class
            Class<?> mainClass = classLoader.loadClass("com.cobol2java.generated.MSGSample");
            
            // Get the main method
            Method mainMethod = mainClass.getMethod("main", String[].class);
            
            // Invoke the main method
            mainMethod.invoke(null, (Object) args);
            
        } catch (Exception e) {
            System.err.println("Error launching MSGSample: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}