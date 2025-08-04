public class HelloASP {
    public static void main(String[] args) {
        System.out.println("Hello ASP");
        System.out.println("Java program executed successfully from TESTCL1");
        
        // Check if any parameters were passed
        if (args.length > 0) {
            System.out.println("Parameters received:");
            for (int i = 0; i < args.length; i++) {
                System.out.println("  Arg[" + i + "]: " + args[i]);
            }
        }
        
        // Simulate some processing
        try {
            Thread.sleep(1000); // 1 second delay
            System.out.println("Processing completed");
        } catch (InterruptedException e) {
            System.err.println("Interrupted: " + e.getMessage());
        }
    }
}