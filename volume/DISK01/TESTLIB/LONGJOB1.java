/**
 * LONGJOB1 - Long Running Java Program
 * Executes for 3+ minutes with resource monitoring
 */
public class LONGJOB1 {
    public static void main(String[] args) {
        System.out.println("LONGJOB1: Starting long running process...");
        System.out.println("Long running job starting...");
        
        long startTime = System.currentTimeMillis();
        int totalSteps = 180; // 3 minutes worth of steps
        
        try {
            for (int step = 1; step <= totalSteps; step++) {
                // Simulate CPU-intensive work
                performCPUIntensiveWork();
                
                // Simulate memory allocation
                byte[] memoryBlock = new byte[1024 * 1024]; // 1MB allocation
                
                // Print progress every 10 steps
                if (step % 10 == 0) {
                    long elapsed = (System.currentTimeMillis() - startTime) / 1000;
                    double progress = (double) step / totalSteps * 100;
                    
                    System.out.printf("LONGJOB1: Step %d/%d (%.1f%%) - Elapsed: %d seconds%n", 
                                    step, totalSteps, progress, elapsed);
                    System.out.printf("Long job: Step %d/%d (%.1f%%) - Elapsed time: %d seconds%n", 
                                    step, totalSteps, progress, elapsed);
                    
                    // Report resource usage
                    Runtime runtime = Runtime.getRuntime();
                    long totalMemory = runtime.totalMemory();
                    long freeMemory = runtime.freeMemory();
                    long usedMemory = totalMemory - freeMemory;
                    
                    System.out.printf("Memory Usage: %d MB / %d MB%n", 
                                    usedMemory / (1024 * 1024), totalMemory / (1024 * 1024));
                    System.out.printf("Memory usage: %d MB / %d MB%n", 
                                    usedMemory / (1024 * 1024), totalMemory / (1024 * 1024));
                }
                
                // Sleep for 1 second
                Thread.sleep(1000);
                
                // Clean up memory periodically
                if (step % 20 == 0) {
                    System.gc();
                }
            }
            
            long totalTime = (System.currentTimeMillis() - startTime) / 1000;
            System.out.println("LONGJOB1: Process completed successfully!");
            System.out.println("Total execution time: " + totalTime + " seconds");
            System.out.println("Long job completed normally!");
            System.out.println("Total execution time: " + totalTime + " seconds");
            
        } catch (InterruptedException e) {
            System.err.println("LONGJOB1: Process interrupted - " + e.getMessage());
            System.err.println("Process was interrupted - " + e.getMessage());
        } catch (Exception e) {
            System.err.println("LONGJOB1: Error occurred - " + e.getMessage());
            System.err.println("Error occurred - " + e.getMessage());
        }
    }
    
    private static void performCPUIntensiveWork() {
        // Simulate CPU-intensive calculation
        double result = 0;
        for (int i = 0; i < 100000; i++) {
            result += Math.sqrt(i) * Math.sin(i) * Math.cos(i);
        }
    }
}