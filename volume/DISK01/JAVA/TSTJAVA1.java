public class TSTJAVA1 {
    public static void main(String[] args) {
        System.out.println("TSTJAVA1 program started.");
        System.out.println("Waiting for 60 seconds...");
        
        try {
            // Wait for 60 seconds
            Thread.sleep(60000);
            System.out.println("60 seconds wait completed. Terminating program.");
        } catch (InterruptedException e) {
            System.out.println("Program interrupted: " + e.getMessage());
        }
    }
}