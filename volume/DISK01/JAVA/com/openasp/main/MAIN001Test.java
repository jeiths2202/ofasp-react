package com.openasp.main;

import java.util.HashMap;
import java.util.Map;
import com.openasp.common.JSONResponse;

/**
 * Test runner for MAIN001 program
 * This class provides a main method to test the MAIN001 program independently
 */
public class MAIN001Test {
    
    public static void main(String[] args) {
        System.out.println("=== MAIN001 Test Execution ===");
        System.out.println("Starting MAIN001 program test...\n");
        
        // Create instance of MAIN001
        MAIN001 program = new MAIN001();
        
        // Prepare input data
        Map<String, String> inputData = new HashMap<>();
        inputData.put("terminal_id", "TERM001");
        
        // For testing, inject mock user selection
        if (args.length > 0) {
            System.out.println("Using command line argument as selection: " + args[0]);
            // We'll simulate the selection through the program
        }
        
        try {
            // Execute the program
            JSONResponse response = program.execute(inputData);
            
            // Display the result
            System.out.println("\n=== Program Execution Result ===");
            System.out.println(response.toString());
            
        } catch (Exception e) {
            System.err.println("Error executing MAIN001: " + e.getMessage());
            e.printStackTrace();
        }
        
        System.out.println("\n=== Test Completed ===");
    }
}