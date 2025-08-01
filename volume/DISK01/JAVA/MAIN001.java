import java.util.*;

/**
 * MAIN001ASP - Simplified MAIN001 for ASP System Execution
 * 8-byte naming convention: MAIN001ASP
 * 
 * This is a simplified version of MAIN001 without Spring/SLF4J dependencies
 * specifically for ASP system execution that requires a main method.
 */
public class MAIN001 {
    
    // Constants from COBOL working storage
    private static final String TITLE_LINE = "=== 管理メニュー ===";
    private static final String OPTION_1 = "１）参照";
    private static final String OPTION_2 = "２）追加";
    private static final String OPTION_3 = "３）更新";
    private static final String OPTION_4 = "４）削除";
    private static final String SELECTION_PROMPT = "選択：";
    private static final int MAX_RETRIES = 3;
    
    // Working storage fields
    private String wsUserSelection = "";
    private boolean wsValidOption = false;
    private String wsProgramToCall = "";
    private int wsRetryCount = 0;
    private String wsErrorMessage = "";
    private String wsStatusMessage = "";
    private String currentTerminalId = "ASP001";
    private static String[] programArgs;
    
    /**
     * Main method for ASP system execution
     */
    public static void main(String[] args) {
        System.out.println("*** MAIN001 - Fujitsu ASP COBOLG -> Java Conversion Program ***");
        System.out.println("8-byte naming convention: MAIN001");
        System.out.println("SJIS encoding support\n");
        
        try {
            // Save command line arguments for later use
            programArgs = args;
            
            MAIN001 program = new MAIN001();
            
            // Set terminal ID from command line or default
            if (args.length > 0) {
                program.currentTerminalId = args[0];
                System.out.println("Terminal ID: " + args[0]);
            } else {
                System.out.println("Terminal ID: ASP001 (default)");
            }
            
            // Execute main menu process
            int exitCode = program.processMainMenu();
            
            System.out.println("\nMAIN001 execution completed. Exit code: " + exitCode);
            System.exit(exitCode);
            
        } catch (Exception e) {
            System.err.println("ERROR: MAIN001 execution failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(-1);
        }
    }
    
    /**
     * Process main menu - ASP terminal compatible version
     */
    private int processMainMenu() {
        // Display menu once
        displayMenu();
        
        // For ASP terminal, show all options and simulate user interaction
        System.out.println("ASP Terminal Mode: Displaying menu options");
        System.out.println("To interact with this program:");
        System.out.println("- Use CALL PGM-MAIN001.JAVA,PARA-(1) for option 1 (INQUIRY1)");
        System.out.println("- Use CALL PGM-MAIN001.JAVA,PARA-(2) for option 2 (CREATE1)");
        System.out.println("- Use CALL PGM-MAIN001.JAVA,PARA-(3) for option 3 (UPDATE1)");
        System.out.println("- Use CALL PGM-MAIN001.JAVA,PARA-(4) for option 4 (DELETE1)");
        
        // Check if command line argument was provided for selection
        String selection = getSelectionFromArgs();
        if (selection != null) {
            wsUserSelection = selection;
            validateSelection();
            
            if (wsValidOption) {
                int returnCode = callProgram();
                System.out.println("\n=== Execution Result ===");
                System.out.println("Selected program: " + wsProgramToCall);
                System.out.println("Status message: " + wsStatusMessage);
                System.out.println("Return code: " + returnCode);
                return returnCode;
            } else {
                System.out.println("ERROR: Invalid selection: " + selection);
                return 8;
            }
        } else {
            System.out.println("\nNo selection parameter provided.");
            System.out.println("Program displayed menu successfully.");
            return 0;
        }
    }
    
    /**
     * Display menu - SMED map format for ASP terminal
     */
    private void displayMenu() {
        // Output SMED map data as JSON for ASP terminal rendering
        outputSmedMapData();
        
        // Also output text version for compatibility
        System.out.println("\n" + TITLE_LINE);
        System.out.println();
        System.out.println(OPTION_1);
        System.out.println(OPTION_2);
        System.out.println(OPTION_3);
        System.out.println(OPTION_4);
        System.out.println();
    }
    
    /**
     * Output SMED map data as JSON for ASP terminal
     */
    private void outputSmedMapData() {
        try {
            // Create SMED map data structure
            Map<String, Object> smedData = new HashMap<>();
            smedData.put("action", "display_map");
            smedData.put("map_file", "MAIN001");
            smedData.put("terminal_id", currentTerminalId);
            smedData.put("program_name", "MAIN001");
            
            // Create fields data
            Map<String, Object> fields = new HashMap<>();
            fields.put("TITLE", "社員管理メニュー");
            fields.put("OPTION1", "1) 照会");
            fields.put("OPTION2", "2) 追加");
            fields.put("OPTION3", "3) 更新");
            fields.put("OPTION4", "4) 削除");
            fields.put("PROMPT", "選択：");
            fields.put("INPUT", "");
            
            smedData.put("fields", fields);
            
            // Add screen layout information
            smedData.put("rows", 24);
            smedData.put("cols", 80);
            smedData.put("background_color", "BLACK");
            smedData.put("foreground_color", "GREEN");
            
            // Convert to JSON and output
            String jsonOutput = convertToJson(smedData);
            System.out.println(jsonOutput);
            
        } catch (Exception e) {
            System.err.println("Error outputting SMED map data: " + e.getMessage());
        }
    }
    
    /**
     * Simple JSON converter for SMED data
     */
    private String convertToJson(Map<String, Object> data) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            if (!first) json.append(",");
            json.append("\"").append(entry.getKey()).append("\":");
            
            Object value = entry.getValue();
            if (value instanceof String) {
                json.append("\"").append(value).append("\"");
            } else if (value instanceof Number) {
                json.append(value);
            } else if (value instanceof Map) {
                json.append(convertMapToJson((Map<String, Object>) value));
            } else {
                json.append("\"").append(value.toString()).append("\"");
            }
            first = false;
        }
        
        json.append("}");
        return json.toString();
    }
    
    /**
     * Convert Map to JSON string
     */
    private String convertMapToJson(Map<String, Object> map) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) json.append(",");
            json.append("\"").append(entry.getKey()).append("\":\"").append(entry.getValue()).append("\"");
            first = false;
        }
        
        json.append("}");
        return json.toString();
    }
    
    
    /**
     * Validate selection
     */
    private void validateSelection() {
        wsValidOption = false;
        wsErrorMessage = "";
        
        switch (wsUserSelection) {
            case "1":
                wsValidOption = true;
                wsProgramToCall = "INQUIRY1";
                wsStatusMessage = "참조처리를 시작합니다";
                break;
                
            case "2":
                wsValidOption = true;
                wsProgramToCall = "CREATE1";
                wsStatusMessage = "추가처리를 시작합니다";
                break;
                
            case "3":
                wsValidOption = true;
                wsProgramToCall = "UPDATE1";
                wsStatusMessage = "업데이트처리를 시작합니다";
                break;
                
            case "4":
                wsValidOption = true;
                wsProgramToCall = "DELETE1";
                wsStatusMessage = "삭제처리를 시작합니다";
                break;
                
            default:
                wsValidOption = false;
                wsErrorMessage = "잘못된 선택입니다. 1-4를 입력해주세요";
        }
    }
    
    /**
     * Display error message
     */
    private void displayErrorMessage() {
        System.out.println("ERROR: " + wsErrorMessage);
        System.out.println("Retry: " + wsRetryCount + "/" + MAX_RETRIES);
    }
    
    /**
     * Call program - mock implementation
     */
    private int callProgram() {
        System.out.println("\n[CALL] " + wsProgramToCall + " program call simulation");
        System.out.println("Status: " + wsStatusMessage);
        
        // Simulate program call
        try {
            Thread.sleep(1000); // Simulate processing time
            System.out.println("[OK] " + wsProgramToCall + " program execution completed");
            return 0; // Success
        } catch (InterruptedException e) {
            return -1; // Error
        }
    }
    
    /**
     * Get selection from command line arguments
     */
    private String getSelectionFromArgs() {
        if (programArgs != null && programArgs.length > 1) {
            // Second argument would be the selection (first is terminal ID)
            return programArgs[1];
        }
        return null;
    }
}