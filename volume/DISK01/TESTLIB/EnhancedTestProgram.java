import java.io.*;

/**
 * Enhanced ASP Java Program demonstrating Phase 2 features
 * - Custom return codes
 * - Different action types  
 * - Error handling
 * - Program continuation
 */
public class EnhancedTestProgram {
    public static void main(String[] args) {
        try {
            // Read comprehensive input from stdin
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            StringBuilder inputBuilder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                inputBuilder.append(line);
            }
            
            String inputJson = inputBuilder.toString();
            System.err.println("Enhanced Test Program received: " + inputJson);
            
            // Parse input values
            String program = extractJsonValue(inputJson, "program");
            String library = extractJsonValue(inputJson, "library");
            String volume = extractJsonValue(inputJson, "volume");
            String user = extractJsonValue(inputJson, "user");
            String sessionId = extractJsonValue(inputJson, "id");
            
            // Determine test scenario based on parameters
            String testMode = "normal";
            if (inputJson.contains("\"parameters\"") && inputJson.contains("test_mode")) {
                testMode = extractParameterValue(inputJson, "test_mode");
            }
            
            System.err.println("Test mode: " + testMode);
            
            // Generate different responses based on test mode
            StringBuilder response = new StringBuilder();
            response.append("{");
            
            switch (testMode) {
                case "error":
                    // Test error response
                    response.append("\"action\": \"error\",");
                    response.append("\"message\": \"Simulated program error for testing\",");
                    response.append("\"error_code\": 825,");
                    response.append("\"details\": \"This is a test error from the Java program\"");
                    break;
                    
                case "continue":
                    // Test program continuation
                    response.append("\"action\": \"continue\",");
                    response.append("\"message\": \"Program needs to continue execution\",");
                    response.append("\"continuation_type\": \"wait_input\",");
                    response.append("\"return_code\": 0,");
                    response.append("\"next_step\": \"Wait for user input\"");
                    break;
                    
                case "custom_return":
                    // Test custom return code
                    response.append("\"action\": \"display_text\",");
                    response.append("\"message\": \"Program completed with custom return code\",");
                    response.append("\"return_code\": 42");
                    break;
                    
                default:
                    // Normal SMED map display
                    response.append("\"action\": \"display_map\",");
                    response.append("\"map_file\": \"").append(library).append("/MAINMENU.smed\",");
                    response.append("\"return_code\": 0,");
                    response.append("\"fields\": {");
                    response.append("\"USERID\": \"\",");
                    response.append("\"PASSWD\": \"\",");
                    response.append("\"PROGRAM\": \"").append(program).append("\"");
                    response.append("},");
                    response.append("\"messages\": [");
                    response.append("\"Enhanced OpenASP System v1.0\",");
                    response.append("\"Program: ").append(program).append(" executed successfully\",");
                    response.append("\"Library: ").append(library).append("\",");
                    response.append("\"Volume: ").append(volume).append("\",");
                    response.append("\"User: ").append(user).append("\",");
                    response.append("\"Session: ").append(sessionId).append("\"");
                    if (inputJson.contains("parameters")) {
                        response.append(",\"Parameters received and processed\"");
                    }
                    response.append("],");
                    response.append("\"next_action\": \"wait_input\",");
                    response.append("\"program_info\": {");
                    response.append("\"version\": \"2.0\",");
                    response.append("\"type\": \"ENHANCED_TEST\",");
                    response.append("\"capabilities\": [\"SMED_DISPLAY\", \"PARAMETER_HANDLING\", \"ERROR_SIMULATION\"]");
                    response.append("}");
                    break;
            }
            
            response.append("}");
            
            // Output response
            System.out.println(response.toString());
            
        } catch (Exception e) {
            // Error response
            System.out.println("{\"action\": \"error\", \"message\": \"Java program exception: " + e.getMessage() + "\", \"error_code\": 999}");
            System.err.println("Error in EnhancedTestProgram: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    private static String extractJsonValue(String json, String key) {
        try {
            String searchKey = "\"" + key + "\":";
            int startIndex = json.indexOf(searchKey);
            if (startIndex == -1) {
                return "";
            }
            
            startIndex += searchKey.length();
            while (startIndex < json.length() && (json.charAt(startIndex) == ' ' || json.charAt(startIndex) == '"')) {
                startIndex++;
            }
            
            int endIndex = startIndex;
            while (endIndex < json.length() && json.charAt(endIndex) != '"' && json.charAt(endIndex) != ',') {
                endIndex++;
            }
            
            return json.substring(startIndex, endIndex);
        } catch (Exception e) {
            return "";
        }
    }
    
    private static String extractParameterValue(String json, String paramKey) {
        try {
            String searchPattern = paramKey + "=";
            int startIndex = json.indexOf(searchPattern);
            if (startIndex == -1) {
                return "normal";
            }
            
            startIndex += searchPattern.length();
            int endIndex = startIndex;
            while (endIndex < json.length() && json.charAt(endIndex) != ',' && json.charAt(endIndex) != '"' && json.charAt(endIndex) != '}') {
                endIndex++;
            }
            
            return json.substring(startIndex, endIndex);
        } catch (Exception e) {
            return "normal";
        }
    }
}