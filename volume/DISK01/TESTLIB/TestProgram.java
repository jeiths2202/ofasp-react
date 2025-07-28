import java.io.*;

/**
 * Sample ASP Java Program for CALL command testing
 * This program demonstrates basic SMED map integration
 */
public class TestProgram {
    public static void main(String[] args) {
        try {
            // Read input from stdin (JSON format)
            BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
            StringBuilder inputBuilder = new StringBuilder();
            String line;
            while ((line = reader.readLine()) != null) {
                inputBuilder.append(line);
            }
            
            String inputJson = inputBuilder.toString();
            System.err.println("Received input: " + inputJson);
            
            // Simple JSON parsing (extract key values manually)
            String program = extractJsonValue(inputJson, "program");
            String library = extractJsonValue(inputJson, "library");
            String volume = extractJsonValue(inputJson, "volume");
            String user = extractJsonValue(inputJson, "user");
            
            // Dynamic map selection based on program logic (remove hardcoding)
            String mapToDisplay = determineMapToDisplay(program, library, volume, user, inputJson);
            
            // Create JSON response manually (simple format)
            StringBuilder response = new StringBuilder();
            response.append("{");
            
            if (mapToDisplay != null && !mapToDisplay.isEmpty()) {
                response.append("\"action\": \"display_map\",");
                response.append("\"map_file\": \"").append(mapToDisplay).append("\",");
                response.append("\"fields\": {");
                response.append("\"USERID\": \"\",");
                response.append("\"PASSWD\": \"\"");
                response.append("},");
                response.append("\"messages\": [");
                response.append("\"Welcome to OpenASP System\",");
                response.append("\"Program: ").append(program).append(" executed successfully\"");
                if (inputJson.contains("parameters")) {
                    response.append(",\"Parameters received from input\"");
                }
                response.append("],");
                response.append("\"next_action\": \"wait_input\",");
                response.append("\"function_keys\": {");
                response.append("\"F3\": \"exit_program\"");
                response.append("}");
            } else {
                // Simple text output when no map is determined
                response.append("\"action\": \"display_text\",");
                response.append("\"message\": \"Program ").append(program).append(" executed successfully from ").append(library).append("\"");
            }
            response.append("}");
            
            // Output response as JSON
            System.out.println(response.toString());
            
        } catch (Exception e) {
            // Error response
            System.out.println("{\"action\": \"error\", \"message\": \"Java program error: " + e.getMessage() + "\"}");
            System.err.println("Error in TestProgram: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Determine which map to display based on program logic
     * This replaces hardcoded MAINMENU with dynamic selection from catalog
     */
    private static String determineMapToDisplay(String program, String library, String volume, String user, String inputJson) {
        try {
            // Business logic to determine which map to show
            
            // Check if specific map is requested via parameters
            String requestedMap = extractJsonValue(inputJson, "requested_map");
            if (requestedMap != null && !requestedMap.isEmpty()) {
                return library + "/" + requestedMap;
            }
            
            // Get MAP information from catalog (passed via JSON input)
            String catalogMapName = extractJsonValue(inputJson, "program_map");
            if (catalogMapName != null && !catalogMapName.isEmpty()) {
                System.err.println("Using map from catalog: " + catalogMapName);
                return library + "/" + catalogMapName;
            }
            
            // Program-specific logic (fallback)
            if ("TestProgram".equals(program)) {
                // Look for common menu file names in order of preference
                String[] commonMenuNames = {"MAINMENU", "MENU", "INDEX", "MAIN"};
                for (String menuName : commonMenuNames) {
                    // In a real implementation, you would check if file exists
                    // For now, return the first common name found in the library
                    return library + "/" + menuName;
                }
            }
            
            // Default behavior - could be based on user role, time of day, etc.
            // For now, return null to indicate no specific map
            return null;
            
        } catch (Exception e) {
            System.err.println("Error determining map to display: " + e.getMessage());
            return null;
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
            // Skip whitespace and quotes
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
}