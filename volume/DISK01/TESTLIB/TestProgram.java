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
            
            // Create JSON response manually (simple format)
            StringBuilder response = new StringBuilder();
            response.append("{");
            response.append("\"action\": \"display_map\",");
            response.append("\"map_file\": \"").append(library).append("/MAINMENU\",");
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
            response.append("\"next_action\": \"wait_input\"");
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