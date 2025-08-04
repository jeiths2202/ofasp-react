package com.openasp.smed;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.*;
import java.nio.file.*;
import java.util.*;

/**
 * SMED Web Grid Display System
 * 
 * Requirements (1차 목표 사양):
 * - Read SMED맵명.json files
 * - Output to 24 rows x 80 columns web grid (정확히 24행 x 80열)
 * - With data: display map + data
 * - Without data: display map only
 */
public class SMEDWebGridDisplay {
    
    // 1차 목표 사양: 24행 x 80열 웹 그리드
    private static final int GRID_ROWS = 24;
    private static final int GRID_COLS = 80;
    
    private ObjectMapper objectMapper;
    private char[][] displayGrid;
    private String[][] colorGrid;
    private Map<String, String> fieldData;
    
    public SMEDWebGridDisplay() {
        this.objectMapper = new ObjectMapper();
        this.displayGrid = new char[GRID_ROWS][GRID_COLS];
        this.colorGrid = new String[GRID_ROWS][GRID_COLS];
        this.fieldData = new HashMap<>();
        initializeGrid();
    }
    
    /**
     * Display SMED map from JSON file
     * 
     * @param jsonFilePath Path to SMED JSON file
     * @param data Optional field data to display (can be null)
     * @return DisplayResult containing HTML output and metadata
     */
    public DisplayResult displaySMEDMap(String jsonFilePath, Map<String, String> data) {
        DisplayResult result = new DisplayResult();
        result.setJsonFilePath(jsonFilePath);
        result.setHasData(data != null && !data.isEmpty());
        
        try {
            // Step 1: Load and parse JSON file
            System.out.println("Loading SMED JSON file: " + jsonFilePath);
            JsonNode mapData = loadJSONMap(jsonFilePath);
            if (mapData == null) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to load JSON map file");
                return result;
            }
            
            // Step 2: Set field data
            if (data != null) {
                this.fieldData = new HashMap<>(data);
                System.out.println("Field data provided: " + data.size() + " fields");
            } else {
                this.fieldData.clear();
                System.out.println("No field data provided - displaying map only");
            }
            
            // Step 3: Initialize display grid
            initializeGrid();
            
            // Step 4: Render map fields
            String mapName = mapData.get("mapName").asText();
            JsonNode fields = mapData.get("fields");
            
            if (!renderMapFields(fields)) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to render map fields");
                return result;
            }
            
            // Step 5: Generate HTML output
            String htmlOutput = generateHTMLOutput(mapName, result.isHasData());
            result.setHtmlOutput(htmlOutput);
            
            // Step 6: Generate metadata
            result.setMapName(mapName);
            result.setTotalFields(fields.size());
            result.setInputFields(countInputFields(fields));
            result.setOutputFields(countOutputFields(fields));
            
            result.setSuccess(true);
            
            System.out.println("=== SMED Web Grid Display Generated ===");
            System.out.println("Map Name: " + mapName);
            System.out.println("Total Fields: " + result.getTotalFields());
            System.out.println("Input Fields: " + result.getInputFields());
            System.out.println("Output Fields: " + result.getOutputFields());
            System.out.println("With Data: " + result.isHasData());
            
        } catch (Exception e) {
            result.setSuccess(false);
            result.setErrorMessage("Exception during display generation: " + e.getMessage());
            e.printStackTrace();
        }
        
        return result;
    }
    
    /**
     * Load JSON map file
     */
    private JsonNode loadJSONMap(String jsonFilePath) {
        try {
            Path path = Paths.get(jsonFilePath);
            if (!Files.exists(path)) {
                System.err.println("JSON file does not exist: " + jsonFilePath);
                return null;
            }
            
            String jsonContent = new String(Files.readAllBytes(path), "UTF-8");
            return objectMapper.readTree(jsonContent);
            
        } catch (IOException e) {
            System.err.println("Failed to load JSON file: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Initialize display grid with spaces
     */
    private void initializeGrid() {
        for (int row = 0; row < GRID_ROWS; row++) {
            for (int col = 0; col < GRID_COLS; col++) {
                displayGrid[row][col] = ' ';
                colorGrid[row][col] = "#FFFFFF";
            }
        }
    }
    
    /**
     * Render map fields onto display grid
     */
    private boolean renderMapFields(JsonNode fields) {
        try {
            for (JsonNode field : fields) {
                if (!renderField(field)) {
                    return false;
                }
            }
            return true;
            
        } catch (Exception e) {
            System.err.println("Failed to render map fields: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Render individual field
     */
    private boolean renderField(JsonNode field) {
        try {
            String name = field.get("name").asText();
            int row = field.get("row").asInt();
            int col = field.get("col").asInt();
            int length = field.get("length").asInt();
            String type = field.get("type").asText();
            String prompt = field.has("prompt") ? field.get("prompt").asText() : "";
            String color = field.has("color") ? field.get("color").asText() : "#FFFFFF";
            boolean editable = field.has("editable") ? field.get("editable").asBoolean() : true;
            
            // Validate bounds
            if (row < 0 || row >= GRID_ROWS || col < 0 || col >= GRID_COLS) {
                System.out.println("Field " + name + " position out of bounds: (" + row + "," + col + ")");
                return true; // Continue with other fields
            }
            
            // Determine what to display based on 1차 목표 사양
            String displayText = "";
            
            if (!editable || ("text".equals(type) || !prompt.isEmpty())) {
                // 출력 전용 필드: PROMPT 텍스트 표시
                displayText = prompt;
            } else if (editable && "input".equals(type)) {
                // 입력 가능 필드: 데이터가 있으면 표시, 없으면 입력 플레이스홀더
                displayText = fieldData.getOrDefault(name, "");
                if (displayText.isEmpty()) {
                    displayText = "_".repeat(Math.min(length, 10)); // Show input placeholder
                }
            }
            
            // Render text onto grid
            renderTextOnGrid(displayText, row, col, length, color);
            
            return true;
            
        } catch (Exception e) {
            System.err.println("Failed to render field: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Render text onto display grid
     */
    private void renderTextOnGrid(String text, int row, int col, int maxLength, String color) {
        if (text == null) text = "";
        
        int endCol = Math.min(col + maxLength, GRID_COLS);
        int textIndex = 0;
        
        for (int c = col; c < endCol && textIndex < text.length(); c++) {
            if (row < GRID_ROWS && c < GRID_COLS) {
                displayGrid[row][c] = text.charAt(textIndex);
                colorGrid[row][c] = color;
                textIndex++;
            }
        }
        
        // Fill remaining space with spaces for input fields
        for (int c = col + textIndex; c < endCol; c++) {
            if (row < GRID_ROWS && c < GRID_COLS) {
                if (displayGrid[row][c] == ' ') {
                    colorGrid[row][c] = color;
                }
            }
        }
    }
    
    /**
     * Generate HTML output for web display
     */
    private String generateHTMLOutput(String mapName, boolean hasData) {
        StringBuilder html = new StringBuilder();
        
        // HTML Header
        html.append("<!DOCTYPE html>\n");
        html.append("<html lang=\"ko\">\n");
        html.append("<head>\n");
        html.append("    <meta charset=\"UTF-8\">\n");
        html.append("    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n");
        html.append("    <title>SMED Map Display - ").append(mapName).append("</title>\n");
        html.append("    <style>\n");
        html.append(generateCSS());
        html.append("    </style>\n");
        html.append("</head>\n");
        html.append("<body>\n");
        
        // Title
        html.append("    <div class=\"header\">\n");
        html.append("        <h1>SMED Map Display: ").append(mapName).append("</h1>\n");
        html.append("        <p>Mode: ").append(hasData ? "Map + Data" : "Map Only").append("</p>\n");
        html.append("        <p>Grid Size: 24 rows × 80 columns</p>\n");
        html.append("    </div>\n");
        
        // Grid Container
        html.append("    <div class=\"grid-container\">\n");
        html.append("        <div class=\"grid\">\n");
        
        // Generate grid rows
        for (int row = 0; row < GRID_ROWS; row++) {
            html.append("            <div class=\"grid-row\">\n");
            html.append("                <span class=\"row-number\">").append(String.format("%2d", row + 1)).append("</span>\n");
            
            for (int col = 0; col < GRID_COLS; col++) {
                char ch = displayGrid[row][col];
                String color = colorGrid[row][col];
                String displayChar = ch == ' ' ? "&nbsp;" : String.valueOf(ch);
                
                html.append("                <span class=\"grid-cell\" style=\"color: ").append(color).append("\">");
                html.append(displayChar);
                html.append("</span>");
            }
            
            html.append("\n            </div>\n");
        }
        
        html.append("        </div>\n");
        html.append("    </div>\n");
        
        // Footer
        html.append("    <div class=\"footer\">\n");
        html.append("        <p>Generated by SMED Web Grid Display System</p>\n");
        html.append("        <p>Generation Time: ").append(new Date().toString()).append("</p>\n");
        html.append("    </div>\n");
        
        html.append("</body>\n");
        html.append("</html>\n");
        
        return html.toString();
    }
    
    /**
     * Generate CSS styles for web display
     */
    private String generateCSS() {
        return """
            body {
                font-family: 'Courier New', monospace;
                margin: 0;
                padding: 20px;
                background-color: #000000;
                color: #00FF00;
            }
            
            .header {
                text-align: center;
                margin-bottom: 20px;
                color: #00FFFF;
            }
            
            .header h1 {
                margin: 0;
                font-size: 24px;
            }
            
            .header p {
                margin: 5px 0;
                font-size: 14px;
            }
            
            .grid-container {
                display: flex;
                justify-content: center;
                margin: 20px 0;
            }
            
            .grid {
                border: 2px solid #00FFFF;
                background-color: #000040;
                padding: 10px;
                font-size: 12px;
                line-height: 1.2;
            }
            
            .grid-row {
                display: flex;
                white-space: nowrap;
            }
            
            .row-number {
                color: #FFFF00;
                margin-right: 10px;
                width: 25px;
                text-align: right;
            }
            
            .grid-cell {
                font-family: 'Courier New', monospace;
                font-size: 12px;
                width: 8px;
                text-align: center;
                display: inline-block;
            }
            
            .footer {
                text-align: center;
                margin-top: 20px;
                font-size: 12px;
                color: #888888;
            }
            
            .footer p {
                margin: 5px 0;
            }
            
            /* Responsive design */
            @media (max-width: 768px) {
                .grid {
                    font-size: 8px;
                }
                
                .grid-cell {
                    width: 6px;
                    font-size: 8px;
                }
                
                .row-number {
                    width: 20px;
                    font-size: 8px;
                }
            }
            """;
    }
    
    /**
     * Count input fields
     */
    private int countInputFields(JsonNode fields) {
        int count = 0;
        for (JsonNode field : fields) {
            String type = field.get("type").asText();
            String prompt = field.has("prompt") ? field.get("prompt").asText() : "";
            
            if ("input".equals(type) || prompt.isEmpty()) {
                count++;
            }
        }
        return count;
    }
    
    /**
     * Count output fields
     */
    private int countOutputFields(JsonNode fields) {
        int count = 0;
        for (JsonNode field : fields) {
            String type = field.get("type").asText();
            String prompt = field.has("prompt") ? field.get("prompt").asText() : "";
            
            if ("text".equals(type) || !prompt.isEmpty()) {
                count++;
            }
        }
        return count;
    }
    
    /**
     * Save HTML output to file
     */
    public boolean saveHTMLOutput(String htmlContent, String outputFilePath) {
        try {
            Path outputPath = Paths.get(outputFilePath);
            Files.write(outputPath, htmlContent.getBytes("UTF-8"), 
                       StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            
            System.out.println("HTML output saved to: " + outputFilePath);
            return true;
            
        } catch (IOException e) {
            System.err.println("Failed to save HTML output: " + e.getMessage());
            return false;
        }
    }
    
    // Data class
    public static class DisplayResult {
        private boolean success;
        private String errorMessage;
        private String jsonFilePath;
        private String mapName;
        private String htmlOutput;
        private boolean hasData;
        private int totalFields;
        private int inputFields;
        private int outputFields;
        
        // Getters and setters
        public boolean isSuccess() { return success; }
        public void setSuccess(boolean success) { this.success = success; }
        
        public String getErrorMessage() { return errorMessage; }
        public void setErrorMessage(String errorMessage) { this.errorMessage = errorMessage; }
        
        public String getJsonFilePath() { return jsonFilePath; }
        public void setJsonFilePath(String jsonFilePath) { this.jsonFilePath = jsonFilePath; }
        
        public String getMapName() { return mapName; }
        public void setMapName(String mapName) { this.mapName = mapName; }
        
        public String getHtmlOutput() { return htmlOutput; }
        public void setHtmlOutput(String htmlOutput) { this.htmlOutput = htmlOutput; }
        
        public boolean isHasData() { return hasData; }
        public void setHasData(boolean hasData) { this.hasData = hasData; }
        
        public int getTotalFields() { return totalFields; }
        public void setTotalFields(int totalFields) { this.totalFields = totalFields; }
        
        public int getInputFields() { return inputFields; }
        public void setInputFields(int inputFields) { this.inputFields = inputFields; }
        
        public int getOutputFields() { return outputFields; }
        public void setOutputFields(int outputFields) { this.outputFields = outputFields; }
    }
}