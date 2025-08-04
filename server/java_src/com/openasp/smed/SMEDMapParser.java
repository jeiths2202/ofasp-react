package com.openasp.smed;

import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.fasterxml.jackson.databind.node.ArrayNode;

/**
 * SMED Map Parser for Legacy SJIS-encoded SMED Map Files
 * Parses SMED map files and converts them to JSON format
 * 
 * Requirements:
 * - Parse SJIS encoded Legacy SMED map files
 * - Validate parsing results
 * - Convert to JSON format on successful parsing
 * - Support field type classification based on PROMPT presence
 */
public class SMEDMapParser {
    
    private static final String SJIS_ENCODING = "Shift_JIS";
    private static final String UTF8_ENCODING = "UTF-8";
    
    // Regular expression patterns for SMED field parsing
    private static final Pattern MAPNAME_PATTERN = Pattern.compile("^MAPNAME\\s+(\\w+)", Pattern.CASE_INSENSITIVE);
    private static final Pattern ITEM_PATTERN = Pattern.compile("^ITEM\\s+(\\w+)\\s*(.*)$", Pattern.CASE_INSENSITIVE);
    private static final Pattern TYPE_PATTERN = Pattern.compile("TYPE=([A-Z])", Pattern.CASE_INSENSITIVE);
    private static final Pattern POS_PATTERN = Pattern.compile("POS=\\((\\d+),(\\d+)\\)", Pattern.CASE_INSENSITIVE);
    private static final Pattern PROMPT_PATTERN = Pattern.compile("PROMPT=\"([^\"]*)\"", Pattern.CASE_INSENSITIVE);
    private static final Pattern COLOR_PATTERN = Pattern.compile("COLOR=(#[0-9A-Fa-f]{6})", Pattern.CASE_INSENSITIVE);
    private static final Pattern LEN_PATTERN = Pattern.compile("LEN=(\\d+)", Pattern.CASE_INSENSITIVE);
    
    private ObjectMapper objectMapper;
    
    public SMEDMapParser() {
        this.objectMapper = new ObjectMapper();
    }
    
    /**
     * Parse SMED map file with SJIS encoding support
     * 
     * @param filePath Path to SMED map file
     * @return ParseResult containing parsed data and validation status
     */
    public ParseResult parseSMEDFile(String filePath) {
        ParseResult result = new ParseResult();
        result.setFilePath(filePath);
        result.setParseTime(new Date());
        
        try {
            // Read file content with SJIS encoding
            String content = readSMEDFile(filePath);
            if (content == null) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to read SMED file");
                return result;
            }
            
            // Parse content
            SMEDMapData mapData = parseContent(content);
            if (mapData == null) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to parse SMED content");
                return result;
            }
            
            // Validate parsing results
            if (!validateMapData(mapData)) {
                result.setSuccess(false);
                result.setErrorMessage("SMED map validation failed");
                return result;
            }
            
            // Set parsed data
            result.setMapData(mapData);
            result.setSuccess(true);
            
            // Convert to JSON
            String jsonContent = convertToJSON(mapData);
            result.setJsonContent(jsonContent);
            
            System.out.println("Successfully parsed SMED file: " + filePath);
            System.out.println("Map name: " + mapData.getMapName());
            System.out.println("Total fields: " + mapData.getFields().size());
            
        } catch (Exception e) {
            result.setSuccess(false);
            result.setErrorMessage("Exception during parsing: " + e.getMessage());
            e.printStackTrace();
        }
        
        return result;
    }
    
    /**
     * Read SMED file with SJIS encoding fallback to UTF-8
     */
    private String readSMEDFile(String filePath) {
        try {
            Path path = Paths.get(filePath);
            if (!Files.exists(path)) {
                System.err.println("SMED file does not exist: " + filePath);
                return null;
            }
            
            // Try SJIS encoding first
            try {
                byte[] bytes = Files.readAllBytes(path);
                String content = new String(bytes, Charset.forName(SJIS_ENCODING));
                System.out.println("Successfully read SMED file with SJIS encoding: " + filePath);
                return content;
            } catch (Exception sjisError) {
                System.out.println("SJIS decoding failed, trying UTF-8: " + sjisError.getMessage());
                // Fallback to UTF-8
                return new String(Files.readAllBytes(path), Charset.forName(UTF8_ENCODING));
            }
            
        } catch (IOException e) {
            System.err.println("Failed to read SMED file: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Parse SMED content and extract map data
     */
    private SMEDMapData parseContent(String content) {
        SMEDMapData mapData = new SMEDMapData();
        List<SMEDField> fields = new ArrayList<>();
        
        String[] lines = content.split("\\r?\\n");
        
        for (int lineNum = 0; lineNum < lines.length; lineNum++) {
            String line = lines[lineNum].trim();
            
            // Skip empty lines and comments
            if (line.isEmpty() || line.startsWith("#") || line.startsWith("//")) {
                continue;
            }
            
            // Extract map name
            Matcher mapNameMatcher = MAPNAME_PATTERN.matcher(line);
            if (mapNameMatcher.find()) {
                mapData.setMapName(mapNameMatcher.group(1));
                continue;
            }
            
            // Parse field definitions
            SMEDField field = parseFieldLine(line, lineNum + 1);
            if (field != null) {
                fields.add(field);
            }
        }
        
        mapData.setFields(fields);
        mapData.setTotalLines(lines.length);
        mapData.setParsedFields(fields.size());
        
        return mapData;
    }
    
    /**
     * Parse individual field line
     */
    private SMEDField parseFieldLine(String line, int lineNum) {
        try {
            // Parse ITEM format: ITEM name TYPE=type POS=(row,col) PROMPT="text" COLOR=color LEN=length
            Matcher itemMatcher = ITEM_PATTERN.matcher(line);
            if (itemMatcher.find()) {
                return parseItemField(itemMatcher.group(1), itemMatcher.group(2));
            }
            
            // Parse simple format: name row col [length] [type]
            String[] parts = line.split("\\s+");
            if (parts.length >= 3) {
                return parseSimpleField(parts);
            }
            
            // Unrecognized format
            System.out.println("Unrecognized field format on line " + lineNum + ": " + line);
            return null;
            
        } catch (Exception e) {
            System.out.println("Error parsing field line " + lineNum + ": " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Parse ITEM field format
     */
    private SMEDField parseItemField(String name, String attributes) {
        SMEDField field = new SMEDField();
        field.setName(name);
        field.setRow(0);
        field.setCol(0);
        field.setLength(10);
        field.setType("input");
        field.setPrompt("");
        field.setColor("#00FF00");
        
        // Parse TYPE
        Matcher typeMatcher = TYPE_PATTERN.matcher(attributes);
        if (typeMatcher.find()) {
            String typeCode = typeMatcher.group(1);
            switch (typeCode.toUpperCase()) {
                case "T":
                    field.setType("text");
                    break;
                case "I":
                    field.setType("input");
                    break;
                default:
                    field.setType("input");
            }
        }
        
        // Parse POS
        Matcher posMatcher = POS_PATTERN.matcher(attributes);
        if (posMatcher.find()) {
            field.setRow(Integer.parseInt(posMatcher.group(1)));
            field.setCol(Integer.parseInt(posMatcher.group(2)));
        }
        
        // Parse PROMPT
        Matcher promptMatcher = PROMPT_PATTERN.matcher(attributes);
        if (promptMatcher.find()) {
            field.setPrompt(promptMatcher.group(1));
            field.setLength(field.getPrompt().length());
        }
        
        // Parse COLOR
        Matcher colorMatcher = COLOR_PATTERN.matcher(attributes);
        if (colorMatcher.find()) {
            field.setColor(colorMatcher.group(1));
        }
        
        // Parse LEN
        Matcher lenMatcher = LEN_PATTERN.matcher(attributes);
        if (lenMatcher.find()) {
            field.setLength(Integer.parseInt(lenMatcher.group(1)));
            // If LEN is specified but no PROMPT, it's an input field
            if (field.getPrompt().isEmpty()) {
                field.setType("input");
            }
        }
        
        // Apply field type classification logic
        classifyFieldType(field);
        
        return field;
    }
    
    /**
     * Parse simple field format
     */
    private SMEDField parseSimpleField(String[] parts) {
        SMEDField field = new SMEDField();
        field.setName(parts[0]);
        field.setRow(isNumeric(parts[1]) ? Integer.parseInt(parts[1]) : 0);
        field.setCol(isNumeric(parts[2]) ? Integer.parseInt(parts[2]) : 0);
        field.setLength(parts.length > 3 && isNumeric(parts[3]) ? Integer.parseInt(parts[3]) : 10);
        field.setType(parts.length > 4 ? parts[4] : "input");
        field.setPrompt("");
        field.setColor("#00FF00");
        
        // Apply field type classification logic
        classifyFieldType(field);
        
        return field;
    }
    
    /**
     * Apply field type classification logic based on PROMPT presence
     * Requirements: PROMPT 없음 → 입력 가능, PROMPT 있음 → 출력 전용
     */
    private void classifyFieldType(SMEDField field) {
        if (field.getPrompt() == null || field.getPrompt().isEmpty()) {
            // No PROMPT → 입력 가능 필드
            field.setFieldTypeCategory("INPUT_ENABLED");
            field.setEditable(true);
            if (!"input".equals(field.getType())) {
                field.setType("input");
            }
        } else {
            // PROMPT exists → 출력 전용 필드
            field.setFieldTypeCategory("OUTPUT_ONLY");
            field.setEditable(false);
            if (!"text".equals(field.getType())) {
                field.setType("text");
            }
        }
    }
    
    /**
     * Validate parsed SMED map data
     */
    private boolean validateMapData(SMEDMapData mapData) {
        if (mapData == null) {
            return false;
        }
        
        // Check if map name exists
        if (mapData.getMapName() == null || mapData.getMapName().isEmpty()) {
            System.err.println("Validation failed: No map name found");
            return false;
        }
        
        // Check if fields exist
        if (mapData.getFields() == null || mapData.getFields().isEmpty()) {
            System.err.println("Validation failed: No fields found");
            return false;
        }
        
        // Validate individual fields
        for (SMEDField field : mapData.getFields()) {
            if (!validateField(field)) {
                return false;
            }
        }
        
        // Check field positions are within 24x80 grid bounds
        for (SMEDField field : mapData.getFields()) {
            if (field.getRow() < 0 || field.getRow() >= 24) {
                System.err.println("Validation failed: Field " + field.getName() + " row out of bounds: " + field.getRow());
                return false;
            }
            if (field.getCol() < 0 || field.getCol() >= 80) {
                System.err.println("Validation failed: Field " + field.getName() + " column out of bounds: " + field.getCol());
                return false;
            }
        }
        
        System.out.println("SMED map validation successful");
        return true;
    }
    
    /**
     * Validate individual field
     */
    private boolean validateField(SMEDField field) {
        if (field.getName() == null || field.getName().isEmpty()) {
            System.err.println("Validation failed: Field name is empty");
            return false;
        }
        
        if (field.getLength() <= 0) {
            System.err.println("Validation failed: Field " + field.getName() + " has invalid length: " + field.getLength());
            return false;
        }
        
        return true;
    }
    
    /**
     * Convert SMED map data to JSON format
     */
    public String convertToJSON(SMEDMapData mapData) throws Exception {
        ObjectNode rootNode = objectMapper.createObjectNode();
        
        rootNode.put("mapName", mapData.getMapName());
        rootNode.put("totalLines", mapData.getTotalLines());
        rootNode.put("parsedFields", mapData.getParsedFields());
        rootNode.put("parseTime", mapData.getParseTime() != null ? mapData.getParseTime().toString() : new Date().toString());
        
        ArrayNode fieldsArray = objectMapper.createArrayNode();
        
        for (SMEDField field : mapData.getFields()) {
            ObjectNode fieldNode = objectMapper.createObjectNode();
            fieldNode.put("name", field.getName());
            fieldNode.put("row", field.getRow());
            fieldNode.put("col", field.getCol());
            fieldNode.put("length", field.getLength());
            fieldNode.put("type", field.getType());
            fieldNode.put("prompt", field.getPrompt());
            fieldNode.put("color", field.getColor());
            fieldNode.put("fieldTypeCategory", field.getFieldTypeCategory());
            fieldNode.put("editable", field.isEditable());
            
            fieldsArray.add(fieldNode);
        }
        
        rootNode.set("fields", fieldsArray);
        
        return objectMapper.writerWithDefaultPrettyPrinter().writeValueAsString(rootNode);
    }
    
    /**
     * Utility method to check if string is numeric
     */
    private boolean isNumeric(String str) {
        try {
            Integer.parseInt(str);
            return true;
        } catch (NumberFormatException e) {
            return false;
        }
    }
    
    // Data classes
    public static class ParseResult {
        private boolean success;
        private String errorMessage;
        private String filePath;
        private Date parseTime;
        private SMEDMapData mapData;
        private String jsonContent;
        
        // Getters and setters
        public boolean isSuccess() { return success; }
        public void setSuccess(boolean success) { this.success = success; }
        
        public String getErrorMessage() { return errorMessage; }
        public void setErrorMessage(String errorMessage) { this.errorMessage = errorMessage; }
        
        public String getFilePath() { return filePath; }
        public void setFilePath(String filePath) { this.filePath = filePath; }
        
        public Date getParseTime() { return parseTime; }
        public void setParseTime(Date parseTime) { this.parseTime = parseTime; }
        
        public SMEDMapData getMapData() { return mapData; }
        public void setMapData(SMEDMapData mapData) { this.mapData = mapData; }
        
        public String getJsonContent() { return jsonContent; }
        public void setJsonContent(String jsonContent) { this.jsonContent = jsonContent; }
    }
    
    public static class SMEDMapData {
        private String mapName;
        private List<SMEDField> fields;
        private int totalLines;
        private int parsedFields;
        private Date parseTime;
        
        public SMEDMapData() {
            this.fields = new ArrayList<>();
            this.parseTime = new Date();
        }
        
        // Getters and setters
        public String getMapName() { return mapName; }
        public void setMapName(String mapName) { this.mapName = mapName; }
        
        public List<SMEDField> getFields() { return fields; }
        public void setFields(List<SMEDField> fields) { this.fields = fields; }
        
        public int getTotalLines() { return totalLines; }
        public void setTotalLines(int totalLines) { this.totalLines = totalLines; }
        
        public int getParsedFields() { return parsedFields; }
        public void setParsedFields(int parsedFields) { this.parsedFields = parsedFields; }
        
        public Date getParseTime() { return parseTime; }
        public void setParseTime(Date parseTime) { this.parseTime = parseTime; }
    }
    
    public static class SMEDField {
        private String name;
        private int row;
        private int col;
        private int length;
        private String type;
        private String prompt;
        private String color;
        private String fieldTypeCategory;
        private boolean editable;
        
        // Getters and setters
        public String getName() { return name; }
        public void setName(String name) { this.name = name; }
        
        public int getRow() { return row; }
        public void setRow(int row) { this.row = row; }
        
        public int getCol() { return col; }
        public void setCol(int col) { this.col = col; }
        
        public int getLength() { return length; }
        public void setLength(int length) { this.length = length; }
        
        public String getType() { return type; }
        public void setType(String type) { this.type = type; }
        
        public String getPrompt() { return prompt; }
        public void setPrompt(String prompt) { this.prompt = prompt; }
        
        public String getColor() { return color; }
        public void setColor(String color) { this.color = color; }
        
        public String getFieldTypeCategory() { return fieldTypeCategory; }
        public void setFieldTypeCategory(String fieldTypeCategory) { this.fieldTypeCategory = fieldTypeCategory; }
        
        public boolean isEditable() { return editable; }
        public void setEditable(boolean editable) { this.editable = editable; }
    }
}