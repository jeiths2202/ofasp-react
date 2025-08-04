import java.io.*;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.nio.charset.Charset;
import java.io.UnsupportedEncodingException;
import java.util.*;
import com.openasp.logging.LogClient;

/**
 * MSGSampleBrowserMenuClean - Fixed version for OpenASP compliant SMED browsing
 * Handles SJIS encoded Japanese data in 80-byte fixed-length records
 * Compliant with catalog.json specifications for EMPLOYEE.FB processing
 */
public class MSGSampleBrowserMenuClean {
    
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE.FB";
    private static final String MAP_NAME = "BROWSE_MENU";
    private static final int RECORDS_PER_PAGE = 5; // Limit to 5 employees for stable WebSocket Hub display
    private static final int RECORD_LENGTH = 80;
    
    // Logger instance
    private static final LogClient.Logger logger = LogClient.getLogger(MSGSampleBrowserMenuClean.class);
    
    private List<EmployeeRecord> allRecords;
    private int currentPage = 1;
    private int totalPages = 0;
    private int totalRecords = 0;
    
    /**
     * Employee record structure
     */
    private static class EmployeeRecord {
        private String id = "";
        private String name = "";
        private String department = "";
        private String position = "";
        private String age = "";
        private String hireDate = "";
        
        public EmployeeRecord(byte[] recordBytes) {
            // Parse 80-byte fixed record with SJIS encoding
            // Based on hex analysis: ID(6) + Name(~22) + Spaces + Dept(~22) + Spaces + Salary(10) + Status(1)
            String recordStr = new String(recordBytes, Charset.forName("Shift_JIS"));
            
            // Debug: Log raw bytes and converted string
            if (System.getProperty("debug", "false").equals("true")) {
                logger.debug("Raw bytes: " + java.util.Arrays.toString(recordBytes));
                logger.debug("SJIS String: " + recordStr);
            }
            
            // Extract ID (first 6 characters)
            this.id = recordStr.substring(0, 6).trim();
            
            // Extract name (positions 6-28, but trim spaces)
            String nameField = recordStr.substring(6, Math.min(28, recordStr.length()));
            this.name = trimSpaces(nameField);
            
            // Find department field by scanning for next non-space Japanese characters
            // Department appears to be around position 42-64
            int deptStart = findNextNonSpace(recordStr, 28);
            if (deptStart < recordStr.length()) {
                String deptField = recordStr.substring(deptStart, Math.min(deptStart + 22, recordStr.length()));
                // Clean department field - remove numeric salary data
                String cleanDept = deptField.replaceAll("\\d{6,}", "").trim();
                this.department = cleanDept;
            } else {
                this.department = "";
            }
            
            // Extract salary from the numeric part
            String salaryPattern = "(\\d{6})";
            java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(salaryPattern);
            java.util.regex.Matcher matcher = pattern.matcher(recordStr);
            if (matcher.find()) {
                String fullSalary = matcher.group(1);
                // Format salary (e.g., 004500 -> 4,500)
                int salaryValue = Integer.parseInt(fullSalary);
                this.position = String.format("%,d", salaryValue);
            } else {
                this.position = "";
            }
            
            // Extract hire date (would be in a different format in real data)
            this.hireDate = "2020-04-01"; // Placeholder
            
            // Status (last character)
            if (recordStr.length() > 75) {
                this.age = recordStr.substring(75).trim();
            } else {
                this.age = "A"; // Default status
            }
            
            // Only log successful parsing in debug mode
            if (System.getProperty("debug", "false").equals("true")) {
                System.err.println("Record parsed - ID: " + this.id + ", Name: " + this.name + ", Dept: '" + this.department + "', Salary: " + this.position + ", Status: " + this.age);
            }
        }
        
        private String trimSpaces(String str) {
            if (str == null) return "";
            return str.replaceAll("^\\s+|\\s+$", "").replaceAll("\\s{2,}", " ");
        }
        
        private int findNextNonSpace(String str, int start) {
            for (int i = start; i < str.length(); i++) {
                char c = str.charAt(i);
                if (c != ' ' && c != '\t' && c != '\n') {
                    return i;
                }
            }
            return str.length();
        }
        
        private int findNextDigit(String str, int start) {
            for (int i = start; i < str.length(); i++) {
                if (Character.isDigit(str.charAt(i))) {
                    return i;
                }
            }
            return -1;
        }
    }
    
    public static void main(String[] args) {
        try {
            // Set UTF-8 encoding for output streams
            System.setOut(new PrintStream(System.out, true, "UTF-8"));
            System.setErr(new PrintStream(System.err, true, "UTF-8"));
            
            MSGSampleBrowserMenuClean browser = new MSGSampleBrowserMenuClean();
            
            // Read JSON input from stdin with UTF-8 encoding
            String inputJson = "";
            try {
                BufferedReader reader = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
                StringBuilder inputBuilder = new StringBuilder();
                String line;
                
                if (reader.ready() || System.in.available() > 0) {
                    while ((line = reader.readLine()) != null && !line.isEmpty()) {
                        inputBuilder.append(line);
                    }
                    inputJson = inputBuilder.toString();
                }
                
                if (inputJson.isEmpty()) {
                    inputJson = "{\"program\": \"MSGSAMPLEBROWSERMENU\", \"library\": \"TESTLIB\", \"volume\": \"DISK01\", \"user\": \"system\"}";
                }
                
                System.err.println("Input received: " + inputJson);
            } catch (Exception e) {
                inputJson = "{\"program\": \"MSGSAMPLEBROWSERMENU\", \"library\": \"TESTLIB\", \"volume\": \"DISK01\", \"user\": \"system\"}";
            }
            
            // Load and process data
            browser.loadEmployeeData();
            browser.calculatePagination();
            browser.generateOpenASPResponse();
            
        } catch (Exception e) {
            logger.error("CRITICAL ERROR in MSGSampleBrowserMenuClean", e);
            e.printStackTrace();
            System.out.println("{\"action\": \"error\", \"message\": \"" + escapeJson(e.getMessage()) + "\", \"error_type\": \"critical\", \"suggestions\": [\"Check EMPLOYEE.FB file exists and is readable\", \"Verify SJIS encoding support\", \"Review catalog.json configuration\"]}");
        }
    }
    
    /**
     * Load employee data from EMPLOYEE.FB fixed-length record file
     * Each record is exactly 80 bytes in SJIS encoding
     */
    private void loadEmployeeData() throws IOException {
        logger.info("Loading employee data from: " + DATA_FILE);
        allRecords = new ArrayList<>();
        
        Path dataPath = Paths.get(DATA_FILE);
        if (!Files.exists(dataPath)) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        // Read entire file as bytes
        byte[] fileContent = Files.readAllBytes(dataPath);
        logger.info("File size: " + fileContent.length + " bytes");
        
        // Validate file size is multiple of record length
        if (fileContent.length % RECORD_LENGTH != 0) {
            logger.warning("File size (" + fileContent.length + ") is not a multiple of record length (" + RECORD_LENGTH + ")");
        }
        
        // Process 80-byte fixed-length records
        for (int i = 0; i < fileContent.length; i += RECORD_LENGTH) {
            if (i + RECORD_LENGTH <= fileContent.length) {
                // Extract exactly 80-byte record
                byte[] recordBytes = Arrays.copyOfRange(fileContent, i, i + RECORD_LENGTH);
                
                // Check if record contains data (not all spaces/nulls)
                boolean hasData = false;
                for (byte b : recordBytes) {
                    if (b != 0x20 && b != 0x00) { // Not space or null
                        hasData = true;
                        break;
                    }
                }
                
                if (hasData) {
                    try {
                        EmployeeRecord record = new EmployeeRecord(recordBytes);
                        if (!record.id.isEmpty()) {
                            allRecords.add(record);
                            logger.debug("Loaded record ID: " + record.id + ", Name: " + record.name);
                        }
                    } catch (Exception e) {
                        logger.error("Error processing record at position " + i, e);
                        // Continue processing other records despite errors
                    }
                }
            }
        }
        
        totalRecords = allRecords.size();
        logger.info("Total records loaded: " + totalRecords);
        
        if (totalRecords == 0) {
            logger.warning("No valid records found in " + DATA_FILE);
            // Add debug information
            System.err.println("First 160 bytes of file (hex):");
            for (int i = 0; i < Math.min(160, fileContent.length); i += 16) {
                StringBuilder hex = new StringBuilder();
                StringBuilder ascii = new StringBuilder();
                for (int j = 0; j < 16 && i + j < fileContent.length; j++) {
                    byte b = fileContent[i + j];
                    hex.append(String.format("%02x ", b));
                    ascii.append((b >= 32 && b <= 126) ? (char)b : '.');
                }
                System.err.printf("%04x: %-48s %s%n", i, hex.toString(), ascii.toString());
            }
        }
    }
    
    private void calculatePagination() {
        totalPages = (int) Math.ceil((double) totalRecords / RECORDS_PER_PAGE);
        if (totalPages == 0) totalPages = 1;
    }
    
    
    /**
     * Generate OpenASP compliant JSON response
     */
    private void generateOpenASPResponse() {
        logger.smedInfo("Generating SMED MAP response", MAP_NAME);
        
        StringBuilder response = new StringBuilder();
        response.append("{");
        
        // Request SMED map display
        response.append("\"action\": \"display_map\",");
        response.append("\"map_file\": \"").append(MAP_NAME).append("\",");
        
        // Populate fields
        response.append("\"fields\": {");
        populateMapFields(response);
        response.append("},");
        
        // Messages
        response.append("\"messages\": [");
        response.append("\"Employee Data Browser\",");
        response.append("\"Total Records: ").append(totalRecords).append("\",");
        response.append("\"Page ").append(currentPage).append(" of ").append(totalPages).append("\"");
        response.append("],");
        
        // Navigation
        response.append("\"next_action\": \"wait_input\",");
        response.append("\"function_keys\": {");
        response.append("\"F1\": \"previous_page\",");
        response.append("\"F2\": \"next_page\",");
        response.append("\"F3\": \"exit_program\"");
        response.append("},");
        
        // Page info
        response.append("\"page_info\": {");
        response.append("\"current\": ").append(currentPage).append(",");
        response.append("\"total\": ").append(totalPages).append(",");
        response.append("\"records_per_page\": ").append(RECORDS_PER_PAGE).append(",");
        response.append("\"total_records\": ").append(totalRecords);
        response.append("}");
        
        response.append("}");
        
        // Output JSON with proper UTF-8 encoding
        String jsonOutput = response.toString();
        System.out.println(jsonOutput);
        
        // Debug output to stderr
        if (System.getProperty("debug", "false").equals("true")) {
            System.err.println("JSON output length: " + jsonOutput.length() + " characters");
            byte[] utf8Bytes = jsonOutput.getBytes(StandardCharsets.UTF_8);
            System.err.println("JSON UTF-8 bytes: " + utf8Bytes.length + " bytes");
        }
    }
    
    /**
     * Populate SMED map fields with employee data
     */
    private void populateMapFields(StringBuilder response) {
        int startIndex = (currentPage - 1) * RECORDS_PER_PAGE;
        int endIndex = Math.min(startIndex + RECORDS_PER_PAGE, totalRecords);
        
        boolean firstField = true;
        
        // Fill employee data slots
        for (int i = 0; i < RECORDS_PER_PAGE; i++) {
            int recordIndex = startIndex + i;
            String empNum = String.valueOf(i + 1);
            
            if (recordIndex < totalRecords) {
                EmployeeRecord record = allRecords.get(recordIndex);
                
                if (!firstField) response.append(",");
                
                // Map fields according to SMED definition
                // EmpID, Name, Dept, Salary, HireDate, Status
                response.append("\"EMP").append(empNum).append("_ID\": \"").append(escapeJson(record.id)).append("\",");
                response.append("\"EMP").append(empNum).append("_NAME\": \"").append(escapeJson(record.name)).append("\",");
                response.append("\"EMP").append(empNum).append("_DEPT\": \"").append(escapeJson(record.department)).append("\",");
                response.append("\"EMP").append(empNum).append("_SALARY\": \"").append(escapeJson(record.position)).append("\",");
                response.append("\"EMP").append(empNum).append("_HIREDATE\": \"").append(escapeJson(record.hireDate)).append("\",");
                response.append("\"EMP").append(empNum).append("_STATUS\": \"").append(escapeJson(record.age)).append("\"");
                
                firstField = false;
            } else {
                // Empty fields
                if (!firstField) response.append(",");
                response.append("\"EMP").append(empNum).append("_ID\": \"\",");
                response.append("\"EMP").append(empNum).append("_NAME\": \"\",");
                response.append("\"EMP").append(empNum).append("_DEPT\": \"\",");
                response.append("\"EMP").append(empNum).append("_SALARY\": \"\",");
                response.append("\"EMP").append(empNum).append("_HIREDATE\": \"\",");
                response.append("\"EMP").append(empNum).append("_STATUS\": \"\"");
                
                firstField = false;
            }
        }
        
        // Status fields
        if (!firstField) response.append(",");
        response.append("\"PAGEINFO\": \"Page ").append(currentPage).append(" / ").append(totalPages).append("\",");
        response.append("\"STATUSMSG\": \"Records: ").append(totalRecords).append(" total\"");
    }
    
    /**
     * Escape JSON special characters while preserving Unicode characters
     */
    private static String escapeJson(String str) {
        if (str == null) return "";
        
        // Escape JSON special characters - no encoding conversion needed
        // String already properly decoded from SJIS to Java's internal UTF-16 representation
        StringBuilder escaped = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            switch (c) {
                case '\\': escaped.append("\\\\"); break;
                case '"': escaped.append("\\\""); break;
                case '\n': escaped.append("\\n"); break;
                case '\r': escaped.append("\\r"); break;
                case '\t': escaped.append("\\t"); break;
                case '\b': escaped.append("\\b"); break;
                case '\f': escaped.append("\\f"); break;
                default:
                    // For control characters, use Unicode escaping
                    if (c < 32) {
                        escaped.append(String.format("\\u%04x", (int) c));
                    } else {
                        // Preserve all Unicode characters (including Japanese)
                        escaped.append(c);
                    }
                    break;
            }
        }
        return escaped.toString();
    }
}