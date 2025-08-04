import java.io.*;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * Test browser with English data to verify field mapping
 */
public class TestBrowserEnglish {
    
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE_EN.DAT";
    private static final String MAP_NAME = "BROWSE_MENU";
    private static final int RECORDS_PER_PAGE = 10;
    private static final int RECORD_LENGTH = 80;
    
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
        
        public EmployeeRecord(String csvLine) {
            try {
                // Parse CSV: E001,Name,Department,Position,Age,HireDate
                String[] parts = csvLine.split(",", -1);
                if (parts.length >= 6) {
                    this.id = parts[0].trim();
                    this.name = parts[1].trim();
                    this.department = parts[2].trim();
                    this.position = parts[3].trim();
                    this.age = parts[4].trim();
                    this.hireDate = parts[5].trim();
                } else {
                    System.err.println("Invalid CSV format: " + csvLine);
                }
            } catch (Exception e) {
                System.err.println("Error parsing CSV: " + e.getMessage());
            }
        }
    }
    
    public static void main(String[] args) {
        try {
            TestBrowserEnglish browser = new TestBrowserEnglish();
            browser.loadEmployeeData();
            browser.calculatePagination();
            browser.generateOpenASPResponse();
        } catch (Exception e) {
            System.out.println("{\"action\": \"error\", \"message\": \"" + escapeJson(e.getMessage()) + "\"}");
            e.printStackTrace();
        }
    }
    
    /**
     * Load employee data from 80-byte padded CSV file
     */
    private void loadEmployeeData() throws IOException {
        allRecords = new ArrayList<>();
        
        Path dataPath = Paths.get(DATA_FILE);
        if (!Files.exists(dataPath)) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        // Read entire file as bytes
        byte[] fileContent = Files.readAllBytes(dataPath);
        System.err.println("File size: " + fileContent.length + " bytes");
        
        // Process 80-byte records
        for (int i = 0; i < fileContent.length; i += RECORD_LENGTH) {
            if (i + RECORD_LENGTH <= fileContent.length) {
                // Extract 80-byte record
                byte[] recordBytes = Arrays.copyOfRange(fileContent, i, i + RECORD_LENGTH);
                
                // Convert to string and trim padding
                String recordStr = new String(recordBytes, StandardCharsets.UTF_8).trim();
                
                if (!recordStr.isEmpty() && recordStr.startsWith("E")) {
                    // Remove any trailing data from previous record
                    int nextRecordPos = recordStr.indexOf("E", 1);
                    if (nextRecordPos > 0) {
                        recordStr = recordStr.substring(0, nextRecordPos).trim();
                    }
                    
                    allRecords.add(new EmployeeRecord(recordStr));
                    System.err.println("Parsed record: " + recordStr);
                    System.err.println("  ID: " + allRecords.get(allRecords.size()-1).id);
                    System.err.println("  Name: " + allRecords.get(allRecords.size()-1).name);
                    System.err.println("  Dept: " + allRecords.get(allRecords.size()-1).department);
                    System.err.println("  Position: " + allRecords.get(allRecords.size()-1).position);
                    System.err.println("  Age: " + allRecords.get(allRecords.size()-1).age);
                    System.err.println("  HireDate: " + allRecords.get(allRecords.size()-1).hireDate);
                }
            }
        }
        
        totalRecords = allRecords.size();
        System.err.println("Total records loaded: " + totalRecords);
    }
    
    private void calculatePagination() {
        totalPages = (int) Math.ceil((double) totalRecords / RECORDS_PER_PAGE);
        if (totalPages == 0) totalPages = 1;
    }
    
    /**
     * Generate OpenASP compliant JSON response
     */
    private void generateOpenASPResponse() {
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
        response.append("\"Employee Data Browser - English Test\",");
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
        
        System.out.println(response.toString());
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
                System.err.println("\nMapping for EMP" + empNum + ":");
                System.err.println("  EMP" + empNum + "_ID = " + record.id);
                System.err.println("  EMP" + empNum + "_NAME = " + record.name);
                System.err.println("  EMP" + empNum + "_DEPT = " + record.department);
                System.err.println("  EMP" + empNum + "_SALARY = " + record.position + " (using position as salary)");
                System.err.println("  EMP" + empNum + "_HIREDATE = " + record.hireDate);
                System.err.println("  EMP" + empNum + "_STATUS = " + record.age + " (using age as status)");
                
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
     * Escape JSON special characters
     */
    private static String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
}