import java.io.*;
import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * MSGSampleBrowserMenuJSON - JSON-based SMED output for OpenASP
 * 
 * Uses JSON format for terminal communication instead of ANSI sequences
 * Compatible with OpenASP terminal processing pipeline
 */
public class MSGSampleBrowserMenuJSON {
    
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE_TEST.FB";
    private static final int RECORDS_PER_PAGE = 5;  // Display only 5 employees
    
    private List<EmployeeRecord> allRecords;
    private int currentPage = 1;
    private int totalPages = 0;
    private int totalRecords = 0;
    
    private static class EmployeeRecord {
        private String id, name, department, salary, hireDate, status;
        
        public EmployeeRecord(String line) {
            try {
                if (line.length() >= 50) {
                    this.id = line.substring(0, 6).trim();
                    this.name = line.substring(6, 31).trim();
                    this.department = line.substring(31, 37).trim();
                    String salaryRaw = line.substring(37, 43).trim();
                    this.salary = formatSalary(salaryRaw);
                    String dateRaw = line.substring(43, 51).trim();
                    this.hireDate = formatDate(dateRaw);
                    this.status = line.substring(51).trim();
                } else {
                    this.id = this.name = this.department = this.salary = this.hireDate = this.status = "";
                }
            } catch (Exception e) {
                System.err.println("Error parsing line: " + line + " - " + e.getMessage());
                this.id = this.name = this.department = this.salary = this.hireDate = this.status = "";
            }
        }
        
        private String formatSalary(String raw) {
            try {
                int salary = Integer.parseInt(raw);
                return String.format("$%,d", salary * 100); // Convert to actual salary
            } catch (NumberFormatException e) {
                return raw;
            }
        }
        
        private String formatDate(String raw) {
            try {
                if (raw.length() == 8) {
                    return raw.substring(0, 4) + "-" + raw.substring(4, 6) + "-" + raw.substring(6, 8);
                }
                return raw;
            } catch (Exception e) {
                return raw;
            }
        }
    }
    
    public static void main(String[] args) {
        MSGSampleBrowserMenuJSON browser = new MSGSampleBrowserMenuJSON();
        browser.run();
    }
    
    public void run() {
        try {
            loadEmployeeData();
            calculatePagination();
            outputSMEDMapAsJSON();
        } catch (Exception e) {
            System.err.println("Error: " + e.getMessage());
        }
    }
    
    private void loadEmployeeData() throws IOException {
        allRecords = new ArrayList<>();
        if (!Files.exists(Paths.get(DATA_FILE))) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        List<String> lines = Files.readAllLines(Paths.get(DATA_FILE), StandardCharsets.UTF_8);
        int count = 0;
        for (String line : lines) {
            if (line.trim().length() > 0 && count < 5) {  // Load only 5 employees
                allRecords.add(new EmployeeRecord(line));
                count++;
            }
        }
        totalRecords = allRecords.size();
    }
    
    private void calculatePagination() {
        totalPages = (int) Math.ceil((double) totalRecords / RECORDS_PER_PAGE);
        if (totalPages == 0) totalPages = 1;
    }
    
    private void outputSMEDMapAsJSON() {
        System.out.println("{");
        System.out.println("  \"type\": \"smed_map\",");
        System.out.println("  \"map_name\": \"BROWSE_MENU\",");
        System.out.println("  \"title\": \"Employee Information\",");
        System.out.println("  \"subtitle\": \"------------------\",");
        System.out.println("  \"headers\": [\"ID\", \"Name\", \"Dept\"],");
        System.out.println("  \"page_info\": { \"current\": 1, \"total\": 1 },");
        System.out.println("  \"function_keys\": \"F3=Quit\",");
        System.out.println("  \"status\": \"Showing 5 employees\",");
        System.out.println("  \"data\": [");
        
        // Display only 5 employees with minimal fields
        for (int i = 0; i < Math.min(5, totalRecords); i++) {
            EmployeeRecord record = allRecords.get(i);
            System.out.printf("    { \"id\": \"%s\", \"name\": \"%s\", \"dept\": \"%s\" }%s%n",
                record.id, record.name, record.department,
                (i < Math.min(5, totalRecords) - 1) ? "," : "");
        }
        
        System.out.println("  ]");
        System.out.println("}");
    }
}