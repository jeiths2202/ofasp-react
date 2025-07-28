package com.openasp.file;

import java.io.*;
import java.nio.file.*;
import java.nio.charset.Charset;
import java.util.*;

/**
 * Employee File I/O Processor for EMPLOYEE.FB dataset
 * Handles Fixed Block (FB) format with RECLEN=80, SHIFT_JIS encoding
 */
public class EmployeeFileProcessor {
    
    private static final String EMPLOYEE_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE.FB";
    private static final int RECORD_LENGTH = 80;
    private static final Charset SJIS_CHARSET = Charset.forName("Shift_JIS");
    
    public static void main(String[] args) {
        if (args.length < 2) {
            System.out.println("Usage: java EmployeeFileProcessor <empId> <action> [params...]");
            System.out.println("Actions: INQ, ADD, UPD, DEL, SCAN, RPT, SUM");
            return;
        }
        
        String empId = args[0];
        String action = args[1];
        
        EmployeeFileProcessor processor = new EmployeeFileProcessor();
        
        try {
            switch (action.toUpperCase()) {
                case "INQ":
                    processor.inquireEmployee(empId);
                    break;
                case "ADD":
                    if (args.length >= 4) {
                        processor.addEmployee(empId, args[2], args[3]);
                    }
                    break;
                case "UPD":
                    if (args.length >= 4) {
                        processor.updateEmployee(empId, args[2], args[3]);
                    }
                    break;
                case "DEL":
                    processor.deleteEmployee(empId);
                    break;
                case "SCAN":
                    processor.scanAllEmployees();
                    break;
                case "RPT":
                    processor.generateReport();
                    break;
                case "SUM":
                    processor.generateSummary();
                    break;
                default:
                    System.out.println("Unknown action: " + action);
            }
        } catch (Exception e) {
            System.err.println("Error processing employee file: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    public void inquireEmployee(String empId) throws IOException {
        System.out.println("=== Employee Inquiry ===");
        System.out.println("Searching for Employee ID: " + empId);
        
        List<String> records = readAllRecords();
        boolean found = false;
        
        for (String record : records) {
            if (record.startsWith(empId)) {
                System.out.println("Found: " + record);
                found = true;
                break;
            }
        }
        
        if (!found) {
            System.out.println("Employee not found: " + empId);
        }
    }
    
    public void addEmployee(String empId, String name, String dept) throws IOException {
        System.out.println("=== Employee Add ===");
        System.out.println("Adding Employee: " + empId + " - " + name + " (" + dept + ")");
        
        // Create new employee record (80 bytes fixed)
        String record = String.format("%-8s%-12s%-12s%-20s¥350000 2025-07-25", 
                                     empId, name, dept, "Tokyo");
        
        // Pad to exactly 80 bytes
        if (record.length() > RECORD_LENGTH) {
            record = record.substring(0, RECORD_LENGTH);
        } else {
            record = String.format("%-80s", record);
        }
        
        appendRecord(record);
        System.out.println("Employee added successfully");
    }
    
    public void updateEmployee(String empId, String field, String value) throws IOException {
        System.out.println("=== Employee Update ===");
        System.out.println("Updating Employee: " + empId + " field: " + field + " = " + value);
        
        List<String> records = readAllRecords();
        boolean updated = false;
        
        for (int i = 0; i < records.size(); i++) {
            String record = records.get(i);
            if (record.startsWith(empId)) {
                // Simple field update simulation
                String updatedRecord = record.substring(0, 40) + 
                                     String.format("%-40s", "Updated:" + value);
                records.set(i, updatedRecord);
                updated = true;
                break;
            }
        }
        
        if (updated) {
            writeAllRecords(records);
            System.out.println("Employee updated successfully");
        } else {
            System.out.println("Employee not found for update: " + empId);
        }
    }
    
    public void deleteEmployee(String empId) throws IOException {
        System.out.println("=== Employee Delete ===");
        System.out.println("Deleting Employee: " + empId);
        
        List<String> records = readAllRecords();
        boolean removed = records.removeIf(record -> record.startsWith(empId));
        
        if (removed) {
            writeAllRecords(records);
            System.out.println("Employee deleted successfully");
        } else {
            System.out.println("Employee not found for deletion: " + empId);
        }
    }
    
    public void scanAllEmployees() throws IOException {
        System.out.println("=== Employee File Scan ===");
        List<String> records = readAllRecords();
        
        System.out.println("Total records: " + records.size());
        for (int i = 0; i < records.size(); i++) {
            System.out.println(String.format("%03d: %s", i+1, records.get(i)));
            
            // Simulate heavy processing for long job
            try {
                Thread.sleep(10); // 10ms per record
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
    
    public void generateReport() throws IOException {
        System.out.println("=== Employee Report Generation ===");
        List<String> records = readAllRecords();
        
        System.out.println("Employee Report - Generated: " + new Date());
        System.out.println("============================================");
        
        for (String record : records) {
            if (record.trim().length() > 0) {
                String empId = record.length() >= 8 ? record.substring(0, 8).trim() : "";
                String name = record.length() >= 20 ? record.substring(8, 20).trim() : "";
                String dept = record.length() >= 32 ? record.substring(20, 32).trim() : "";
                
                System.out.println(String.format("ID: %s | Name: %s | Dept: %s", 
                                                empId, name, dept));
            }
        }
        
        System.out.println("============================================");
        System.out.println("Total employees: " + records.size());
    }
    
    public void generateSummary() throws IOException {
        System.out.println("=== Employee Summary ===");
        List<String> records = readAllRecords();
        
        Map<String, Integer> deptCount = new HashMap<>();
        
        for (String record : records) {
            if (record.trim().length() > 0 && record.length() >= 32) {
                String dept = record.substring(20, 32).trim();
                deptCount.put(dept, deptCount.getOrDefault(dept, 0) + 1);
            }
        }
        
        System.out.println("Department Summary:");
        for (Map.Entry<String, Integer> entry : deptCount.entrySet()) {
            System.out.println(entry.getKey() + ": " + entry.getValue() + " employees");
        }
        
        System.out.println("Total employees: " + records.size());
    }
    
    private List<String> readAllRecords() throws IOException {
        List<String> records = new ArrayList<>();
        
        if (!Files.exists(Paths.get(EMPLOYEE_FILE))) {
            return records;
        }
        
        byte[] fileBytes = Files.readAllBytes(Paths.get(EMPLOYEE_FILE));
        
        // Process as fixed-length records (80 bytes each)
        for (int offset = 0; offset < fileBytes.length; offset += RECORD_LENGTH) {
            int length = Math.min(RECORD_LENGTH, fileBytes.length - offset);
            byte[] recordBytes = Arrays.copyOfRange(fileBytes, offset, offset + length);
            
            // Convert from SHIFT_JIS to UTF-8
            String record = new String(recordBytes, SJIS_CHARSET);
            if (record.trim().length() > 0) {
                records.add(record);
            }
        }
        
        return records;
    }
    
    private void writeAllRecords(List<String> records) throws IOException {
        try (FileOutputStream fos = new FileOutputStream(EMPLOYEE_FILE)) {
            for (String record : records) {
                // Ensure record is exactly 80 bytes
                String paddedRecord = String.format("%-80s", record);
                if (paddedRecord.length() > RECORD_LENGTH) {
                    paddedRecord = paddedRecord.substring(0, RECORD_LENGTH);
                }
                
                byte[] recordBytes = paddedRecord.getBytes(SJIS_CHARSET);
                fos.write(recordBytes);
            }
        }
    }
    
    private void appendRecord(String record) throws IOException {
        // Ensure record is exactly 80 bytes
        String paddedRecord = String.format("%-80s", record);
        if (paddedRecord.length() > RECORD_LENGTH) {
            paddedRecord = paddedRecord.substring(0, RECORD_LENGTH);
        }
        
        try (FileOutputStream fos = new FileOutputStream(EMPLOYEE_FILE, true)) {
            byte[] recordBytes = paddedRecord.getBytes(SJIS_CHARSET);
            fos.write(recordBytes);
        }
    }
}