import java.io.*;
import java.nio.file.*;
import java.nio.charset.Charset;
import java.util.*;
import java.net.http.*;
import java.net.URI;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * MSGSampleBrowser - Japanese SMED browsing system with strict data integrity preservation
 * 
 * CRITICAL DATA INTEGRITY REQUIREMENTS:
 * - Read EMPLOYEE.FB records byte-by-byte exactly as stored
 * - Output exactly the record length with NO modifications
 * - Never add line breaks, spaces, or any character modifications
 * - Preserve original data format whether Fixed Block or Variable Block
 * - Use binary file reading to get exact byte sequences
 * - Maintain original SJIS encoding in data stream
 * 
 * Function Keys:
 * F1 = 前頁 (Previous page)
 * F2 = 次頁 (Next page)  
 * F3 = 終了 (Quit program)
 */
public class MSGSampleBrowser {
    
    // Constants for data integrity preservation
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE.FB";
    private static final int RECORDS_PER_PAGE = 10;
    private static final int RECORD_LENGTH = 96;  // Fixed Block record length (excluding newline)
    private static final int TOTAL_RECORD_SIZE = 97;  // Including newline
    private static final Charset SJIS_CHARSET = Charset.forName("Shift_JIS");
    private static final String ENCODING_API_URL = "http://localhost:3003/convert";
    
    // Session state
    private List<EmployeeRecord> allRecords;
    private int currentPage = 1;
    private int totalPages = 0;
    private int totalRecords = 0;
    private boolean isRunning = true;
    private Scanner scanner;
    
    // Employee record structure with byte-level data preservation
    private static class EmployeeRecord {
        private byte[] rawData;  // Original 96-byte record data (preserved exactly)
        private String id;
        private String name;
        private String department;
        private String salary;
        private String hireDate;
        private String status;
        
        /**
         * Constructor that preserves original byte data exactly as stored
         * No modifications, no trimming, no character conversion at storage level
         */
        public EmployeeRecord(byte[] recordBytes) {
            // Store exact byte data for absolute preservation
            this.rawData = Arrays.copyOf(recordBytes, recordBytes.length);
            
            // Parse fields from raw bytes for display purposes only
            // Field positions based on EMPLOYEE.FB structure analysis:
            // Bytes 0-7: Employee ID
            // Bytes 8-29: Employee Name (Japanese SJIS)
            // Bytes 30-41: Department (Japanese SJIS)  
            // Bytes 42-51: Salary
            // Bytes 52-60: Hire Date
            // Bytes 61-70: Status (Japanese SJIS)
            
            try {
                this.id = extractField(0, 8);
                this.name = extractField(8, 22);
                this.department = extractField(30, 12);
                this.salary = extractField(42, 10);
                this.hireDate = extractField(52, 9);
                this.status = extractField(61, 10);
            } catch (Exception e) {
                System.err.println("Warning: Error parsing record fields: " + e.getMessage());
                // Set safe defaults if parsing fails
                this.id = "";
                this.name = "";
                this.department = "";
                this.salary = "";
                this.hireDate = "";
                this.status = "";
            }
        }
        
        /**
         * Extract field from raw bytes without any modification
         * Returns the exact byte sequence as a string using SJIS encoding
         */
        private String extractField(int start, int length) {
            if (start + length > rawData.length) {
                length = rawData.length - start;
            }
            if (length <= 0) return "";
            
            byte[] fieldBytes = Arrays.copyOfRange(rawData, start, start + length);
            return new String(fieldBytes, SJIS_CHARSET);
        }
        
        /**
         * Get the original raw byte data exactly as stored
         * This method ensures zero data modification
         */
        public byte[] getRawData() {
            return Arrays.copyOf(rawData, rawData.length);
        }
        
        /**
         * Get exact record length as stored
         */
        public int getRecordLength() {
            return rawData.length;
        }
        
        @Override
        public String toString() {
            // Display version only - never modifies the original raw data
            return String.format("%-8s %-20s %-10s %-10s %-8s %-10s", 
                id, name, department, salary, hireDate, status);
        }
    }
    
    public static void main(String[] args) {
        MSGSampleBrowser browser = new MSGSampleBrowser();
        browser.run();
    }
    
    public void run() {
        scanner = new Scanner(System.in);
        
        try {
            // Initialize system
            displayLoadingMessage();
            loadEmployeeData();
            calculatePagination();
            
            // Main program loop
            while (isRunning) {
                displayCurrentPage();
                processUserInput();
            }
            
        } catch (Exception e) {
            displayErrorMessage("System Error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            if (scanner != null) {
                scanner.close();
            }
            displayShutdownMessage();
        }
    }
    
    private void displayLoadingMessage() {
        clearScreen();
        System.out.println("╔══════════════════════════════════════════════════════════════════════════════╗");
        System.out.println("║                          Employee Data Browser                               ║");
        System.out.println("║                      Employee Data Browsing System                          ║");
        System.out.println("╚══════════════════════════════════════════════════════════════════════════════╝");
        System.out.println();
        System.out.println("Loading employee data...");
        System.out.println();
    }
    
    private void loadEmployeeData() throws IOException {
        allRecords = new ArrayList<>();
        
        if (!Files.exists(Paths.get(DATA_FILE))) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        // Read file with SJIS encoding
        List<String> lines = Files.readAllLines(Paths.get(DATA_FILE), SJIS_CHARSET);
        
        for (String line : lines) {
            if (line.trim().length() > 0) {
                allRecords.add(new EmployeeRecord(line));
            }
        }
        
        totalRecords = allRecords.size();
        System.out.println("Data loaded: " + totalRecords + " records found");
        System.out.println("Data loaded successfully: " + totalRecords + " records found");
        
        // Small delay for user experience
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    private void calculatePagination() {
        totalPages = (int) Math.ceil((double) totalRecords / RECORDS_PER_PAGE);
        if (totalPages == 0) totalPages = 1;
    }
    
    private void displayCurrentPage() {
        clearScreen();
        displayHeader();
        displayPageData();
        displayFooter();
        displayFunctionKeys();
    }
    
    private void displayHeader() {
        System.out.println("╔══════════════════════════════════════════════════════════════════════════════╗");
        System.out.println("║                          Employee Data List                                  ║");
        System.out.println("║                       Employee Data List Display                            ║");
        System.out.println("╠══════════════════════════════════════════════════════════════════════════════╣");
        
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String pageInfo = String.format("Page %d / %d", currentPage, totalPages);
        String recordInfo = String.format("Total Records: %d", totalRecords);
        
        System.out.printf("║ %s                                    %s ║%n", timestamp, pageInfo);
        System.out.printf("║ %s                                                        ║%n", recordInfo);
        System.out.println("╠══════════════════════════════════════════════════════════════════════════════╣");
        System.out.println("║ EmpID    Name                 Dept       Salary     HireDate Status   ║");
        System.out.println("║ EmpID    Name                 Dept       Salary     HireDate Status   ║");
        System.out.println("╠══════════════════════════════════════════════════════════════════════════════╣");
    }
    
    private void displayPageData() {
        int startIndex = (currentPage - 1) * RECORDS_PER_PAGE;
        int endIndex = Math.min(startIndex + RECORDS_PER_PAGE, totalRecords);
        
        for (int i = startIndex; i < endIndex; i++) {
            EmployeeRecord record = allRecords.get(i);
            System.out.printf("║ %s ║%n", formatRecordLine(record));
        }
        
        // Fill empty lines if page not full
        for (int i = endIndex - startIndex; i < RECORDS_PER_PAGE; i++) {
            System.out.println("║" + " ".repeat(78) + "║");
        }
    }
    
    private String formatRecordLine(EmployeeRecord record) {
        // Use the new SJISToUnicodeConverter for proper Japanese character handling
        return SJISToUnicodeConverter.formatEmployeeRecord(
            record.id,
            record.name,
            record.department,
            record.salary,
            record.hireDate,
            record.status
        );
    }
    
    private String truncateString(String str, int maxLength) {
        // Delegate to the SJISToUnicodeConverter for consistent character handling
        return SJISToUnicodeConverter.truncateToDisplayWidth(str, maxLength);
    }
    
    private void displayFooter() {
        System.out.println("╚══════════════════════════════════════════════════════════════════════════════╝");
        System.out.println();
    }
    
    private void displayFunctionKeys() {
        System.out.println("┌──────────────────────────────────────────────────────────────────────────────┐");
        System.out.println("│ Function Keys:                                                           │");
        System.out.println("│ F1=Previous Page  F2=Next Page  F3=Quit                                 │");
        System.out.println("└──────────────────────────────────────────────────────────────────────────────┘");
        System.out.print("Enter command: ");
    }
    
    private void processUserInput() {
        String input = scanner.nextLine().trim().toUpperCase();
        
        switch (input) {
            case "F1":
                processPreviousPage();
                break;
            case "F2":
                processNextPage();
                break;
            case "F3":
                processQuit();
                break;
            case "":
                // Just refresh the display
                break;
            default:
                displayStatusMessage("Invalid command: " + input);
                break;
        }
    }
    
    private void processPreviousPage() {
        if (currentPage > 1) {
            currentPage--;
            displayStatusMessage("Moved to previous page");
        } else {
            displayStatusMessage("Already at first page");
        }
        
        // Brief pause to show status message
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    private void processNextPage() {
        if (currentPage < totalPages) {
            currentPage++;
            displayStatusMessage("Moved to next page");
        } else {
            displayStatusMessage("Already at last page");
        }
        
        // Brief pause to show status message
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    private void processQuit() {
        displayStatusMessage("Exiting program...");
        isRunning = false;
        
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    private void displayStatusMessage(String message) {
        System.out.println("\nStatus: " + message);
    }
    
    private void displayErrorMessage(String message) {
        System.err.println("\nError: " + message);
    }
    
    private void displayShutdownMessage() {
        clearScreen();
        System.out.println("╔══════════════════════════════════════════════════════════════════════════════╗");
        System.out.println("║                          Program Terminated                                 ║");
        System.out.println("║                         Program Terminated                                  ║");
        System.out.println("╚══════════════════════════════════════════════════════════════════════════════╝");
        System.out.println();
        System.out.println("Thank you for using the Employee Data Browser");
        System.out.println();
    }
    
    private void clearScreen() {
        // ANSI escape sequence to clear screen
        System.out.print("\033[2J\033[H");
        System.out.flush();
    }
    
    /**
     * Convert SJIS encoded data to EBCDIC using the SJISToUnicodeConverter utility
     * This method integrates with the existing SJIS/EBCDIC conversion service on port 3003
     */
    private String convertToEBCDIC(String sjisData) {
        return SJISToUnicodeConverter.sjisToEBCDIC(sjisData);
    }
    
    /**
     * Convert EBCDIC encoded data back to SJIS for display
     */
    private String convertFromEBCDIC(String ebcdicData) {
        return SJISToUnicodeConverter.ebcdicToSJIS(ebcdicData);
    }
    
    /**
     * Convert SJIS data to Unicode for web terminal display
     */
    private String convertToUnicode(String sjisData) {
        return SJISToUnicodeConverter.sjisToUnicode(sjisData);
    }
    
    /**
     * Convert SJIS data to UTF-8 for web transmission
     */
    private String convertToUTF8(String sjisData) {
        return SJISToUnicodeConverter.sjisToUTF8(sjisData);
    }
}