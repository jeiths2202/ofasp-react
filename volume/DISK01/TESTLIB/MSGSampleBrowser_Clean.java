import java.io.*;
import java.nio.file.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.net.http.*;
import java.net.URI;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * MSGSampleBrowser - Commercial-grade SMED browsing system with Japanese full-width character support
 * Features: Pagination, Session management, Function key processing, SJIS/Unicode integration
 * 
 * Function Keys:
 * F1 = Previous page
 * F2 = Next page  
 * F3 = Quit program
 */
public class MSGSampleBrowser_Clean {
    
    // Constants
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA_JP";
    private static final int RECORDS_PER_PAGE = 10;
    private static final int RECORD_LENGTH = 80;
    private static final Charset DATA_CHARSET = StandardCharsets.UTF_8;
    private static final String ENCODING_API_URL = "http://localhost:3003/convert";
    
    // Session state
    private List<EmployeeRecord> allRecords;
    private int currentPage = 1;
    private int totalPages = 0;
    private int totalRecords = 0;
    private boolean isRunning = true;
    private Scanner scanner;
    
    // Employee record structure
    private static class EmployeeRecord {
        String id;
        String name;
        String department;
        String salary;
        String hireDate;
        String status;
        
        public EmployeeRecord(String line) {
            if (line.length() >= RECORD_LENGTH) {
                this.id = line.substring(0, 8).trim();
                this.name = line.substring(8, 30).trim();
                this.department = line.substring(30, 42).trim();
                this.salary = line.substring(42, 52).trim();
                this.hireDate = line.substring(52, 61).trim();
                this.status = line.substring(61, 71).trim();
            } else {
                // Handle shorter lines gracefully
                this.id = line.length() > 0 ? line.substring(0, Math.min(8, line.length())).trim() : "";
                this.name = line.length() > 8 ? line.substring(8, Math.min(30, line.length())).trim() : "";
                this.department = line.length() > 30 ? line.substring(30, Math.min(42, line.length())).trim() : "";
                this.salary = line.length() > 42 ? line.substring(42, Math.min(52, line.length())).trim() : "";
                this.hireDate = line.length() > 52 ? line.substring(52, Math.min(61, line.length())).trim() : "";
                this.status = line.length() > 61 ? line.substring(61, Math.min(71, line.length())).trim() : "";
            }
        }
        
        @Override
        public String toString() {
            return String.format("%-8s %-20s %-10s %-10s %-8s %-10s", 
                id, name, department, salary, hireDate, status);
        }
    }
    
    public static void main(String[] args) {
        MSGSampleBrowser_Clean browser = new MSGSampleBrowser_Clean();
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
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println("|                       Employee Data Browser System                          |");
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println();
        System.out.println("Loading employee data...");
        System.out.println();
    }
    
    private void loadEmployeeData() throws IOException {
        allRecords = new ArrayList<>();
        
        if (!Files.exists(Paths.get(DATA_FILE))) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        // Read file with UTF-8 encoding
        List<String> lines = Files.readAllLines(Paths.get(DATA_FILE), DATA_CHARSET);
        
        for (String line : lines) {
            if (line.trim().length() > 0) {
                allRecords.add(new EmployeeRecord(line));
            }
        }
        
        totalRecords = allRecords.size();
        System.out.println("Data loaded: " + totalRecords + " records found");
        System.out.println("Loading complete. Press ENTER to continue...");
        
        // Wait for user to press enter
        scanner.nextLine();
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
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println("|                       Employee Data List Display                            |");
        System.out.println("+------------------------------------------------------------------------------+");
        
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        String pageInfo = String.format("Page %d / %d", currentPage, totalPages);
        String recordInfo = String.format("Total Records: %d", totalRecords);
        
        System.out.printf("| %s                                    %s |%n", timestamp, pageInfo);
        System.out.printf("| %s                                                        |%n", recordInfo);
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println("| EmpID    Name                 Dept       Salary     HireDate Status       |");
        System.out.println("+------------------------------------------------------------------------------+");
    }
    
    private void displayPageData() {
        int startIndex = (currentPage - 1) * RECORDS_PER_PAGE;
        int endIndex = Math.min(startIndex + RECORDS_PER_PAGE, totalRecords);
        
        for (int i = startIndex; i < endIndex; i++) {
            EmployeeRecord record = allRecords.get(i);
            System.out.printf("| %s |%n", formatRecordLine(record));
        }
        
        // Fill empty lines if page not full
        for (int i = endIndex - startIndex; i < RECORDS_PER_PAGE; i++) {
            System.out.println("|" + " ".repeat(76) + " |");
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
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println();
    }
    
    private void displayFunctionKeys() {
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println("| Function Keys:                                                               |");
        System.out.println("| F1=Previous Page  F2=Next Page  F3=Quit                                     |");
        System.out.println("+------------------------------------------------------------------------------+");
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
            Thread.sleep(1000);
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
            Thread.sleep(1000);
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
        System.out.println("+------------------------------------------------------------------------------+");
        System.out.println("|                         Program Terminated                                  |");
        System.out.println("+------------------------------------------------------------------------------+");
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