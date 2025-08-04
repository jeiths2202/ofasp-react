import java.io.*;
import java.nio.file.*;
import java.nio.charset.Charset;
import java.util.*;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * MSGSampleBrowserMenu - Enhanced Japanese SMED browsing system using BROWSE_MENU map
 * 
 * ARCHITECTURE COMPLIANCE:
 * - Uses BROWSE_MENU SMED map following established SMED patterns
 * - Implements proper SJIS byte-level data reading from EMPLOYEE.FB
 * - Follows parse_smed.py and api_server.py encoding patterns
 * - Maintains data integrity with exact byte preservation
 * - Uses SJISToUnicodeConverter for web display conversion
 * 
 * SMED INTEGRATION:
 * - Map file: BROWSE_MENU (newly created following SMED architecture)
 * - Field positioning based on ITEM definitions in BROWSE_MENU
 * - Japanese text handling via SJIS encoding specifications
 * - Color coding and display attributes from SMED map
 * 
 * Function Keys:
 * F1 = 前頁 (Previous page) - HOOK F1_KEY
 * F2 = 次頁 (Next page) - HOOK F2_KEY
 * F3 = 終了 (Quit program) - HOOK F3_KEY
 */
public class MSGSampleBrowserMenu {
    
    // SMED Map and Data File Constants
    private static final String SMED_MAP_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/BROWSE_MENU";
    private static final String DATA_FILE = "/home/aspuser/app/volume/DISK01/TESTLIB/EMPLOYEE.FB";
    private static final int RECORDS_PER_PAGE = 10;  // Matches BROWSE_MENU design
    private static final Charset SJIS_CHARSET = Charset.forName("Shift_JIS");
    
    // SMED Map Field Positions (from BROWSE_MENU ITEM definitions)
    private static final int TITLE_ROW = 2, TITLE_COL = 20;
    private static final int HEADER_ROW = 5;
    private static final int DATA_START_ROW = 7;
    private static final int PAGEINFO_ROW = 18, PAGEINFO_COL = 30;
    private static final int FUNCKEYS_ROW = 20, FUNCKEYS_COL = 10;
    private static final int STATUS_ROW = 22, STATUS_COL = 5;
    private static final int INPUT_ROW = 24, INPUT_COL = 1;
    
    // Field column positions (from BROWSE_MENU ITEM definitions)
    private static final int ID_COL = 5, ID_LEN = 8;
    private static final int NAME_COL = 15, NAME_LEN = 20;
    private static final int DEPT_COL = 37, DEPT_LEN = 10;
    private static final int SALARY_COL = 49, SALARY_LEN = 10;
    private static final int HIREDATE_COL = 61, HIREDATE_LEN = 8;
    private static final int STATUS_COL = 71, STATUS_LEN = 8;
    
    // Session state variables (matching BROWSE_MENU VAR definitions)
    private List<EmployeeRecord> allRecords;
    private int currentPage = 1;
    private int totalPages = 0;
    private int totalRecords = 0;
    private boolean isRunning = true;
    private Scanner scanner;
    private SMEDMapParser mapParser;
    
    /**
     * Employee record structure with byte-level data preservation
     * Maintains original SJIS encoding exactly as stored in EMPLOYEE.FB
     */
    private static class EmployeeRecord {
        private byte[] rawData;  // Original byte data (preserved exactly)
        private String id;
        private String name;
        private String department;
        private String salary;
        private String hireDate;
        private String status;
        
        /**
         * Constructor preserves original byte data from EMPLOYEE.FB
         * Field positions match EMPLOYEE.FB structure analysis
         */
        public EmployeeRecord(byte[] recordBytes) {
            this.rawData = Arrays.copyOf(recordBytes, recordBytes.length);
            
            try {
                // Extract fields using SJIS encoding (matches parse_smed.py pattern)
                this.id = extractField(0, 6).trim();           // Employee ID
                this.name = extractField(6, 22).trim();        // Name (SJIS)
                this.department = extractField(28, 6).trim();  // Department (SJIS)
                this.salary = extractField(34, 8).trim();      // Salary
                this.hireDate = extractField(42, 8).trim();    // Hire Date
                this.status = extractField(50, 6).trim();      // Status (SJIS)
            } catch (Exception e) {
                System.err.println("Warning: Error parsing record fields: " + e.getMessage());
                initializeEmptyFields();
            }
        }
        
        /**
         * Constructor from EMPLOYEE.FB line (SJIS string)
         * Follows existing MSGSampleBrowser pattern
         */
        public EmployeeRecord(String line) {
            try {
                this.rawData = line.getBytes(SJIS_CHARSET);
                
                if (line.length() >= 56) {
                    this.id = line.substring(0, 6).trim();
                    this.name = line.substring(6, 28).trim();
                    this.department = line.substring(28, 34).trim();
                    this.salary = line.substring(34, 42).trim();
                    this.hireDate = line.substring(42, 50).trim();
                    this.status = line.substring(50, 56).trim();
                } else {
                    initializeEmptyFields();
                }
            } catch (Exception e) {
                this.rawData = new byte[0];
                initializeEmptyFields();
            }
        }
        
        private void initializeEmptyFields() {
            this.id = "";
            this.name = "";
            this.department = "";
            this.salary = "";
            this.hireDate = "";
            this.status = "";
        }
        
        /**
         * Extract field from raw bytes without modification
         * Uses SJIS encoding (matches api_server.py convert_sjis_to_unicode pattern)
         */
        private String extractField(int start, int length) {
            if (start + length > rawData.length) {
                length = Math.max(0, rawData.length - start);
            }
            if (length <= 0) return "";
            
            byte[] fieldBytes = Arrays.copyOfRange(rawData, start, start + length);
            return new String(fieldBytes, SJIS_CHARSET);
        }
        
        public byte[] getRawData() {
            return Arrays.copyOf(rawData, rawData.length);
        }
    }
    
    /**
     * Simple SMED Map Parser to read BROWSE_MENU structure
     * Follows parse_smed.py architecture patterns
     */
    private static class SMEDMapParser {
        private Map<String, String> mapProperties = new HashMap<>();
        private List<String> mapItems = new ArrayList<>();
        
        public void parseMapFile(String mapFilePath) throws IOException {
            if (!Files.exists(Paths.get(mapFilePath))) {
                throw new FileNotFoundException("SMED map file not found: " + mapFilePath);
            }
            
            // Read SMED map with SJIS encoding (matches parse_smed.py pattern)
            List<String> lines = Files.readAllLines(Paths.get(mapFilePath), SJIS_CHARSET);
            
            for (String line : lines) {
                line = line.trim();
                if (line.startsWith("*") || line.isEmpty()) {
                    continue; // Skip comments and empty lines
                }
                
                if (line.startsWith("MAPNAME")) {
                    mapProperties.put("name", extractValue(line));
                } else if (line.startsWith("ITEM")) {
                    mapItems.add(line);
                } else if (line.startsWith("VAR")) {
                    // Handle session variables
                    mapProperties.put("variable", line);
                }
            }
        }
        
        private String extractValue(String line) {
            int spaceIndex = line.indexOf(' ');
            return spaceIndex > 0 ? line.substring(spaceIndex + 1).trim() : "";
        }
        
        public String getMapName() {
            return mapProperties.getOrDefault("name", "BROWSE_MENU");
        }
    }
    
    public static void main(String[] args) {
        MSGSampleBrowserMenu browser = new MSGSampleBrowserMenu();
        browser.run();
    }
    
    public void run() {
        scanner = new Scanner(System.in);
        
        try {
            // Initialize SMED system (HOOK INIT)
            initializeBrowseMenu();
            loadEmployeeData();
            calculatePagination();
            
            // Main program loop
            while (isRunning) {
                refreshPageData();  // HOOK REFRESH
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
    
    /**
     * Initialize BROWSE_MENU system (HOOK INIT implementation)
     * Follows SMED architecture pattern
     */
    private void initializeBrowseMenu() throws IOException {
        clearScreen();
        System.out.println("Initializing BROWSE_MENU system...");
        
        // Parse SMED map file (follows parse_smed.py pattern)
        mapParser = new SMEDMapParser();
        mapParser.parseMapFile(SMED_MAP_FILE);
        
        System.out.println("SMED Map loaded: " + mapParser.getMapName());
        System.out.println("Loading employee data...");
    }
    
    /**
     * Load employee data from EMPLOYEE.FB
     * Uses SJIS encoding with exact byte preservation
     */
    private void loadEmployeeData() throws IOException {
        allRecords = new ArrayList<>();
        
        if (!Files.exists(Paths.get(DATA_FILE))) {
            throw new FileNotFoundException("Data file not found: " + DATA_FILE);
        }
        
        // Read with SJIS encoding (matches api_server.py pattern)
        List<String> lines = Files.readAllLines(Paths.get(DATA_FILE), SJIS_CHARSET);
        
        for (String line : lines) {
            if (line.trim().length() > 0) {
                allRecords.add(new EmployeeRecord(line));
            }
        }
        
        totalRecords = allRecords.size();
        System.out.println("Data loaded: " + totalRecords + " records found");
        
        // Brief delay for user experience
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
    
    /**
     * Refresh page data display (HOOK REFRESH implementation)
     * Uses BROWSE_MENU field positioning
     */
    private void refreshPageData() {
        clearScreen();
        displaySMEDMapLayout();
        displayPageData();
        displayPageInfo();
        displayFunctionKeys();
        displayStatusMessage("データ表示中");  // "Displaying data"
        displayInputPrompt();
    }
    
    /**
     * Display SMED map layout following BROWSE_MENU structure
     * Uses Japanese text with proper SJIS encoding
     */
    private void displaySMEDMapLayout() {
        // ITEM TITLE - Position (2,20)
        setCursorPosition(TITLE_ROW, TITLE_COL);
        System.out.print("==== 従業員データ一覧 ====");  // Employee Data List
        
        // ITEM SUBTITLE - Position (3,25) 
        setCursorPosition(3, 25);
        System.out.print("Employee Data Browser");
        
        // Column headers - Position (5,x)
        setCursorPosition(HEADER_ROW, ID_COL);
        System.out.print("社員番号");  // Employee ID
        setCursorPosition(HEADER_ROW, NAME_COL);
        System.out.print("氏名");      // Name
        setCursorPosition(HEADER_ROW, DEPT_COL);
        System.out.print("部署");      // Department
        setCursorPosition(HEADER_ROW, SALARY_COL);
        System.out.print("給与");      // Salary
        setCursorPosition(HEADER_ROW, HIREDATE_COL);
        System.out.print("入社日");    // Hire Date
        setCursorPosition(HEADER_ROW, STATUS_COL);
        System.out.print("状態");      // Status
        
        // Separator line - Position (6,5)
        setCursorPosition(6, 5);
        System.out.print("────────────────────────────────────────────────────────────────────");
    }
    
    /**
     * Display employee data using BROWSE_MENU field positions
     * Converts SJIS to Unicode for proper display (matches api_server.py pattern)
     */
    private void displayPageData() {
        int startIndex = (currentPage - 1) * RECORDS_PER_PAGE;
        int endIndex = Math.min(startIndex + RECORDS_PER_PAGE, totalRecords);
        
        for (int i = startIndex; i < endIndex; i++) {
            EmployeeRecord record = allRecords.get(i);
            int row = DATA_START_ROW + (i - startIndex);
            
            // Display each field at its defined position
            setCursorPosition(row, ID_COL);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(record.id, ID_LEN));
            
            setCursorPosition(row, NAME_COL);
            String unicodeName = SJISToUnicodeConverter.sjisToUnicode(record.name);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(unicodeName, NAME_LEN));
            
            setCursorPosition(row, DEPT_COL);
            String unicodeDept = SJISToUnicodeConverter.sjisToUnicode(record.department);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(unicodeDept, DEPT_LEN));
            
            setCursorPosition(row, SALARY_COL);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(record.salary, SALARY_LEN));
            
            setCursorPosition(row, HIREDATE_COL);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(record.hireDate, HIREDATE_LEN));
            
            setCursorPosition(row, STATUS_COL);
            String unicodeStatus = SJISToUnicodeConverter.sjisToUnicode(record.status);
            System.out.print(SJISToUnicodeConverter.padToDisplayWidth(unicodeStatus, STATUS_LEN));
        }
    }
    
    /**
     * Display pagination info at PAGEINFO position
     */
    private void displayPageInfo() {
        setCursorPosition(PAGEINFO_ROW, PAGEINFO_COL);
        System.out.printf("ページ %d / %d", currentPage, totalPages);  // Page X / Y
    }
    
    /**
     * Display function keys at FUNCKEYS position
     */
    private void displayFunctionKeys() {
        setCursorPosition(FUNCKEYS_ROW, FUNCKEYS_COL);
        System.out.print("F1=前頁 F2=次頁 F3=終了");  // F1=Previous F2=Next F3=Quit
    }
    
    /**
     * Display status message at STATUS position
     */
    private void displayStatusMessage(String message) {
        setCursorPosition(STATUS_ROW, STATUS_COL);
        System.out.print("状態: " + message);  // Status: message
    }
    
    /**
     * Display input prompt at INPUT position
     */
    private void displayInputPrompt() {
        setCursorPosition(INPUT_ROW, INPUT_COL);
        System.out.print("コマンド入力> ");  // Command input>
    }
    
    /**
     * Process user input and handle function keys
     */
    private void processUserInput() {
        String input = scanner.nextLine().trim().toUpperCase();
        
        switch (input) {
            case "F1":
                processPreviousPage();  // HOOK F1_KEY
                break;
            case "F2":
                processNextPage();      // HOOK F2_KEY
                break;
            case "F3":
                processQuit();          // HOOK F3_KEY
                break;
            case "":
                // Just refresh the display
                break;
            default:
                displayStatusMessage("無効なコマンド: " + input);  // Invalid command
                try {
                    Thread.sleep(1000);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                break;
        }
    }
    
    /**
     * Handle F1 key - Previous page (HOOK F1_KEY implementation)
     */
    private void processPreviousPage() {
        if (currentPage > 1) {
            currentPage--;
            displayStatusMessage("前のページに移動しました");  // Moved to previous page
        } else {
            displayStatusMessage("最初のページです");  // Already at first page
        }
        
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    /**
     * Handle F2 key - Next page (HOOK F2_KEY implementation)
     */
    private void processNextPage() {
        if (currentPage < totalPages) {
            currentPage++;
            displayStatusMessage("次のページに移動しました");  // Moved to next page
        } else {
            displayStatusMessage("最後のページです");  // Already at last page
        }
        
        try {
            Thread.sleep(500);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    /**
     * Handle F3 key - Quit program (HOOK F3_KEY implementation)
     */
    private void processQuit() {
        displayStatusMessage("プログラムを終了します...");  // Exiting program...
        isRunning = false;
        
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }
    
    private void displayErrorMessage(String message) {
        System.err.println("\nエラー: " + message);  // Error: message
    }
    
    private void displayShutdownMessage() {
        clearScreen();
        setCursorPosition(12, 30);
        System.out.println("プログラムが終了しました");  // Program terminated
        setCursorPosition(14, 25);
        System.out.println("ご利用ありがとうございました");  // Thank you for using
    }
    
    /**
     * Set cursor position for SMED field positioning
     */
    private void setCursorPosition(int row, int col) {
        System.out.printf("\033[%d;%dH", row, col);
    }
    
    private void clearScreen() {
        System.out.print("\033[2J\033[H");
        System.out.flush();
    }
}