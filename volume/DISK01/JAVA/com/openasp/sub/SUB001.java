package com.openasp.sub;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.nio.charset.Charset;

/**
 * SUB001 - Employee Information Display Program (Enhanced Version)
 * Converted from COBOL SUB001.cob to Java with MAIN001 patterns
 * 
 * Enhanced with safe function key handling and program state management:
 * - Reads EMP.INFO sequential file (80-byte fixed records)
 * - Populates SUB001 SMED map structure with employee data
 * - Sends map data via WebSocket Hub to web UI
 * - Handles SJIS encoding for Japanese names
 * - Enhanced F3 key handling to prevent auto-reentry to MAIN001
 * - Complete program state initialization and cleanup
 * - COBOL EXIT semantics implementation
 */
public class SUB001 {
    
    // COBOL Data Division equivalents - File paths and constants
    private static final String EMP_INFO_PATH = "/home/aspuser/app/volume/DISK01/TESTLIB/EMP.INFO";
    private static final String SMED_MAP_PATH = "/home/aspuser/app/volume/DISK01/SMED/SUB001";
    private static final int RECORD_LENGTH = 80;  // 80-byte fixed records
    private static final Charset SJIS = Charset.forName("Shift_JIS");
    private static final int RECORDS_PER_PAGE = 5; // Updated to 5 based on new SMED map structure (DATA_1 to DATA_15)
    
    // Program state management
    public enum ProgramState {
        INITIALIZING("Initializing"),
        RUNNING("Running"),
        PROCESSING_INPUT("Processing Input"),
        EXITING("Exiting"),
        COMPLETED("Completed");
        
        private final String description;
        ProgramState(String description) { this.description = description; }
        public String getDescription() { return description; }
    }
    
    // Enhanced program context
    public static class ProgramContext {
        private ProgramState state;
        private String wsFileStatus = "00";
        private String wsEofFlag = "N";
        private int wsRecordCount = 0;
        private int currentPage = 1;
        private int totalRecords = 0;
        private int totalPages = 0;
        private boolean continueProcessing = true;
        private String currentTerminalId = "webui";
        
        // Complete program state initialization
        public void initialize(String terminalId) {
            setState(ProgramState.INITIALIZING);
            this.currentTerminalId = terminalId;
            this.wsFileStatus = "00";
            this.wsEofFlag = "N";
            this.wsRecordCount = 0;
            this.currentPage = 1;
            this.totalRecords = 0;
            this.totalPages = 0;
            this.continueProcessing = true;
            
            System.out.println("[SUB001] Program context initialization completed");
            System.out.println("[SUB001] - Terminal ID: " + terminalId);
            System.out.println("[SUB001] - Initial state: " + state.getDescription());
        }
        
        public void setState(ProgramState newState) {
            ProgramState oldState = this.state;
            this.state = newState;
            System.out.println("[STATE_CHANGE] " + 
                (oldState != null ? oldState.getDescription() : "null") + 
                " -> " + newState.getDescription());
        }
        
        // Complete cleanup method
        public void cleanup() {
            setState(ProgramState.COMPLETED);
            this.continueProcessing = false;
            System.out.println("[SUB001] Program context cleanup completed");
        }
        
        // Getters
        public ProgramState getState() { return state; }
        public boolean shouldContinueProcessing() { return continueProcessing; }
        public int getCurrentPage() { return currentPage; }
        public int getTotalPages() { return totalPages; }
        public String getTerminalId() { return currentTerminalId; }
        
        // Setters
        public void setRecordCounts(int total) {
            this.totalRecords = total;
            this.totalPages = Math.max(1, (total + RECORDS_PER_PAGE - 1) / RECORDS_PER_PAGE);
            System.out.println("[SUB001] Record statistics set - Total " + total + " records, " + totalPages + " pages");
        }
        
        public void nextPage() {
            if (currentPage < totalPages) {
                currentPage++;
                System.out.println("[SUB001] Moved to next page: " + currentPage + "/" + totalPages);
            }
        }
        
        public void previousPage() {
            if (currentPage > 1) {
                currentPage--;
                System.out.println("[SUB001] Moved to previous page: " + currentPage + "/" + totalPages);
            }
        }
        
        public void resetToFirstPage() {
            this.currentPage = 1;
            System.out.println("[SUB001] Reset to first page: " + currentPage + "/" + totalPages);
        }
        
        public void requestExit() {
            this.continueProcessing = false;
            setState(ProgramState.EXITING);
            System.out.println("[SUB001] Program exit requested");
        }
    }
    
    // Safe Function Key Handler
    public static class FunctionKeyHandler {
        private final ProgramContext context;
        
        public FunctionKeyHandler(ProgramContext context) {
            this.context = context;
        }
        
        public void handleFunctionKey(String functionKey) {
            if (functionKey == null || functionKey.trim().isEmpty()) {
                System.out.println("[KEY_HANDLER] Invalid Function Key: null or empty value");
                return;
            }
            
            String key = functionKey.toLowerCase().trim();
            System.out.println("[KEY_HANDLER] Processing Function Key: " + key);
            
            context.setState(ProgramState.PROCESSING_INPUT);
            
            switch (key) {
                case "f1":
                    handleF1Key();
                    break;
                case "f3":
                    handleF3Key();
                    break;
                case "f7":
                    handleF7KeySafe();
                    break;
                case "f8":
                    handleF8KeySafe();
                    break;
                case "stay":
                    handleStayKey();
                    break;
                default:
                    System.out.println("[키처리] 지원하지 않는 Function Key: " + key);
                    System.out.println("[키처리] 현재 페이지 유지");
                    context.setState(ProgramState.RUNNING);
                    break;
            }
        }
        
        private void handleF1Key() {
            System.out.println("[F1키] 초기 페이지로 복귀 처리");
            context.resetToFirstPage();
            context.setState(ProgramState.RUNNING);
        }
        
        private void handleF3Key() {
            System.out.println("[F3키] 프로그램 종료 처리 - COBOL EXIT 시맨틱스");
            
            // CRITICAL FIX: F3 키는 완전한 프로그램 종료를 의미
            // MAIN001로 제어권을 깔끔하게 반환하고 완료 메시지 전송하지 않음
            context.requestExit();
            
            System.out.println("[F3키] SUB001 종료, MAIN001로 제어권 반환");
            System.out.println("[F3키] 완료 메시지 전송하지 않음 - MAIN001이 메뉴 처리");
        }
        
        private void handleF7KeySafe() {
            System.out.println("[F7키] 다음 페이지 이동 처리 (안전)");
            try {
                context.nextPage();
                context.setState(ProgramState.RUNNING);
                System.out.println("[F7키] 다음 페이지 이동 완료");
            } catch (Exception e) {
                System.err.println("[F7키] 페이지 이동 실패: " + e.getMessage());
                context.setState(ProgramState.RUNNING);
            }
        }
        
        private void handleF8KeySafe() {
            System.out.println("[F8키] 이전 페이지 이동 처리 (안전)");
            try {
                context.previousPage();
                context.setState(ProgramState.RUNNING);
                System.out.println("[F8키] 이전 페이지 이동 완료");
            } catch (Exception e) {
                System.err.println("[F8키] 페이지 이동 실패: " + e.getMessage());
                context.setState(ProgramState.RUNNING);
            }
        }
        
        private void handleStayKey() {
            System.out.println("[Stay] 현재 페이지 유지");
            context.setState(ProgramState.RUNNING);
        }
        
        @Deprecated
        private void handleF7Key() {
            handleF7KeySafe();
        }
        
        @Deprecated
        private void handleF8Key() {
            handleF8KeySafe();
        }
    }

    // Working Storage Section - COBOL equivalent variables
    private String wsFileStatus = "00";           // PIC XX
    private String wsEofFlag = "N";               // PIC X VALUE 'N'
    private int wsRecordCount = 0;                // PIC 9(3) VALUE ZERO
    private int wsRowIndex = 1;                   // PIC 9(1) VALUE 1
    private String gsDispFile = "SUB001";         // PIC X(8) VALUE "SUB001"
    private String currentTerminalId = "webui";   // Terminal for WebSocket communication
    
    // Enhanced state management
    private final ProgramContext context = new ProgramContext();
    private final FunctionKeyHandler keyHandler = new FunctionKeyHandler(context);
    
    // Paging control variables (deprecated - moved to ProgramContext)
    private int currentPage = 1;                  // Current page number (1-based)
    private int recordsPerPage = 5;               // Records per page
    private int totalRecords = 0;                 // Total records in file
    private int totalPages = 0;                   // Total pages
    // REMOVED: private boolean continueProcessing = true; // MOVED to ProgramContext - CRITICAL FIX
    
    // SMED Map Item Structure - replaces hardcoded MENU001 structure
    public static class SmedMapItem {
        String itemName;
        String type;
        int row;
        int col;
        String prompt;
        String color;
        
        public SmedMapItem(String itemName, String type, int row, int col, String prompt, String color) {
            this.itemName = itemName;
            this.type = type;
            this.row = row;
            this.col = col;
            this.prompt = prompt;
            this.color = color;
        }
        
        @Override
        public String toString() {
            return String.format("  ITEM %s TYPE=%s POS=(%d,%d) PROMPT=\"%s\" COLOR=%s", 
                itemName, type, row, col, prompt, color);
        }
    }
    
    // Employee Record structure - matches COBOL FD EMP-FILE record
    private static class EmployeeRecord {
        String empId;      // PIC X(5) - 5-byte employee ID
        String empName;    // PIC X(20) - 20-byte SJIS name  
        String empEmail;   // PIC X(20) - 20-byte email
        String empFiller;  // PIC X(35) - 35-byte filler
        
        EmployeeRecord(String id, String name, String email, String filler) {
            this.empId = id;
            this.empName = name;
            this.empEmail = email;
            this.empFiller = filler;
        }
    }
    
    private List<SmedMapItem> smedMapTemplate = new ArrayList<>();
    private List<SmedMapItem> dynamicSmedMap = new ArrayList<>();
    private List<EmployeeRecord> employeeList = new ArrayList<>();
    
    /**
     * Main entry point - DEPRECATED: Use execute() method directly from MAIN001 ClassLoader
     * This method is kept for standalone testing only
     */
    @Deprecated
    private static void main(String[] args) {
        System.out.println("*** SUB001 - Employee Information Display (COBOL->Java Conversion) ***");
        System.out.println("WARNING: This is deprecated. SUB001 should be called via MAIN001 ClassLoader");
        System.out.println("EMP.INFO sequential file processing with SUB001 SMED map");
        System.out.println("WebSocket Hub integration following MAIN001 patterns\n");
        
        try {
            SUB001 program = new SUB001();
            
            // Get terminal ID from environment or default
            String envTerminalId = System.getenv("ASP_TERMINAL_ID");
            if (envTerminalId != null && !envTerminalId.isEmpty()) {
                program.currentTerminalId = envTerminalId;
                System.out.println("Terminal ID: " + envTerminalId + " (from environment)");
            }
            
            // Execute main procedure
            program.execute();
            
        } catch (Exception e) {
            System.err.println("ERROR: SUB001 execution failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(-1);
        }
    }
    
    /**
     * Main execution method implementing COBOL MAIN-PROCEDURE with enhanced state management
     * COBOL: MAIN-PROCEDURE.
     * COBOL:     PERFORM INITIALIZE-PROGRAM.
     * COBOL:     PERFORM MAIN-DISPLAY-LOOP UNTIL END-PROGRAM.
     * COBOL:     PERFORM TERMINATE-PROGRAM.
     * COBOL:     STOP RUN.
     */
    public void execute() {
        System.out.println("=".repeat(60));
        System.out.println("*** SUB001 Enhanced - 사원 정보 조회 (개선된 버전) ***");
        System.out.println("개선 사항:");
        System.out.println("- 안전한 Function Key 처리 (F3 키 개선)");
        System.out.println("- 프로그램 상태 완전 초기화");
        System.out.println("- COBOL EXIT 시맨틱스 완전 구현");
        System.out.println("=".repeat(60));
        
        try {
            // 프로그램 초기화
            context.initialize(currentTerminalId);
            
            // 메인 처리 루프
            performMainProcedure();
            
        } catch (Exception e) {
            System.err.println("[SUB001] 실행 오류: " + e.getMessage());
            e.printStackTrace();
        } finally {
            // 정리 작업
            performTermination();
        }
    }
    
    private void performMainProcedure() throws Exception {
        context.setState(ProgramState.RUNNING);
        System.out.println("[SUB001] 메인 처리 시작");
        
        // COBOL: PERFORM INITIALIZE-PROGRAM
        performInitializeProgram();
        
        // COBOL: PERFORM MAIN-DISPLAY-LOOP UNTIL END-PROGRAM (enhanced)
        performMainDisplayLoop();
        
        System.out.println("[SUB001] 메인 처리 완료");
    }
    
    private void performTermination() {
        System.out.println("[SUB001] 프로그램 종료 처리 시작");
        
        // COBOL: PERFORM TERMINATE-PROGRAM
        performTerminateProgram();
        
        context.cleanup();
        
        System.out.println("[SUB001] 프로그램 종료 처리 완료");
        System.out.println("[SUB001] MAIN001로 제어권 반환");
    }
    /**
     * COBOL INITIALIZE-PROGRAM implementation
     * COBOL: INITIALIZE-PROGRAM.
     * COBOL:     OPEN INPUT EMP-FILE.
     * COBOL:     OPEN I-O DISPFILE.
     * COBOL:     IF WS-FILE-STATUS NOT = "00" ... END-IF.
     * COBOL:     * Initialize map fields
     */
    private void performInitializeProgram() {
        System.out.println("[SUB001] INITIALIZE-PROGRAM - Opening files and initializing map");
        
        try {
            // COBOL: OPEN INPUT EMP-FILE
            openEmpFile();
            
            // COBOL: OPEN I-O DISPFILE  
            openDispFile();
            
            // Load SMED map template from file
            loadSmedMapTemplate();
            
            System.out.println("[SUB001] SMED map template initialized:");
            System.out.println("[SUB001] Template items loaded: " + smedMapTemplate.size());
            for (SmedMapItem item : smedMapTemplate) {
                if (item.itemName.startsWith("TEXT_")) {
                    System.out.println("[SUB001] " + item.itemName + ": " + item.prompt);
                }
            }
            
        } catch (Exception e) {
            System.err.println("[SUB001] ERROR: Cannot open EMP.INFO file");
            throw new RuntimeException("File initialization failed", e);
        }
    }
    
    /**
     * Main display loop with enhanced function key processing
     * COBOL: MAIN-DISPLAY-LOOP.
     * COBOL:     PERFORM DISPLAY-CURRENT-PAGE.
     * COBOL:     PERFORM PROCESS-USER-INPUT.
     * COBOL:     EVALUATE FUNCTION-KEY.
     */
    private void performMainDisplayLoop() {
        System.out.println("[SUB001] MAIN-DISPLAY-LOOP - Starting interactive display");
        
        try {
            // Read all employee records first
            readEmployeeData();
            
            // Set record counts in context
            context.setRecordCounts(employeeList.size());
            
            // Main interactive loop with enhanced state management
            int loopCounter = 0;
            final int MAX_LOOP_ITERATIONS = 50; // Reduced for testing
            
            while (context.shouldContinueProcessing() && loopCounter < MAX_LOOP_ITERATIONS) {
                loopCounter++;
                System.out.println("[SUB001] Interactive loop iteration: " + loopCounter);
                
                try {
                    // Display current page using context
                    displayCurrentPage();
                    
                    // Enhanced user input processing with function key handler
                    processUserInput();
                    
                    // Debug: Show current state from context
                    System.out.println("[SUB001] Loop state - shouldContinue: " + context.shouldContinueProcessing() + 
                                     ", currentPage: " + context.getCurrentPage() + "/" + context.getTotalPages());
                    
                } catch (Exception e) {
                    System.err.println("[SUB001] Error in interactive loop iteration " + loopCounter + ": " + e.getMessage());
                    System.err.println("[SUB001] Setting exit condition to prevent infinite loop");
                    // CRITICAL FIX: Use context instead of direct variable access
                    context.requestExit();
                    break;
                }
            }
            
            System.out.println("[SUB001] Interactive loop terminated, iterations: " + loopCounter);
            
        } catch (IOException e) {
            System.err.println("[SUB001] Error in MAIN-DISPLAY-LOOP: " + e.getMessage());
            throw new RuntimeException("Main display loop failed", e);
        } catch (Exception e) {
            System.err.println("[SUB001] Error in MAIN-DISPLAY-LOOP: " + e.getMessage());
            throw e;
        }
    }
    
    /**
     * Display current page of employee records using enhanced context
     * COBOL: DISPLAY-CURRENT-PAGE.
     */
    private void displayCurrentPage() {
        try {
            System.out.println("[SUB001] DISPLAY-CURRENT-PAGE - Page " + context.getCurrentPage() + " of " + context.getTotalPages());
            
            // Create dynamic SMED map with current page data
            createDynamicSmedMap();
            
            // Send map to display
            writeDispFile();
            
            System.out.println("[SUB001] Dynamic SMED map displayed for page " + context.getCurrentPage() + "/" + context.getTotalPages());
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error in DISPLAY-CURRENT-PAGE: " + e.getMessage());
            throw e;
        }
    }
    
    /**
     * Load SMED map template from file
     */
    private void loadSmedMapTemplate() throws IOException {
        System.out.println("[SUB001] Loading SMED template from " + SMED_MAP_PATH);
        
        List<String> lines = Files.readAllLines(Paths.get(SMED_MAP_PATH), SJIS);
        smedMapTemplate.clear();
        
        java.util.regex.Pattern pattern = java.util.regex.Pattern.compile(
            "\\s*ITEM\\s+(\\w+)\\s+TYPE=([A-Z])\\s+POS=\\((\\d+),(\\d+)\\)\\s+PROMPT=\"([^\"]*)\"\\s+COLOR=(.+)");
        
        for (String line : lines) {
            line = line.trim();
            if (line.startsWith("ITEM ")) {
                java.util.regex.Matcher matcher = pattern.matcher(line);
                if (matcher.matches()) {
                    String itemName = matcher.group(1);
                    String type = matcher.group(2);
                    int row = Integer.parseInt(matcher.group(3));
                    int col = Integer.parseInt(matcher.group(4));
                    String prompt = matcher.group(5);
                    String color = matcher.group(6);
                    
                    SmedMapItem item = new SmedMapItem(itemName, type, row, col, prompt, color);
                    smedMapTemplate.add(item);
                    System.out.println("[SUB001] Template: " + item);
                } else {
                    System.err.println("[SUB001] Failed to parse SMED item: " + line);
                }
            }
        }
        
        System.out.println("[SUB001] SMED template loaded: " + smedMapTemplate.size() + " items");
    }
    
    /**
     * Create dynamic SMED map with employee data
     */
    private void createDynamicSmedMap() {
        System.out.println("[SUB001] Creating dynamic SMED map for page " + context.getCurrentPage());
        
        dynamicSmedMap.clear();
        
        // Calculate current page data
        int startIndex = (context.getCurrentPage() - 1) * RECORDS_PER_PAGE;
        int endIndex = Math.min(startIndex + RECORDS_PER_PAGE, employeeList.size());
        List<EmployeeRecord> pageEmployees = employeeList.subList(startIndex, endIndex);
        
        System.out.println("[SUB001] Page employees: " + pageEmployees.size());
        
        for (SmedMapItem template : smedMapTemplate) {
            if (template.itemName.startsWith("DATA_")) {
                String data = "";
                
                // SUB001 SMED map structure (updated to match new SMED map structure):
                // DATA_1-3: Employee 1, DATA_4-6: Employee 2, DATA_7-9: Employee 3, 
                // DATA_10-12: Employee 4, DATA_13-15: Employee 5
                // 5 employees per page, 3 fields each (ID, Name, Email)
                switch (template.itemName) {
                    // Employee 1
                    case "DATA_1":
                        if (pageEmployees.size() > 0) data = pageEmployees.get(0).empId;
                        break;
                    case "DATA_2":
                        if (pageEmployees.size() > 0) data = pageEmployees.get(0).empName;
                        break;
                    case "DATA_3":
                        if (pageEmployees.size() > 0) data = pageEmployees.get(0).empEmail;
                        break;
                    // Employee 2
                    case "DATA_4":
                        if (pageEmployees.size() > 1) data = pageEmployees.get(1).empId;
                        break;
                    case "DATA_5":
                        if (pageEmployees.size() > 1) data = pageEmployees.get(1).empName;
                        break;
                    case "DATA_6":
                        if (pageEmployees.size() > 1) data = pageEmployees.get(1).empEmail;
                        break;
                    // Employee 3
                    case "DATA_7":
                        if (pageEmployees.size() > 2) data = pageEmployees.get(2).empId;
                        break;
                    case "DATA_8":
                        if (pageEmployees.size() > 2) data = pageEmployees.get(2).empName;
                        break;
                    case "DATA_9":
                        if (pageEmployees.size() > 2) data = pageEmployees.get(2).empEmail;
                        break;
                    // Employee 4
                    case "DATA_10":
                        if (pageEmployees.size() > 3) data = pageEmployees.get(3).empId;
                        break;
                    case "DATA_11":
                        if (pageEmployees.size() > 3) data = pageEmployees.get(3).empName;
                        break;
                    case "DATA_12":
                        if (pageEmployees.size() > 3) data = pageEmployees.get(3).empEmail;
                        break;
                    // Employee 5
                    case "DATA_13":
                        if (pageEmployees.size() > 4) data = pageEmployees.get(4).empId;
                        break;
                    case "DATA_14":
                        if (pageEmployees.size() > 4) data = pageEmployees.get(4).empName;
                        break;
                    case "DATA_15":
                        if (pageEmployees.size() > 4) data = pageEmployees.get(4).empEmail;
                        break;
                }
                
                SmedMapItem dynamicItem = new SmedMapItem(
                    template.itemName, template.type, template.row, template.col,
                    data, template.color);
                dynamicSmedMap.add(dynamicItem);
                System.out.println("[SUB001] Dynamic: " + dynamicItem);
            } else {
                // TEXT items remain unchanged
                SmedMapItem staticItem = new SmedMapItem(
                    template.itemName, template.type, template.row, template.col,
                    template.prompt, template.color);
                dynamicSmedMap.add(staticItem);
                System.out.println("[SUB001] Static: " + staticItem);
            }
        }
        
        System.out.println("[SUB001] Dynamic SMED map created: " + dynamicSmedMap.size() + " items");
    }
    
    
    
    /**
     * COBOL TERMINATE-PROGRAM implementation
     * COBOL: TERMINATE-PROGRAM.
     * COBOL:     CLOSE EMP-FILE.
     * COBOL:     CLOSE DISPFILE.
     */
    private void performTerminateProgram() {
        System.out.println("[SUB001] TERMINATE-PROGRAM - Closing files");
        
        try {
            // COBOL: CLOSE EMP-FILE
            closeEmpFile();
            
            // COBOL: CLOSE DISPFILE
            closeDispFile();
            
            System.out.println("[SUB001] All files closed successfully");
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error in TERMINATE-PROGRAM: " + e.getMessage());
        }
    }
    
    /**
     * Open EMP-FILE for input - COBOL: OPEN INPUT EMP-FILE
     */
    private void openEmpFile() {
        System.out.println("[SUB001] OPEN INPUT EMP-FILE - " + EMP_INFO_PATH);
        
        // Check if file exists and is readable
        File empFile = new File(EMP_INFO_PATH);
        if (!empFile.exists()) {
            wsFileStatus = "35"; // File not found
            throw new RuntimeException("EMP.INFO file not found: " + EMP_INFO_PATH);
        }
        
        if (!empFile.canRead()) {
            wsFileStatus = "37"; // Permission denied
            throw new RuntimeException("Cannot read EMP.INFO file: " + EMP_INFO_PATH);
        }
        
        wsFileStatus = "00"; // Success
        System.out.println("[SUB001] EMP-FILE opened successfully, FILE-STATUS = " + wsFileStatus);
    }
    
    /**
     * Open DISPFILE for I-O - COBOL: OPEN I-O DISPFILE
     * Following MAIN001 pattern for WebSocket Hub integration
     */
    private void openDispFile() {
        System.out.println("[SUB001] OPEN I-O DISPFILE - Initializing WebSocket Hub connection");
        System.out.println("[SUB001] SYMBOLIC DESTINATION 'DSP' - SUB001 map display");
        System.out.println("[SUB001] WebSocket Hub communication active for terminal: " + currentTerminalId);
        // DISPFILE is now "open" for I-O operations
    }
    
    /**
     * Close EMP-FILE - COBOL: CLOSE EMP-FILE
     */
    private void closeEmpFile() {
        System.out.println("[SUB001] CLOSE EMP-FILE - Employee file closed");
    }
    
    /**
     * Close DISPFILE - COBOL: CLOSE DISPFILE
     */
    private void closeDispFile() {
        System.out.println("[SUB001] CLOSE DISPFILE - WebSocket Hub connection closed");
    }
    
    /**
     * Read employee data from EMP.INFO fixed block file
     * Implements 80-byte fixed record processing with SJIS encoding
     */
    private void readEmployeeData() throws IOException {
        System.out.println("[SUB001] Reading EMP.INFO file - 80-byte fixed records");
        
        // Read Fixed Block file (no newlines, pure binary data)
        byte[] fileData = Files.readAllBytes(Paths.get(EMP_INFO_PATH));
        System.out.println("[SUB001] File size: " + fileData.length + " bytes");
        
        // Process each 80-byte record
        int recordCount = fileData.length / RECORD_LENGTH;
        System.out.println("[SUB001] Expected records: " + recordCount);
        
        for (int i = 0; i < recordCount; i++) { // Read all records
            int startPos = i * RECORD_LENGTH;
            byte[] recordData = Arrays.copyOfRange(fileData, startPos, startPos + RECORD_LENGTH);
            
            // Parse record structure (5 + 20 + 20 + 35 = 80 bytes)
            String empId = new String(Arrays.copyOfRange(recordData, 0, 5), "ASCII").trim();
            String empName = new String(Arrays.copyOfRange(recordData, 5, 25), SJIS).trim();
            String empEmail = new String(Arrays.copyOfRange(recordData, 25, 45), "ASCII").trim();
            String empFiller = new String(Arrays.copyOfRange(recordData, 45, 80), "ASCII");
            
            EmployeeRecord emp = new EmployeeRecord(empId, empName, empEmail, empFiller);
            employeeList.add(emp);
            
            System.out.println("[SUB001] Record " + (i+1) + ": ID=" + empId + " Name=" + empName + " Email=" + empEmail);
        }
        
        System.out.println("[SUB001] Total employee records loaded: " + employeeList.size());
    }
    
    /**
     * Write DISPFILE - COBOL: WRITE DISPFILE FROM DISP-RECORD
     * Sends SUB001 dynamic SMED map data to WebSocket Hub
     */
    private void writeDispFile() {
        System.out.println("[SUB001] WRITE DISPFILE FROM DISP-RECORD - Sending SUB001 dynamic SMED map to WebSocket Hub");
        
        try {
            // Convert Menu001Record to SMED map format
            Map<String, Object> smedData = createSmedDisplayData();
            
            // Send to WebSocket Hub via Python client (following MAIN001 pattern)
            boolean success = sendToWebSocketHub(smedData);
            
            if (success) {
                System.out.println("[SUB001] SUB001 dynamic SMED map sent to WebSocket Hub successfully");
                System.out.println("[SUB001] Employee data displayed on terminal: " + currentTerminalId);
            } else {
                System.err.println("[SUB001] WARNING: Failed to send dynamic SMED map data to WebSocket Hub");
                // Fallback to console display
                displayEmployeeInfoFallback();
            }
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error in WRITE DISPFILE: " + e.getMessage());
            displayEmployeeInfoFallback();
        }
    }
    
    /**
     * Create SMED display data for WebSocket Hub transmission
     * Using dynamic SMED map instead of hardcoded structure
     */
    private Map<String, Object> createSmedDisplayData() {
        Map<String, Object> smedData = new HashMap<>();
        
        // Main SMED structure
        smedData.put("action", "display_map");
        smedData.put("map_file", "SUB001");  // Use SUB001 map
        smedData.put("map_name", "SUB001");
        smedData.put("terminal_id", currentTerminalId);
        smedData.put("program_name", "SUB001");
        smedData.put("encoding", "SJIS");
        smedData.put("source_type", "dynamic_dataset");
        
        // Convert dynamic SMED map to field data
        Map<String, String> fields = new HashMap<>();
        for (SmedMapItem item : dynamicSmedMap) {
            if (!item.prompt.isEmpty()) {
                fields.put(item.itemName, item.prompt);
            }
        }
        smedData.put("fields", fields);
        
        // Page information
        Map<String, Object> pageInfo = new HashMap<>();
        pageInfo.put("current", context.getCurrentPage());
        pageInfo.put("total", context.getTotalPages());
        pageInfo.put("records_per_page", RECORDS_PER_PAGE);
        pageInfo.put("total_records", employeeList.size());
        smedData.put("page_info", pageInfo);
        
        // Screen layout information
        smedData.put("rows", 24);
        smedData.put("cols", 80);
        smedData.put("background_color", "BLACK");
        smedData.put("foreground_color", "GREEN");
        
        return smedData;
    }
    
    
    /**
     * Send SMED data to WebSocket Hub - following MAIN001 sendToWebSocketHub() pattern
     */
    private boolean sendToWebSocketHub(Map<String, Object> smedData) {
        try {
            // Create temporary JSON file for WebSocket Hub client
            String tempFile = "/tmp/sub001_smed_data.json";
            String jsonData = convertToJson(smedData);
            
            // Write JSON data to temporary file  
            Files.write(Paths.get(tempFile), jsonData.getBytes(Charset.forName("UTF-8")));
            
            // Debug: Show the JSON data being sent
            System.out.println("[SUB001] DEBUG - JSON data being sent:");
            System.out.println(jsonData);
            
            // Call WebSocket Hub client via Python
            ProcessBuilder pb = new ProcessBuilder(
                "python3",
                "/home/aspuser/app/server/websocket_hub_client.py",
                "--map", "SUB001",
                "--terminal", currentTerminalId,
                "--json-file", tempFile,
                "--verbose"
            );
            
            pb.redirectOutput(ProcessBuilder.Redirect.INHERIT);
            pb.redirectError(ProcessBuilder.Redirect.INHERIT);
            
            Process process = pb.start();
            int exitCode = process.waitFor();
            
            // Clean up temporary file
            Files.deleteIfExists(Paths.get(tempFile));
            
            return exitCode == 0;
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error sending to WebSocket Hub: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Convert Map to JSON string - following MAIN001 convertToJson() pattern
     */
    private String convertToJson(Map<String, Object> data) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : data.entrySet()) {
            if (!first) json.append(",\n");
            json.append("  \"").append(entry.getKey()).append("\": ");
            
            Object value = entry.getValue();
            if (value instanceof String) {
                json.append("\"").append(escapeJsonString((String) value)).append("\"");
            } else if (value instanceof Number) {
                json.append(value);
            } else if (value instanceof Map) {
                json.append(convertMapToJson((Map<String, Object>) value));
            } else {
                json.append("\"").append(escapeJsonString(value.toString())).append("\"");
            }
            first = false;
        }
        
        json.append("\n}");
        return json.toString();
    }
    
    /**
     * Convert nested Map to JSON - following MAIN001 pattern
     */
    private String convertMapToJson(Map<String, Object> map) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) json.append(",\n");
            
            Object value = entry.getValue();
            json.append("    \"").append(entry.getKey()).append("\": ");
            
            if (value instanceof String) {
                json.append("\"").append(escapeJsonString((String) value)).append("\"");
            } else if (value instanceof Map) {
                // Nested map (for layout positions)
                Map<String, Object> nestedMap = (Map<String, Object>) value;
                json.append("{");
                boolean nestedFirst = true;
                for (Map.Entry<String, Object> nestedEntry : nestedMap.entrySet()) {
                    if (!nestedFirst) json.append(", ");
                    json.append("\"").append(nestedEntry.getKey()).append("\": ").append(nestedEntry.getValue());
                    nestedFirst = false;
                }
                json.append("}");
            } else {
                json.append("\"").append(escapeJsonString(value.toString())).append("\"");
            }
            first = false;
        }
        
        json.append("\n  }");
        return json.toString();
    }
    
    /**
     * Escape special characters in JSON strings - following MAIN001 pattern
     */
    private String escapeJsonString(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                 .replace("\"", "\\\"")
                 .replace("\n", "\\n")
                 .replace("\r", "\\r")
                 .replace("\t", "\\t");
    }
    
    /**
     * Fallback employee display for console when WebSocket Hub unavailable
     */
    private void displayEmployeeInfoFallback() {
        System.out.println("\n=== SUB001 Employee Information Display ===");
        
        // Find title from dynamic SMED map
        String title = "";
        for (SmedMapItem item : dynamicSmedMap) {
            if (item.itemName.equals("TEXT_1")) {
                title = item.prompt;
                break;
            }
        }
        
        System.out.println("=== " + title + " ===");
        System.out.printf("%-8s %-20s %s%n", "ID", "Name", "Email");
        System.out.println("------------------------------------------------------------");
        
        // Display employee data from dynamic SMED map (SUB001 shows 5 employees per page)
        String[] empIds = new String[5];
        String[] empNames = new String[5];
        String[] empEmails = new String[5];
        
        // Initialize arrays
        for (int i = 0; i < 5; i++) {
            empIds[i] = "";
            empNames[i] = "";
            empEmails[i] = "";
        }
        
        for (SmedMapItem item : dynamicSmedMap) {
            switch (item.itemName) {
                // Employee 1
                case "DATA_1": empIds[0] = item.prompt; break;
                case "DATA_2": empNames[0] = item.prompt; break;
                case "DATA_3": empEmails[0] = item.prompt; break;
                // Employee 2
                case "DATA_4": empIds[1] = item.prompt; break;
                case "DATA_5": empNames[1] = item.prompt; break;
                case "DATA_6": empEmails[1] = item.prompt; break;
                // Employee 3
                case "DATA_7": empIds[2] = item.prompt; break;
                case "DATA_8": empNames[2] = item.prompt; break;
                case "DATA_9": empEmails[2] = item.prompt; break;
                // Employee 4
                case "DATA_10": empIds[3] = item.prompt; break;
                case "DATA_11": empNames[3] = item.prompt; break;
                case "DATA_12": empEmails[3] = item.prompt; break;
                // Employee 5
                case "DATA_13": empIds[4] = item.prompt; break;
                case "DATA_14": empNames[4] = item.prompt; break;
                case "DATA_15": empEmails[4] = item.prompt; break;
            }
        }
        
        // Display all employees on current page
        for (int i = 0; i < 5; i++) {
            if (!empIds[i].isEmpty()) {
                System.out.printf("%-8s %-20s %s%n", empIds[i], empNames[i], empEmails[i]);
            }
        }
        
        System.out.println("------------------------------------------------------------");
        
        // Find help text
        for (SmedMapItem item : dynamicSmedMap) {
            if (item.itemName.equals("TEXT_5")) {
                System.out.println(item.prompt);
                break;
            }
        }
    }
    
    /**
     * Process user input with enhanced function key handling - IMPROVED STABILITY
     * COBOL: PROCESS-USER-INPUT.
     * COBOL:     READ DISPFILE INTO DISP-RECORD.
     * COBOL:     EVALUATE FUNCTION-KEY.
     */
    private void processUserInput() {
        try {
            System.out.println("[SUB001] PROCESS-USER-INPUT - Page " + context.getCurrentPage() + "/" + context.getTotalPages() + 
                             " - Waiting for function key (F1/F3/F7/F8)");
            
            // CRITICAL FIX: 재시도 메커니즘과 안전한 입력 처리
            String userInput = getInputWithRetry();
            
            if (userInput != null && !userInput.trim().isEmpty()) {
                System.out.println("[SUB001] User input received: " + userInput);
                // 안전한 Function Key 처리
                keyHandler.handleFunctionKey(userInput);
            } else {
                // FIX: null 입력 시 바로 종료하지 않고 현재 페이지 유지
                System.out.println("[SUB001] No valid input received, staying in current page");
                // 현재 페이지 유지하고 계속 처리
            }
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error in PROCESS-USER-INPUT: " + e.getMessage());
            // FIX: 예외 발생 시 바로 종료하지 않고 안전한 복구
            handleInputException(e);
        }
    }
    
    /**
     * Evaluate function key input (DEPRECATED - Use FunctionKeyHandler instead)
     * COBOL: EVALUATE FUNCTION-KEY.
     * COBOL:     WHEN F1 PERFORM RESET-TO-FIRST-PAGE.
     * COBOL:     WHEN F3 PERFORM EXIT-PROGRAM.  
     * COBOL:     WHEN F7 PERFORM NEXT-PAGE.
     * COBOL:     WHEN F8 PERFORM PREVIOUS-PAGE.
     * 
     * @deprecated Use keyHandler.handleFunctionKey() instead for enhanced functionality
     */
    @Deprecated
    private void evaluateFunctionKey(String input) {
        System.out.println("[SUB001] DEPRECATED: evaluateFunctionKey called, redirecting to enhanced handler");
        keyHandler.handleFunctionKey(input);
    }
    
    /**
     * Reset to first page - F1 function (DEPRECATED)
     * COBOL: RESET-TO-FIRST-PAGE.
     * @deprecated Use context.resetToFirstPage() instead
     */
    @Deprecated
    private void performResetToFirstPage() {
        context.resetToFirstPage();
    }
    
    /**
     * Exit program - F3 function (DEPRECATED)
     * COBOL: EXIT-PROGRAM.
     * @deprecated Use context.requestExit() instead
     */
    @Deprecated
    private void performExitProgram() {
        System.out.println("[SUB001] DEPRECATED: performExitProgram called, redirecting to enhanced exit");
        context.requestExit();
    }
    
    /**
     * Next page - F7 function (DEPRECATED)
     * COBOL: NEXT-PAGE.
     * @deprecated Use context.nextPage() instead
     */
    @Deprecated
    private void performNextPage() {
        context.nextPage();
    }
    
    /**
     * Previous page - F8 function (DEPRECATED)
     * COBOL: PREVIOUS-PAGE.
     * @deprecated Use context.previousPage() instead
     */
    @Deprecated
    private void performPreviousPage() {
        context.previousPage();
    }
    
    /**
     * Get WebSocket timeout from configuration file
     */
    private int getWebSocketTimeout() {
        int defaultTimeout = 86400; // Default 24 hours
        String configFile = "/home/aspuser/app/config/asp.conf";
        
        try {
            Properties props = new Properties();
            props.load(new FileInputStream(configFile));
            String timeoutStr = props.getProperty("websocket.timeout");
            
            if (timeoutStr != null) {
                int timeout = Integer.parseInt(timeoutStr.trim());
                System.out.println("[SUB001] WebSocket timeout loaded from config: " + timeout + " seconds");
                return timeout;
            }
        } catch (Exception e) {
            System.err.println("[SUB001] Warning: Could not load timeout from config: " + e.getMessage());
        }
        
        System.out.println("[SUB001] Using default WebSocket timeout: " + defaultTimeout + " seconds");
        return defaultTimeout;
    }
    
    /**
     * Wait for user input from WebSocket Hub - similar to MAIN001 pattern
     */
    private String waitForWebSocketInput() {
        try {
            String inputFile = "/tmp/asp_input_" + currentTerminalId + ".txt";
            File file = new File(inputFile);
            
            // Clean up any existing input file
            if (file.exists()) {
                file.delete();
            }
            
            int maxWaitSeconds = getWebSocketTimeout();
            System.out.println("[SUB001] Waiting for WebSocket Hub input... (timeout: " + maxWaitSeconds + " seconds)");
            
            int waitedSeconds = 0;
            
            while (waitedSeconds < maxWaitSeconds) {
                if (file.exists() && file.length() > 0) {
                    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                        String input = reader.readLine();
                        if (input != null && !input.trim().isEmpty()) {
                            System.out.println("[SUB001] WebSocket input received: " + input.trim());
                            file.delete(); // Clean up
                            return input.trim();
                        }
                    }
                }
                
                Thread.sleep(1000);
                waitedSeconds++;
            }
            
            System.out.println("[SUB001] Timeout waiting for WebSocket input");
            return null;
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error waiting for WebSocket input: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Get console input as fallback
     */
    private String getConsoleInput() {
        try {
            System.out.print("[SUB001] Enter function key (F1/F3/F7/F8): ");
            Scanner scanner = new Scanner(System.in);
            String input = scanner.nextLine();
            return input.trim();
        } catch (Exception e) {
            System.err.println("[SUB001] Error reading console input: " + e.getMessage());
            return "f3"; // Default to exit on error
        }
    }
    
    /**
     * Send completion message to WebSocket Hub - following MAIN001 sendCompletionMessage() pattern
     */
    private void sendCompletionMessage() {
        try {
            System.out.println("[SUB001] Sending completion message for employee inquiry");
            
            // Create completion message
            Map<String, Object> completionData = new HashMap<>();
            completionData.put("action", "completion_message");
            completionData.put("map_file", "SUB001_RESULT");
            completionData.put("terminal_id", currentTerminalId);
            completionData.put("program_name", "SUB001");
            
            // Add completion details
            Map<String, Object> resultFields = new HashMap<>();
            resultFields.put("SUBPROGRAM", "SUB001");
            resultFields.put("STATUS", "COMPLETED");
            resultFields.put("RECORDS_FOUND", String.valueOf(wsRecordCount));
            resultFields.put("RESULT", "Employee Information Display completed");
            resultFields.put("MESSAGE", "Employee inquiry completed successfully. " + wsRecordCount + " records found.");
            resultFields.put("TIMESTAMP", new java.util.Date().toString());
            completionData.put("fields", resultFields);
            
            // Send to WebSocket Hub
            boolean success = sendToWebSocketHub(completionData);
            if (success) {
                System.out.println("[SUB001] Completion message sent successfully");
            } else {
                System.out.println("[SUB001] Failed to send completion message");
            }
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error sending completion message: " + e.getMessage());
        }
    }
    
    /**
     * 재시도 메커니즘이 있는 안전한 입력 처리
     * CODING_RULES.md: 하드코딩 금지 - 설정 파일 기반
     */
    private String getInputWithRetry() {
        // CODING_RULES: 하드코딩 금지 - 설정 파일에서 로드
        int maxRetries = getConfigValue("input.max_retries", 3);
        int retryDelay = getConfigValue("input.retry_delay_ms", 2000);
        
        for (int retry = 0; retry < maxRetries; retry++) {
            try {
                // WebSocket 입력 시도
                String webInput = waitForWebSocketInputSafe();
                if (webInput != null && !webInput.trim().isEmpty()) {
                    return webInput;
                }
                
                // 재시도 간격
                if (retry < maxRetries - 1) {
                    System.out.println("[SUB001] Input retry " + (retry + 1) + "/" + maxRetries + 
                                     " in " + (retryDelay/1000) + " seconds");
                    Thread.sleep(retryDelay);
                }
                
            } catch (Exception e) {
                System.err.println("[SUB001] Input attempt " + (retry + 1) + " failed: " + e.getMessage());
            }
        }
        
        // 모든 재시도 실패 시 콘솔 입력으로 폴백
        System.out.println("[SUB001] WebSocket input failed, falling back to console");
        return getConsoleInputSafe();
    }
    
    /**
     * 안전한 WebSocket 입력 대기 (설정 기반 타임아웃)
     * CODING_RULES.md: 하드코딩 금지 - 환경 설정 사용
     */
    private String waitForWebSocketInputSafe() {
        try {
            String inputFile = "/tmp/asp_input_" + currentTerminalId + ".txt";
            File file = new File(inputFile);
            
            // 기존 입력 파일 정리
            if (file.exists()) {
                file.delete();
            }
            
            // CODING_RULES: 하드코딩 금지 - 설정에서 타임아웃 로드
            int maxWaitSeconds = getConfigValue("websocket.timeout", 86400);  // FIX: 올바른 설정 키 사용
            int checkInterval = getConfigValue("websocket.check_interval_ms", 500);
            
            System.out.println("[SUB001] Waiting for WebSocket input... (timeout: " + maxWaitSeconds + " seconds)");
            
            int waitedMs = 0;
            int maxWaitMs = maxWaitSeconds * 1000;
            
            while (waitedMs < maxWaitMs) {
                if (file.exists() && file.length() > 0) {
                    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                        String input = reader.readLine();
                        if (input != null && !input.trim().isEmpty()) {
                            System.out.println("[SUB001] WebSocket input received: " + input.trim());
                            file.delete();
                            return input.trim();
                        }
                    }
                }
                
                Thread.sleep(checkInterval);
                waitedMs += checkInterval;
            }
            
            System.out.println("[SUB001] WebSocket input timeout (safe)");
            return null;
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error in safe WebSocket input: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * 안전한 콘솔 입력 (에러 시 종료하지 않음)
     */
    private String getConsoleInputSafe() {
        try {
            System.out.print("[SUB001] Enter function key (F1/F3/F7/F8) [or ENTER to stay]: ");
            Scanner scanner = new Scanner(System.in);
            String input = scanner.nextLine();
            
            // 빈 입력 시 현재 상태 유지
            if (input == null || input.trim().isEmpty()) {
                return "stay";
            }
            
            return input.trim();
            
        } catch (Exception e) {
            System.err.println("[SUB001] Error reading console input: " + e.getMessage());
            return "stay"; // 에러 시 현재 상태 유지
        }
    }
    
    /**
     * 입력 예외 안전 처리
     */
    private void handleInputException(Exception e) {
        System.err.println("[SUB001] Input exception handled safely: " + e.getMessage());
        
        // 예외 유형별 처리
        if (e instanceof InterruptedException) {
            System.out.println("[SUB001] Input interrupted, continuing in current page");
            Thread.currentThread().interrupt();
        } else if (e instanceof java.io.IOException) {
            System.out.println("[SUB001] I/O error, retrying input");
        } else {
            System.out.println("[SUB001] Unexpected error, staying in current page");
        }
        
        // 프로그램 상태를 RUNNING으로 유지 (종료하지 않음)
        context.setState(ProgramState.RUNNING);
    }
    
    /**
     * 설정값 로드 (CODING_RULES.md: 하드코딩 금지)
     */
    private int getConfigValue(String key, int defaultValue) {
        try {
            // /home/aspuser/app/config/asp.conf에서 설정 로드
            Properties props = new Properties();
            File configFile = new File("/home/aspuser/app/config/asp.conf");
            
            if (configFile.exists()) {
                props.load(new FileInputStream(configFile));
                String value = props.getProperty(key);
                if (value != null) {
                    return Integer.parseInt(value.trim());
                }
            }
        } catch (Exception e) {
            System.err.println("[SUB001] Warning: Could not load config for " + key + ": " + e.getMessage());
        }
        
        return defaultValue;
    }
}