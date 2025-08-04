import java.util.*;
import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.*;
import java.lang.reflect.*;
import com.openasp.sub.SUB001;

/**
 * MAIN001 - Employee Management Menu System (Enhanced Version)
 * Converted from COBOL to Java with DSP file handling
 * 
 * Enhanced with session management and execution mode separation:
 * - WRITE DISPFILE sends display data to web UI via WebSocket Hub
 * - READ DISPFILE receives user input from web UI
 * - Uses SMED maps for screen layout
 * - Handles SJIS encoding for Korean text
 * - Implements proper COBOL program flow and subprogram calling
 * - Added WebSocket session management with input buffer clearing
 * - Separated ASP Command mode from WebUI Interactive mode
 * - Fixed F3 key auto-reentry issue in SUB001
 */
public class MAIN001 {
    
    // COBOL Data Division equivalents
    // File Section - DISPFILE record structure (from MITDSP copybook)
    private static class DispRecord {
        String filler1 = " ";                           // PIC X(1) VALUE SPACE
        String menuTitle = "Manager Menu";               // PIC X(20) VALUE Korean text
        String filler2 = " ";                           // PIC X(1) VALUE SPACE  
        String option1 = "1) Inquiry";                  // PIC X(20) VALUE Korean text
        String filler3 = " ";                           // PIC X(1) VALUE SPACE
        String option2 = "2) Add";                      // PIC X(20) VALUE Korean text
        String filler4 = " ";                           // PIC X(1) VALUE SPACE
        String option3 = "3) Update";                   // PIC X(20) VALUE Korean text
        String filler5 = " ";                           // PIC X(1) VALUE SPACE
        String option4 = "4) Delete";                   // PIC X(20) VALUE Korean text
        String filler6 = " ";                           // PIC X(1) VALUE SPACE
        String selectionPrompt = "Selection:";           // PIC X(20) VALUE Korean text
        String empFunc = "";                            // PIC X(1) - User input field
        
        // Convert to SMED map format for web UI
        Map<String, Object> toSmedMap() {
            Map<String, Object> fields = new HashMap<>();
            fields.put("MENU_TITLE", menuTitle);
            fields.put("OPTION_1", option1);
            fields.put("OPTION_2", option2);
            fields.put("OPTION_3", option3);
            fields.put("OPTION_4", option4);
            fields.put("SELECTION_PROMPT", selectionPrompt);
            fields.put("EMP_FUNC", empFunc);
            return fields;
        }
        
        // Update from user input
        void updateFromInput(String input) {
            if (input != null && input.length() > 0) {
                empFunc = input.substring(0, 1);
            }
        }
    }
    
    // Execution Environment Detection
    public enum ExecutionMode {
        ASP_COMMAND("ASP System Command 모드 - 단발성 실행"),
        WEBUI_INTERACTIVE("WebUI Interactive 모드 - 지속적 세션");
        
        private final String description;
        ExecutionMode(String description) { this.description = description; }
        public String getDescription() { return description; }
    }
    
    // WebSocket Session Manager for improved input handling
    private static class WebSocketSessionManager {
        private String sessionId;
        private String terminalId;
        private volatile boolean inputCleared = false;
        private final Object sessionLock = new Object();
        private final Queue<String> inputBuffer = new LinkedList<>();
        private long sessionStartTime;

        public void initializeSession(String terminalId) {
            synchronized(sessionLock) {
                this.sessionId = "session_" + System.currentTimeMillis() + "_" + Math.random();
                this.terminalId = terminalId;
                this.sessionStartTime = System.currentTimeMillis();
                this.inputCleared = false;
                this.inputBuffer.clear();
                
                System.out.println("[세션관리] 세션 초기화 완료");
                System.out.println("[세션관리] - 세션 ID: " + sessionId);
                System.out.println("[세션관리] - 터미널 ID: " + terminalId);
            }
        }

        public void clearInputBuffer() {
            synchronized(sessionLock) {
                int bufferedCount = inputBuffer.size();
                inputBuffer.clear();
                inputCleared = true;
                
                System.out.println("[세션관리] 입력 버퍼 클리어 완료");
                System.out.println("[세션관리] - 제거된 입력 수: " + bufferedCount);
            }
        }

        public void terminateSession() {
            synchronized(sessionLock) {
                long sessionDuration = System.currentTimeMillis() - sessionStartTime;
                clearInputBuffer();
                
                System.out.println("[세션관리] 세션 종료");
                System.out.println("[세션관리] - 세션 지속 시간: " + sessionDuration + "ms");
                System.out.println("[세션관리] - 세션 ID: " + sessionId);
            }
        }

        public String getSessionId() { return sessionId; }
        public String getTerminalId() { return terminalId; }
        public boolean isInputCleared() { 
            synchronized(sessionLock) { return inputCleared; }
        }
        
        public void resetInputCleared() {
            synchronized(sessionLock) { inputCleared = false; }
        }
    }

    // Working Storage Section
    private String wsEndFlag = "N";                     // PIC X(1) VALUE "N"
    private String gsDispFile = "MAIN001";              // PIC X(8) VALUE "MAIN001"
    private DispRecord dispRecord = new DispRecord();   // Main record structure
    private String currentTerminalId = "webui";         // Terminal for WebSocket communication
    private static String[] programArgs;
    private WebSocketSessionManager sessionManager = new WebSocketSessionManager(); // Session management
    private ExecutionMode currentExecutionMode = ExecutionMode.WEBUI_INTERACTIVE;   // Current execution mode
    
    /**
     * Main method - Entry point implementing COBOL PROCEDURE DIVISION with execution mode detection
     * COBOL: PROCEDURE DIVISION.
     * COBOL: BEGIN.
     * 
     * Enhanced with execution mode separation:
     * - ASP Command mode: Direct execution with immediate exit
     * - WebUI Interactive mode: Persistent session with main loop
     */
    public static void main(String[] args) {
        System.out.println("*** MAIN001 - Employee Management Menu (Enhanced with Session Management) ***");
        System.out.println("DSP File Handling with WebSocket Hub Integration");
        System.out.println("SJIS encoding support for Korean text");
        System.out.println("장기 개선 방안 적용: 모드 분리, 세션 관리, COBOL 시맨틱스\n");
        
        try {
            programArgs = args;
            MAIN001 program = new MAIN001();
            
            // Detect execution environment and set mode
            program.detectExecutionMode(args);
            
            // Get terminal ID from environment variable, command line, or default
            String envTerminalId = System.getenv("ASP_TERMINAL_ID");
            if (envTerminalId != null && !envTerminalId.isEmpty()) {
                program.currentTerminalId = envTerminalId;
                System.out.println("Terminal ID: " + envTerminalId + " (from environment)");
            } else if (args.length > 1) {
                program.currentTerminalId = args[1];
                System.out.println("Terminal ID: " + args[1] + " (from arguments)");
            } else {
                program.currentTerminalId = "webui"; // Default to webui for web terminal
                System.out.println("Terminal ID: webui (default)");
            }
            
            // Initialize session management
            program.sessionManager.initializeSession(program.currentTerminalId);
            
            // Execute based on detected mode
            if (program.currentExecutionMode == ExecutionMode.ASP_COMMAND) {
                System.out.println("[MAIN001] 실행 모드: " + program.currentExecutionMode.getDescription());
                String directOption = args[0].trim();
                System.out.println("[MAIN001] Direct execution parameter: " + directOption);
                
                // ASP Command mode: Execute subprogram directly and exit immediately
                program.executeDirectOptionCommand(directOption);
            } else {
                System.out.println("[MAIN001] 실행 모드: " + program.currentExecutionMode.getDescription());
                
                // WebUI Interactive mode: Normal menu mode with persistent session
                program.begin();
            }
            
        } catch (Exception e) {
            System.err.println("ERROR: MAIN001 execution failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(-1);
        }
    }
    
    /**
     * Detect execution mode based on command line arguments
     */
    private void detectExecutionMode(String[] args) {
        System.out.println("[환경감지] 실행 환경 분석 중...");
        System.out.println("[환경감지] 전달된 인수: " + Arrays.toString(args));
        
        if (isAspSystemCommand(args)) {
            System.out.println("[환경감지] → ASP System Command 모드로 판단");
            currentExecutionMode = ExecutionMode.ASP_COMMAND;
        } else {
            System.out.println("[환경감지] → WebUI Interactive 모드로 판단");
            currentExecutionMode = ExecutionMode.WEBUI_INTERACTIVE;
        }
    }
    
    private boolean isAspSystemCommand(String[] args) {
        // ASP Command 조건: 직접 옵션 파라미터 존재
        if (args.length > 0 && args[0] != null && !args[0].trim().isEmpty()) {
            String option = args[0].trim();
            boolean isValidOption = option.matches("[1-4]");
            System.out.println("[환경감지] 옵션 파라미터 '" + option + "' 유효성: " + isValidOption);
            return isValidOption;
        }
        System.out.println("[환경감지] 옵션 파라미터 없음");
        return false;
    }

    /**
     * Execute subprogram directly in ASP Command mode (FIXED - No main loop return)
     */
    private void executeDirectOptionCommand(String option) {
        try {
            System.out.println("[MAIN001Command] ASP Command 모드 직접 실행 - 옵션: " + option);
            
            // COBOL: OPEN I-O DISPFILE (minimal for command mode)
            openDispFile();
            
            // Set the option and execute directly
            dispRecord.empFunc = option;
            System.out.println("[MAIN001Command] Direct execution - EMP-FUNC set to: '" + option + "'");
            
            // COBOL: EVALUATE EMP-FUNC (direct execution)
            evaluateEmpFunc();
            
            System.out.println("[MAIN001Command] 서브프로그램 실행 완료");
            
            // COBOL: CLOSE DISPFILE
            closeDispFile();
            
            // CRITICAL FIX: Command mode exits immediately without returning to main menu
            System.out.println("[MAIN001Command] Command 모드 완료 - 즉시 종료 (STOP RUN)");
            sessionManager.terminateSession();
            System.exit(0);
            
        } catch (Exception e) {
            System.err.println("[MAIN001Command] Error in ASP Command mode: " + e.getMessage());
            e.printStackTrace();
            sessionManager.terminateSession();
            System.exit(-1);
        }
    }

    /**
     * Execute subprogram directly without menu display (DEPRECATED - kept for compatibility)
     */
    private void executeDirectOption(String option) {
        // This method is deprecated in favor of mode-specific execution
        System.out.println("[MAIN001] DEPRECATED: executeDirectOption called, redirecting to mode-specific method");
        executeDirectOptionCommand(option);
    }

    /**
     * COBOL BEGIN paragraph implementation with session management
     * COBOL: BEGIN.
     * COBOL:     OPEN I-O DISPFILE
     * COBOL:     PERFORM MAIN-LOOP
     * COBOL:     CLOSE DISPFILE
     * COBOL:     STOP RUN.
     */
    private void begin() {
        try {
            System.out.println("[MAIN001Interactive] WebUI Interactive 모드 - 지속적 세션 관리");
            
            // COBOL: OPEN I-O DISPFILE
            openDispFile();
            
            // COBOL: PERFORM MAIN-LOOP (with session management)
            performMainLoop();
            
            // COBOL: CLOSE DISPFILE
            closeDispFile();
            
            // Session cleanup
            sessionManager.terminateSession();
            
            // COBOL: STOP RUN
            System.out.println("[MAIN001Interactive] Interactive 모드 정상 완료 - STOP RUN");
            System.exit(0);
            
        } catch (Exception e) {
            System.err.println("[MAIN001Interactive] Error in Interactive mode: " + e.getMessage());
            e.printStackTrace();
            sessionManager.terminateSession();
            System.exit(-1);
        }
    }
    
    /**
     * COBOL DSP File Operations - OPEN I-O DISPFILE
     * Initializes WebSocket Hub connection for DSP communication
     */
    private void openDispFile() {
        System.out.println("[MAIN001] OPEN I-O DISPFILE - Initializing DSP connection");
        System.out.println("[MAIN001] SYMBOLIC DESTINATION 'DSP' - WebSocket Hub communication active");
        System.out.println("[MAIN001] SELECTED FUNCTION IS EMP-FUNC - Ready for user input");
        // DSP file is now "open" for I-O operations
    }
    
    /**
     * COBOL DSP File Operations - CLOSE DISPFILE
     * Closes WebSocket Hub connection
     */
    private void closeDispFile() {
        System.out.println("[MAIN001] CLOSE DISPFILE - DSP connection closed");
    }
    
    /**
     * COBOL MAIN-LOOP paragraph implementation
     * COBOL: MAIN-LOOP.
     * COBOL:     WRITE DISPFILE FROM DISP-RECORD
     * COBOL:     READ  DISPFILE INTO DISP-RECORD
     * COBOL:     EVALUATE EMP-FUNC
     * COBOL:         WHEN "1" CALL "SUB001"
     * COBOL:         WHEN "2" CALL "SUB002"
     * COBOL:         WHEN "3" CALL "SUB003"  
     * COBOL:         WHEN "4" CALL "SUB004"
     * COBOL:         WHEN OTHER MOVE "Y" TO WS-END-FLAG
     * COBOL:     END-EVALUATE
     * COBOL:     IF WS-END-FLAG = "N" GO TO MAIN-LOOP.
     */
    private void performMainLoop() {
        System.out.println("[MAIN001] MAIN-LOOP - Starting main processing loop");
        
        int loopCounter = 0;
        int consecutiveErrors = 0;
        final int MAX_LOOP_ITERATIONS = 100; // Prevent infinite loop
        final int MAX_CONSECUTIVE_ERRORS = 3; // Allow some errors before giving up
        
        while (wsEndFlag.equals("N")) {
            loopCounter++;
            System.out.println("[MAIN001] MAIN-LOOP iteration: " + loopCounter);
            
            // Safety check to prevent infinite loop
            if (loopCounter > MAX_LOOP_ITERATIONS) {
                System.err.println("[MAIN001] WARNING: Maximum loop iterations exceeded (" + MAX_LOOP_ITERATIONS + ")");
                System.err.println("[MAIN001] Forcing program termination to prevent infinite loop");
                wsEndFlag = "Y";
                break;
            }
            
            try {
                // COBOL: WRITE DISPFILE FROM DISP-RECORD
                writeDispFile();
                
                // COBOL: READ DISPFILE INTO DISP-RECORD  
                System.out.println("[MAIN001] About to read DISPFILE - current empFunc: '" + dispRecord.empFunc + "'");
                readDispFile();
                System.out.println("[MAIN001] DISPFILE read completed - new empFunc: '" + dispRecord.empFunc + "'");
                
                // COBOL: EVALUATE EMP-FUNC
                evaluateEmpFunc();
                
                // Reset error counter on successful iteration
                consecutiveErrors = 0;
                
                // Debug: Show current state after each iteration
                System.out.println("[MAIN001] Loop state - wsEndFlag: '" + wsEndFlag + "', empFunc: '" + dispRecord.empFunc + "'");
                
            } catch (Exception e) {
                consecutiveErrors++;
                System.err.println("[MAIN001] Error in main loop iteration " + loopCounter + ": " + e.getMessage());
                System.err.println("[MAIN001] Consecutive errors: " + consecutiveErrors + "/" + MAX_CONSECUTIVE_ERRORS);
                
                if (consecutiveErrors >= MAX_CONSECUTIVE_ERRORS) {
                    System.err.println("[MAIN001] Too many consecutive errors, setting end flag");
                    wsEndFlag = "Y";
                    break;
                } else {
                    System.err.println("[MAIN001] Attempting to recover and continue...");
                    // Reset input field and try to continue
                    dispRecord.empFunc = "";
                }
            }
        }
        
        System.out.println("[MAIN001] MAIN-LOOP - Loop terminated, WS-END-FLAG = Y, iterations: " + loopCounter);
    }
    
    /**
     * COBOL DSP File Operations - WRITE DISPFILE FROM DISP-RECORD
     * Sends display data to web UI via WebSocket Hub using SMED map format
     */
    private void writeDispFile() {
        System.out.println("[MAIN001] WRITE DISPFILE FROM DISP-RECORD - Sending display to webUI");
        
        try {
            // Convert COBOL record to SMED map format
            Map<String, Object> smedData = createSmedDisplayData();
            
            // Send to WebSocket Hub via Python client
            boolean success = sendToWebSocketHub(smedData);
            
            if (success) {
                System.out.println("[MAIN001] Display data sent to WebSocket Hub successfully");
                System.out.println("[MAIN001] Menu displayed on terminal: " + currentTerminalId);
            } else {
                System.err.println("[MAIN001] WARNING: Failed to send display data to WebSocket Hub");
                // Continue execution - fallback to console display
                displayMenuFallback();
            }
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error in WRITE DISPFILE: " + e.getMessage());
            displayMenuFallback();
        }
    }
    
    /**
     * COBOL DSP File Operations - READ DISPFILE INTO DISP-RECORD
     * Receives user input from web UI via WebSocket Hub or command line
     * Enhanced with session management and input buffer clearing
     */
    private void readDispFile() {
        System.out.println("[MAIN001] READ DISPFILE INTO DISP-RECORD - Waiting for user input");
        
        try {
            String userInput = null;
            
            // CRITICAL FIX: Clear previous input buffer to prevent auto-reentry
            if (currentExecutionMode == ExecutionMode.WEBUI_INTERACTIVE) {
                sessionManager.clearInputBuffer();
                System.out.println("[MAIN001] Interactive 모드: 이전 입력 버퍼 클리어 완료");
            }
            
            // Check if input provided via command line arguments (for ASP system calls)
            if (programArgs != null && programArgs.length > 0) {
                userInput = programArgs[0];
                System.out.println("[MAIN001] Input from command line: " + userInput);
                // Clear programArgs after first use to prevent infinite loop
                programArgs = null;
            } else {
                // Check for cleared input buffer first
                if (sessionManager.isInputCleared()) {
                    System.out.println("[MAIN001] 입력 버퍼가 클리어된 상태, 새로운 입력 대기");
                    sessionManager.resetInputCleared();
                }
                
                // Wait for WebSocket Hub input or use fallback
                userInput = waitForWebSocketInput();
                if (userInput == null) {
                    System.out.println("[MAIN001] No WebSocket input, using console fallback");
                    userInput = getConsoleInput();
                }
            }
            
            // Update DISP-RECORD with user input
            if (userInput != null && !userInput.trim().isEmpty()) {
                dispRecord.updateFromInput(userInput.trim());
                System.out.println("[MAIN001] EMP-FUNC set to: '" + dispRecord.empFunc + "'");
            } else {
                // FIX: Don't exit immediately when no input received after subprogram call
                // Instead, reset input field and continue with menu display
                System.out.println("[MAIN001] No input received, resetting input field and continuing");
                dispRecord.empFunc = "";  // Reset to empty to redisplay menu
                // Do NOT set wsEndFlag = "Y" here - let the menu continue
            }
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error in READ DISPFILE: " + e.getMessage());
            System.err.println("[MAIN001] Attempting to recover and continue with menu");
            // FIX: Try to recover instead of immediately exiting
            dispRecord.empFunc = "";  // Reset input field
            // Only exit after multiple consecutive errors
            // This will be handled by the main loop's iteration counter
        }
    }
    
    /**
     * COBOL EVALUATE EMP-FUNC implementation
     * Modified to return to main menu after subprogram execution
     */
    private void evaluateEmpFunc() {
        System.out.println("[MAIN001] EVALUATE EMP-FUNC - Processing user selection: '" + dispRecord.empFunc + "'");
        
        switch (dispRecord.empFunc) {
            case "1":
                System.out.println("[MAIN001] WHEN '1' - Calling SUB001");
                callSubprogram("SUB001");
                // Return to main menu after SUB001 execution (COBOL CALL behavior)
                System.out.println("[MAIN001] SUB001 completed, returning to main menu");
                // Reset input field for next selection
                dispRecord.empFunc = "";
                break;
                
            case "2": 
                System.out.println("[MAIN001] WHEN '2' - Calling SUB002");
                callSubprogram("SUB002");
                // Return to main menu after SUB002 execution
                System.out.println("[MAIN001] SUB002 completed, returning to main menu");
                dispRecord.empFunc = "";
                break;
                
            case "3":
                System.out.println("[MAIN001] WHEN '3' - Calling SUB003");
                callSubprogram("SUB003");
                // Return to main menu after SUB003 execution
                System.out.println("[MAIN001] SUB003 completed, returning to main menu");
                dispRecord.empFunc = "";
                break;
                
            case "4":
                System.out.println("[MAIN001] WHEN '4' - Calling SUB004");
                callSubprogram("SUB004");
                // Return to main menu after SUB004 execution
                System.out.println("[MAIN001] SUB004 completed, returning to main menu");
                dispRecord.empFunc = "";
                break;
                
            case "0":
            case "":
                System.out.println("[MAIN001] WHEN '0' or EMPTY - Exit requested");
                // COBOL: MOVE "Y" TO WS-END-FLAG
                wsEndFlag = "Y";
                break;
                
            default:
                System.out.println("[MAIN001] WHEN OTHER - Invalid selection: '" + dispRecord.empFunc + "'");
                System.out.println("[MAIN001] Valid options: 1-4 (menu options), 0 (exit)");
                // Reset invalid input and continue
                dispRecord.empFunc = "";
                break;
        }
    }
    
    /**
     * Create SMED display data for WebSocket Hub transmission
     * Converts COBOL DISP-RECORD to SMED map format
     */
    private Map<String, Object> createSmedDisplayData() {
        Map<String, Object> smedData = new HashMap<>();
        
        // Main SMED structure
        smedData.put("action", "display_map");
        smedData.put("map_file", gsDispFile);  // "MAIN001"
        smedData.put("terminal_id", currentTerminalId);
        smedData.put("program_name", "MAIN001");
        smedData.put("encoding", "SJIS");
        
        // Field data from DISP-RECORD (MITDSP copybook structure)
        Map<String, Object> fields = dispRecord.toSmedMap();
        smedData.put("fields", fields);
        
        // Screen layout information
        smedData.put("rows", 24);
        smedData.put("cols", 80);
        smedData.put("background_color", "BLACK");
        smedData.put("foreground_color", "GREEN");
        
        // Add position-based layout (traditional COBOL screen format)
        smedData.put("layout", createPositionBasedLayout());
        
        return smedData;
    }
    
    /**
     * Create position-based layout for traditional COBOL screen display
     */
    private Map<String, Object> createPositionBasedLayout() {
        Map<String, Object> layout = new HashMap<>();
        
        // Define field positions (row, col) following COBOL screen conventions
        layout.put("title_position", Map.of("row", 2, "col", 30));
        layout.put("option1_position", Map.of("row", 5, "col", 20));
        layout.put("option2_position", Map.of("row", 7, "col", 20));
        layout.put("option3_position", Map.of("row", 9, "col", 20));
        layout.put("option4_position", Map.of("row", 11, "col", 20));
        layout.put("prompt_position", Map.of("row", 15, "col", 20));
        layout.put("input_position", Map.of("row", 15, "col", 30));
        
        return layout;
    }
    
    /**
     * Send SMED data to WebSocket Hub via Python client
     */
    private boolean sendToWebSocketHub(Map<String, Object> smedData) {
        try {
            // Create temporary JSON file for WebSocket Hub client
            String tempFile = "/tmp/main001_smed_data.json";
            String jsonData = convertToJson(smedData);
            
            // Write JSON data to temporary file
            Files.write(Paths.get(tempFile), jsonData.getBytes(Charset.forName("UTF-8")));
            
            // Call WebSocket Hub client via Python
            ProcessBuilder pb = new ProcessBuilder(
                "python3",
                "/home/aspuser/app/server/websocket_hub_client.py",
                "--map", "MAIN001",
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
            System.err.println("[MAIN001] Error sending to WebSocket Hub: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Simple JSON converter for SMED data with proper escaping
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
     * Convert Map to JSON string with proper formatting
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
     * Escape special characters in JSON strings
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
     * Fallback menu display for console when WebSocket Hub unavailable
     */
    private void displayMenuFallback() {
        System.out.println("\n=== " + dispRecord.menuTitle + " ===");
        System.out.println();
        System.out.println(dispRecord.option1);
        System.out.println(dispRecord.option2);
        System.out.println(dispRecord.option3);
        System.out.println(dispRecord.option4);
        System.out.println();
        System.out.print(dispRecord.selectionPrompt + " ");
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
                System.out.println("[MAIN001] WebSocket timeout loaded from config: " + timeout + " seconds");
                return timeout;
            }
        } catch (Exception e) {
            System.err.println("[MAIN001] Warning: Could not load timeout from config: " + e.getMessage());
        }
        
        System.out.println("[MAIN001] Using default WebSocket timeout: " + defaultTimeout + " seconds");
        return defaultTimeout;
    }
    
    /**
     * Wait for user input from WebSocket Hub
     * Checks for input file created by WebSocket Hub client
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
            System.out.println("[MAIN001] Waiting for WebSocket Hub input... (timeout: " + maxWaitSeconds + " seconds)");
            System.out.println("[MAIN001] Input file: " + inputFile);
            
            int waitedSeconds = 0;
            
            while (waitedSeconds < maxWaitSeconds) {
                if (file.exists() && file.length() > 0) {
                    try (BufferedReader reader = new BufferedReader(new FileReader(file))) {
                        String input = reader.readLine();
                        if (input != null && !input.trim().isEmpty()) {
                            System.out.println("[MAIN001] WebSocket input received: " + input.trim());
                            file.delete(); // Clean up
                            return input.trim();
                        }
                    }
                }
                
                Thread.sleep(1000);
                waitedSeconds++;
                
                if (waitedSeconds % 5 == 0) {
                    System.out.println("[MAIN001] Still waiting... (" + waitedSeconds + "/" + maxWaitSeconds + " seconds)");
                }
            }
            
            System.out.println("[MAIN001] Timeout waiting for WebSocket input");
            return null;
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error waiting for WebSocket input: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * Get console input as fallback when WebSocket unavailable
     */
    private String getConsoleInput() {
        try {
            System.out.print(dispRecord.selectionPrompt + " ");
            Scanner scanner = new Scanner(System.in);
            String input = scanner.nextLine();
            return input.trim();
        } catch (Exception e) {
            System.err.println("[MAIN001] Error reading console input: " + e.getMessage());
            return null;
        }
    }
    
    /**
     * COBOL CALL statement implementation
     * COBOL: CALL "SUB001", CALL "SUB002", etc.
     */
    private void callSubprogram(String programName) {
        System.out.println("[MAIN001] CALL \"" + programName + "\" - Subprogram execution");
        
        try {
            switch (programName) {
                case "SUB001":
                    callSub001();
                    break;
                    
                case "SUB002":
                    callSub002();
                    break;
                    
                case "SUB003":
                    callSub003();
                    break;
                    
                case "SUB004":
                    callSub004();
                    break;
                    
                default:
                    System.err.println("[MAIN001] WARNING: Unknown subprogram: " + programName);
                    System.err.println("[MAIN001] Program not implemented, returning to main menu");
            }
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error calling subprogram " + programName + ": " + e.getMessage());
            e.printStackTrace();
        }
        
        System.out.println("[MAIN001] Subprogram " + programName + " completed, returning to main menu");
    }
    
    /**
     * Call SUB001 - Employee Information Display using ClassLoader
     * Implements proper COBOL CALL semantics - subprogram executes and returns control
     */
    private void callSub001() {
        System.out.println("[MAIN001] CALL \"SUB001\" - Employee Information Display using ClassLoader");
        
        try {
            // Debug: Print current classpath
            System.out.println("[MAIN001] Current classpath: " + System.getProperty("java.class.path"));
            System.out.println("[MAIN001] Current working directory: " + System.getProperty("user.dir"));
            
            // Debug: Check if SUB001 class file exists
            String classFilePath = System.getProperty("user.dir") + "/com/openasp/sub/SUB001.class";
            File classFile = new File(classFilePath);
            System.out.println("[MAIN001] SUB001 class file exists at " + classFilePath + ": " + classFile.exists());
            
            // Try multiple approaches to load the class
            Class<?> sub001Class = null;
            
            // Approach 1: Use system class loader with full package name
            try {
                sub001Class = Class.forName("com.openasp.sub.SUB001");
                System.out.println("[MAIN001] SUB001 loaded using Class.forName()");
            } catch (ClassNotFoundException e1) {
                System.out.println("[MAIN001] Class.forName() failed, trying URLClassLoader...");
                
                // Approach 2: Create URLClassLoader with explicit path
                try {
                    File currentDir = new File(System.getProperty("user.dir"));
                    java.net.URL[] urls = new java.net.URL[] { currentDir.toURI().toURL() };
                    java.net.URLClassLoader urlClassLoader = new java.net.URLClassLoader(urls);
                    sub001Class = urlClassLoader.loadClass("com.openasp.sub.SUB001");
                    System.out.println("[MAIN001] SUB001 loaded using URLClassLoader");
                } catch (Exception e2) {
                    System.out.println("[MAIN001] URLClassLoader failed: " + e2.getMessage());
                    
                    // Approach 3: Try direct instantiation if in same JVM
                    try {
                        // Direct instantiation - this will work if classes are in same classpath
                        com.openasp.sub.SUB001 directSub001 = new com.openasp.sub.SUB001();
                        directSub001.execute();
                        System.out.println("[MAIN001] SUB001 executed using direct instantiation");
                        System.out.println("[MAIN001] SUB001 call completed, returning to main menu");
                        return;
                    } catch (Exception e3) {
                        System.out.println("[MAIN001] Direct instantiation failed: " + e3.getMessage());
                        throw new ClassNotFoundException("All loading methods failed");
                    }
                }
            }
            
            if (sub001Class != null) {
                System.out.println("[MAIN001] SUB001 class loaded successfully from: " + sub001Class.getProtectionDomain().getCodeSource().getLocation());
                
                // Create instance using reflection
                Object sub001Instance = sub001Class.getDeclaredConstructor().newInstance();
                System.out.println("[MAIN001] SUB001 instance created");
                
                // Call execute() method - this implements COBOL CALL semantics
                java.lang.reflect.Method executeMethod = sub001Class.getMethod("execute");
                System.out.println("[MAIN001] Calling SUB001.execute() method...");
                
                // Execute subprogram - this will block until SUB001 completes
                executeMethod.invoke(sub001Instance);
                
                System.out.println("[MAIN001] SUB001.execute() completed successfully");
                System.out.println("[MAIN001] Control returned to MAIN001 from SUB001");
            }
            
        } catch (ClassNotFoundException e) {
            System.err.println("[MAIN001] ERROR: SUB001 class not found in classpath");
            System.err.println("[MAIN001] Expected location: com.openasp.sub.SUB001");
            System.err.println("[MAIN001] Classpath: " + System.getProperty("java.class.path"));
            System.err.println("[MAIN001] Falling back to simulation");
            simulateEmployeeInquiry();
            
        } catch (NoSuchMethodException e) {
            System.err.println("[MAIN001] ERROR: SUB001.execute() method not found");
            System.err.println("[MAIN001] SUB001 class must implement public void execute() method");
            simulateEmployeeInquiry();
            
        } catch (Exception e) {
            System.err.println("[MAIN001] ERROR: Exception during SUB001 execution: " + e.getMessage());
            e.printStackTrace();
            System.err.println("[MAIN001] Falling back to simulation");
            simulateEmployeeInquiry();
        }
        
        System.out.println("[MAIN001] SUB001 call completed, returning to main menu");
        
        // CRITICAL FIX: Clean up input state after SUB001 returns
        // Clear any residual input files that might interfere with menu input
        try {
            String inputFile = "/tmp/asp_input_" + currentTerminalId + ".txt";
            File file = new File(inputFile);
            if (file.exists()) {
                file.delete();
                System.out.println("[MAIN001] Cleaned up input file after SUB001 return: " + inputFile);
            }
            
            // Clear session input buffer to ensure fresh start
            if (currentExecutionMode == ExecutionMode.WEBUI_INTERACTIVE) {
                sessionManager.clearInputBuffer();
                System.out.println("[MAIN001] Cleared session input buffer after SUB001 return");
            }
            
            // Reset input field to ensure clean state
            dispRecord.empFunc = "";
            System.out.println("[MAIN001] Reset input field after SUB001 return");
            
        } catch (Exception cleanupError) {
            System.err.println("[MAIN001] Warning: Input cleanup failed: " + cleanupError.getMessage());
        }
    }
    
    /**
     * Call SUB002 - Employee Addition (placeholder)
     */
    private void callSub002() {
        System.out.println("[MAIN001] SUB002 - Employee Addition");
        System.out.println("[MAIN001] Employee addition function executing");
        System.out.println("[MAIN001] (Function not yet implemented)");
    }
    
    /**
     * Call SUB003 - Employee Update (placeholder)
     */
    private void callSub003() {
        System.out.println("[MAIN001] SUB003 - Employee Update");
        System.out.println("[MAIN001] Employee update function executing");
        System.out.println("[MAIN001] (Function not yet implemented)");
    }
    
    /**
     * Call SUB004 - Employee Deletion (placeholder)
     */
    private void callSub004() {
        System.out.println("[MAIN001] SUB004 - Employee Deletion");
        System.out.println("[MAIN001] Employee deletion function executing");
        System.out.println("[MAIN001] (Function not yet implemented)");
    }
    
    /**
     * Simulate employee inquiry when SUB001 not available
     */
    private void simulateEmployeeInquiry() {
        System.out.println("[MAIN001] === Employee Information Inquiry ===");
        System.out.println("[MAIN001] EmpID    Name             Email");
        System.out.println("[MAIN001] -------- ---------------- --------------------");
        System.out.println("[MAIN001] EMP001   Kim Chul Su      kim@company.com");
        System.out.println("[MAIN001] EMP002   Lee Young Hee    lee@company.com");
        System.out.println("[MAIN001] EMP003   Park Min Su      park@company.com");
        System.out.println("[MAIN001] ");
        System.out.println("[MAIN001] Total 3 employee records found.");
    }
    
    /**
     * Send completion message to WebSocket Hub after subprogram execution
     */
    private void sendCompletionMessage(String option) {
        try {
            System.out.println("[MAIN001] Sending completion message for option: " + option);
            
            // Create completion message
            Map<String, Object> completionData = new HashMap<>();
            completionData.put("action", "completion_message");
            completionData.put("map_file", "MAIN001_RESULT");
            completionData.put("terminal_id", currentTerminalId);
            completionData.put("program_name", "MAIN001");
            
            // Add completion details
            Map<String, Object> resultFields = new HashMap<>();
            resultFields.put("OPTION", option);
            resultFields.put("STATUS", "COMPLETED");
            
            String message = "";
            switch (option) {
                case "1":
                    message = "Employee inquiry completed successfully. 3 records found.";
                    resultFields.put("SUBPROGRAM", "SUB001");
                    resultFields.put("RESULT", "Employee Information Display completed");
                    break;
                case "2":
                    message = "Employee addition function executed.";
                    resultFields.put("SUBPROGRAM", "SUB002");
                    resultFields.put("RESULT", "Employee Addition completed");
                    break;
                case "3":
                    message = "Employee update function executed.";
                    resultFields.put("SUBPROGRAM", "SUB003");
                    resultFields.put("RESULT", "Employee Update completed");
                    break;
                case "4":
                    message = "Employee deletion function executed.";
                    resultFields.put("SUBPROGRAM", "SUB004");
                    resultFields.put("RESULT", "Employee Deletion completed");
                    break;
                default:
                    message = "Unknown option processed.";
                    resultFields.put("SUBPROGRAM", "UNKNOWN");
                    resultFields.put("RESULT", "Unknown operation");
            }
            
            resultFields.put("MESSAGE", message);
            resultFields.put("TIMESTAMP", new java.util.Date().toString());
            completionData.put("fields", resultFields);
            
            // Send to WebSocket Hub
            boolean success = sendToWebSocketHub(completionData);
            if (success) {
                System.out.println("[MAIN001] Completion message sent successfully");
            } else {
                System.out.println("[MAIN001] Failed to send completion message");
            }
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error sending completion message: " + e.getMessage());
        }
    }
    
    /**
     * Send error message to WebSocket Hub when execution fails
     */
    private void sendErrorMessage(String option, String errorMsg) {
        try {
            System.out.println("[MAIN001] Sending error message for option: " + option);
            
            // Create error message
            Map<String, Object> errorData = new HashMap<>();
            errorData.put("action", "error_message");
            errorData.put("map_file", "MAIN001_ERROR");
            errorData.put("terminal_id", currentTerminalId);
            errorData.put("program_name", "MAIN001");
            
            // Add error details
            Map<String, Object> errorFields = new HashMap<>();
            errorFields.put("OPTION", option);
            errorFields.put("STATUS", "ERROR");
            errorFields.put("ERROR_MESSAGE", errorMsg);
            errorFields.put("TIMESTAMP", new java.util.Date().toString());
            errorData.put("fields", errorFields);
            
            // Send to WebSocket Hub
            sendToWebSocketHub(errorData);
            
        } catch (Exception e) {
            System.err.println("[MAIN001] Error sending error message: " + e.getMessage());
        }
    }
}
