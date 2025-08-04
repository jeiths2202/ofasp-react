package com.openasp.main;

import com.openasp.cobolg.file.CobolFileManager;
import com.openasp.cobolg.file.FileControlDefinition;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import java.io.IOException;

/**
 * MAIN001 - Main Menu Program (Converted from COBOL using COBOL G infrastructure)
 * 
 * Original COBOL program converted to Java with proper DSP file handling
 * 
 * COBOL FILE-CONTROL:
 * SELECT DISPFILE ASSIGN TO GS-DISPFILE
 *     SYMBOLIC DESTINATION IS "DSP"
 *     SELECTED FUNCTION IS EMP-FUNC.
 * 
 * Original COBOL flow:
 * 1. OPEN I-O DISPFILE
 * 2. WRITE DISPFILE FROM DISP-RECORD
 * 3. READ DISPFILE INTO DISP-RECORD  
 * 4. EVALUATE EMP-FUNC (call SUB001-SUB004)
 * 5. CLOSE DISPFILE
 */
@Component
public class MAIN001 {
    
    private static final Logger logger = LoggerFactory.getLogger(MAIN001.class);
    
    // Working Storage Section equivalent
    private String wsEndFlag = "N";
    private String gsDispfile = "MAIN001";
    private String terminalId = "TERM001";
    
    // File Control Definition (COBOL SELECT statement)
    private FileControlDefinition fileControl;
    private CobolFileManager fileManager;
    private DspFile<MitdspRecord> dispFile;
    private WebUIService webUIService;
    
    /**
     * Constructor - Initialize file control definition
     */
    public MAIN001() {
        initializeFileControl();
        this.fileManager = new CobolFileManager();
        this.webUIService = new MockWebUIService(); // Use mock for testing
    }
    
    /**
     * Initialize FILE-CONTROL definition equivalent to COBOL SELECT statement
     */
    private void initializeFileControl() {
        this.fileControl = FileControlDefinition.builder()
            .fileName("DISPFILE")
            .assignTo(gsDispfile)
            .symbolicDestination(FileControlDefinition.SymbolicDestination.DSP)
            .organization(FileControlDefinition.OrganizationType.SEQUENTIAL)
            .accessMode(FileControlDefinition.AccessMode.SEQUENTIAL)
            .build();
            
        ValidationResult validation = fileControl.validate();
        if (!validation.isValid()) {
            throw new RuntimeException("Invalid file control definition: " + validation.getAllErrors());
        }
    }
    
    /**
     * Main program execution - equivalent to COBOL PROCEDURE DIVISION
     */
    public void execute() {
        logger.info("Starting MAIN001 execution");
        
        try {
            // COBOL: OPEN I-O DISPFILE
            openDispFile();
            
            // COBOL: PERFORM MAIN-LOOP
            mainLoop();
            
        } catch (Exception e) {
            logger.error("Error in MAIN001 execution", e);
            throw new RuntimeException("MAIN001 execution failed", e);
        } finally {
            // COBOL: CLOSE DISPFILE
            closeDispFile();
        }
        
        logger.info("MAIN001 execution completed");
    }
    
    /**
     * COBOL: OPEN I-O DISPFILE
     * Initialize DSP connection for web UI interaction
     */
    private void openDispFile() throws IOException {
        logger.debug("Opening DISPFILE with DSP destination");
        
        // Create DSP file instance
        dispFile = new DspFile<>(
            fileControl.getFileName(),
            MitdspRecord.class,
            CobolFileManager.FileMode.I_O,
            fileControl
        );
        
        // Configure web UI service
        dispFile.configureWebUI(webUIService, terminalId);
        
        logger.debug("DISPFILE opened successfully");
    }
    
    /**
     * COBOL: CLOSE DISPFILE
     * Clean up DSP connection
     */
    private void closeDispFile() {
        if (dispFile != null) {
            try {
                dispFile.close();
                logger.debug("DISPFILE closed successfully");
            } catch (IOException e) {
                logger.warn("Error closing DISPFILE", e);
            }
        }
    }
    
    /**
     * COBOL: MAIN-LOOP
     * Main processing loop with DSP file operations
     */
    private void mainLoop() throws IOException {
        logger.debug("Entering main loop");
        
        // Create display record with default values
        MitdspRecord dispRecord = MitdspRecord.createDefaultRecord();
        
        // COBOL: WRITE DISPFILE FROM DISP-RECORD
        writeDispFile(dispRecord);
        
        // COBOL: READ DISPFILE INTO DISP-RECORD
        dispRecord = readDispFile();
        
        // COBOL: EVALUATE EMP-FUNC
        evaluateEmpFunc(dispRecord.getEmpFunc());
        
        logger.debug("Main loop completed");
    }
    
    /**
     * COBOL: WRITE DISPFILE FROM DISP-RECORD
     * Send display data to web UI
     */
    private void writeDispFile(MitdspRecord dispRecord) throws IOException {
        logger.debug("Writing display record to DSP");
        
        if (dispFile == null) {
            throw new IllegalStateException("DISPFILE not open");
        }
        
        // Send display to web UI
        dispFile.writeRecord(dispRecord);
        
        logger.debug("Display record written successfully");
    }
    
    /**
     * COBOL: READ DISPFILE INTO DISP-RECORD
     * Receive user input from web UI
     */
    private MitdspRecord readDispFile() throws IOException {
        logger.debug("Reading input from DSP");
        
        if (dispFile == null) {
            throw new IllegalStateException("DISPFILE not open");
        }
        
        // Wait for user input from web UI
        MitdspRecord inputRecord = dispFile.readRecord();
        
        logger.debug("Input received: EMP-FUNC = '{}'", inputRecord.getEmpFunc());
        return inputRecord;
    }
    
    /**
     * COBOL: EVALUATE EMP-FUNC
     * Call appropriate subprogram based on user selection
     */
    private void evaluateEmpFunc(String empFunc) {
        logger.debug("Evaluating EMP-FUNC: '{}'", empFunc);
        
        switch (empFunc) {
            case "1":
                // COBOL: CALL "SUB001"
                callSubprogram("SUB001");
                break;
                
            case "2":
                // COBOL: CALL "SUB002"
                callSubprogram("SUB002");
                break;
                
            case "3":
                // COBOL: CALL "SUB003"
                callSubprogram("SUB003");
                break;
                
            case "4":
                // COBOL: CALL "SUB004"
                callSubprogram("SUB004");
                break;
                
            default:
                // COBOL: WHEN OTHER - MOVE "Y" TO WS-END-FLAG
                logger.debug("Invalid EMP-FUNC value: '{}', setting end flag", empFunc);
                wsEndFlag = "Y";
        }
    }
    
    /**
     * Call subprogram (equivalent to COBOL CALL statement)
     */
    private void callSubprogram(String programName) {
        logger.info("Calling subprogram: {}", programName);
        
        try {
            // In a real implementation, this would invoke the actual subprogram
            // For now, we'll just log the call
            System.out.println("Called program: " + programName);
            
            // Simulate program execution
            Thread.sleep(100);
            
            logger.info("Subprogram {} completed successfully", programName);
            
        } catch (Exception e) {
            logger.error("Error calling subprogram {}", programName, e);
            throw new RuntimeException("Failed to call subprogram: " + programName, e);
        }
    }
    
    /**
     * Set terminal ID for web UI communication
     */
    public void setTerminalId(String terminalId) {
        this.terminalId = terminalId;
    }
    
    /**
     * Set web UI service
     */
    public void setWebUIService(WebUIService webUIService) {
        this.webUIService = webUIService;
    }
    
    /**
     * Get current end flag status
     */
    public String getEndFlag() {
        return wsEndFlag;
    }
    
    /**
     * Main method for standalone execution
     */
    public static void main(String[] args) {
        System.out.println("MAIN001 - COBOL to Java conversion with proper DSP file handling");
        System.out.println("Using COBOL G infrastructure for file operations");
        System.out.println();
        
        try {
            MAIN001 program = new MAIN001();
            
            // Set terminal ID from command line if provided
            if (args.length > 0) {
                program.setTerminalId(args[0]);
                System.out.println("Terminal ID: " + args[0]);
            } else {
                System.out.println("Terminal ID: TERM001 (default)");
            }
            
            // Execute the program
            program.execute();
            
            System.out.println("\nProgram completed successfully");
            
        } catch (Exception e) {
            System.err.println("Program execution failed: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
}