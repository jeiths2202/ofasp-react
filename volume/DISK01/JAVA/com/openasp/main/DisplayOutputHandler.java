package com.openasp.main;

import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * Display Output Handler for DSP (Display) symbolic destination
 * Handles web UI communication for COBOL DISPFILE operations
 */
public class DisplayOutputHandler implements OutputHandler {
    
    private WebUIService webUIService;
    private String terminalId;
    private boolean isOpen = false;
    
    public DisplayOutputHandler() {
        // Will be injected or configured during initialization
    }
    
    public void setWebUIService(WebUIService webUIService) {
        this.webUIService = webUIService;
    }
    
    public void setTerminalId(String terminalId) {
        this.terminalId = terminalId;
    }
    
    @Override
    public void open() {
        if (webUIService != null && terminalId != null) {
            webUIService.initializeSession(terminalId);
            isOpen = true;
        } else {
            throw new RuntimeException("WebUIService or terminalId not configured for DSP handler");
        }
    }
    
    @Override
    public void close() {
        if (webUIService != null && terminalId != null) {
            webUIService.closeSession(terminalId);
        }
        isOpen = false;
    }
    
    @Override
    public boolean isOpen() {
        return isOpen;
    }
    
    /**
     * Write display record to web UI (COBOL WRITE DISPFILE)
     */
    @Override
    public void writeRecord(Object record) {
        if (!isOpen) {
            throw new IllegalStateException("Display handler not open");
        }
        
        if (webUIService == null) {
            throw new IllegalStateException("WebUIService not configured");
        }
        
        // Convert record to web UI display format
        Map<String, Object> displayData = convertRecordToDisplayData(record);
        
        // Send to web UI
        webUIService.sendDisplayData(terminalId, displayData);
    }
    
    /**
     * Read input from web UI (COBOL READ DISPFILE)
     */
    @Override
    public Object readRecord(Class<?> recordClass) {
        if (!isOpen) {
            throw new IllegalStateException("Display handler not open");
        }
        
        if (webUIService == null) {
            throw new IllegalStateException("WebUIService not configured");
        }
        
        try {
            // Wait for user input from web UI
            Map<String, Object> inputData = webUIService.waitForUserInput(terminalId);
            
            // Convert input data back to record object
            return convertInputDataToRecord(inputData, recordClass);
            
        } catch (Exception e) {
            throw new RuntimeException("Failed to read input from web UI", e);
        }
    }
    
    /**
     * Convert COBOL record to web UI display format
     */
    private Map<String, Object> convertRecordToDisplayData(Object record) {
        Map<String, Object> displayData = new java.util.HashMap<>();
        
        if (record instanceof MitdspRecord) {
            MitdspRecord mitdsp = (MitdspRecord) record;
            
            displayData.put("type", "menu_display");
            displayData.put("menu_title", mitdsp.getMenuTitle());
            displayData.put("option_1", mitdsp.getOption1());
            displayData.put("option_2", mitdsp.getOption2());
            displayData.put("option_3", mitdsp.getOption3());
            displayData.put("option_4", mitdsp.getOption4());
            displayData.put("selection_prompt", mitdsp.getSelectionPrompt());
            displayData.put("current_selection", mitdsp.getEmpFunc());
            displayData.put("encoding", "UTF-8");
            displayData.put("timestamp", System.currentTimeMillis());
            
        } else {
            // Generic record handling
            displayData.put("type", "generic_display");
            displayData.put("data", record.toString());
        }
        
        return displayData;
    }
    
    /**
     * Convert web UI input data back to COBOL record
     */
    private Object convertInputDataToRecord(Map<String, Object> inputData, Class<?> recordClass) {
        try {
            if (recordClass == MitdspRecord.class) {
                MitdspRecord record = MitdspRecord.createDefaultRecord();
                
                // Extract user selection from input
                Object selection = inputData.get("selection");
                if (selection != null) {
                    String empFunc = selection.toString().trim();
                    if (empFunc.length() == 1 && "1234".contains(empFunc)) {
                        record.setEmpFunc(empFunc);
                    }
                }
                
                return record;
                
            } else {
                // Generic record creation
                return recordClass.getDeclaredConstructor().newInstance();
            }
            
        } catch (Exception e) {
            throw new RuntimeException("Failed to convert input data to record", e);
        }
    }
    
    @Override
    public void flush() {
        // DSP operations are immediate, no buffering needed
    }
}