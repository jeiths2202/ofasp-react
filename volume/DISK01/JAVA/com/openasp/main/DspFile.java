package com.openasp.main;

import com.openasp.cobolg.file.CobolFileManager;
import com.openasp.cobolg.file.FileControlDefinition;
import com.openasp.cobolg.file.CobolRecordParser;

import java.io.IOException;

/**
 * DSP (Display) File implementation for COBOL G
 * Handles SYMBOLIC DESTINATION IS "DSP" file operations
 * Integrates with web UI for user interaction
 */
public class DspFile<T> extends CobolFileManager.CobolFile {
    
    private final CobolRecordParser<T> parser;
    private final OutputHandler outputHandler;
    private final FileControlDefinition fileControl;
    private String selectedFunction;
    
    public DspFile(String fileName, Class<T> recordClass, 
                   CobolFileManager.FileMode mode, 
                   FileControlDefinition fileControl) {
        super(fileName, recordClass, mode);
        this.parser = new CobolRecordParser<>(recordClass);
        this.fileControl = fileControl;
        
        // Create appropriate output handler based on symbolic destination
        if (fileControl.getSymbolicDestination() == FileControlDefinition.SymbolicDestination.DSP) {
            this.outputHandler = new DisplayOutputHandler();
        } else {
            throw new IllegalArgumentException("Unsupported symbolic destination: " + 
                fileControl.getSymbolicDestination());
        }
        
        try {
            open();
        } catch (IOException e) {
            throw new RuntimeException("Failed to open DSP file: " + fileName, e);
        }
    }
    
    @Override
    public void open() throws IOException {
        outputHandler.open();
        isOpen = true;
    }
    
    @Override
    public void close() throws IOException {
        if (outputHandler != null) {
            outputHandler.close();
        }
        isOpen = false;
    }
    
    /**
     * Write record to DSP destination (COBOL WRITE DISPFILE FROM record)
     */
    public void writeRecord(T record) throws IOException {
        if (!isOpen) {
            throw new IllegalStateException("DSP file not open");
        }
        
        outputHandler.writeRecord(record);
    }
    
    /**
     * Read record from DSP destination (COBOL READ DISPFILE INTO record)
     */
    @SuppressWarnings("unchecked")
    public T readRecord() throws IOException {
        if (!isOpen) {
            throw new IllegalStateException("DSP file not open");
        }
        
        Object record = outputHandler.readRecord(recordClass);
        
        // Extract SELECTED FUNCTION value if applicable
        if (record instanceof MitdspRecord) {
            MitdspRecord mitdsp = (MitdspRecord) record;
            this.selectedFunction = mitdsp.getEmpFunc();
        }
        
        return (T) record;
    }
    
    /**
     * Get the selected function value (for COBOL SELECTED FUNCTION IS)
     */
    public String getSelectedFunction() {
        return selectedFunction;
    }
    
    /**
     * Set up DSP file handler with web UI service
     */
    public void configureWebUI(WebUIService webUIService, String terminalId) {
        if (outputHandler instanceof DisplayOutputHandler) {
            DisplayOutputHandler displayHandler = (DisplayOutputHandler) outputHandler;
            displayHandler.setWebUIService(webUIService);
            displayHandler.setTerminalId(terminalId);
        }
    }
    
    /**
     * Flush any pending operations
     */
    public void flush() throws IOException {
        if (outputHandler != null) {
            outputHandler.flush();
        }
    }
}