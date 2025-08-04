package com.openasp.main;

/**
 * Interface for COBOL output handlers
 * Supports different symbolic destinations (DSP, PRT, APL)
 */
public interface OutputHandler {
    
    /**
     * Open the output destination
     */
    void open();
    
    /**
     * Close the output destination
     */
    void close();
    
    /**
     * Check if the handler is open
     */
    boolean isOpen();
    
    /**
     * Write a record to the destination
     * @param record The record to write
     */
    void writeRecord(Object record);
    
    /**
     * Read a record from the destination (for interactive destinations like DSP)
     * @param recordClass The class of record to read
     * @return The record read from the destination
     */
    Object readRecord(Class<?> recordClass);
    
    /**
     * Flush any buffered output
     */
    void flush();
}