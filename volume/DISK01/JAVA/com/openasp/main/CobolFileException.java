package com.openasp.main;

/**
 * Exception for COBOL file operations
 */
public class CobolFileException extends RuntimeException {
    
    public CobolFileException(String message) {
        super(message);
    }
    
    public CobolFileException(String message, Throwable cause) {
        super(message, cause);
    }
}