package com.openasp.main;

/**
 * Exception for COBOL data conversion errors
 */
public class CobolDataConversionException extends RuntimeException {
    
    public CobolDataConversionException(String message) {
        super(message);
    }
    
    public CobolDataConversionException(String message, Throwable cause) {
        super(message, cause);
    }
}