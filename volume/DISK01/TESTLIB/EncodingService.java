package com.openasp.cobol;

/**
 * EncodingService Interface
 * 
 * Provides character encoding conversion services
 * Handles SJIS/UTF-8 conversion for ASP system compatibility
 */
public interface EncodingService {
    
    /**
     * Convert SJIS encoded string to UTF-8
     * 
     * @param sjisString String in SJIS encoding
     * @return UTF-8 encoded string
     * @throws Exception if conversion fails
     */
    String convertSjisToUtf8(String sjisString) throws Exception;
    
    /**
     * Convert UTF-8 encoded string to SJIS
     * 
     * @param utf8String String in UTF-8 encoding
     * @return SJIS encoded string
     * @throws Exception if conversion fails
     */
    String convertUtf8ToSjis(String utf8String) throws Exception;
    
    /**
     * Convert from SJIS to internal format (UTF-8)
     * 
     * @param sjisString String in SJIS encoding
     * @return Internal format string
     * @throws Exception if conversion fails
     */
    String convertFromSjis(String sjisString) throws Exception;
    
    /**
     * Convert to SJIS from internal format (UTF-8)
     * 
     * @param internalString String in internal format
     * @return SJIS encoded string
     * @throws Exception if conversion fails
     */
    String convertToSjis(String internalString) throws Exception;
    
    /**
     * Detect character encoding of byte array
     * 
     * @param data Byte array to analyze
     * @return Detected encoding name
     */
    String detectEncoding(byte[] data);
    
    /**
     * Validate if string contains valid characters for encoding
     * 
     * @param text String to validate
     * @param encoding Target encoding
     * @return true if valid, false otherwise
     */
    boolean isValidForEncoding(String text, String encoding);
}