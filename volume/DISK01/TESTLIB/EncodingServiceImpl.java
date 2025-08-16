package com.openasp.cobol;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.charset.CharsetEncoder;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CodingErrorAction;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * EncodingServiceImpl - Implementation of EncodingService
 * 
 * Handles character encoding conversion between SJIS and UTF-8
 * Essential for ASP system compatibility with different character sets
 */
public class EncodingServiceImpl implements EncodingService {
    
    private static final Logger logger = Logger.getLogger(EncodingServiceImpl.class.getName());
    
    // Character sets
    private static final Charset SJIS_CHARSET = Charset.forName("Shift_JIS");
    private static final Charset UTF8_CHARSET = StandardCharsets.UTF_8;
    
    // Encoder/Decoder with error handling
    private final CharsetEncoder sjisEncoder;
    private final CharsetDecoder sjisDecoder;
    private final CharsetEncoder utf8Encoder;
    private final CharsetDecoder utf8Decoder;
    
    public EncodingServiceImpl() {
        // Initialize encoders/decoders with proper error handling
        this.sjisEncoder = SJIS_CHARSET.newEncoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE);
            
        this.sjisDecoder = SJIS_CHARSET.newDecoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE);
            
        this.utf8Encoder = UTF8_CHARSET.newEncoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE);
            
        this.utf8Decoder = UTF8_CHARSET.newDecoder()
            .onMalformedInput(CodingErrorAction.REPLACE)
            .onUnmappableCharacter(CodingErrorAction.REPLACE);
    }
    
    @Override
    public String convertSjisToUtf8(String sjisString) throws Exception {
        if (sjisString == null) {
            return null;
        }
        
        if (sjisString.isEmpty()) {
            return "";
        }
        
        try {
            logger.fine("Converting SJIS to UTF-8: " + sjisString.length() + " characters");
            
            // Convert string to SJIS byte array
            byte[] sjisBytes = sjisString.getBytes(SJIS_CHARSET);
            
            // Convert bytes to UTF-8 string
            String utf8String = new String(sjisBytes, UTF8_CHARSET);
            
            logger.fine("SJIS to UTF-8 conversion completed");
            return utf8String;
            
        } catch (Exception e) {
            logger.log(Level.WARNING, "SJIS to UTF-8 conversion failed", e);
            throw new Exception("Failed to convert SJIS to UTF-8: " + e.getMessage(), e);
        }
    }
    
    @Override
    public String convertUtf8ToSjis(String utf8String) throws Exception {
        if (utf8String == null) {
            return null;
        }
        
        if (utf8String.isEmpty()) {
            return "";
        }
        
        try {
            logger.fine("Converting UTF-8 to SJIS: " + utf8String.length() + " characters");
            
            // Convert string to UTF-8 byte array
            byte[] utf8Bytes = utf8String.getBytes(UTF8_CHARSET);
            
            // Convert bytes to SJIS string
            String sjisString = new String(utf8Bytes, SJIS_CHARSET);
            
            logger.fine("UTF-8 to SJIS conversion completed");
            return sjisString;
            
        } catch (Exception e) {
            logger.log(Level.WARNING, "UTF-8 to SJIS conversion failed", e);
            throw new Exception("Failed to convert UTF-8 to SJIS: " + e.getMessage(), e);
        }
    }
    
    @Override
    public String convertFromSjis(String sjisString) throws Exception {
        // For internal processing, convert SJIS to UTF-8
        return convertSjisToUtf8(sjisString);
    }
    
    @Override
    public String convertToSjis(String internalString) throws Exception {
        // For external output, convert UTF-8 to SJIS
        return convertUtf8ToSjis(internalString);
    }
    
    @Override
    public String detectEncoding(byte[] data) {
        if (data == null || data.length == 0) {
            return "UNKNOWN";
        }
        
        try {
            // Try UTF-8 first
            CharBuffer utf8Buffer = utf8Decoder.decode(ByteBuffer.wrap(data));
            if (isValidUtf8(data)) {
                return "UTF-8";
            }
        } catch (Exception e) {
            // UTF-8 failed, continue
        }
        
        try {
            // Try SJIS
            CharBuffer sjisBuffer = sjisDecoder.decode(ByteBuffer.wrap(data));
            return "SJIS";
        } catch (Exception e) {
            // SJIS failed
        }
        
        // Default to ASCII if all else fails
        if (isAscii(data)) {
            return "ASCII";
        }
        
        return "UNKNOWN";
    }
    
    @Override
    public boolean isValidForEncoding(String text, String encoding) {
        if (text == null || encoding == null) {
            return false;
        }
        
        try {
            Charset charset = Charset.forName(encoding);
            CharsetEncoder encoder = charset.newEncoder();
            
            return encoder.canEncode(text);
            
        } catch (Exception e) {
            logger.log(Level.FINE, "Encoding validation failed", e);
            return false;
        }
    }
    
    /**
     * Check if byte array contains valid UTF-8
     */
    private boolean isValidUtf8(byte[] data) {
        try {
            utf8Decoder.reset();
            CharBuffer result = utf8Decoder.decode(ByteBuffer.wrap(data));
            return true;
        } catch (Exception e) {
            return false;
        }
    }
    
    /**
     * Check if byte array contains only ASCII characters
     */
    private boolean isAscii(byte[] data) {
        for (byte b : data) {
            if ((b & 0x80) != 0) {  // Check if high bit is set
                return false;
            }
        }
        return true;
    }
    
    /**
     * Get charset by name with fallback
     */
    private Charset getCharsetSafely(String charsetName) {
        try {
            return Charset.forName(charsetName);
        } catch (Exception e) {
            logger.warning("Unsupported charset: " + charsetName + ", using UTF-8");
            return UTF8_CHARSET;
        }
    }
}