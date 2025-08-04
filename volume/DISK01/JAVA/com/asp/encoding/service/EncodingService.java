package com.asp.encoding.service;

import org.springframework.stereotype.Service;
import java.nio.charset.Charset;

/**
 * EncodingService - Handles character encoding conversions
 * Primary use is for SJIS/UTF-8 conversion for Japanese text
 */
@Service
public class EncodingService {
    
    private static final Charset UTF8 = Charset.forName("UTF-8");
    private static final Charset SJIS = Charset.forName("Shift_JIS");
    
    /**
     * Convert UTF-8 text to SJIS encoding
     * @param utf8Text Input text in UTF-8
     * @return Text converted to SJIS
     */
    public String convertToSjis(String utf8Text) {
        if (utf8Text == null) return null;
        try {
            byte[] sjisBytes = utf8Text.getBytes(SJIS);
            return new String(sjisBytes, SJIS);
        } catch (Exception e) {
            // If conversion fails, return original
            return utf8Text;
        }
    }
    
    /**
     * Convert SJIS text to UTF-8 encoding
     * @param sjisText Input text in SJIS
     * @return Text converted to UTF-8
     */
    public String convertFromSjis(String sjisText) {
        if (sjisText == null) return null;
        try {
            byte[] sjisBytes = sjisText.getBytes(SJIS);
            return new String(sjisBytes, UTF8);
        } catch (Exception e) {
            // If conversion fails, return original
            return sjisText;
        }
    }
    
    /**
     * Check if encoding service is available
     * @return true if service is operational
     */
    public boolean isAvailable() {
        return true;
    }
}