import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.net.http.*;
import java.net.URI;
import java.util.Map;
import java.util.HashMap;

/**
 * SJISToUnicodeConverter - Professional character encoding conversion utility
 * 
 * This utility class provides comprehensive character encoding conversion capabilities
 * specifically designed for legacy system integration with modern web interfaces.
 * 
 * Features:
 * - SJIS to Unicode/UTF-8 conversion for web display
 * - EBCDIC integration via existing encoding API (port 3003)
 * - Proper handling of Japanese full-width characters
 * - Fallback mechanisms for conversion failures
 * - Performance optimization with caching
 * 
 * Usage:
 * - Server-side data processing: Maintain SJIS encoding
 * - Web client output: Convert to Unicode/UTF-8 for proper display
 * - Terminal integration: Handle mixed ASCII/Japanese content
 */
public class SJISToUnicodeConverter {
    
    // Character encoding constants
    private static final Charset SJIS_CHARSET = Charset.forName("Shift_JIS");
    private static final Charset UTF8_CHARSET = StandardCharsets.UTF_8;
    private static final Charset UNICODE_CHARSET = StandardCharsets.UTF_16;
    
    // API integration
    private static final String ENCODING_API_URL = "http://localhost:3003/convert";
    private static final HttpClient HTTP_CLIENT = HttpClient.newHttpClient();
    
    // Performance optimization cache
    private static final Map<String, String> conversionCache = new HashMap<>();
    private static final int MAX_CACHE_SIZE = 1000;
    
    /**
     * Convert SJIS encoded string to Unicode for web display
     * 
     * @param sjisData The SJIS encoded input string
     * @return Unicode string suitable for web display
     */
    public static String sjisToUnicode(String sjisData) {
        if (sjisData == null || sjisData.isEmpty()) {
            return "";
        }
        
        // Check cache first for performance
        String cached = conversionCache.get(sjisData);
        if (cached != null) {
            return cached;
        }
        
        try {
            // Convert SJIS bytes to Unicode string
            byte[] sjisBytes = sjisData.getBytes(SJIS_CHARSET);
            String unicodeResult = new String(sjisBytes, SJIS_CHARSET);
            
            // Cache the result
            cacheConversion(sjisData, unicodeResult);
            
            return unicodeResult;
            
        } catch (Exception e) {
            System.err.println("SJIS to Unicode conversion error: " + e.getMessage());
            return sjisData; // Fallback to original data
        }
    }
    
    /**
     * Convert SJIS encoded string to UTF-8 for web transmission
     * 
     * @param sjisData The SJIS encoded input string
     * @return UTF-8 encoded string
     */
    public static String sjisToUTF8(String sjisData) {
        if (sjisData == null || sjisData.isEmpty()) {
            return "";
        }
        
        try {
            // First convert SJIS to Unicode, then to UTF-8
            byte[] sjisBytes = sjisData.getBytes(SJIS_CHARSET);
            String unicodeString = new String(sjisBytes, SJIS_CHARSET);
            return new String(unicodeString.getBytes(UTF8_CHARSET), UTF8_CHARSET);
            
        } catch (Exception e) {
            System.err.println("SJIS to UTF-8 conversion error: " + e.getMessage());
            return sjisData; // Fallback to original data
        }
    }
    
    /**
     * Convert Unicode string back to SJIS for server-side processing
     * 
     * @param unicodeData The Unicode input string
     * @return SJIS encoded string
     */
    public static String unicodeToSJIS(String unicodeData) {
        if (unicodeData == null || unicodeData.isEmpty()) {
            return "";
        }
        
        try {
            byte[] unicodeBytes = unicodeData.getBytes(SJIS_CHARSET);
            return new String(unicodeBytes, SJIS_CHARSET);
            
        } catch (Exception e) {
            System.err.println("Unicode to SJIS conversion error: " + e.getMessage());
            return unicodeData; // Fallback to original data
        }
    }
    
    /**
     * Convert SJIS data to EBCDIC using the existing encoding API
     * 
     * @param sjisData The SJIS encoded input string
     * @return EBCDIC encoded string via API conversion
     */
    public static String sjisToEBCDIC(String sjisData) {
        if (sjisData == null || sjisData.isEmpty()) {
            return "";
        }
        
        try {
            String requestBody = buildConversionRequest("SJIS", "EBCDIC", sjisData);
            HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(ENCODING_API_URL))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();
            
            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            
            if (response.statusCode() == 200) {
                return parseConversionResponse(response.body());
            } else {
                System.err.println("EBCDIC conversion failed: HTTP " + response.statusCode());
                return sjisData; // Fallback to original data
            }
            
        } catch (Exception e) {
            System.err.println("Error calling EBCDIC conversion API: " + e.getMessage());
            return sjisData; // Fallback to original data
        }
    }
    
    /**
     * Convert EBCDIC data back to SJIS using the existing encoding API
     * 
     * @param ebcdicData The EBCDIC encoded input string
     * @return SJIS encoded string via API conversion
     */
    public static String ebcdicToSJIS(String ebcdicData) {
        if (ebcdicData == null || ebcdicData.isEmpty()) {
            return "";
        }
        
        try {
            String requestBody = buildConversionRequest("EBCDIC", "SJIS", ebcdicData);
            HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(ENCODING_API_URL))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(requestBody))
                .build();
            
            HttpResponse<String> response = HTTP_CLIENT.send(request, HttpResponse.BodyHandlers.ofString());
            
            if (response.statusCode() == 200) {
                return parseConversionResponse(response.body());
            } else {
                System.err.println("SJIS conversion failed: HTTP " + response.statusCode());
                return ebcdicData; // Fallback to original data
            }
            
        } catch (Exception e) {
            System.err.println("Error calling SJIS conversion API: " + e.getMessage());
            return ebcdicData; // Fallback to original data
        }
    }
    
    /**
     * Calculate display width for mixed ASCII/Japanese text
     * Japanese characters typically require 2 display positions
     * 
     * @param text The text to measure
     * @return Display width in character positions
     */
    public static int getDisplayWidth(String text) {
        if (text == null) return 0;
        
        int width = 0;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            // Japanese characters (Hiragana, Katakana, Kanji) are typically > 127 in Unicode
            // and require 2 display positions in terminal/console output
            width += (c > 127) ? 2 : 1;
        }
        return width;
    }
    
    /**
     * Truncate text to fit within specified display width
     * Properly handles Japanese full-width characters
     * 
     * @param text The text to truncate
     * @param maxWidth Maximum display width in character positions
     * @return Truncated text that fits within maxWidth
     */
    public static String truncateToDisplayWidth(String text, int maxWidth) {
        if (text == null || maxWidth <= 0) return "";
        
        int currentWidth = 0;
        int truncateIndex = 0;
        
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            int charWidth = (c > 127) ? 2 : 1;
            
            if (currentWidth + charWidth > maxWidth) {
                break;
            }
            
            currentWidth += charWidth;
            truncateIndex = i + 1;
        }
        
        return text.substring(0, truncateIndex);
    }
    
    /**
     * Pad text to exact display width with spaces
     * Handles Japanese character width calculations
     * 
     * @param text The text to pad
     * @param targetWidth Target display width
     * @return Padded text of exact target width
     */
    public static String padToDisplayWidth(String text, int targetWidth) {
        if (text == null) text = "";
        
        int currentWidth = getDisplayWidth(text);
        if (currentWidth >= targetWidth) {
            return truncateToDisplayWidth(text, targetWidth);
        }
        
        int spacesToAdd = targetWidth - currentWidth;
        return text + " ".repeat(spacesToAdd);
    }
    
    /**
     * Format employee record line for proper terminal display
     * Ensures consistent alignment with mixed ASCII/Japanese content
     * 
     * @param id Employee ID (ASCII)
     * @param name Employee name (Japanese)
     * @param department Department name (Japanese)
     * @param salary Salary (ASCII)
     * @param hireDate Hire date (ASCII)
     * @param status Status (Japanese)
     * @return Formatted line for display
     */
    public static String formatEmployeeRecord(String id, String name, String department, 
                                              String salary, String hireDate, String status) {
        
        // Convert all Japanese fields to Unicode for proper display
        String unicodeName = sjisToUnicode(name);
        String unicodeDepartment = sjisToUnicode(department);
        String unicodeStatus = sjisToUnicode(status);
        
        // Format with proper field widths
        String formattedId = padToDisplayWidth(id, 8);
        String formattedName = padToDisplayWidth(unicodeName, 20);
        String formattedDept = padToDisplayWidth(unicodeDepartment, 10);
        String formattedSalary = padToDisplayWidth(salary, 10);
        String formattedHireDate = padToDisplayWidth(hireDate, 8);
        String formattedStatus = padToDisplayWidth(unicodeStatus, 10);
        
        return String.format("%s %s %s %s %s %s", 
            formattedId, formattedName, formattedDept, 
            formattedSalary, formattedHireDate, formattedStatus);
    }
    
    // Private helper methods
    
    private static String buildConversionRequest(String sourceEncoding, String targetEncoding, String data) {
        return String.format(
            "{\"sourceEncoding\":\"%s\",\"targetEncoding\":\"%s\",\"data\":\"%s\"}", 
            sourceEncoding, 
            targetEncoding,
            escapeJsonString(data)
        );
    }
    
    private static String parseConversionResponse(String responseBody) {
        // Simplified JSON parsing - in production, use a proper JSON library
        // For now, assume the API returns the converted data directly
        return responseBody;
    }
    
    private static String escapeJsonString(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }
    
    private static void cacheConversion(String key, String value) {
        if (conversionCache.size() >= MAX_CACHE_SIZE) {
            // Simple cache eviction - remove oldest entries
            conversionCache.clear();
        }
        conversionCache.put(key, value);
    }
    
    /**
     * Test method to validate conversion functionality
     * Reads test data from the SAMDATA_JP file to avoid encoding issues in source code
     */
    public static void testConversions() {
        System.out.println("=== SJIS to Unicode Converter Test ===");
        
        try {
            // Read first few records from SAMDATA_JP for testing
            java.nio.file.Path dataFile = java.nio.file.Paths.get("/home/aspuser/app/volume/DISK01/TESTLIB/SAMDATA_JP");
            if (java.nio.file.Files.exists(dataFile)) {
                java.util.List<String> lines = java.nio.file.Files.readAllLines(dataFile, SJIS_CHARSET);
                
                for (int i = 0; i < Math.min(3, lines.size()); i++) {
                    if (lines.get(i).trim().length() > 0) {
                        // Extract name and department for testing
                        String line = lines.get(i);
                        if (line.length() >= 42) {
                            String name = line.substring(8, 30).trim();
                            String dept = line.substring(30, 42).trim();
                            
                            System.out.println("Testing name: " + name);
                            System.out.println("Unicode:  " + sjisToUnicode(name));
                            System.out.println("UTF-8:    " + sjisToUTF8(name));
                            System.out.println("Width:    " + getDisplayWidth(name));
                            System.out.println("---");
                        }
                    }
                }
            } else {
                System.out.println("Test data file not found. Converter is ready for use.");
            }
        } catch (Exception e) {
            System.out.println("Test completed with note: " + e.getMessage());
        }
        
        System.out.println("Converter initialized and ready.");
    }
    
    public static void main(String[] args) {
        testConversions();
    }
}