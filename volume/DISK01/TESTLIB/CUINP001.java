import java.io.*;
import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

/**
 * CUINP001 - Customer Data Input Program
 * 
 * This program writes customer data to FB (Fixed Block) format SAM files
 * with 128-byte records as specified in OpenASP CL sample.
 * 
 * Parameters: 001,ABC (customer_id, customer_code)
 */
public class CUINP001 {
    
    private static final int RECORD_LENGTH = 128;
    
    public static void main(String[] args) {
        try {
            // Parse ASP input JSON using simple string parsing
            String inputJson = System.getProperty("asp.input", "{}");
            
            // Extract parameters using regex
            String paramString = extractJsonValue(inputJson, "arg1");
            if (paramString == null || paramString.isEmpty()) {
                // Try alternative parameter formats
                paramString = extractJsonValue(inputJson, "parameters");
                if (paramString == null) {
                    paramString = "001,ABC"; // Default for testing
                }
            }
            
            // Parse parameters: "001,ABC"
            String[] params = paramString.split(",");
            if (params.length < 2) {
                System.err.println("[ERROR] CUINP001: Invalid parameters. Expected: customer_id,customer_code");
                System.exit(1);
            }
            
            String customerId = params[0].trim();
            String customerCode = params[1].trim();
            
            // Get file information from input
            String library = extractJsonValue(inputJson, "library");
            String volume = extractJsonValue(inputJson, "volume");
            
            // Use defaults if not found in JSON
            if (library == null) library = "TESTLIB";
            if (volume == null) volume = "DISK01";
            
            // Build file path
            String volumeRoot = "/home/aspuser/app/volume";
            String filePath = volumeRoot + "/" + volume + "/" + library + "/CUSTOMER.SAM001";
            
            // Generate customer record data
            String recordData = generateCustomerRecord(customerId, customerCode);
            
            // Write to FB format file
            writeToFBFile(filePath, recordData);
            
            // Output success message
            System.out.println("[INFO] CUINP001: Customer record written successfully");
            System.out.println("[INFO] Customer ID: " + customerId);
            System.out.println("[INFO] Customer Code: " + customerCode);
            System.out.println("[INFO] Record length: " + RECORD_LENGTH + " bytes");
            System.out.println("[INFO] File: " + filePath);
            
        } catch (Exception e) {
            System.err.println("[ERROR] CUINP001: " + e.getMessage());
            e.printStackTrace();
            System.exit(1);
        }
    }
    
    /**
     * Generate a 128-byte customer record
     */
    private static String generateCustomerRecord(String customerId, String customerCode) {
        StringBuilder record = new StringBuilder();
        
        // Current timestamp
        String timestamp = LocalDateTime.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"));
        
        // Build record fields (total 128 bytes)
        record.append(String.format("%-10s", customerId));           // Customer ID (10 bytes)
        record.append(String.format("%-10s", customerCode));         // Customer Code (10 bytes)
        record.append(String.format("%-30s", "CUSTOMER_NAME_" + customerId)); // Name (30 bytes)
        record.append(String.format("%-40s", "ADDRESS_" + customerId));       // Address (40 bytes)
        record.append(String.format("%-19s", timestamp));            // Timestamp (19 bytes)
        record.append(String.format("%-19s", "ACTIVE"));             // Status (19 bytes)
        
        // Ensure exactly 128 bytes
        String result = record.toString();
        if (result.length() > RECORD_LENGTH) {
            result = result.substring(0, RECORD_LENGTH);
        } else {
            result = String.format("%-" + RECORD_LENGTH + "s", result);
        }
        
        return result;
    }
    
    /**
     * Write data to FB (Fixed Block) format file
     */
    private static void writeToFBFile(String filePath, String recordData) throws IOException {
        // FB format: No line breaks, fixed-length records
        try (FileOutputStream fos = new FileOutputStream(filePath, true);
             OutputStreamWriter writer = new OutputStreamWriter(fos, StandardCharsets.UTF_8)) {
            
            // Write record without line break (FB format characteristic)
            writer.write(recordData);
            writer.flush();
        }
    }
    
    /**
     * Simple JSON value extraction using regex
     */
    private static String extractJsonValue(String json, String key) {
        try {
            Pattern pattern = Pattern.compile("\"" + key + "\"\\s*:\\s*\"([^\"]+)\"");
            Matcher matcher = pattern.matcher(json);
            if (matcher.find()) {
                return matcher.group(1);
            }
            
            // Try without quotes for simple values
            pattern = Pattern.compile("\"" + key + "\"\\s*:\\s*([^,\\}]+)");
            matcher = pattern.matcher(json);
            if (matcher.find()) {
                return matcher.group(1).trim();
            }
        } catch (Exception e) {
            // Ignore parsing errors
        }
        return null;
    }
}