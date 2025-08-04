import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

/**
 * Test program to verify SJIS to UTF-8 encoding conversion
 * Tests the specific bytes: 93 63 92 86 91 be 98 59 which should represent "田中太郎" in SJIS
 */
public class EncodingTest {
    public static void main(String[] args) {
        // Test bytes that represent "田中太郎" in SJIS
        byte[] sjisBytes = {(byte)0x93, (byte)0x63, (byte)0x92, (byte)0x86, (byte)0x91, (byte)0xbe, (byte)0x98, (byte)0x59};
        
        System.out.println("Original SJIS bytes: ");
        for (byte b : sjisBytes) {
            System.out.printf("%02x ", b & 0xFF);
        }
        System.out.println();
        
        try {
            // Convert SJIS bytes to Java String (internally UTF-16)
            String japaneseText = new String(sjisBytes, Charset.forName("Shift_JIS"));
            System.out.println("Decoded from SJIS: " + japaneseText);
            
            // Convert to UTF-8 bytes for JSON output
            byte[] utf8Bytes = japaneseText.getBytes(StandardCharsets.UTF_8);
            System.out.println("UTF-8 bytes: ");
            for (byte b : utf8Bytes) {
                System.out.printf("%02x ", b & 0xFF);
            }
            System.out.println();
            
            // Verify round-trip
            String fromUtf8 = new String(utf8Bytes, StandardCharsets.UTF_8);
            System.out.println("Round-trip result: " + fromUtf8);
            System.out.println("Round-trip successful: " + japaneseText.equals(fromUtf8));
            
            // Test JSON escaping
            String jsonValue = escapeJson(japaneseText);
            System.out.println("JSON escaped: \"" + jsonValue + "\"");
            
        } catch (Exception e) {
            System.err.println("Encoding test failed: " + e.getMessage());
            e.printStackTrace();
        }
    }
    
    /**
     * Simple JSON escaping (same as in main program)
     */
    private static String escapeJson(String str) {
        if (str == null) return "";
        
        StringBuilder escaped = new StringBuilder();
        for (int i = 0; i < str.length(); i++) {
            char c = str.charAt(i);
            switch (c) {
                case '\\': escaped.append("\\\\"); break;
                case '"': escaped.append("\\\""); break;
                case '\n': escaped.append("\\n"); break;
                case '\r': escaped.append("\\r"); break;
                case '\t': escaped.append("\\t"); break;
                case '\b': escaped.append("\\b"); break;
                case '\f': escaped.append("\\f"); break;
                default:
                    if (c < 32) {
                        escaped.append(String.format("\\u%04x", (int) c));
                    } else {
                        escaped.append(c);
                    }
                    break;
            }
        }
        return escaped.toString();
    }
}