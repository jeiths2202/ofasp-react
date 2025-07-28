import java.io.*;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

/**
 * TESTPGM - EMPLOYEE.FB ファイル読み込みプログラム
 * Simple Java Program for reading EMPLOYEE.FB dataset
 */
public class TESTPGM {
    
    // File paths
    private static final String VOLUME_PATH = "/home/aspuser/app/volume";
    private static final String EMPLOYEE_FILE = "DISK01/TESTLIB/EMPLOYEE.FB";
    
    public static void main(String[] args) {
        int returnCode = 0;
        
        try {
            // Build full file path
            Path filePath = Paths.get(VOLUME_PATH, EMPLOYEE_FILE);
            
            System.out.println("=== TESTPGM: EMPLOYEE.FB データ読み込み開始 ===");
            System.out.println("ファイルパス: " + filePath.toString());
            
            // Check if file exists
            if (!Files.exists(filePath)) {
                System.err.println("エラー: ファイルが存在しません - " + filePath);
                System.exit(8); // File not found return code
            }
            
            // Read file with SJIS encoding
            List<String> lines = Files.readAllLines(filePath, Charset.forName("Shift_JIS"));
            
            System.out.println("\n読み込んだレコード数: " + lines.size());
            System.out.println("\n=== 社員データ一覧 ===");
            
            int recordCount = 0;
            for (String line : lines) {
                if (line.trim().isEmpty()) {
                    continue; // Skip empty lines
                }
                
                recordCount++;
                // Parse fixed-length fields (社員番号6, 社員名20, 部門3, 給与8, 入社日8)
                String empNo = line.substring(0, 6).trim();
                String empName = line.substring(6, 26).trim();
                String dept = line.substring(26, 29).trim();
                String salary = line.substring(29, 37).trim();
                String hireDate = line.substring(37, 45).trim();
                
                System.out.println(String.format("%d: 社員番号=%s, 社員名=%s, 部門=%s, 給与=%s, 入社日=%s", 
                        recordCount, empNo, empName, dept, salary, hireDate));
            }
            
            System.out.println("\n処理済みレコード数: " + recordCount);
            System.out.println("=== TESTPGM: 正常終了 ===");
            
        } catch (IOException e) {
            System.err.println("エラー: ファイル読み込み中にエラーが発生しました");
            System.err.println("詳細: " + e.getMessage());
            returnCode = 12; // I/O error return code
        } catch (Exception e) {
            System.err.println("エラー: 予期しないエラーが発生しました");
            System.err.println("詳細: " + e.getMessage());
            returnCode = 16; // General error return code
        }
        
        // Exit with return code
        System.exit(returnCode);
    }
}