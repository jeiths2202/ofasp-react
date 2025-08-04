package com.openasp.main;

import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;
import com.openasp.common.JSONResponse;

/**
 * Interactive test runner for MAIN001 program
 * This version includes user input simulation
 */
public class MAIN001Interactive {
    
    public static void main(String[] args) {
        System.out.println("=== MAIN001 Interactive Test ===");
        System.out.println("COBOL → Java 변환 프로그램 테스트\n");
        
        // Create enhanced MAIN001 with user input simulation
        MAIN001Enhanced program = new MAIN001Enhanced();
        
        // Prepare input data
        Map<String, String> inputData = new HashMap<>();
        inputData.put("terminal_id", "TERM001");
        
        try {
            // Execute the program
            JSONResponse response = program.execute(inputData);
            
            // Display the result
            System.out.println("\n=== 최종 실행 결과 ===");
            System.out.println(response.toString());
            
        } catch (Exception e) {
            System.err.println("프로그램 실행 에러: " + e.getMessage());
            e.printStackTrace();
        }
        
        System.out.println("\n=== 테스트 완료 ===");
    }
}

/**
 * Enhanced MAIN001 with user input simulation
 */
class MAIN001Enhanced extends MAIN001 {
    
    private Scanner scanner = new Scanner(System.in);
    
    @Override
    public JSONResponse execute(Map<String, String> inputData) {
        System.out.println("🎌 Fujitsu ASP COBOLG MAIN001 프로그램 시작");
        System.out.println("8바이트 네이밍 규칙: MAIN001");
        System.out.println("SJIS 인코딩 지원: 일본어 메뉴\n");
        
        return super.execute(inputData);
    }
    
    /**
     * Override acceptUserInput to provide console input
     */
    @Override
    protected String acceptUserInput() {
        System.out.println("\n=== 管理メニュー ===");
        System.out.println("１）参照");
        System.out.println("２）追加");
        System.out.println("３）更新");
        System.out.println("４）削除");
        System.out.print("\n選択を入力してください (1-4): ");
        
        String input = scanner.nextLine().trim();
        System.out.println("입력된 선택: " + input);
        
        return input;
    }
    
    /**
     * Override validateSelection to show validation result
     */
    @Override
    protected void validateSelection() {
        super.validateSelection();
        
        if (isValidOption()) {
            System.out.println("✅ 유효한 선택: " + getSelectedProgram());
            System.out.println("📄 상태 메시지: " + getStatusMessage());
        } else {
            System.out.println("❌ " + getErrorMessage());
            System.out.println("🔄 재시도 횟수: " + getRetryCount() + "/3");
        }
    }
    
    /**
     * Make methods accessible for override
     */
    protected String acceptUserInput() { 
        return super.acceptUserInput(); 
    }
    
    protected void validateSelection() { 
        super.validateSelection(); 
    }
}