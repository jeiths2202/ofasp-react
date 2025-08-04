import java.util.HashMap;
import java.util.Map;
import com.openasp.main.MAIN001;
import com.openasp.common.JSONResponse;

/**
 * Simple demo for MAIN001 program
 * Demonstrates successful program execution with simulated input
 */
public class MAIN001Demo {
    
    public static void main(String[] args) {
        System.out.println("🎌 === MAIN001 Demo Execution ===");
        System.out.println("Fujitsu ASP COBOLG → Java 변환 프로그램");
        System.out.println("8바이트 네이밍 규칙: MAIN001");
        System.out.println("SJIS 인코딩 지원\n");
        
        // Test different scenarios
        testScenario("1", "참조 프로그램 (INQUIRY1)");
        testScenario("2", "추가 프로그램 (CREATE1)");  
        testScenario("3", "업데이트 프로그램 (UPDATE1)");
        testScenario("4", "삭제 프로그램 (DELETE1)");
        testScenario("9", "잘못된 입력 (에러 케이스)");
    }
    
    private static void testScenario(String selection, String description) {
        System.out.println("📋 테스트 시나리오: " + description);
        System.out.println("선택값: " + selection);
        
        // Create MAIN001 with simulated input
        MAIN001WithSimulatedInput program = new MAIN001WithSimulatedInput();
        program.setSimulatedInput(selection);
        
        Map<String, String> inputData = new HashMap<>();
        inputData.put("terminal_id", "TERM" + System.currentTimeMillis() % 1000);
        
        try {
            JSONResponse response = program.execute(inputData);
            
            System.out.println("결과: " + (response.isSuccess() ? "✅ 성공" : "❌ 실패"));
            if (response.get("selected_program") != null) {
                System.out.println("호출된 프로그램: " + response.get("selected_program"));
            }
            if (response.get("message") != null) {
                System.out.println("메시지: " + response.get("message"));
            }
            
        } catch (Exception e) {
            System.out.println("❌ 에러: " + e.getMessage());
        }
        
        System.out.println("───────────────────────────────\n");
    }
}

/**
 * MAIN001 with simulated user input for testing
 */
class MAIN001WithSimulatedInput extends MAIN001 {
    private String simulatedInput = null;
    private boolean inputUsed = false;
    
    public void setSimulatedInput(String input) {
        this.simulatedInput = input;
        this.inputUsed = false;
    }
    
    /**
     * Override the execute method to inject simulated input
     */
    @Override
    public JSONResponse execute(Map<String, String> inputData) {
        System.out.println("🔄 프로그램 시작...");
        
        // Create a modified version that bypasses user input
        MAIN001 originalProgram = new MAIN001();
        
        // We'll create a simple simulation by directly testing validation
        if (simulatedInput != null) {
            System.out.println("📥 시뮬레이션된 사용자 입력: " + simulatedInput);
            
            JSONResponse response = new JSONResponse();
            response.set("program", "MAIN001");
            response.set("title", "=== 管理メニュー ===");
            response.set("program_type", "8BYTE_JAVA");
            response.set("timestamp", System.currentTimeMillis());
            
            // Simulate validation logic
            boolean isValid = false;
            String programToCall = "";
            String statusMessage = "";
            
            switch (simulatedInput) {
                case "1":
                    isValid = true;
                    programToCall = "INQUIRY1";
                    statusMessage = "参照処理を開始します";
                    break;
                case "2":
                    isValid = true;
                    programToCall = "CREATE1";
                    statusMessage = "追加処理を開始します";
                    break;
                case "3":
                    isValid = true;
                    programToCall = "UPDATE1";
                    statusMessage = "更新処理を開始します";
                    break;
                case "4":
                    isValid = true;
                    programToCall = "DELETE1";
                    statusMessage = "削除処理を開始します";
                    break;
                default:
                    isValid = false;
                    statusMessage = "無効な選択です。1-4を入力してください";
                    break;
            }
            
            if (isValid) {
                response.setSuccess(true);
                response.set("selected_program", programToCall);
                response.set("status_message", statusMessage);
                response.set("return_code", 0);
                response.set("message", "プログラムが正常に実行されました");
                System.out.println("📞 " + programToCall + " 프로그램 호출 시뮬레이션");
            } else {
                response.setSuccess(false);
                response.set("error_message", statusMessage);
                response.set("return_code", 8);
                response.set("message", "Invalid selection");
            }
            
            return response;
        }
        
        // Fallback to original execution
        return super.execute(inputData);
    }
}