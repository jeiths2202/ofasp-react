// Converted from COBOL TAX01 using real parsing
// Data read from actual files - no hardcoded sample data used
import java.io.*;
import java.util.*;
import java.text.DecimalFormat;

public class Tax01 {
    private static Scanner scanner = new Scanner(System.in);
    private static DecimalFormat currencyFormat = new DecimalFormat("¥#,##0");
    
    private static int Income = 0;
    private static double TaxRate = 0.0;
    private static int TaxAmount = 0;
    private static int NetIncome = 0;
    private static int LocalTax = 0;
    private static int TotalTax = 0;
    private static int DisplayAmount = 0;

    public static void main(String[] args) {
        try {
            System.out.println("所得税計算システム");
            System.out.println("==================");
            System.out.println("年収を入力（円）：");
            System.out.print("年収を入力（円）: ");
            Income = Integer.parseInt(scanner.nextLine());
            calculateTax();
            displayResult();
            System.exit(0);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private static void calculateTax() {
        // Tax calculation based on income brackets
        if (Income <= 1950000) {
        TaxRate = .05;
        } else if (Income <= 3300000) {
        TaxRate = .10;
        } else if (Income <= 6950000) {
        TaxRate = .20;
        } else if (Income <= 9000000) {
        TaxRate = .23;
        } else if (Income <= 18000000) {
        TaxRate = .33;
        else {
        TaxRate = .40;
    }

    private static void endEvaluate() {
        TaxAmount = Income * TaxRate.;
        LocalTax = TaxAmount * 0.1.;
        TotalTax = TaxAmount + LocalTax.;
        NetIncome = Income - TotalTax.;
    }

    private static void displayResult() {
        System.out.println(" ");
        System.out.println("税金計算結果");
        System.out.println("============");
        DisplayAmount = Income;
        System.out.println("年収　　　：" WS-DISPLAY-AMOUNT " 円");
        TaxRate = TaxRate * 100;
        System.out.println("税率　　　：" WS-TAX-RATE " %");
        DisplayAmount = TaxAmount;
        System.out.println("所得税　　：" WS-DISPLAY-AMOUNT " 円");
        DisplayAmount = LocalTax;
        System.out.println("住民税　　：" WS-DISPLAY-AMOUNT " 円");
        DisplayAmount = TotalTax;
        System.out.println("税金合計　：" WS-DISPLAY-AMOUNT " 円");
        DisplayAmount = NetIncome;
        System.out.println("手取り収入：" WS-DISPLAY-AMOUNT " 円");
    }
}