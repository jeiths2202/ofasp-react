package com.openasp.main;

import com.openasp.cobolg.data.CobolField;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

/**
 * MITDSP copybook record definition
 * Converted from COBOL COPY MITDSP OF XMLLIB
 * 
 * Original COBOL structure:
 * 01  MAIN001.
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  MENU-TITLE      PIC X(20) VALUE "管理メニュー".
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  OPTION-1        PIC X(20) VALUE "１）参照".
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  OPTION-2        PIC X(20) VALUE "２）追加".
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  OPTION-3        PIC X(20) VALUE "３）更新".
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  OPTION-4        PIC X(20) VALUE "４）削除".
 *     05  FILLER          PIC X(1) VALUE SPACE.
 *     05  SELECTION-PROMPT PIC X(20) VALUE "選択：".
 *     05  EMP-FUNC        PIC X(1).
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class MitdspRecord {
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler1 = " ";
    
    // COBOL: 05  MENU-TITLE      PIC X(20) VALUE "管理メニュー".
    @CobolField(pic = "X(20)", level = 5, name = "MENU-TITLE", value = "管理メニュー")
    private String menuTitle = "管理メニュー";
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler2 = " ";
    
    // COBOL: 05  OPTION-1        PIC X(20) VALUE "１）参照".
    @CobolField(pic = "X(20)", level = 5, name = "OPTION-1", value = "１）参照")
    private String option1 = "１）参照";
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler3 = " ";
    
    // COBOL: 05  OPTION-2        PIC X(20) VALUE "２）追加".
    @CobolField(pic = "X(20)", level = 5, name = "OPTION-2", value = "２）追加")
    private String option2 = "２）追加";
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler4 = " ";
    
    // COBOL: 05  OPTION-3        PIC X(20) VALUE "３）更新".
    @CobolField(pic = "X(20)", level = 5, name = "OPTION-3", value = "３）更新")
    private String option3 = "３）更新";
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler5 = " ";
    
    // COBOL: 05  OPTION-4        PIC X(20) VALUE "４）削除".
    @CobolField(pic = "X(20)", level = 5, name = "OPTION-4", value = "４）削除")
    private String option4 = "４）削除";
    
    // COBOL: 05  FILLER          PIC X(1) VALUE SPACE.
    @CobolField(pic = "X(1)", level = 5, value = " ")
    private String filler6 = " ";
    
    // COBOL: 05  SELECTION-PROMPT PIC X(20) VALUE "選択：".
    @CobolField(pic = "X(20)", level = 5, name = "SELECTION-PROMPT", value = "選択：")
    private String selectionPrompt = "選択：";
    
    // COBOL: 05  EMP-FUNC        PIC X(1).
    @CobolField(pic = "X(1)", level = 5, name = "EMP-FUNC")
    private String empFunc = "";
    
    /**
     * Create a copy of this record with default menu values
     */
    public static MitdspRecord createDefaultRecord() {
        MitdspRecord record = new MitdspRecord();
        record.setFiller1(" ");
        record.setMenuTitle("管理メニュー");
        record.setFiller2(" ");
        record.setOption1("１）参照");
        record.setFiller3(" ");
        record.setOption2("２）追加");
        record.setFiller4(" ");
        record.setOption3("３）更新");
        record.setFiller5(" ");
        record.setOption4("４）削除");
        record.setFiller6(" ");
        record.setSelectionPrompt("選択：");
        record.setEmpFunc("");
        return record;
    }
    
    /**
     * Get total record length (for validation)
     */
    public static int getRecordLength() {
        return 1 + 20 + 1 + 20 + 1 + 20 + 1 + 20 + 1 + 20 + 1 + 20 + 1; // 127 characters
    }
    
    /**
     * Clear input fields (keep display fields, clear input fields)
     */
    public void clearInputFields() {
        this.empFunc = "";
    }
    
    /**
     * Validate EMP-FUNC value
     */
    public boolean isValidEmpFunc() {
        return empFunc != null && 
               (empFunc.equals("1") || empFunc.equals("2") || 
                empFunc.equals("3") || empFunc.equals("4"));
    }
}