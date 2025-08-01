       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN001.
      *****************************************************************
      * MAIN MENU PROGRAM - FUJITSU ASP COBOLG DISPLAY FILE         *
      * PROCESSES MAIN001 SMED MAP FOR MENU SELECTION               *
      * SUPPORTS OPTIONS 1-4 WITH PROPER ERROR HANDLING             *
      * USES DESTINATION IDS DSP PATTERN WITH SJIS ENCODING         *
      *****************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT MAIN001 ASSIGN TO "MAIN001"
                  ORGANIZATION IS DISPLAY FILE
                  DESTINATION IDS "DSP"
                  ENCODING SJIS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  MAIN001
       COPY MITDSP OF XMLLIB
       JOINING DSP AS PREFIX.
       
       WORKING-STORAGE SECTION.
       01  WS-SCREEN-FIELDS.
           05  WS-TITLE-LINE       PIC X(30) 
               VALUE "=== 管理メニュー ===".
           05  WS-OPTION-1         PIC X(20) VALUE "１）参照".
           05  WS-OPTION-2         PIC X(20) VALUE "２）追加".
           05  WS-OPTION-3         PIC X(20) VALUE "３）更新".
           05  WS-OPTION-4         PIC X(20) VALUE "４）削除".
           05  WS-SELECTION-PROMPT PIC X(20) VALUE "選択：".
           05  WS-MESSAGE-LINE     PIC X(50).
       
       01  WS-INPUT-DATA.
           05  WS-USER-SELECTION   PIC X(1).
       
       01  WS-CONTROL-FIELDS.
           05  WS-VALID-OPTION     PIC X(1) VALUE 'N'.
           05  WS-PROGRAM-TO-CALL  PIC X(8).
           05  WS-RETRY-COUNT      PIC 9(2) VALUE 0.
           05  WS-MAX-RETRIES      PIC 9(2) VALUE 3.
           05  DESTINATION-TYPE    PIC X(20) VALUE "DSP".
           05  WS-ENCODING-TYPE    PIC X(10) VALUE "SJIS".
       
       01  WS-MESSAGE-AREA.
           05  WS-ERROR-MESSAGE    PIC X(50).
           05  WS-STATUS-MESSAGE   PIC X(50).
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           PERFORM INIT-PROGRAM
           PERFORM OPEN-DISPLAY-FILE
           PERFORM PROCESS-MAIN-MENU
           PERFORM CLOSE-DISPLAY-FILE
           STOP RUN
           .
       
       INIT-PROGRAM.
           MOVE "DSP" TO DESTINATION-TYPE
           MOVE "SJIS" TO WS-ENCODING-TYPE
           MOVE SPACES TO WS-ERROR-MESSAGE
           MOVE SPACES TO WS-STATUS-MESSAGE
           MOVE ZEROS TO WS-RETRY-COUNT
           MOVE 'N' TO WS-VALID-OPTION
           .
       
       OPEN-DISPLAY-FILE.
           OPEN OUTPUT MAIN001
           .
       
       CLOSE-DISPLAY-FILE.
           CLOSE MAIN001
           .
       
       PROCESS-MAIN-MENU.
           PERFORM UNTIL WS-VALID-OPTION = 'Y' 
                     OR WS-RETRY-COUNT >= WS-MAX-RETRIES
               PERFORM DISPLAY-MENU-MAP
               PERFORM ACCEPT-USER-INPUT
               PERFORM VALIDATE-SELECTION
               IF WS-VALID-OPTION = 'N'
                   ADD 1 TO WS-RETRY-COUNT
                   PERFORM DISPLAY-ERROR-MSG
               END-IF
           END-PERFORM
           
           IF WS-VALID-OPTION = 'Y'
               PERFORM CALL-PROGRAM
           ELSE
               MOVE "最大試行回数に達しました" TO WS-MESSAGE-LINE
               DISPLAY WS-MESSAGE-LINE
           END-IF
           .
       
       DISPLAY-MENU-MAP.
      *    FUJITSU ASP COBOLG DISPLAY FILE MAP OUTPUT WITH DSP
           MOVE "DSP" TO DESTINATION-TYPE
           MOVE WS-TITLE-LINE TO WS-MESSAGE-LINE
           DISPLAY WS-SCREEN-FIELDS
           WRITE MAIN001-MAP
           .
       
       ACCEPT-USER-INPUT.
      *    ACCEPT INPUT FROM USER
           MOVE "選択を入力してください (1-4):" TO WS-MESSAGE-LINE
           DISPLAY WS-MESSAGE-LINE
           ACCEPT WS-USER-SELECTION
           .
       
       VALIDATE-SELECTION.
           MOVE 'N' TO WS-VALID-OPTION
           MOVE SPACES TO WS-ERROR-MESSAGE
           
           EVALUATE WS-USER-SELECTION
               WHEN '1'
                   MOVE 'Y' TO WS-VALID-OPTION
                   MOVE 'INQUIRY1' TO WS-PROGRAM-TO-CALL
                   MOVE "参照処理を開始します" TO WS-STATUS-MESSAGE
               WHEN '2'
                   MOVE 'Y' TO WS-VALID-OPTION
                   MOVE 'CREATE1 ' TO WS-PROGRAM-TO-CALL
                   MOVE "追加処理を開始します" TO WS-STATUS-MESSAGE
               WHEN '3'
                   MOVE 'Y' TO WS-VALID-OPTION
                   MOVE 'UPDATE1 ' TO WS-PROGRAM-TO-CALL
                   MOVE "更新処理を開始します" TO WS-STATUS-MESSAGE
               WHEN '4'
                   MOVE 'Y' TO WS-VALID-OPTION
                   MOVE 'DELETE1 ' TO WS-PROGRAM-TO-CALL
                   MOVE "削除処理を開始します" TO WS-STATUS-MESSAGE
               WHEN OTHER
                   MOVE 'N' TO WS-VALID-OPTION
                   MOVE "無効な選択です。1-4を入力してください"
                        TO WS-ERROR-MESSAGE
           END-EVALUATE
           .
       
       DISPLAY-ERROR-MSG.
           MOVE WS-ERROR-MESSAGE TO WS-MESSAGE-LINE
           DISPLAY "エラー: " WS-MESSAGE-LINE
           DISPLAY "再試行してください (" WS-RETRY-COUNT " / " 
                   WS-MAX-RETRIES ")"
           .
       
       CALL-PROGRAM.
           DISPLAY WS-STATUS-MESSAGE
           
           EVALUATE WS-USER-SELECTION
               WHEN '1'
                   CALL 'INQUIRY1'
               WHEN '2'
                   CALL 'CREATE1'
               WHEN '3'
                   CALL 'UPDATE1'
               WHEN '4'
                   CALL 'DELETE1'
           END-EVALUATE
           
           IF RETURN-CODE NOT = 0
               DISPLAY "プログラム呼び出しエラー: " WS-PROGRAM-TO-CALL
               DISPLAY "リターンコード: " RETURN-CODE
           END-IF
           .