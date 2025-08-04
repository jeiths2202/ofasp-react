       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMPLOYEE-INQUIRY.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  EMPLOYEE-RECORD.
           03  EMP-ID          PIC X(10).
           03  EMP-NAME        PIC X(20).
           03  EMP-DEPT        PIC X(15).
           03  EMP-SALARY      PIC 9(8)V99.
           03  EMP-STATUS      PIC X(1).

       01  SCREEN-FIELDS.
           03  TITLE-LINE      PIC X(30) VALUE "=== Employee Information ===".
           03  ID-LABEL        PIC X(10) VALUE "ID:".
           03  ID-FIELD        PIC X(10).
           03  NAME-LABEL      PIC X(10) VALUE "Name:".
           03  NAME-FIELD      PIC X(20).
           03  DEPT-LABEL      PIC X(15) VALUE "Department:".
           03  DEPT-FIELD      PIC X(15).
           03  SALARY-LABEL    PIC X(10) VALUE "Salary:".
           03  SALARY-FIELD    PIC 9(8)V99.
           03  MSG-LINE        PIC X(50).

       01  INPUT-FIELDS.
           03  SEARCH-ID       PIC X(10).
           03  UPDATE-NAME     PIC X(20).
           03  UPDATE-DEPT     PIC X(15).
           03  UPDATE-SALARY   PIC 9(8)V99.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM DISPLAY-SCREEN.
           PERFORM ACCEPT-INPUT.
           PERFORM PROCESS-REQUEST.
           PERFORM INTERACTIVE-PROCESS.
           STOP RUN.

       DISPLAY-SCREEN.
           MOVE "Employee Information Display" TO MSG-LINE.
           DISPLAY SCREEN-FIELDS.
           
       * DESTINATION specification for interactive processing
           MOVE "DISPLAY" TO DESTINATION-TYPE.

       ACCEPT-INPUT.
           MOVE "Enter employee ID to search:" TO MSG-LINE.
           ACCEPT INPUT-FIELDS.

       PROCESS-REQUEST.
           MOVE SEARCH-ID TO EMP-ID.
           PERFORM DATABASE-LOOKUP.
           
           IF EMP-ID NOT = SPACES
               MOVE EMP-NAME TO NAME-FIELD
               MOVE EMP-DEPT TO DEPT-FIELD
               MOVE EMP-SALARY TO SALARY-FIELD
               MOVE "Employee found and displayed" TO MSG-LINE
           ELSE
               MOVE "Employee not found" TO MSG-LINE
           END-IF.

       DATABASE-LOOKUP.
      *    Simulate database lookup
           IF SEARCH-ID = "EMP001"
               MOVE "田中太郎" TO EMP-NAME
               MOVE "開発部" TO EMP-DEPT
               MOVE 500000.00 TO EMP-SALARY
               MOVE "A" TO EMP-STATUS
           ELSE IF SEARCH-ID = "EMP002"
               MOVE "佐藤花子" TO EMP-NAME
               MOVE "営業部" TO EMP-DEPT
               MOVE 450000.00 TO EMP-SALARY
               MOVE "A" TO EMP-STATUS
           ELSE
               MOVE SPACES TO EMP-NAME
               MOVE SPACES TO EMP-DEPT
               MOVE ZEROS TO EMP-SALARY
               MOVE SPACES TO EMP-STATUS
           END-IF.

       INTERACTIVE-PROCESS.
      *    Handle interactive DESTINATION processing
           PERFORM UNTIL FUNCTION-KEY = "F3" OR "F12"
               DISPLAY SCREEN-FIELDS
               ACCEPT INPUT-FIELDS
               PERFORM PROCESS-REQUEST
           END-PERFORM.