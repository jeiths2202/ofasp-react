      IDENTIFICATION DIVISION.
       PROGRAM-ID. COBDSPFL.
       AUTHOR. YOURNAME.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE ASSIGN TO DA-S-INFILE.
           SELECT DSPF   ASSIGN TO GS-DSPF
               ORGANIZATION IS SEQUENTIAL
               PROCESSING MODE IS DISPLAY
               SYMBOLIC DESTINATION IS "DSP".

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           LABEL RECORD STANDARD.
       01  INFILE-REC.
           05  IN-ITEM-ID          PIC X(10).
           05  IN-ITEM-NAME        PIC X(20).
           05  IN-ITEM-QTY         PIC 9(05).

       FD  DSPF.
           COPY MITDSP OF XMDLIB JOINING DSP AS PREFIX.

       WORKING-STORAGE SECTION.
       01  WS-CONTROL-FIELDS.
           05  WS-DSPF-FORMAT-NAME   PIC X(08) VALUE 'MITDSP'.
           05  WS-DSPF-GROUP-NAME    PIC X(08) VALUE 'GROUP1'.
           05  WS-FILE-STATUS        PIC X(02). 
           05  WS-EOF-FLAG           PIC X(01) VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT INFILE.
           OPEN I-O DSPF.

           MOVE WS-DSPF-FORMAT-NAME TO DSP-FORMAT-NAME OF DSPF-RECORD.
           MOVE WS-DSPF-GROUP-NAME TO DSP-GROUP-NAME OF DSPF-RECORD.
           WRITE DSPF-RECORD.
           MOVE FILE-STATUS OF DSPF TO WS-FILE-STATUS.
           IF WS-FILE-STATUS NOT = '00' THEN
               DISPLAY 'DSPF 초기 WRITE 오류: ' WS-FILE-STATUS
               PERFORM END-PROGRAM
           END-IF.

           PERFORM UNTIL WS-EOF-FLAG = 'Y'
               READ INFILE INTO INFILE-REC
                   AT END
                       MOVE 'Y' TO WS-EOF-FLAG
                   NOT AT END
                       MOVE IN-ITEM-ID     OF INFILE-REC 
                                 TO DSP-ITEM-ID OF DSPF-RECORD
                       MOVE IN-ITEM-NAME   OF INFILE-REC 
                                 TO DSP-ITEM-NAME OF DSPF-RECORD
                       MOVE IN-ITEM-QTY    OF INFILE-REC 
                                 TO DSP-ITEM-QTY OF DSPF-RECORD

                       WRITE DSPF-RECORD.
                       MOVE FILE-STATUS OF DSPF TO WS-FILE-STATUS
                       IF WS-FILE-STATUS NOT = '00' THEN
                           DISPLAY 'DSPF WRITE ERRIR ' WS-FILE-STATUS
                           PERFORM END-PROGRAM
                       END-IF.
               END-READ
           END-PERFORM.

       END-PROGRAM.
           CLOSE INFILE.
           CLOSE DSPF.
           STOP RUN.