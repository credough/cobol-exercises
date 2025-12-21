       IDENTIFICATION DIVISION.
       PROGRAM-ID. LAMESA.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE  ASSIGN TO "BILANG.TXT".
           SELECT OUTFILE ASSIGN TO "DAMI.TXT".

       DATA DIVISION.
       FILE SECTION.

       FD INFILE.
       01 DAGDAG.
          05 YR  PIC 9.
          05 CC  PIC X(5).
          05 SNO PIC X(10).
          05 SNA PIC X(25).

       FD OUTFILE.
       01 BAWAS PIC X(80).

       WORKING-STORAGE SECTION.

       01 EOFSW PIC 9 VALUE 0.

       01 TABLE-YEAR.
          05 YEAR-ENTRY OCCURS 4 TIMES.
             10 BSCS-CNT PIC 9(4) VALUE 0.
             10 BSIT-CNT PIC 9(4) VALUE 0.

       01 TOTAL-YEAR-TABLE.
          05 TOTAL-YEAR OCCURS 4 TIMES PIC 9(4) VALUE 0.

       01 TOTAL-BSCS PIC 9(4) VALUE 0.
       01 TOTAL-BSIT PIC 9(4) VALUE 0.

       01 I PIC 9 VALUE 1.

       01 LINE-OUT.
          05 OUT-YEAR PIC X(20).
          05 OUT-BSCS PIC 9(4).
          05 FILLER   PIC X(3) VALUE SPACE.
          05 OUT-BSIT PIC 9(4).
          05 FILLER   PIC X(3) VALUE SPACE.
          05 OUT-TOT  PIC 9(4).

       PROCEDURE DIVISION.
       MAIN.
           OPEN INPUT INFILE
           OPEN OUTPUT OUTFILE
           PERFORM READ-REC
           PERFORM PROCESS UNTIL EOFSW = 1
           PERFORM PRINT-ALL
           CLOSE INFILE
           CLOSE OUTFILE
           STOP RUN.

       READ-REC.
           READ INFILE
               AT END MOVE 1 TO EOFSW
           END-READ.

       PROCESS.
           MOVE YR TO I

           IF CC = "BSCS"
           ADD 1 TO BSCS-CNT (I)
           ADD 1 TO TOTAL-BSCS
           ELSE
           IF CC = "BSIT"
            ADD 1 TO BSIT-CNT (I)
            ADD 1 TO TOTAL-BSIT
           END-IF
           END-IF

           ADD 1 TO TOTAL-YEAR (I)
           PERFORM READ-REC.


       PRINT-ALL.
           MOVE 1 TO I
           PERFORM PRINT-YEAR UNTIL I > 4
           PERFORM PRINT-TOTAL.

       PRINT-YEAR.
           MOVE SPACES TO LINE-OUT

           IF I = 1 MOVE "Freshman"  TO OUT-YEAR END-IF
           IF I = 2 MOVE "Sophomore" TO OUT-YEAR END-IF
           IF I = 3 MOVE "Junior"    TO OUT-YEAR END-IF
           IF I = 4 MOVE "Senior"    TO OUT-YEAR END-IF

           MOVE BSCS-CNT (I)   TO OUT-BSCS
           MOVE BSIT-CNT (I)   TO OUT-BSIT
           MOVE TOTAL-YEAR (I) TO OUT-TOT

           WRITE BAWAS FROM LINE-OUT
           ADD 1 TO I.

       PRINT-TOTAL.
           MOVE SPACES TO LINE-OUT
           MOVE "TOTAL"      TO OUT-YEAR
           MOVE TOTAL-BSCS   TO OUT-BSCS
           MOVE TOTAL-BSIT   TO OUT-BSIT
           MOVE TOTAL-BSCS   TO OUT-TOT
           ADD  TOTAL-BSIT   TO OUT-TOT
           WRITE BAWAS FROM LINE-OUT.
