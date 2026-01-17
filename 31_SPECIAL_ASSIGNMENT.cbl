       IDENTIFICATION DIVISION.
       PROGRAM-ID. SPECIAL.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTFILE ASSIGN TO "celindro.txt"
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD OUTFILE.
       01 OUTREC            PIC X(120).
       WORKING-STORAGE SECTION.
       01 WS-COUNT          PIC 9 VALUE 0.
       01 WS-MAX            PIC 9 VALUE 5.
       01 TOTAL-PASSED      PIC 99 VALUE 0.
       01 TOTAL-FAILED      PIC 99 VALUE 0.
       01 PASSING-SCORE     PIC 9(3).
       01 EXAMINO           PIC 9(10).
       01 EXAMINA           PIC X(25).
       01 DATEOFBIRTH       PIC X(20).
       01 UNICODE           PIC 9.
       01 UNINAME           PIC X(5).
       01 COUDE             PIC 9.
       01 COUNA             PIC X(4).
       01 TOTAL-IN          PIC 9(3).
       01 TEST-RES          PIC 9(3).
       01 REMARK            PIC X(6).
       01 REPLY             PIC X.
       PROCEDURE DIVISION.
           OPEN OUTPUT OUTFILE
           MOVE "Professional Regulation Commission" TO OUTREC
           WRITE OUTREC
           MOVE " " TO OUTREC
           WRITE OUTREC
           MOVE " " TO OUTREC
           WRITE OUTREC
           MOVE "IT Professional Board Exam Result" TO OUTREC
           WRITE OUTREC
           MOVE " " TO OUTREC
           WRITE OUTREC
           MOVE SPACES TO OUTREC
           WRITE OUTREC
           PERFORM UNTIL WS-COUNT = WS-MAX
               ADD 1 TO WS-COUNT
               DISPLAY "EXAMINEE NUMBER: "
               ACCEPT EXAMINO
               DISPLAY "EXAMINEE NAME: "
               ACCEPT EXAMINA
               DISPLAY "DATE OF BIRTH: "
               ACCEPT DATEOFBIRTH
               DISPLAY "UNIVERSITY CODE (1-5): "
               ACCEPT UNICODE
               IF UNICODE = 1
                   MOVE "UP" TO UNINAME
               ELSE
               IF UNICODE = 2
                   MOVE "PUP" TO UNINAME
               ELSE
               IF UNICODE = 3
                   MOVE "DLSU" TO UNINAME
               ELSE
               IF UNICODE = 4
                   MOVE "ADMU" TO UNINAME
               ELSE
               IF UNICODE = 5
                   MOVE "MAPUA" TO UNINAME
               ELSE
                   MOVE "UNK" TO UNINAME
               END-IF END-IF END-IF END-IF END-IF
               DISPLAY "COURSE CODE (1-3): "
               ACCEPT COUDE
               IF COUDE = 1
                   MOVE "BSIT" TO COUNA
               ELSE
               IF COUDE = 2
                   MOVE "BSCS" TO COUNA
               ELSE
               IF COUDE = 3
                   MOVE "BSIS" TO COUNA
               ELSE
                   MOVE "UNK" TO COUNA
               END-IF END-IF END-IF
               DISPLAY "TOTAL NO. OF ITEMS: "
               ACCEPT TOTAL-IN
               DISPLAY "TEST RESULT (SCORE): "
               ACCEPT TEST-RES
               IF COUDE = 1
                   COMPUTE PASSING-SCORE =
                       (TOTAL-IN * 60) / 100
               ELSE
               IF COUDE = 2
                   COMPUTE PASSING-SCORE =
                       (TOTAL-IN * 70) / 100
               ELSE
               IF COUDE = 3
                   COMPUTE PASSING-SCORE =
                       (TOTAL-IN * 50) / 100
               END-IF END-IF END-IF
               IF TEST-RES >= PASSING-SCORE
                   MOVE "PASSED" TO REMARK
                   ADD 1 TO TOTAL-PASSED
               ELSE
                   MOVE "FAILED" TO REMARK
                   ADD 1 TO TOTAL-FAILED
               END-IF
               MOVE SPACES TO OUTREC
               STRING "Examinee Number:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   EXAMINO DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Examinee Name:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   EXAMINA DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Date of Birth:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   DATEOFBIRTH DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "University Code:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   UNICODE DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "University Name:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   UNINAME DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Course Code:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   COUDE DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Course Name:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   COUNA DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Total No. of Items:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   TOTAL-IN DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Test Result (Score):" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   TEST-RES DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               STRING "Remarks:" DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   REMARK DELIMITED BY SIZE
                   INTO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               WRITE OUTREC
               DISPLAY "Input Another Record (Y/N)?"
               ACCEPT REPLY
               MOVE "Input Another Record (Y/N)?" TO OUTREC
               WRITE OUTREC
               MOVE SPACES TO OUTREC
               WRITE OUTREC
           END-PERFORM
           MOVE SPACES TO OUTREC
           STRING "Total No. of Passed: " DELIMITED BY SIZE
               TOTAL-PASSED DELIMITED BY SIZE
               INTO OUTREC
           WRITE OUTREC
           MOVE SPACES TO OUTREC
           STRING "Total No. of Failed: " DELIMITED BY SIZE
               TOTAL-FAILED DELIMITED BY SIZE
               INTO OUTREC
           WRITE OUTREC
           CLOSE OUTFILE
           STOP RUN.
