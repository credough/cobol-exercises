       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALSS.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OUTPUT-FILE
               ASSIGN TO "CELINDRO.TXT"
               ORGANIZATION IS SEQUENTIAL.


       DATA DIVISION.
       FILE SECTION.
       FD  OUTPUT-FILE.
       01  OUTPUT-REC.
           05  OUT-MONTH-NAME      PIC X(12).
           05  FILLER              PIC X(5) VALUE SPACES.
           05  OUT-TV-SHOW         PIC X(25).
           05  FILLER              PIC X(5) VALUE SPACES.
           05  OUT-TELEVIEWERS     PIC 9(6).

       WORKING-STORAGE SECTION.
       01  WS-SHOW-CODE            PIC 9.
       01  WS-SHOW-NAME            PIC X(25).
       01  WS-MONTH-CODE           PIC 99.
       01  WS-MONTH-NAME           PIC X(12).
       01  WS-TIME-CODE            PIC 9.
       01  WS-AIR-TIME             PIC X(10).
       01  WS-TELEVIEWERS          PIC 9(6).
       01  WS-CONTINUE             PIC X VALUE 'Y'.

       01  WS-TOTAL-VIEWERS        PIC 9(6) VALUE 0.
       01  WS-MAX-VIEWERS          PIC 9(6) VALUE 0.
       01  WS-MOST-WATCHED-SHOW    PIC X(25).
       01  WS-MOST-WATCHED-TIME    PIC X(10).

       PROCEDURE DIVISION.
           OPEN OUTPUT OUTPUT-FILE.

           PERFORM UNTIL WS-CONTINUE NOT = 'Y'

               DISPLAY "Show Code (1-Eat Bulaga, 2-Its Showtime,"
               DISPLAY "3-PBB, 4-Probinsiano): " WITH NO ADVANCING
               ACCEPT WS-SHOW-CODE

               EVALUATE WS-SHOW-CODE
                   WHEN 1
                       MOVE "Eat Bulaga" TO WS-SHOW-NAME
                   WHEN 2
                       MOVE "Its Showtime" TO WS-SHOW-NAME
                   WHEN 3
                       MOVE "PBB" TO WS-SHOW-NAME
                   WHEN 4
                       MOVE "Probinsiano" TO WS-SHOW-NAME
                   WHEN OTHER
                       MOVE "Unknown" TO WS-SHOW-NAME
               END-EVALUATE

               DISPLAY "Month Code (01-12): " WITH NO ADVANCING
               ACCEPT WS-MONTH-CODE

               EVALUATE WS-MONTH-CODE
                   WHEN 01
                       MOVE "January" TO WS-MONTH-NAME
                   WHEN 02
                       MOVE "February" TO WS-MONTH-NAME
                   WHEN 03
                       MOVE "March" TO WS-MONTH-NAME
                   WHEN 04
                       MOVE "April" TO WS-MONTH-NAME
                   WHEN 05
                       MOVE "May" TO WS-MONTH-NAME
                   WHEN 06
                       MOVE "June" TO WS-MONTH-NAME
                   WHEN 07
                       MOVE "July" TO WS-MONTH-NAME
                   WHEN 08
                       MOVE "August" TO WS-MONTH-NAME
                   WHEN 09
                       MOVE "September" TO WS-MONTH-NAME
                   WHEN 10
                       MOVE "October" TO WS-MONTH-NAME
                   WHEN 11
                       MOVE "November" TO WS-MONTH-NAME
                   WHEN 12
                       MOVE "December" TO WS-MONTH-NAME
                   WHEN OTHER
                       MOVE "Unknown" TO WS-MONTH-NAME
               END-EVALUATE

               DISPLAY "Time Code (1 or 2): "
               ACCEPT WS-TIME-CODE

               EVALUATE WS-TIME-CODE
                   WHEN 1
                       MOVE "1200-2:30pm" TO WS-AIR-TIME
                   WHEN 2
                       MOVE "8:00-10:00pm" TO WS-AIR-TIME
                   WHEN OTHER
                       MOVE "Unknown" TO WS-AIR-TIME
               END-EVALUATE

               DISPLAY "Number of Televiewers: " WITH NO ADVANCING
               ACCEPT WS-TELEVIEWERS

               MOVE WS-MONTH-NAME TO OUT-MONTH-NAME
               MOVE WS-SHOW-NAME TO OUT-TV-SHOW
               MOVE WS-TELEVIEWERS TO OUT-TELEVIEWERS
               WRITE OUTPUT-REC

               ADD WS-TELEVIEWERS TO WS-TOTAL-VIEWERS

               IF WS-TELEVIEWERS > WS-MAX-VIEWERS
                   MOVE WS-TELEVIEWERS TO WS-MAX-VIEWERS
                   MOVE WS-SHOW-NAME TO WS-MOST-WATCHED-SHOW
                   MOVE WS-AIR-TIME TO WS-MOST-WATCHED-TIME
               END-IF

               DISPLAY " "
               DISPLAY "Input Another Record (Y/N)? "
                   WITH NO ADVANCING
               ACCEPT WS-CONTINUE
           END-PERFORM

           DISPLAY " "
           DISPLAY "========================================="
           DISPLAY "   Television's Leading Primetime Shows"
           DISPLAY "   ABS-CBN and GMA Network"
           DISPLAY "   "
           DISPLAY "   January-December, 2025 Survey"
           DISPLAY "========================================="
           DISPLAY "Most Watched TV Show: " WS-MOST-WATCHED-SHOW
           DISPLAY "Air Time: " WS-MOST-WATCHED-TIME
           DISPLAY "Total no. of Televiewers: " WS-TOTAL-VIEWERS
           DISPLAY "========================================="
           DISPLAY " "
           DISPLAY "Data saved to CELINDRO.TXT"

           CLOSE OUTPUT-FILE.
           STOP RUN.
