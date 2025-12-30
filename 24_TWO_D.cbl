       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWO_D.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 TABLA.
           05 ROW OCCURS 3 TIMES.
             10 COLU PIC 99 OCCURS 4 TIMES.

       01 I PIC 9.
       01 J PIC 9.

       PROCEDURE DIVISION.
           MOVE 10 TO COLU(1,1)
           MOVE 12 TO COLU(1,2)
           MOVE 14 TO COLU(1,3)
           MOVE 15 TO COLU(1,4)
           MOVE 16 TO COLU(2,1)
           MOVE 17 TO COLU(2,2)
           MOVE 18 TO COLU(2,3)
           MOVE 19 TO COLU(2,4)
           MOVE 20 TO COLU(3,1)
           MOVE 30 TO COLU(3,2)
           MOVE 40 TO COLU(3,3)
           MOVE 50 TO COLU(3,4)

           DISPLAY "RESULT: "
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                 DISPLAY "("I","J") = " COLU(I,J)
               END-PERFORM
           END-PERFORM.

           STOP RUN.
