       IDENTIFICATION DIVISION.
       PROGRAM-ID. TWODIMETABLE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARRAY.
           05 ARAYMO OCCURS 4 TIMES.
              10 ARS PIC 99 OCCURS 5 TIMES.

       01 I PIC 9.
       01 J PIC 9.

       PROCEDURE DIVISION.
           MOVE 10 TO ARS(1,1)
           MOVE 15 TO ARS(1,2)
           MOVE 20 TO ARS(1,3)
           MOVE 25 TO ARS(1,4)
           MOVE 30 TO ARS(1,5)
           MOVE 35 TO ARS(2,1)
           MOVE 40 TO ARS(2,2)
           MOVE 50 TO ARS(2,3)
           MOVE 12 TO ARS(2,4)
           MOVE 11 TO ARS(2,5)
           MOVE 48 TO ARS(3,1)
           MOVE 99 TO ARS(3,2)
           MOVE 98 TO ARS(3,3)
           MOVE 96 TO ARS(3,4)
           MOVE 88 TO ARS(3,5)
           MOVE 56 TO ARS(4,1)
           MOVE 90 TO ARS(4,2)
           MOVE 44 TO ARS(4,3)
           MOVE 43 TO ARS(4,4)
           MOVE 42 TO ARS(4,5)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 6
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 5
                   DISPLAY "ARR("I","J") = " ARS(I,J)
                   END-PERFORM
           END-PERFORM.

           STOP RUN.
