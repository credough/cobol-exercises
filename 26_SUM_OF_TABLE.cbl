       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUMOFTWODIME.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 MATRIX.
           05 ROW OCCURS 2 TIMES.
            10 COLL PIC 99 OCCURS 3 TIMES.

       01 I PIC 9.
       01 J PIC 9.
       01 TOTAL-SUM PIC 999 VALUE 0.

       PROCEDURE DIVISION.
           MOVE 10 TO COLL(1,1)
           MOVE 20 TO COLL(2,1)
           MOVE 30 TO COLL(1,2)
           MOVE 40 TO COLL(2,2)
           MOVE 50 TO COLL(1,3)
           MOVE 60 TO COLL(2,3)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 3
                   ADD COLL(I,J) TO TOTAL-SUM
               END-PERFORM
           END-PERFORM.

           DISPLAY TOTAL-SUM

           STOP RUN.
