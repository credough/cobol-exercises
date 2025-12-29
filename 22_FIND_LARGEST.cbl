       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND_LARGEST.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMBE.
           05 NUM PIC 99 OCCURS 5 TIMES.
       01 I PIC 99.
       01 MAX PIC 99 VALUE 0.

       PROCEDURE DIVISION.
           MOVE 15 TO NUM(1)
           MOVE 20 TO NUM(2)
           MOVE 12 TO NUM(3)
           MOVE 16 TO NUM(4)
           MOVE 32 TO NUM(5)

           MOVE NUM(1) TO MAX

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF NUM(I) > MAX
                   MOVE NUM(I) TO MAX
               END-IF
           END-PERFORM.

           DISPLAY "MAX = " MAX.
           STOP RUN.
