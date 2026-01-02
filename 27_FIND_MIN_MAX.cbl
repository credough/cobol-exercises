       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINDMINMAX.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUMB.
           05 NUM PIC 999 OCCURS 5 TIMES.
       01 I PIC 9 VALUE 1.

       01 MIN PIC 99 VALUE 0.
       01 MAX PIC 99 VALUE 0.

       PROCEDURE DIVISION.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
           ACCEPT NUM(I)
           END-PERFORM

           MOVE NUM(1) TO MAX
           MOVE NUM(1) TO MIN

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 5
               IF NUM(I) > MAX
                   MOVE NUM(I) TO MAX
               END-IF

               IF NUM(I) < MIN
                   MOVE NUM(I) TO MIN
               END-IF
           END-PERFORM

           DISPLAY "MAX:" MAX
           DISPLAY "MIN:" MIN

           STOP RUN.
