       IDENTIFICATION DIVISION.
       PROGRAM-ID. GRADEAVE.


       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 STUDENT.
           02 STUD OCCURS 2 TIMES.
            03 GRADE OCCURS 4 TIMES.

       01 I PIC 9.
       01 J PIC 9.

       01 TOTAL PIC 999 VALUE 0.
       01 AVERAGE PIC 99V99 VALUE 0.



       PROCEDURE DIVISION.

           MOVE 79 TO GRADE(1,1)
           MOVE 98 TO GRADE(1,2)
           MOVE 80 TO GRADE(1,3)
           MOVE 82 TO GRADE(1,4)

           MOVE 99 TO GRADE(2,1)
           MOVE 92 TO GRADE(2,2)
           MOVE 93 TO GRADE(2,3)
           MOVE 89 TO GRADE(2,4)

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 2
               DISPLAY "STUDENT #" I
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > 4
                   ADD GRADE(I,J) TO TOTAL
                   END-PERFORM
           DISPLAY "AVERAGE OF STUDENT #" I
           COMPUTE AVERAGE = TOTAL / 4
           DISPLAY AVERAGE
           END-PERFORM.

       STOP RUN.
