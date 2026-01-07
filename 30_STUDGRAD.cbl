       IDENTIFICATION DIVISION.
       PROGRAM-ID. STUDENT-GRADES.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 SNO         PIC 9(5).
       01 SNA         PIC X(25).
       01 COURSE      PIC X(5).
       01 YEAR-LVL    PIC 9.
       01 SECTIONS    PIC 9.

       01 MIDTERM     PIC 9V99.
       01 FINALS      PIC 9V99.
       01 AVERAGE     PIC 9V99.

       01 REMARKS     PIC X(10).
       01 CHOICE      PIC X VALUE 'Y'.

       PROCEDURE DIVISION.

       MAIN-PARA.
           PERFORM UNTIL CHOICE NOT = 'Y'

            DISPLAY "Enter Student Number: "
            ACCEPT SNO

            DISPLAY "Enter Student Name: "
            ACCEPT SNA

            DISPLAY "Enter Course: "
            ACCEPT COURSE

            DISPLAY "Enter Year Level: "
            ACCEPT YEAR-LVL

            DISPLAY "Enter Section: "
            ACCEPT SECTIONS

            DISPLAY "Enter Midterm Grade: "
            ACCEPT MIDTERM

            DISPLAY "Enter Final Grade: "
            ACCEPT FINALS

            COMPUTE AVERAGE = (MIDTERM + FINALS) / 2

            IF AVERAGE < 3.01
                MOVE "PASSED" TO REMARKS
            ELSE
                MOVE "FAILED" TO REMARKS
            END-IF

            DISPLAY "---------------------------"
            DISPLAY "STUDENT NUMBER : " SNO
            DISPLAY "NAME           : " SNA
            DISPLAY "COURSE         : " COURSE
            DISPLAY "YEAR & SECTION : " YEAR-LVL "-" SECTIONS
            DISPLAY "MIDTERM GRADE  : " MIDTERM
            DISPLAY "FINAL GRADE    : " FINALS
            DISPLAY "AVERAGE        : " AVERAGE
            DISPLAY "REMARKS        : " REMARKS
            DISPLAY "---------------------------"

            DISPLAY "DO YOU WANT TO ENTER ANOTHER RECORD? (Y/N)"
            ACCEPT CHOICE

           END-PERFORM.

           STOP RUN.
