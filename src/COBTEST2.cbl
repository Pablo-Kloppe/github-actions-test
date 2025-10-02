IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTEST2.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-NUM1          PIC 9(5) VALUE 0.
       01  WS-NUM2          PIC 9(5) VALUE 0.
       01  WS-SUM           PIC 9(6).
       01  WS-DIFF          PIC S9(6).
       01  WS-PROD          PIC S9(10).
       01  WS-QUOT          PIC 9(6)V9(2).
       01  WS-TEMP          PIC X(10).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           DISPLAY "== COBTEST2 - agent smoke test calculator =="

           DISPLAY "Enter first number: " WITH NO ADVANCING
           ACCEPT WS-TEMP
           MOVE FUNCTION NUMVAL(WS-TEMP) TO WS-NUM1

           DISPLAY "Enter second number: " WITH NO ADVANCING
           ACCEPT WS-TEMP
           MOVE FUNCTION NUMVAL(WS-TEMP) TO WS-NUM2

           COMPUTE WS-SUM  = WS-NUM1 + WS-NUM2
           COMPUTE WS-DIFF = WS-NUM1 - WS-NUM2
           COMPUTE WS-PROD = WS-NUM1 * WS-NUM2

           IF WS-NUM2 NOT = 0
              COMPUTE WS-QUOT = WS-NUM1 / WS-NUM2
              DISPLAY "Quotient: " WS-QUOT
           ELSE
              DISPLAY "Division by zero not allowed."
           END-IF

           DISPLAY "Sum: " WS-SUM
           DISPLAY "Difference: " WS-DIFF
           DISPLAY "Product: " WS-PROD

           STOP RUN.
