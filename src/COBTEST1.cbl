IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTEST1.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOG-FILE ASSIGN TO "cobtest.log"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  LOG-FILE.
       01  LOG-REC                         PIC X(120).

       WORKING-STORAGE SECTION.
       01  WS-NAME                         PIC X(40).
       01  WS-AGE-TEXT                     PIC X(3).
       01  WS-AGE                          PIC 9(3) VALUE 0.
       01  WS-DATETIME.
           05 WS-YYYY                      PIC 9(4).
           05 WS-MM                        PIC 9(2).
           05 WS-DD                        PIC 9(2).
           05 WS-REST                      PIC X(14).
       01  WS-BIRTH-YEAR                   PIC 9(4) VALUE 0.
       01  WS-MSG                          PIC X(120).

       PROCEDURE DIVISION.
       MAIN-SECTION.
           *> Obtener fecha actual
           MOVE FUNCTION CURRENT-DATE TO WS-DATETIME

           DISPLAY "== COBTEST1 - agent smoke test =="
           DISPLAY "Enter your name: " WITH NO ADVANCING
           ACCEPT WS-NAME

           DISPLAY "Enter your age (years): " WITH NO ADVANCING
           ACCEPT WS-AGE-TEXT

           *> Convertir a numérico con validación
           IF WS-AGE-TEXT = SPACES OR WS-AGE-TEXT = ""
              DISPLAY "No age provided, using 0."
           ELSE
              COMPUTE WS-AGE = FUNCTION NUMVAL(WS-AGE-TEXT)
           END-IF

           *> Calcular año de nacimiento simple (sin mirar meses/días)
           IF WS-AGE > 0 AND WS-AGE < 130
              COMPUTE WS-BIRTH-YEAR = WS-YYYY - WS-AGE
           ELSE
              MOVE 0 TO WS-BIRTH-YEAR
           END-IF

           DISPLAY "Hello, " WS-NAME
           DISPLAY "Today is: " WS-YYYY "-" WS-MM "-" WS-DD
           IF WS-BIRTH-YEAR > 0
              DISPLAY "Estimated birth year: " WS-BIRTH-YEAR
           ELSE
              DISPLAY "Birth year could not be estimated."
           END-IF

           *> Registrar en log
           OPEN EXTEND LOG-FILE
           STRING
              "Name=" DELIMITED BY SIZE
              WS-NAME DELIMITED BY SIZE
              ", Age=" DELIMITED BY SIZE
              WS-AGE-TEXT DELIMITED BY SIZE
              ", Date=" DELIMITED BY SIZE
              WS-YYYY "-" WS-MM "-" WS-DD DELIMITED BY SIZE
              ", BirthYear=" DELIMITED BY SIZE
              WS-BIRTH-YEAR DELIMITED BY SIZE
              INTO LOG-REC
           END-STRING
           WRITE LOG-REC
           CLOSE LOG-FILE

           DISPLAY "Log written to cobtest.log"
           STOP RUN.
