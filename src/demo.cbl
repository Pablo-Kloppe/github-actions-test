       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEMO.
       AUTHOR. GitHub Copilot.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUMERO-UNO           PIC 9(04).
       01 WS-NUMERO-DOS           PIC 9(04).
       01 WS-SUMA                 PIC 9(05).
       01 WS-RESTA                PIC S9(05).
       01 WS-MULTIPLICACION       PIC 9(08).
       01 WS-DIVISION             PIC 9(04)V99.
       01 WS-MENSAJE-BIENVENIDA   PIC X(50) VALUE
           "Bienvenido al programa demo de COBOL!".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY WS-MENSAJE-BIENVENIDA.

           DISPLAY "Ingrese el primer número (4 dígitos): ".
           ACCEPT WS-NUMERO-UNO.

           DISPLAY "Ingrese el segundo número (4 dígitos): ".
           ACCEPT WS-NUMERO-DOS.

           PERFORM CALCULAR-OPERACIONES.
           PERFORM MOSTRAR-RESULTADOS.

           STOP RUN.

       CALCULAR-OPERACIONES.
      *    REALIZA LAS OPERACIONES MATEMATICAS BASICAS
           COMPUTE WS-SUMA = WS-NUMERO-UNO + WS-NUMERO-DOS.
           COMPUTE WS-RESTA = WS-NUMERO-UNO - WS-NUMERO-DOS.
           COMPUTE WS-MULTIPLICACION = WS-NUMERO-UNO * WS-NUMERO-DOS.
           IF WS-NUMERO-DOS NOT = ZERO
               COMPUTE WS-DIVISION = WS-NUMERO-UNO / WS-NUMERO-DOS
           ELSE
               DISPLAY "Error: División por cero no permitida."
               MOVE ZERO TO WS-DIVISION
           END-IF.

       MOSTRAR-RESULTADOS.
      *    MUESTRA LOS RESULTADOS DE LAS OPERACIONES
           DISPLAY "Suma: " WS-SUMA.
           DISPLAY "Resta: " WS-RESTA.
           DISPLAY "Multiplicación: " WS-MULTIPLICACION.
           DISPLAY "División: " WS-DIVISION.

       END PROGRAM DEMO.
