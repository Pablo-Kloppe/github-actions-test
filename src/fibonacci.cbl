       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIBONACCI.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *---------------------------------------------------------------*
       * Numero de terminos a mostrar en la serie                      *
       *---------------------------------------------------------------*
       77  N               PIC 9(02) VALUE 10.
       77  N-STR           PIC X(03) VALUE SPACES.

       *---------------------------------------------------------------*
       * Contador para el bucle                                        *
       *---------------------------------------------------------------*
       77  I               PIC 9(02) VALUE 1.

       *---------------------------------------------------------------*
       * Variables para almacenar los terminos de la serie             *
       *---------------------------------------------------------------*
       77  A               PIC 9(04) VALUE 0.
       77  B               PIC 9(04) VALUE 1.
       77  C               PIC 9(04) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           * Pedir al usuario el número de términos
           DISPLAY "Ingrese el número de términos de la serie de Fibonacci (>0): "
           ACCEPT N-STR
           MOVE FUNCTION NUMVAL(N-STR) TO N

           * Validar que N sea mayor que 0
           IF N <= 0
               DISPLAY "El número de términos debe ser mayor que 0."
               STOP RUN
           END-IF

           *-----------------------------------------------------------*
           * Programa para calcular la serie de Fibonacci hasta N      *
           * terminos                                                 *
           *-----------------------------------------------------------*
           
           * Mostrar mensaje inicial con la cantidad de terminos       *
           DISPLAY "Serie de Fibonacci hasta " N " terminos:".

           * Mostrar los dos primeros términos de la serie             *
           DISPLAY A.
           DISPLAY B.

           * Calcular y mostrar los siguientes términos de la serie    *
           PERFORM VARYING I FROM 3 BY 1 UNTIL I > N
               * Sumar los dos términos anteriores                     *
               COMPUTE C = A + B
               * Mostrar el término calculado                          *
               DISPLAY C
               * Actualizar los valores para el siguiente ciclo        *
               MOVE B TO A
               MOVE C TO B
           END-PERFORM.

           * Finalizar el programa                                     *
           STOP RUN.




