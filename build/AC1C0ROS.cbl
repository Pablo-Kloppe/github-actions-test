      ******************************************************************
      *A.PD.S
      *     CAMBIO PARA GITHUB
      *     PROGRAMA ON-LINE TRANSACIONAL
      *     ASOCIADO A LA TRANSACCION A003
      *     GESTIONA LA CAPTURA Y MANTENIMIENTO DE LA TABLA DE BIENES
      *     RAICES
      *A.PD.E                                                          *
      *                                                                *
      *B.PD.S                                                          *
      *     TRANSACTIONAL ON-LINE PROGRAM ASSOCIATED WITH              *
      *     A003 TRANSACTION                                           *
      *     MAINTENANCE OF REAL ESTATE TABLE                           *
      *B.PD.E                                                          *
      ******************************************************************
       IDENTIFICATION DIVISION.
      *
       PROGRAM-ID. AC1C0ROS IS INITIAL.
       AUTHOR.       ALNOVA TECHNOLOGIES CORPORATION
       DATE-WRITTEN. 25-03-2003.
      *
      ******************************************************************
      *A.OR.S                                                          *
      *     MODULO DIRECTOR DE MANTENIMIENTO DE LA TABLA DE BIENES     *
      *     RAICES                                                     *
      *                                                                *
      *                                                                *
      *  FICHEROS / TABLAS / AREAS                                     *
      *  -------------------------                                     *
      *                                                                *
      *     NOMBRE        E/S             DESCRIPCION                  *
      *    --------      -----  -----------------------------------    *
      *    QBEC999         E     AREA COM. DE LA LINKAGE               *
      *    TCEC9900       E/S    AREA COM. MODULO TC9C9900             *
      *    ACEC130        E/S    AREA COM. RUTINA AC8C130S             *
      *    ACEC110        E/S    AREA COM. RUTINA AC8C110S             *
      *    QGECABC         E     COPY DE ABEND                         *
      *    QAWCSQL        E/S    COPY DE CONTROL DEL SQLCODE           *
      *    ACWC000         E     COPY DE CONSTANTES                    *
      *A.OR.E                                                          *
      *                                                                *
      *B.OR.S                                                          *
      *     MANAGER MAINTENANCE MODULE OF THE REAL ESTATE TABLE        *
      *                                                                *
      *    FILE / TABLES / AREAS                                       *
      *  ------------------------                                      *
      *                                                                *
      *      NAME         I/O             DESCRIPTION                  *
      *    --------      -----  -----------------------------------    *
      *    QBEC999         I     COMMAREA                              *
      *    TCEC9900       I/O    COMMAREA MODULE TC9C9900              *
      *    ACEC130        I/O    COMMAREA MODULE AC8C130S              *
      *    ACEC110        I/O    COMMAREA MODULE AC8C110S              *
      *    QGECABC         I     ABEND  COPY                           *
      *    QAWCSQL        I/O    SQLCODE CONTROL COPY                  *
      *    ACWC000         I     COPY OF CONSTANTS                     *
      *B.OR.E                                                          *
      ******************************************************************
      *                       MODIFICATIONS LOG                        *
      ******************************************************************
      *   CODE     AUTHOR     DATE     DESCRIPTION                     *
      * ---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *         I D E N T I F I C A T I O N   D I V I S I O N          *
      *                                                                *
      ******************************************************************
      *
      ******************************************************************
      *                  ENVIROMENT DIVISION                           *
      ******************************************************************
      ******************************************************************
      *          01MAY99 CONVERSION DE AREAS DE MEMORIA                *
      *@PAL300D         MARCA INICIAL.                                 *
      *@FAL300D         MARCA FINAL.                                   *
      ******************************************************************


       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

      ******************************************************************
      *                        DATA DIVISION                           *
      ******************************************************************
       DATA DIVISION.

      ******************************************************************
      *                    WORKING-STORAGE SECTION                     *
      ******************************************************************

       WORKING-STORAGE SECTION.

      *@PAL300D
      * COPY FOR AUXILIARY VARIABLES TO MIGRATION.
       COPY QAECPREC.

      * COPY FOR CICS VALUES.
       COPY QAWCCO2C.


      * COPY TO CALL A PROGRAM DYNAMICALLY.
       COPY QAWCCO4C.
      *@FAL300D


      ******************************************************************
      *                            COPYS                               *
      ******************************************************************
      ******************************************************************
      *A.PR.S                                                          *
      *     COPY DE CONSTANTES DE LA APLICACION DE GARANTIAS           *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *     COPY OF CONSTANTS FOR THE COLLATERAL APPLICATION           *
      *B.PR.E                                                          *
      ******************************************************************

       01  VA-ACWC000-01.
           COPY ACWC000.

      ******************************************************************
      *                 COPY OF COMMAREA ABEND ROUTINE                 *
      ******************************************************************

       01  VA-QGECABC-01.
           COPY QGECABC.

      ******************************************************************
      *                 COPY OF STRUCTURAL PARAMETERS                  *
      ******************************************************************
       01  VA-QBEC999-01.
           COPY QBEC999.

      ******************************************************************
      *               COPY OF COMMAREA TC9C9900                        *
      ******************************************************************
       01  VA-TCEC9900-01.
           COPY TCEC9900.

      *
      ******************************************************************
      *               COPY OF COMMAREA QGECSEG                         *
      ******************************************************************
       01  VA-QGECSEG-01.
           COPY QGECSEG.

      *
      ******************************************************************
      *               COPY OF COMMAREA MODULE AC8C130S                 *
      ******************************************************************
       01  VA-ACEC130-01.
           COPY ACEC130.

      ******************************************************************
      *               COPY ACEC000                                     *
      ******************************************************************
       01  VA-ACEC000-01.
           COPY ACEC000.

      ******************************************************************
      *               COPY OF COMMAREA MODULE AC8C110S                 *
      ******************************************************************
       01  VA-ACEC110-01.
           COPY ACEC110.

      ******************************************************************
      *                  COPY QAWCSQL                                  *
      ******************************************************************
       COPY QAWCSQL.

      ******************************************************************
      *                  COPY ACNC0031                                 *
      ******************************************************************
       COPY ACNC0031.
      ******************************************************************
      *                  COPY PEEC190                                  *
      ******************************************************************
       COPY PEEC190.
      *
      ******************************************************************
      *                      VARIABLES                                 *
      ******************************************************************
       01  VA-VARIABLES.

           05  VA-TSQ.
               10  VN-TSQ-LTH              PIC S9(4)   COMP
                                   VALUE +0.

               10  VA-TSQ-A1.
                   15  VA-TSQ-PRFX         PIC X(4)    VALUE SPACES.
                   15  VA-TSQ-SFF          PIC X(4)    VALUE SPACES.

               10  VA-TSQ-CNT.
                   15  VA-DES-FORMAT.
                       20 FILLER           PIC X(8)    VALUE SPACES.

                   15  VA-FRT-CNT          PIC X(900)  VALUE SPACES.

           05  VA-ERR-OBJECT               PIC X(8)    VALUE SPACES.
           05  VA-ERR-REFERENCE            PIC X(20)   VALUE SPACES.
           05  VA-ERR-SQLERRM              PIC X(70)   VALUE SPACES.
           05  VN-ERR-SQLCODE              PIC S9(9)   COMP
                                   VALUE ZEROS.
           05  VA-DES-COMPRADR             PIC X(90).
           05  VA-DES-COMPRCTY             PIC X(65).

      *

      ******************************************************************
      *                  CONSTANTS                                     *
      ******************************************************************
       01  CA-CONSTANTS.

           05  CA-YES                      PIC X(01)   VALUE 'S'.
           05  CA-SCREEN                   PIC X(01)   VALUE 'P'.
           05  CA-QUEUE                    PIC X(04)   VALUE '+DC1'.
           05  CA-QG1CABC                  PIC X(08)   VALUE 'QG1CABC'.
           05  CA-ACE0001                  PIC X(07)   VALUE 'ACE0001'.
           05  CA-ACE0002                  PIC X(07)   VALUE 'ACE0002'.
           05  CA-ACE0009                  PIC X(7)    VALUE 'ACE0009'.
           05  CA-ACE0106                  PIC X(07)   VALUE 'ACE0106'.
           05  CA-FORMAT-NAME              PIC X(07)   VALUE 'ACM0031'.
           05  CA-AC000019                 PIC X(09)
                                   VALUE '@AC000019'.
           05  CA-AC000020                 PIC X(09)
                                   VALUE '@AC000020'.
           05  CA-AC000015                 PIC X(9)
                                   VALUE '@AC000015'.
           05  CA-AC000008                 PIC X(9)
                                   VALUE '@AC000008'.
           05  CA-ERR-ACE0109              PIC X(07)   VALUE 'ACE0109'.
           05  CA-TS-QUEUE                 PIC X(8)    VALUE 'TS-QUEUE'.
           05  CA-TC9C9900                 PIC X(08)   VALUE 'TC9C9900'.
           05  CA-AC8C110S                 PIC X(08)   VALUE 'AC8C110S'.
           05  CA-AC8C130S                 PIC X(08)   VALUE 'AC8C130S'.
           05  CA-TC9C9900-REF             PIC X(11)
                                   VALUE 'TC9C9900 : '.
           05  CA-AC8C110S-REF             PIC X(11)
                                   VALUE 'AC8C110S : '.
           05  CA-AC8C130S-REF             PIC X(11)
                                   VALUE 'AC8C130S : '.
           05  CA-QG2CSEG                  PIC X(08)   VALUE 'QG2CSEG'.
           05  CA-QG2CSEG-REF              PIC X(11)
                                   VALUE 'QG2CSEG  : '.
           05  CA-PROGRAM                  PIC X(08)   VALUE 'AC1C0ROS'.
           05  CA-WRI-REFERENCE            PIC X(08)   VALUE 'WRITEQ  '.
           05  CA-DEL-REFERENCE            PIC X(08)   VALUE 'DELETEQ '.
           05  CA-C                        PIC X       VALUE 'C'.
           05  CA-PE8C1900                 PIC X(08)   VALUE 'PE8C1900'.

      *
      ******************************************************************
      *                                                                *
      *                         LINKAGE SECTION                        *
      *                                                                *
      ******************************************************************
       LINKAGE SECTION.




       01  DFHCOMMAREA.
           COPY QGECCAA.

      *@PAL300D
      * COPY CAA EXTENDIDA.
       COPY QAECCAAE.

      *@FAL300D
           COPY ACNC003.

      ******************************************************************
      *                                                                *
      *                      PROCEDURE DIVISION                        *
      *                                                                *
      ******************************************************************
      *@PAL300D
      *@PAL300D
      *PROCEDURE DIVISION.

       PROCEDURE DIVISION USING DFHEIBLK
                                DFHCOMMAREA
                         .

           IF ADDRESS OF QGECCAA NOT = NULL
               SET CAA-PNT-INPCPY TO ADDRESS OF EATT-CPY-BMS
               SET CAA-AUTHPNT TO ADDRESS OF EATT-AUTHORIZATION
               SET CAA-DTA-PNT TO ADDRESS OF EATT-APPLICATION.
               CONTINUE
           .








      *@FAL300D

           PERFORM  START-PROCESS.

           PERFORM  PROGRAM-PROCESS.

           PERFORM  END-PROCESS.

      *
      ******************************************************************
      *.PN                         START-PROCESS                       *
      *                                                                *
      *A.PR.S                                                          *
      *  - SE DIRECCIONA LA PANTALLA DE ENTRADA.                       *
      *  - INICIALIZA LAS VARIABLES.                                   *
      *  - SE VALIDAN LOS CAMPOS DE ENTRADA DE LA COPY DE PANTALLA.    *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *  - ADDRESS THE INPUT SCREEN.                                   *
      *  - VARIABLES INITIALIZATION                                    *
      *  - CHECK THE INPUT FIELDS OF THE SCREEN COPY                   *
      *B.PR.E                                                          *
      ******************************************************************
       START-PROCESS.

           SET ADDRESS OF ACNC003                TO CAA-PTR-COPYIN
           SET CAA-SW-OUT-TYP-COPYSCRE           TO TRUE

           PERFORM SECURITY-USERID

           INITIALIZE VA-QGECABC-01
                      VA-QBEC999-01
                      TCEC9900
                      VA-VARIABLES
                      ACNC0031

           MOVE SPACES                           TO CAA-SW-COD-WA1
                                                    CAA-SW-ERRCOD
                                                    CAA-ERR-VARIA1
                                                    CAA-ERR-VARIA2

           SET CAA-SW-ACC-OPERANO                TO TRUE

           MOVE N003-OPTION                      TO CAA-SW-OPT

           PERFORM VALIDATE-OPTION

           PERFORM VALIDATE-ENTITY

           PERFORM VALIDATE-NUMBER-GOOD

           PERFORM VALIDATE-GOOD

           PERFORM VALIDATE-LOCAL

           PERFORM DEL-QUEUE.

      ******************************************************************
      *.PN                   SECURITY-USERID                           *
      *                                                                *
      *A.PR.S                                                          *
      *      SE LLAMA AL MODULO QG2CSEG PARA VERIFICAR LA AUTORIZACION *
      *      DEL USUARIO                                               *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *      QG2CSEG USER AUTORITATION VERIFICATION                    *
      *B.PR.E                                                          *
      ******************************************************************
       SECURITY-USERID.

           INITIALIZE QGECSEG.

           MOVE W000-CA-SEG-ACGOOD       TO SEG-SW-FICRECORD

      *@PAV9999D
      *    EXEC CICS
      *      LINK PROGRAM(CA-QG2CSEG)
      *      COMMAREA     (QGECSEG)
      *    END-EXEC.

           MOVE CA-QG2CSEG TO WC04C-PGM-NM.
           MOVE LENGTH OF QGECSEG TO EIBCALEN.
           CALL WC04C-PGM-NM USING DFHEIBLK
                                   QGECSEG
            ON EXCEPTION
               MOVE WCO2C-CICS-PGMIDERR TO EIBRESP
           END-CALL.
           MOVE 0                    TO EIBCALEN.
           MOVE WCO2C-CICS-LINK      TO EIBFN.
           MOVE WC04C-PGM-NM         TO EIBRSRCE.

           EVALUATE EIBRESP
               WHEN WCO2C-CICS-ABENDERR
                   GOBACK
               WHEN WCO2C-CICS-PGMIDERR
                   IF WC04C-PGM-NM NOT EQUAL 'QG1CABC'
                       CONTINUE
                   ELSE
                       MOVE WCO2C-CICS-ABENDERR TO EIBRESP
                       GOBACK
                   END-IF
               WHEN OTHER
                   MOVE WCO2C-CICS-NORMAL    TO EIBRESP
                   MOVE WCO2C-CICS-EIBRCODE-OK TO EIBRCODE
           END-EVALUATE.
      *@FAV9999D

      *@PAV9999D
      *    IF  EIBRESP EQUAL DFHRESP(NORMAL)
           IF  EIBRESP EQUAL WCO2C-CICS-NORMAL
      *@FAV9999D

               IF  SEG-FLG-AUTHORIZ NOT EQUAL CA-YES

                   MOVE CA-ERR-ACE0109   TO CAA-SW-ERRCOD

                   PERFORM END-PROCESS

               END-IF

           ELSE

               MOVE CA-QG2CSEG           TO VA-ERR-OBJECT
               MOVE CA-QG2CSEG-REF       TO VA-ERR-REFERENCE(1:11)
               MOVE SEG-CODERR           TO VA-ERR-REFERENCE(12:9)

               PERFORM VALIDATE-CICS-ERROR

           END-IF.

      *
      ******************************************************************
      *.PN                  VALIDATE-OPTION                            *
      *                                                                *
      *A.PR.S                                                          *
      *  - SE VALIDA EL CAMPO DE ENTRADA OPCION                        *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *  - CHECK THE INPUT FIELD  OPTION                               *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-OPTION.

           IF   NOT CAA-SW-OPE-TYPEREG AND
                NOT CAA-SW-OPE-TYPEANN AND
                NOT CAA-SW-OPE-TYPEMODI

               MOVE CA-ACE0001            TO CAA-SW-ERRCOD
               MOVE SPACES                TO CAA-ERR-VARIA1
               MOVE SPACES                TO CAA-ERR-VARIA2

               PERFORM END-PROCESS

           END-IF.

      ******************************************************************
      *.PN           VALIDATE-ENTITY                                   *
      *                                                                *
      *A.PR.S                                                          *
      *    - OBTENCION DE LOS PARAMETROS ESTRUCTURALES DE LA ENTIDAD   *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *    - FINANCIAL INSTITUTION STRUCTURAL PARAMETERS OBTAINING     *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-ENTITY.

           IF  N003-ENT       EQUAL SPACES OR LOW-VALUES

               MOVE CAA-ENT-ACC                TO N003-ENT

           END-IF

           MOVE W000-CA-NUM-3                  TO  TCEC9900-OPTION
           MOVE N003-ENT                       TO  TCEC9900-KEY
           MOVE CAA-SW-LNG-TERM                TO  TCEC9900-COD-LNGKEY

           CALL CA-TC9C9900 USING TCEC9900

           EVALUATE TCEC9900-COD-RETURN

               WHEN W000-CA-NUM-00

                   MOVE TCEC9900-DATA-PARAM    TO QBEC999-DATA-PARAM

               WHEN W000-CA-NUM-70
                   MOVE CA-ACE0009             TO CAA-SW-ERRCOD
                   MOVE CA-AC000015            TO CAA-ERR-VARIA1
                   MOVE SPACES                 TO CAA-ERR-VARIA2

                   PERFORM END-PROCESS

               WHEN W000-CA-NUM-99

                   MOVE  TCEC9900-DES-TABLE    TO VA-ERR-OBJECT
                   MOVE  CA-TC9C9900-REF       TO VA-ERR-REFERENCE(1:11)
                   MOVE  TCEC9900-COD-RETURN   TO VA-ERR-REFERENCE(12:9)
                   MOVE  TCEC9900-SQLCODE      TO VN-ERR-SQLCODE
                   MOVE  TCEC9900-DTA-SQLERRM  TO VA-ERR-SQLERRM

                   PERFORM DB2-ERROR

               WHEN OTHER

                   MOVE CA-ACE0106             TO CAA-SW-ERRCOD
                   MOVE SPACES                 TO CAA-ERR-VARIA1
                   MOVE SPACES                 TO CAA-ERR-VARIA2

                   PERFORM END-PROCESS

           END-EVALUATE.

      ******************************************************************
      *.PN                  VALIDATE-GOOD                              *
      *                                                                *
      *A.PR.S                                                          *
      *  - SE VALIDA EL TIPO DE BIEN                                   *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *  - CHECK THE INPUT FIELD  TYPE OF GOOD                         *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-GOOD.

           IF  N003-TYP-GOOD EQUAL SPACES OR LOW-VALUES

               MOVE CA-ACE0002            TO CAA-SW-ERRCOD
               MOVE CA-AC000019           TO CAA-ERR-VARIA1
               MOVE SPACES                TO CAA-ERR-VARIA2

               PERFORM END-PROCESS

           END-IF.

      *
      ******************************************************************
      *.PN                  VALIDATE-LOCAL                             *
      *                                                                *
      *A.PR.S                                                          *
      *  - SE VALIDA EL CODIGO DE LOCALIDAD                            *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *  - CHECK THE INPUT FIELD CODE  OF TOWN                         *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-LOCAL.

      *    IF  N003-COD-TOWN EQUAL SPACES OR LOW-VALUES OR ZEROS
           IF  N003-TOWN EQUAL SPACES OR LOW-VALUES OR ZEROS

               MOVE CA-ACE0002            TO CAA-SW-ERRCOD
               MOVE CA-AC000020           TO CAA-ERR-VARIA1
               MOVE SPACES                TO CAA-ERR-VARIA2

               PERFORM END-PROCESS

           END-IF.

      ******************************************************************
      *.PN                     VALIDATE-NUMBER-GOOD                    *
      *                                                                *
      *A.PR.S                                                          *
      *   - SE VALIDA QUE EL NUMERO DEL BIEN ESTE INFORMADO.           *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *   - CHECK GOOD NUMBER    IS INFORMED.                          *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-NUMBER-GOOD.

           IF  N003-NUM-GOOD IS NOT NUMERIC

               MOVE ZEROES              TO N003-NUM-GOOD

           END-IF

           IF  NOT CAA-SW-OPE-TYPEREG     AND
               N003-NUM-GOOD EQUAL ZEROS

               MOVE CA-ACE0002          TO CAA-SW-ERRCOD
               MOVE CA-AC000008         TO CAA-ERR-VARIA1
               MOVE SPACES              TO CAA-ERR-VARIA2

               PERFORM END-PROCESS

           END-IF

           IF  N003-HAIRCUT IS NOT NUMERIC

               MOVE ZEROES              TO N003-HAIRCUT

           END-IF

           IF  N003-NUM-TOT-AREA NOT NUMERIC

               MOVE ZEROES              TO N003-NUM-TOT-AREA

           END-IF

           IF  N003-NUM-BUILT-AREA NOT NUMERIC

               MOVE ZEROES              TO N003-NUM-BUILT-AREA

           END-IF

           IF  N003-GOOD-AMT IS NOT NUMERIC

               MOVE ZEROES              TO N003-GOOD-AMT

           END-IF.

      ******************************************************************
      *.PN                      DEL-QUEUE                              *
      *                                                                *
      *A.PR.S                                                          *
      *        BORRA LA COLA TS                                        *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *       DELETE TS QUEUE                                          *
      *B.PR.E                                                          *
      ******************************************************************
       DEL-QUEUE.

           MOVE CA-QUEUE                  TO VA-TSQ-PRFX
           MOVE CAA-TERMINAL              TO VA-TSQ-SFF

      *@PAV9999D
      *    EXEC CICS
      *        DELETEQ TS
      *        QUEUE(VA-TSQ-A1)
      *        NOHANDLE
      *    END-EXEC

           INITIALIZE QAECTS1C
           MOVE VA-TSQ-A1 TO ETSIC-CICS-QUEUE
           IF ETSIC-CICS-QUEUE-ARCH
              MOVE 'QA6CDL1' TO WC04C-PGM-NM
              CALL WC04C-PGM-NM USING DFHEIBLK
                                      EATT-QAECCAAE-03
                                      QAECTS1C
           ELSE
              MOVE 'QA7CTS3' TO WC04C-PGM-NM
              CALL WC04C-PGM-NM USING DFHEIBLK
                                      EENQC-CICS-FILL
                                      QAECTS1C
           END-IF

      *@FAV9999D

           EVALUATE EIBRESP
      *@PAV9999D
      *        WHEN (DFHRESP(NORMAL))
               WHEN (WCO2C-CICS-NORMAL)
      *@FAV9999D
      *@PAV9999D
      *        WHEN (DFHRESP(QIDERR))
               WHEN (WCO2C-CICS-QIDERR)
      *@FAV9999D
                   CONTINUE
               WHEN OTHER
                   MOVE CA-TS-QUEUE       TO VA-ERR-OBJECT
                   MOVE CA-DEL-REFERENCE  TO VA-ERR-REFERENCE(1:8)

                   PERFORM VALIDATE-CICS-ERROR

           END-EVALUATE.

      ******************************************************************
      *.PN                       PROGRAM-PROCESS                       *
      *                                                                *
      *A.PR.S                                                          *
      *   - MANTENIMIENTO DE LA TABLA DE BIENES RAICES                 *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *   - REAL ESTATE TABLE MAINTENANCE                              *
      *B.PR.E                                                          *
      ******************************************************************
       PROGRAM-PROCESS.

           PERFORM ACCESS-AC8C110S

           IF CAA-SW-OPE-TYPEREG OR CAA-SW-OPE-TYPEMODI

               PERFORM ACCESS-AC8C130S

           END-IF

           MOVE ACNC003               TO ACNC0031
      *
      *    MOVE E110-NUM-GOOD         TO N003-NUM-GOOD
           MOVE E110-NUM-GOOD         TO N0031-NUM-GOOD

           PERFORM WRITEQ.

      ******************************************************************
      *.PN                ACCESS-AC8C110S                              *
      *                                                                *
      *A.PR.S                                                          *
      *   SE ACCEDE A LA RUTINA DE MANTENIMIENTO DE DATOS BASICOS DEL  *
      *   BIEN                                                         *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *   ACCESS TO THE MAINTENANCE ROUTINE OF BASICS GOOD DATA        *
      *B.PR.E                                                          *
      ******************************************************************
       ACCESS-AC8C110S.

           INITIALIZE VA-ACEC110-01
                      VA-ACEC000-01

           MOVE CAA-ENT-ACC                   TO E110-ENT-LAST-MOD
           MOVE CAA-CEN-ACCOUNT               TO E110-CEN-LAST-MOD
           MOVE CAA-USERID                    TO E110-STP-USER
           MOVE CAA-TERMINAL                  TO E110-STP-TERMINAL
           MOVE CAA-COD-TRA                   TO E110-TRANSACTION
           MOVE CAA-2DAT-ACCOUN               TO E110-DAT-PROCESS
           MOVE N003-OPTION                   TO E110-OPTION
           MOVE N003-ENT                      TO E110-ENT
           MOVE N003-NUM-GOOD                 TO E110-NUM-GOOD
           MOVE N003-COD-GOOD                 TO E110-COD-GOOD
           MOVE N003-DAT-BGN-GOOD             TO E110-DAT-BGN-GOOD
           MOVE N003-DAT-MAT-GOOD             TO E110-DAT-MAT-GOOD
           MOVE N003-DES-GOOD                 TO E110-DES-GOOD
           MOVE N003-DES-LOCATION             TO E110-DES-LOCATION
           MOVE N003-BRN-GOOD                 TO E110-BRN-GOOD
           MOVE N003-STATUS-GOOD              TO E110-STATUS-GOOD
           MOVE W000-CA-TYP-DTA-EST           TO E110-TYP-DTA-CMPY
           MOVE N003-FLG-VALID-BAS            TO E110-FLG-VALID-BASEL
           MOVE N003-FLG-EXC-INSU             TO E110-FLG-EXC-INSU
           MOVE N003-HAIRCUT                  TO E110-PER-HC-GOOD
           MOVE N003-COMMENTS                 TO E110-COMMENTS
           MOVE N003-FCC-GOOD                 TO E110-FCC-GOOD
           MOVE N003-GOOD-AMT                 TO E110-AMT-GOOD

           CALL CA-AC8C110S   USING VA-ACEC110-01
                                    VA-ACEC000-01
                                    VA-QBEC999-01

           EVALUATE TRUE

               WHEN E000-SW-RTN-OK

                   IF CAA-SW-OPE-TYPEANN

                       MOVE E000-COD-WA1      TO CAA-SW-COD-WA1
                       MOVE E000-WARN1VARIA1  TO CAA-WARN1VARIA1
                       MOVE E000-WARN1VARIA2  TO CAA-WARN1VARIA2

                       PERFORM END-PROCESS

                   END-IF

               WHEN E000-SW-RTN-DB2

                   MOVE E000-DES-TBLERR       TO VA-ERR-OBJECT
                   MOVE CA-AC8C110S-REF       TO VA-ERR-REFERENCE(1:11)
                   MOVE E000-SW-COD-RTN       TO VA-ERR-REFERENCE(12:9)
                   MOVE E000-COD-SQLCODE      TO VN-ERR-SQLCODE
                   MOVE E000-COD-SQLERRMC     TO VA-ERR-SQLERRM

                   PERFORM DB2-ERROR

               WHEN OTHER

                   MOVE E000-ERRCOD           TO CAA-SW-ERRCOD
                   MOVE E000-COD-WA1          TO CAA-SW-COD-WA1
                   MOVE E000-COD-WA2          TO CAA-SW-COD-WA2
                   MOVE E000-ERR-VARIA1       TO CAA-ERR-VARIA1
                   MOVE E000-ERR-VARIA2       TO CAA-ERR-VARIA2
                   MOVE E000-WARN1VARIA1      TO CAA-WARN1VARIA1
                   MOVE E000-WARN1VARIA2      TO CAA-WARN1VARIA2
                   MOVE E000-WARN2VARIA1      TO CAA-WARN2VARIA1
                   MOVE E000-WARN2VARIA2      TO CAA-WARN2VARIA2

                   PERFORM END-PROCESS

           END-EVALUATE.

      ******************************************************************
      *.PN                ACCESS-AC8C130S                              *
      *                                                                *
      *A.PR.S                                                          *
      *   SE ACCEDE A LA RUTINA DE MANTENIMIENTO DE BIENES RAICES      *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *   ACCESS TO THE MAINTENANCE ROUTINE OF REAL ESTATE             *
      *B.PR.E                                                          *
      ******************************************************************
       ACCESS-AC8C130S.

           INITIALIZE VA-ACEC130-01
                      VA-ACEC000-01

           MOVE CAA-ENT-ACC                   TO E130-ENT-LAST-MOD
           MOVE CAA-CEN-ACCOUNT               TO E130-CEN-LAST-MOD
           MOVE CAA-USERID                    TO E130-STP-USER
           MOVE CAA-TERMINAL                  TO E130-STP-TERMINAL
           MOVE CAA-COD-TRA                   TO E130-TRANSACTION
           MOVE CAA-2DAT-ACCOUN               TO E130-DAT-PROCESS
           MOVE N003-OPTION                   TO E130-OPTION
           MOVE N003-ENT                      TO E130-ENT
           MOVE E110-NUM-GOOD                 TO E130-NUM-GOOD
           MOVE N003-TYP-GOOD                 TO E130-TYP-GOOD
           MOVE N003-NUM-BUILDING             TO E130-NUM-BUILDING
           MOVE N003-NUM-TOT-AREA             TO E130-NUM-TOT-AREA
           MOVE N003-NUM-BUILT-AREA           TO E130-NUM-BUILT-AREA
           MOVE N003-TYP-AREA                 TO E130-TYP-AREA
      *    MOVE N003-DES-STREET               TO E130-DES-STREET
      *    MOVE N003-NUM-STREET               TO E130-NUM-STREET
      *    MOVE N003-COD-BLOCK                TO E130-COD-BLOCK
      *    MOVE N003-COD-DPT                  TO E130-COD-DPT
           MOVE N003-DES-AMNG-STRE1           TO E130-DES-AMNG-STRE1
           MOVE N003-DES-AMNG-STRE2           TO E130-DES-AMNG-STRE2
      *    MOVE N003-COD-DISTRICT             TO E130-COD-DISTRICT
      *    MOVE N003-COD-MAIL                 TO E130-COD-MAIL
      *    MOVE N003-COD-TOWN                 TO E130-COD-TOWN
           MOVE N003-DES-ADM-AREA             TO E130-DES-ADM-AREA
      *    MOVE N003-COD-SUB-REGION           TO E130-COD-SUB-REGION
           MOVE N003-COD-AREA                 TO E130-COD-AREA
           MOVE N003-FLG-LOC-GEO              TO E130-FLG-LOC-GEO
           MOVE N003-DES-SECTION              TO E130-DES-SECTION
           MOVE N003-DES-BLOCK                TO E130-DES-BLOCK
           MOVE N003-COD-PLOT                 TO E130-COD-PLOT
           MOVE N003-COD-PROPERTY             TO E130-COD-PROPERTY
           MOVE N003-TYP-PROPERTY             TO E130-TYP-PROPERTY
           MOVE N003-DES-BUILDING             TO E130-DES-BUILDING
           MOVE N003-COD-OLD                  TO E130-COD-OLD

           IF  N003-DAT-CNTB  EQUAL TO SPACES OR LOW-VALUES
               MOVE W000-CD-DAT-MINIMUM       TO E130-DAT-CNTB
           ELSE
               MOVE N003-DAT-CNTB             TO E130-DAT-CNTB
           END-IF

           MOVE N003-FLG-TAX                  TO E130-FLG-TAX
           MOVE N003-FLG-DISPOSAL             TO E130-FLG-DISPOSAL
           PERFORM 22100-CALL-PE8C1900
           MOVE VA-DES-COMPRADR               TO E130-DES-COMPRADR
           MOVE VA-DES-COMPRCTY               TO E130-DES-COMPRCTY

           CALL CA-AC8C130S   USING VA-ACEC130-01
                                    VA-ACEC000-01
                                    VA-QBEC999-01

           EVALUATE TRUE

               WHEN E000-SW-RTN-OK

                   MOVE E000-COD-WA1          TO CAA-SW-COD-WA1
                   MOVE E000-WARN1VARIA1      TO CAA-WARN1VARIA1
                   MOVE E000-WARN1VARIA2      TO CAA-WARN1VARIA2

               WHEN E000-SW-RTN-DB2

                   MOVE E000-DES-TBLERR       TO VA-ERR-OBJECT
                   MOVE CA-AC8C130S-REF       TO VA-ERR-REFERENCE(1:11)
                   MOVE E000-SW-COD-RTN       TO VA-ERR-REFERENCE(12:9)
                   MOVE E000-COD-SQLCODE      TO VN-ERR-SQLCODE
                   MOVE E000-COD-SQLERRMC     TO VA-ERR-SQLERRM

                   PERFORM DB2-ERROR

               WHEN OTHER

                   MOVE E000-ERRCOD           TO CAA-SW-ERRCOD
                   MOVE E000-COD-WA1          TO CAA-SW-COD-WA1
                   MOVE E000-COD-WA2          TO CAA-SW-COD-WA2
                   MOVE E000-ERR-VARIA1       TO CAA-ERR-VARIA1
                   MOVE E000-ERR-VARIA2       TO CAA-ERR-VARIA2
                   MOVE E000-WARN1VARIA1      TO CAA-WARN1VARIA1
                   MOVE E000-WARN1VARIA2      TO CAA-WARN1VARIA2
                   MOVE E000-WARN2VARIA1      TO CAA-WARN2VARIA1
                   MOVE E000-WARN2VARIA2      TO CAA-WARN2VARIA2

                   PERFORM END-PROCESS

           END-EVALUATE.

      ******************************************************************
      *.PN                    WRITEQ                                   *
      *                                                                *
      *A.PR.S                                                          *
      *        ESCRIBE LA COLA TS                                      *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *        WRITE TS QUEUE                                          *
      *B.PR.E                                                          *
      ******************************************************************
       WRITEQ.

           MOVE CA-QUEUE                 TO  CAA-TB-DES1(1)
           MOVE CA-SCREEN                TO  CAA-TB-SCRDOCU(1)

           MOVE CA-QUEUE                 TO VA-TSQ-PRFX
           MOVE CAA-TERMINAL             TO VA-TSQ-SFF

           MOVE CA-FORMAT-NAME           TO VA-DES-FORMAT
      *    MOVE LENGTH OF ACNC003        TO VN-TSQ-LTH
      *    MOVE ACNC003                  TO VA-FRT-CNT
           MOVE LENGTH OF ACNC0031       TO VN-TSQ-LTH
           MOVE ACNC0031                 TO VA-FRT-CNT
           ADD W000-CN-NUM-8             TO VN-TSQ-LTH

      *@PAV9999D
      *    EXEC CICS
      *       WRITEQ TS QUEUE(VA-TSQ-A1)
      *       FROM(VA-TSQ-CNT)
      *       LENGTH(VN-TSQ-LTH)
      *       MAIN NOHANDLE
      *    END-EXEC

           INITIALIZE QAECTS1C
           INITIALIZE QAECTS1M
           MOVE VA-TSQ-A1 TO ETSIC-CICS-QUEUE
           SET ETS1M-SW-MAIN-YES TO TRUE
           SET ETSIC-SW-REW-NO     TO TRUE
           MOVE 0 TO ETSIC-CICS-ITEM
           MOVE VN-TSQ-LTH TO ETSIC-CICS-LTH
           MOVE 0 TO ETSIC-NUM-ITEM-CICS
           IF ETSIC-CICS-QUEUE-ARCH
              MOVE 'QA6CWR1' TO WC04C-PGM-NM
              CALL WC04C-PGM-NM USING DFHEIBLK
                                      EATT-QAECCAAE-03
                                      QAECTS1C
                                      VA-TSQ-CNT
           ELSE
              IF ETS1M-SW-MAIN-YES
                 MOVE 'QA7CTSM' TO WC04C-PGM-NM
              ELSE
                 MOVE 'QA7CTS2' TO WC04C-PGM-NM
              END-IF
              CALL WC04C-PGM-NM USING DFHEIBLK
                                      EENQC-CICS-FILL
                                      QAECTS1C
                                      VA-TSQ-CNT
           END-IF

      *@FAV9999D

      *@PAV9999D
      *    IF  EIBRESP NOT EQUAL DFHRESP(NORMAL)
           IF  EIBRESP NOT EQUAL WCO2C-CICS-NORMAL
      *@FAV9999D
               MOVE VA-DES-FORMAT        TO VA-ERR-OBJECT
               MOVE CA-WRI-REFERENCE     TO VA-ERR-REFERENCE(1:8)

               PERFORM VALIDATE-CICS-ERROR

           END-IF.


      ******************************************************************
      *.PN                    VALIDATE-CICS-ERROR                      *
      *                                                                *
      *A.PR.S                                                          *
      *       TRATAMIENTO ERROR CICS                                   *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *        CICS ERROR HANDLING                                     *
      *B.PR.E                                                          *
      ******************************************************************
       VALIDATE-CICS-ERROR.

           INITIALIZE VA-QGECABC-01

           MOVE CA-YES            TO ABC-ABEND
           MOVE CA-PROGRAM        TO ABC-DES-PROG
           MOVE EIBFN             TO ABC-EIBFN
           MOVE EIBRCODE          TO ABC-EIBRCODE
           MOVE EIBRSRCE          TO ABC-EIBRSRCE
           MOVE EIBRESP           TO ABC-EIBRESP1
           MOVE EIBRESP2          TO ABC-EIBRESP2
           MOVE VA-ERR-OBJECT     TO ABC-OBJECT-ERROR
           MOVE VA-ERR-REFERENCE  TO ABC-REFERENCE1

           PERFORM ABEND.

      ******************************************************************
      *.PN                    DB2-ERROR                                *
      *                                                                *
      *A.PR.S                                                          *
      *        TRATAMIENTO ERROR DB2. LLAMA A QG1CABC PARA ABENDAR     *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *        DB2 ERROR HANDLING . IT CALLS QG1CABC TO ABEND.         *
      *B.PR.E                                                          *
      ******************************************************************
       DB2-ERROR.

           INITIALIZE VA-QGECABC-01

           MOVE CA-YES            TO ABC-ABEND
           MOVE CA-PROGRAM        TO ABC-DES-PROG
           MOVE VA-ERR-OBJECT     TO ABC-OBJECT-ERROR
           MOVE VA-ERR-REFERENCE  TO ABC-REFERENCE1
           MOVE VN-ERR-SQLCODE    TO ABC-SQLCODE
           MOVE VA-ERR-SQLERRM    TO ABC-SQLERRM

           PERFORM ABEND.

      ******************************************************************
      *.PN                      END-PROCESS                            *
      *                                                                *
      *A.PR.S                                                          *
      *       DEVUELVE EL CONTROL AL CICS.                             *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *       RESTORE CONTROL TO CICS.                                 *
      *B.PR.E                                                          *
      ******************************************************************
       END-PROCESS.

           IF  CAA-SW-ERRCOD NOT EQUAL SPACES

               MOVE W000-CA-NO         TO ABC-ABEND
               MOVE CA-PROGRAM         TO ABC-DES-PROG

               PERFORM ABEND

           END-IF

      *@PAV9999D
      *    EXEC CICS
      *        RETURN
      *    END-EXEC.

           GOBACK.
      *@FAV9999D

      ******************************************************************
      *.PN                    ABEND                                    *
      *                                                                *
      *A.PR.S                                                          *
      *        LLAMA A QG1CABC PARA QUE ABENDE                         *
      *A.PR.E                                                          *
      *                                                                *
      *B.PR.S                                                          *
      *        IT CALLS QG1CABC TO ABEND.                              *
      *B.PR.E                                                          *
      ******************************************************************
       ABEND.

      *@PAV9999D
      *    EXEC CICS
      *        LINK PROGRAM (CA-QG1CABC)
      *        COMMAREA (QGECABC)
      *    END-EXEC.

           MOVE CA-QG1CABC TO WC04C-PGM-NM.
           MOVE LENGTH OF QGECABC TO EIBCALEN.
           CALL WC04C-PGM-NM USING DFHEIBLK
                                   QGECABC
            ON EXCEPTION
               MOVE WCO2C-CICS-PGMIDERR TO EIBRESP
           END-CALL.
           MOVE 0                    TO EIBCALEN.
           MOVE WCO2C-CICS-LINK      TO EIBFN.
           MOVE WC04C-PGM-NM         TO EIBRSRCE.

           EVALUATE EIBRESP
               WHEN WCO2C-CICS-ABENDERR
                   GOBACK
               WHEN WCO2C-CICS-PGMIDERR
                   IF WC04C-PGM-NM NOT EQUAL 'QG1CABC'
                       CONTINUE
                   ELSE
                       MOVE WCO2C-CICS-ABENDERR TO EIBRESP
                       GOBACK
                   END-IF
               WHEN OTHER
                   MOVE WCO2C-CICS-NORMAL    TO EIBRESP
                   MOVE WCO2C-CICS-EIBRCODE-OK TO EIBRCODE
           END-EVALUATE.
      *@FAV9999D
      ******************************************************************
      *.PN              22100-CALL-PE8C1900                            *
      *                                                                *
      *                                                                *
      ******************************************************************
       22100-CALL-PE8C1900.

           INITIALIZE E190-RECORD
           MOVE N003-PORTAL       TO E190-PORTAL
           MOVE N003-DETAIL       TO E190-DETAIL
           MOVE N003-ADRCUS       TO E190-ADR-CUS
           MOVE N003-LOCALIT      TO E190-LOCALITY
           MOVE N003-TOWN         TO E190-TOWN
           MOVE N003-COUNTY       TO E190-COD-COUNTY
           MOVE N003-CODPOST      TO E190-COD-POST
           MOVE N003-COUNTRY      TO E190-COD-COUNTRY

           MOVE CA-C              TO E190-OPTION
           MOVE CAA-ENT-ACC       TO E190-CUS-ENT

           CALL CA-PE8C1900 USING E190-RECORD

           EVALUATE E190-COD-RTN
              WHEN ZEROS
                 MOVE E190-WHOLEADR     TO VA-DES-COMPRADR
                 MOVE E190-TOWNCOMPRALL TO VA-DES-COMPRCTY
              WHEN OTHER
                 MOVE CA-PE8C1900       TO VA-ERR-REFERENCE(1:16)
                 MOVE E190-COD-RTN      TO VA-ERR-REFERENCE(17:3)
                 PERFORM DB2-ERROR
           END-EVALUATE.


