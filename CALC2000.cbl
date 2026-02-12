       IDENTIFICATION DIVISION.

       PROGRAM-ID. CALC2000.

      *****************************************************************
      * Program Name : CALC2000
      * Authors       : <Grant Peverett & Garret Finke>
      * Course       : CIS352 Intro to Enterprise Computing
      * Description  :
      *   This program calculates the future value of an investment
      *   using a fixed interest rate and number of years.
      *   The calculation is performed three times, doubling the
      *   investment amount each time.
      *****************************************************************

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01 INPUT-VALUES.
          05 NUMBER-ENTERED        PIC 9      VALUE 1.
          05 INVESTMENT-AMOUNT     PIC 9(5)   VALUE 1000.
          05 NUMBER-OF-YEARS       PIC 99     VALUE 10.
          05 YEARLY-INTEREST-RATE  PIC 99V9   VALUE 5.5.

       01 WORK-FIELDS.
          05 FUTURE-VALUE          PIC 9(7)V99 VALUE 0.
          05 YEAR-COUNTER          PIC 999     VALUE 0.

          05 EDITED-WHOLE-VALUE    PIC ZZ,ZZZ,ZZ9.
          05 EDITED-DECIMAL-VALUE  PIC ZZZ,ZZZ.99.

       PROCEDURE DIVISION.

       000-CALCULATE-FUTURE-VALUES.
           DISPLAY "***************************************".
           DISPLAY "*        CALC2000 Investment Tool      *".
           DISPLAY "*  Future Value Calculation Program   *".
           DISPLAY "***************************************".
           DISPLAY SPACE.

           PERFORM 100-CALCULATE-FUTURE-VALUE

           COMPUTE INVESTMENT-AMOUNT =
               INVESTMENT-AMOUNT * 2
           PERFORM 100-CALCULATE-FUTURE-VALUE

           COMPUTE INVESTMENT-AMOUNT =
               INVESTMENT-AMOUNT * 2
           PERFORM 100-CALCULATE-FUTURE-VALUE

           DISPLAY "End of session."
           STOP RUN.

       100-CALCULATE-FUTURE-VALUE.
           DISPLAY "Calculating Future Values".

           MOVE INVESTMENT-AMOUNT TO FUTURE-VALUE
           MOVE 1 TO YEAR-COUNTER

           PERFORM 120-CALCULATE-NEXT-FV
               UNTIL YEAR-COUNTER > NUMBER-OF-YEARS

           PERFORM 140-DISPLAY-VALUES.
       
       120-CALCULATE-NEXT-FV.
           COMPUTE FUTURE-VALUE ROUNDED =
              FUTURE-VALUE +
              (FUTURE-VALUE * YEARLY-INTEREST-RATE / 100)
           ADD 1 TO YEAR-COUNTER.

       140-DISPLAY-VALUES.
           MOVE INVESTMENT-AMOUNT TO EDITED-WHOLE-VALUE
           MOVE FUTURE-VALUE     TO EDITED-DECIMAL-VALUE

           DISPLAY "Investment Amount : " EDITED-WHOLE-VALUE
           DISPLAY "Future Value      : " EDITED-DECIMAL-VALUE
           DISPLAY SPACE.