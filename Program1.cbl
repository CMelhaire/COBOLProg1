       identification division.
       program-id. Program1.

       environment division.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT SALES-FILE ASSIGN TO
       "C:\Users\thoma\source\repos\HaylockGrant\COBOLProg1\test.txt"
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS IS SEQUENTIAL.


       configuration section.
           

       data division.
       FILE SECTION.
           FD SALES-FILE.
           01 SALES-FILE-ID.
               05 CUST-NO PIC 9(4).
			   05 FILLER  PIC x VALUE SPACES.
               05 CUST-NAME PIC A(20).
			   05 FILLER  PIC x VALUE SPACES.
               05 UNIT-PRICE PIC 9(5).
			   05 FILLER  PIC x VALUE SPACES.
               05 QUANTITY-SOLD PIC 9(3).
       working-storage section.
           01 SALESFILES-ID.
               05 CUST-NUM PIC 9(4).
			   05 FILLER  PIC x VALUE SPACES.
               05 CUSTO-NAME PIC A(20).
			   05 FILLER  PIC x VALUE SPACES.
               05 UNIT-PRICES PIC 9(5) VALUE 0.
			   05 FILLER  PIC x VALUE SPACES.
               05 QUANTITYS-SOLD PIC 9(3).
           01 TEMP-VAR.
			   05 NEWCUST PIC A.
			   05 TEMP-SALE PIC 9(5).
			   05 TEMP-TAX PIC 9(5).
			   05 TEMP-FINAL PIC 9(5).
		   01 COL-HDR.
               05  FILLER      PIC X(4)   VALUE "#".
               05  FILLER      PIC X      VALUE SPACES.
               05  FILLER      PIC X(20)  VALUE "Name". 
			   05  FILLER      PIC X      VALUE SPACES.
               05  FILLER      PIC X(5)   VALUE "PRICE".
			   05  FILLER      PIC X      VALUE SPACES.
               05  FILLER      PIC X(9)   VALUE "QTY".
           01 REPORT-TOTAL.
			   05 FILLER       PIC X(14)  VALUE "Total Sales: $".
			   05 TOTAL-SALE   PIC 9(5) VALUE 0.
		   01 REPORT-TAX.
			   05 FILLER       PIC X(14)  VALUE "Sales Tax:   $".
			   05 SALES-TAX    PIC 9(5) VALUE 0.
		   01 REPORT-FINAL.
               05 FILLER       PIC x(14)  VALUE "Final Sales: $".
			   05 FINAL-SALE   PIC 9(5) VALUE 0.
           01 WS-CONSTANTS.
			   05 WS-TAX       PIC 9v99 VALUE 0.05.
               


       procedure division.
       100-MAIN.
           OPEN EXTEND SALES-FILE.
		   WRITE SALES-FILE-ID FROM COL-HDR.
		   DISPLAY"ADD NEW CUSTOMER - Y OR N" ACCEPT NEWCUST
               PERFORM 100-LOOP UNTIL NEWCUST EQUALS "N" 
			   PERFORM 100-WRITE-REPORT
		   CLOSE SALES-FILE.


           goback.
       100-LOOP.
		           DISPLAY "CUSTOMER-NO: "
                   ACCEPT CUST-NUM
                   DISPLAY "CUSTOMER-NAME: "
                   ACCEPT CUSTO-NAME
                   DISPLAY "UNIT-PRICE INPUT: "
                   ACCEPT UNIT-PRICES
                   DISPLAY "QUNATITYS-SOLD: "
                   ACCEPT QUANTITYS-SOLD

                   MOVE SALESFILES-ID to SALES-FILE-ID
                   WRITE SALES-FILE-ID.
				   PERFORM 100-CALCULATE-TOTALS
				   DISPLAY"ADD NEW CUSTOMER - Y OR N"
                   ACCEPT NEWCUST.

       100-WRITE-REPORT.
		   WRITE SALES-FILE-ID FROM REPORT-TOTAL
		   WRITE SALES-FILE-ID FROM REPORT-TAX
		   WRITE SALES-FILE-ID FROM REPORT-FINAL.

	   100-CALCULATE-TOTALS.
		   IF QUANTITYS-SOLD > 0
		   COMPUTE TEMP-SALE = QUANTITYS-SOLD * UNIT-PRICES
			   ADD TEMP-SALE to TOTAL-SALE
			   COMPUTE TEMP-TAX = (WS-TAX * TEMP-SALE)
			   ADD TEMP-TAX TO SALES-TAX
		   ELSE IF QUANTITYS-SOLD < 0
		   COMPUTE TEMP-SALE = QUANTITYS-SOLD * UNIT-PRICES
			   subtract TEMP-SALE from TOTAL-SALE
			   COMPUTE TEMP-TAX = (WS-TAX * TEMP-SALE)
			   subtract TEMP-TAX from SALES-TAX
           END-IF
		   COMPUTE FINAL-SALE = SALES-TAX + TOTAL-SALE.
       end program Program1.