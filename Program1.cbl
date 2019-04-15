       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGRAM1.
      ******************************************************************
      *    Creates a sales transaction file to record a series of 
      *    transactions.  User determines the number of records created
      *    and enters all information through the console.  Also
      *    creates a report file to tell if a record was entered into
      *    the sales file or not.
      *
      *    Authors: Grant Haylock, Jack Rasmussen,
      *             Zach Schultz, Austin Lee
      *    Date Written: 04-07-2019
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SALES-FILE ASSIGN TO
               "C:\Users\Caulder\Documents\SalesFile.txt"
           ORGANIZATION IS LINE SEQUENTIAL
           ACCESS IS SEQUENTIAL.

       CONFIGURATION SECTION.

       DATA DIVISION.
       FILE SECTION.
       FD SALES-FILE.
       01 SALES-FILE-ID.
           05 CUST-NO            PIC 9(4).
		   05                    PIC X VALUE SPACES.
           05 CUST-NAME          PIC A(20).
		   05                    PIC X VALUE SPACES.
           05 UNIT-PRICE         PIC 9(5).
		   05                    PIC X VALUE SPACES.
           05 QUANTITY-SOLD      PIC -9(3).
             
       WORKING-STORAGE SECTION.
       01 SALESFILES-ID.
           05 CUST-NUM         PIC 9(4).
		   05 FILLER           PIC X VALUE SPACES.
           05 CUSTO-NAME       PIC A(20).
		   05 FILLER           PIC X VALUE SPACES.
           05 UNIT-PRICES      PIC 9(5) VALUE 0.
		   05 FILLER           PIC X VALUE SPACES.
           05 QUANTITYS-SOLD   PIC S9(3).

       01 TEMP-VAR.
		   05 NEWCUST          PIC A.
		   05 TEMP-SALE        PIC S9(5)V99.
		   05 TEMP-TAX         PIC S9(5)V99.
		   05 TEMP-FINAL       PIC S9(5)V99.

	   01 COL-HDR.
           05  FILLER          PIC X(4)   VALUE "#".
           05  FILLER          PIC X      VALUE SPACES.
           05  FILLER          PIC X(20)  VALUE "Name". 
		   05  FILLER          PIC X      VALUE SPACES.
           05  FILLER          PIC X(5)   VALUE "PRICE".
		   05  FILLER          PIC X      VALUE SPACES.
           05  FILLER          PIC X(9)   VALUE "QTY".

       01 REPORT-TOTAL.
		   05 FILLER           PIC X(14)
               VALUE "Total Sales: ".
		   05 TOTAL-SALE-OUT   PIC -$$,$$$.99         VALUE 0.

	   01 REPORT-TAX.
		   05 FILLER           PIC X(14)
               VALUE  "Sales Tax:   ".
		   05 SALES-TAX-OUT    PIC -$$,$$$.99         VALUE 0.

	   01 REPORT-FINAL.
           05 FILLER           PIC X(14)
               VALUE "Final Sales: ".
		   05 FINAL-SALE-OUT   PIC -$$,$$$.99        VALUE 0.

       01 REPORT-DIVIDER.
           05                  PIC X(36)
           VALUE "************************************".

       01 WS-CONSTANTS.
		   05 WS-TAX           PIC V999 VALUE .065.

       01  WS-WORK-AREA.
           05  TOTAL-SALE      PIC S9(6)V99          VALUE ZEROS.
           05  SALES-TAX       PIC S9(6)V99          VALUE ZEROS.
           05  FINAL-SALE      PIC S9(6)V99          VALUE ZEROS.
           05  ADD-CHECK       PIC X.

       PROCEDURE DIVISION.
      ******************************************************************
      *    100-MAIN-MODULE: Opens Ssales file and report file.   
      *    Writes headers to the sales file.  Prompts user if they  
      *    want to add a new customer record, and then calls a module
      *    to input the customer information.
      *    Calls a module to write the data to the files. 
      *    Lastly closes all files and ends the program.
      ******************************************************************
       100-MAIN.
           OPEN EXTEND SALES-FILE.
		   WRITE SALES-FILE-ID FROM COL-HDR.
           DISPLAY"ADD NEW CUSTOMER - Y OR N"
           ACCEPT NEWCUST
           PERFORM 200-GET-CUST-INFO UNTIL NEWCUST EQUALS "N"
           IF ADD-CHECK EQUALS "Y"
               PERFORM 400-WRITE-REPORT
           END-IF
           WRITE SALES-FILE-ID FROM REPORT-DIVIDER
		   CLOSE SALES-FILE.
           GOBACK
           .
      ******************************************************************
      *    200-GET-CUST-INFO: Displays prompts for customer information
      *    and then accepts the input.  Calls a module to calculate the
      *    totals and taxes, and then prompts the user to input
      *    another record if they would like to.  Also double checks 
      *    if the user wants to add the record to the file.
      ******************************************************************
       200-GET-CUST-INFO.
	      DISPLAY "CUSTOMER NO: "
          ACCEPT CUST-NUM
          DISPLAY "CUSTOMER NAME: "
          ACCEPT CUSTO-NAME
          DISPLAY "UNIT PRICE INPUT: "
          ACCEPT UNIT-PRICES
          DISPLAY "QUANTITIES SOLD: "
          ACCEPT QUANTITYS-SOLD
          MOVE CUST-NUM TO CUST-NO
          MOVE CUSTO-NAME TO CUST-NAME
          MOVE UNIT-PRICES TO UNIT-PRICE
          MOVE QUANTITYS-SOLD to QUANTITY-SOLD
          DISPLAY "ADD RECORD TO FILE?  Y OR N"
          ACCEPT ADD-CHECK
          IF ADD-CHECK EQUALS "Y"
             WRITE SALES-FILE-ID
		     PERFORM 300-CALCULATE-TOTALS
		     DISPLAY "RECORD ADDED. ADD ANOTHER CUSTOMER - Y OR N"
          ELSE
              DISPLAY 
              "RECORD WAS NOT ADDED.  ADD ANOTHER CUSTOMER - Y OR N"
          END-IF
          ACCEPT NEWCUST
          .
      ******************************************************************
      *    300-CALCULATE-TOTALS: Checks to see if quantity entered
      *    above is positive/negative.  If negative, it subtracts 
      *    quantity * price from the running sales total.  If positive, 
      *    it adds quantity * price to the running total.
      *    Calculates sales tax and a subtotal.
      ******************************************************************
	   300-CALCULATE-TOTALS.
		   IF QUANTITYS-SOLD IS POSITIVE
		   COMPUTE TEMP-SALE = QUANTITYS-SOLD * UNIT-PRICES
			   ADD TEMP-SALE TO TOTAL-SALE
			   COMPUTE TEMP-TAX = (WS-TAX * TEMP-SALE)
			   ADD TEMP-TAX TO SALES-TAX
		   ELSE
           COMPUTE TEMP-SALE = QUANTITYS-SOLD * UNIT-PRICES
           MULTIPLY TEMP-SALE by -1 giving TEMP-SALE
			   SUBTRACT TEMP-SALE FROM TOTAL-SALE
			   COMPUTE TEMP-TAX = (WS-TAX * TEMP-SALE)
			   SUBTRACT TEMP-TAX FROM SALES-TAX
           END-IF
           .
      ******************************************************************
      *    400-WRITE-REPORT: Computes the final total sales value,
      *    and then writes to the sales file.
      ******************************************************************
       400-WRITE-REPORT.
		   COMPUTE FINAL-SALE = TOTAL-SALE + SALES-TAX
           MOVE TOTAL-SALE TO TOTAL-SALE-OUT
           MOVE SALES-TAX TO SALES-TAX-OUT
           MOVE FINAL-SALE TO FINAL-SALE-OUT
		   WRITE SALES-FILE-ID FROM REPORT-TOTAL
		   WRITE SALES-FILE-ID FROM REPORT-TAX
		   WRITE SALES-FILE-ID FROM REPORT-FINAL
           .
       END PROGRAM PROGRAM1.
