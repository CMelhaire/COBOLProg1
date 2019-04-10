       identification division.
       program-id. Program1.

       environment division.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT SALES-FILE ASSIGN TO 'input.txt'
                       ORGANIZATION IS LINE SEQUENTIAL.

       configuration section.
           

       data division.
       FILE SECTION.
           FD SALES-FILE.
           01 SALES-FILE-ID.
               05 CUST-NO PIC 9(4).
               05 CUST-NAME PIC A(24).
               05 UNIT-PRICE PIC 9(5).
               05 QUANTITY-SOLD PIC 9(3).
       working-storage section.
           01 SALES-FILE-ID.
               05 CUST-NO PIC 9(4).
               05 CUST-NAME PIC A(24).
               05 UNIT-PRICE PIC 9(5).
               05 QUANTITY-SOLD PIC 9(3).
           01 WS-EOF PIC A(1).

		   01 RUNNING-TOTTAL.
			   05 TOTAL-SALE  PIC 9(5).
			   05 TOTAL-TAX   PIC 9(5).
			   05 TOTAL-FINAl PIC 9(5).

               


       procedure division.
       100-MAIN.
           OPEN INPUT SALES-FILE. 
               PERFORM UNTIL WS-EOF = 'Y'


           goback.
           
       end program Program1.