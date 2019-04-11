       identification division.
       program-id. Program1.

       environment division.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT SALES-FILE ASSIGN TO 'input.txt'
                       ORGANIZATION IS LINE SEQUENTIAL
                       ACCESS IS SEQUENTIAL.


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
           01 SALESFILES-ID.
               05 CUST-NUM PIC 9(4).
               05 CUSTO-NAME PIC A(24).
               05 UNIT-PRICES PIC 9(5).
               05 QUANTITYS-SOLD PIC 9(3).
           



               


       procedure division.
       100-MAIN.
           OPEN EXTEND SALES-FILE.
               PERFORM 2 TIMES 
                   DISPLAY "CUSTOMER-NO: "
                   ACCEPT CUST-NUM
                   DISPLAY "CUSTOMER-NAME: "
                   ACCEPT CUSTO-NAME





               MOVE 
               
                   MOVE 
                   WRITE SALE-FILE
                   END WRITE
               END-PERFORM


           goback.
           
       end program Program1.