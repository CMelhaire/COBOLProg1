       identification division.
       program-id. Program1.

       environment division.
       input-output section.
       FILE-CONTROL.
           select BONUS-TIME assign to 'C:\COBOL\emp.dat'
           ORGANIZATION IS LINE SEQUENTIAL.
       configuration section.

       data division.
       FD BONUS-TIME.
       01 BONUS-REC.
           05 EMPLOY-No pic X(5).
           05 EMPLOY-Name pic X(19).
           05 Territory-No pic 9(2).
           05 Annual-Salary pic 9(23).
           05 Job-Code pic 9(31). 
       working-storage section.
       01 name1 pic X(3).
       01 price2 pic 9(4).
       01 jobtype pic 9(2).
       01 territoryType pic 9(2). 
       01 MORE-DATA pic X(3) value "yes".
       01 IS-DATA-OK pic X(3) value "yes".
          88 OK-DATA value "yes".
       01 totalBonus pic 9(9).




       procedure division.
       main100.
           open output BONUS-TIME.
           read BONUS-TIME.
           perform until MORE-DATA = 'no'.

       CheckMain200.
           Check-type-100.
           if jobtype is equal to 01 then
               if territoryType is equal to 01 or 02 then
      *            12% bonus to salary
               else 
      *            $200 bonus to salary
               end-if
           else if jobType is not equal to 01 or 02 then
               if territoryType is equal to 01 then
      *            10% bonus to salary 
               else if territoryType is equal to 02 then 
      *            $500 bonus to salary
               end-if
           end-if

           if jobtype is equal to 02 then
               if territoryType is equal to 02 or 03 
      *           11% bonus to salary 
               else 
      *            $150 bonus to salary 
           end-if


           perform Check-type-100.



          

           goback.
           
       end program Program1.