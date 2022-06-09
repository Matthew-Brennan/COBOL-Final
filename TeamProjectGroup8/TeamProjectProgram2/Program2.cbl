       program-id. Program2
       as "TeamProjectProgram2.Program2".
       author. "Keenan Binnie-Cormier".   
       date-written. 2018-04-20.
      *Description:
      *   Will split validated input data into two files based on
      *   transaction type for further processing

       environment division.
       configuration section.
       input-output section.
       file-control.

*      configure input and output files 

           select input-file
               assign to "../../data/valid.dat"
               organization is line sequential.
               
           select sales-report-file
               assign to "../../data/SalesRecords_out.dat"
               organization is line sequential.

            select returns-report-file
               assign to "../../data/ReturnRecords_out.dat"
               organization is line sequential.

            select totals-report-file
               assign to "../../data/TotalsRecords_out.dat"
               organization is line sequential.

       data division.

        file section.
       
       fd  input-file 
               data record is employee-rec
               record contains 150 characters.

*      declare input record definition

       01  sales-record.
           05  sr-trans-code           pic x.
           05  sr-trans-amount         pic 9(5)v99. 
           05  sr-payment-type         pic xx.
           05  sr-store-number         pic 99.
           05  sr-invoice-number       pic x(9).
           05  sr-sku-code             pic x(15).

       fd  sales-report-file 
               data record is ws-prt-line
               record contains 99 characters.
*      declare output record definition
           
       01 sales-output-line             pic x(150).

       fd  returns-report-file 
               data record is ws-prt-line
               record contains 99 characters.

       01 returns-output-line         pic x(150).

        fd  totals-report-file 
               data record is ws-prt-line
               record contains 99 characters.

       01 totals-output-line         pic x(150).

       working-storage section.

       01  combined-count-record.
           05  filler                  pic x(1).
           05  filler                  pic x(40)
               value "TOTAL SALES AND LAYAWAY RECORDS: ".
           05  filler                  pic x(1).
           05  filler                  pic xxxx.
           05  ws-combined-count       pic 99
               value 0.

       01  total-sales-records.
           05  filler                  pic x(1).
           05  filler                  pic x(18)
               value "TOTAL SALES COUNT: ".
           05  filler                  pic xxx.
           05  ws-sales-count          pic 99
               value 0.
           05  filler                  pic xxxxxx.
           05  filler                  pic x(19)
               value "TOTAL SALES AMOUNT: ".
           05  ws-total-sales-amount   pic $(4),$(3).99
               value 0.

       01  total-layaway-records.
           05  filler                  pic x(1).
           05  filler                  pic x(20)
               value "TOTAL LAYAWAY COUNT: ".
           05  filler                  pic x(1).
           05  ws-layaway-count        pic 99
               value 0.
           05  filler                  pic xxxx.
           05  filler                  pic x(21)
               value "TOTAL LAYAWAY AMOUNT: ".
           05  ws-total-layaway-amount pic $(4),$(3).99
               value 0.

       01  store-totals-records.
           05  filler                  pic x(1).
           05  filler                  pic x(15)
               value "STORE 01 TOTAL: ".
           05  ws-store-01-total       pic 99
               value 0.
           05  filler                  pic xxxx.
           05  filler                  pic x(15)
               value "STORE 02 TOTAL: ".
           05  ws-store-02-total       pic 99
               value 0.
           05  filler                  pic xxxx.
           05  filler                  pic x(15)
               value "STORE 03 TOTAL: ".
           05  ws-store-03-total       pic 99
               value 0.
           05  filler                  pic xxxx.
           05  filler                  pic x(15)
               value "STORE 07 TOTAL: ".
           05  ws-store-07-total       pic 99
               value 0.

       01  payment-type-percent.
           05  filler                  pic x(1).
           05  filler                  pic x(20)
               value "PERCENT OF TYPE CA: ".
           05  filler                  pic x
               value "%".
           05  ws-ca-percent           pic zz.z
               value 0.
           05  filler                  pic x(2).
           05  filler                  pic x(20)
               value "PERCENT OF TYPE CR: ".
            05  filler                  pic x
               value "%".
           05  ws-cr-percent           pic zz.z
               value 0.
           05  filler                  pic x(2).
           05  filler                  pic x(20)
               value "PERCENT OF TYPE DB: ".
           05  filler                  pic x
               value "%".
           05  ws-db-percent           pic zz.z
               value 0.
           
        01  name-line.
           05  filler                  pic x(20)
               value spaces.
           05  filler                  pic x(28)
               value "Team Project, Group 8".
           05  filler                  pic x(5)
               value spaces.
           05  nl-date                 pic 9(6).
           05  filler                  pic x(5)
               value spaces.
           05  nl-time                 pic 9(8). 

       01 counts-and-totals-heading.
           05 filler                   pic x(20).
           05 filler                   pic x(25)
              value "COUNTS AND TOTALS REPORT".

       01  sw-eof                      pic x
           value 'n'.

       01 ws-sales-code                pic x
           value "S".

       01 ws-layaway-code              pic x
           value "L".

       01 ws-return-code               pic x
           value "R".

       01 ws-sales-amount              pic 9(5)v99
           value 0.

       01 ws-layaway-amount            pic 9(5)v99
           value 0.

       01 ws-store-01-amount           pic 9(5)v99
           value 0.

       01 ws-store-02-amount           pic 9(5)v99
           value 0.

       01 ws-store-03-amount           pic 9(5)v99
           value 0.

       01 ws-store-07-amount           pic 9(5)v99
           value 0.
       
       01 ws-store-01-code             pic 99
           value 01.
      
       01 ws-store-02-code             pic 99
           value 02.

       01 ws-store-03-code             pic 99
           value 03.

       01 ws-store-07-code             pic 99
           value 07.
           
       01 ws-ca-type                   pic xx
           value "CA".

       01 ws-cr-type                   pic xx
           value "CR".

       01 ws-db-type                   pic xx
           value "DB".

       01 ws-ca-total                  pic 99
           value 0.

       01 ws-cr-total                  pic 99
           value 0.

       01 ws-db-total                  pic 99
           value 0.

       01 ws-ca-perc                   pic 99v9
           value 0.

       01 ws-cr-perc                   pic 99v9
           value 0.

       01 ws-db-perc                   pic 99v9
           value 0.

       01 ws-total-records             pic 99
           value 0.

       01 ws-a-hundo                   pic 999
           value 100.

       procedure division.

         open input input-file,
                output sales-report-file,
                output returns-report-file,
                output totals-report-file.
           
           accept nl-date from date.
           accept nl-time from time.

           read input-file 
                   at end move "y" to sw-eof.

           perform 001-process-input until sw-eof equals "y".
           perform 300-calculate-percentages.
           perform 500-move-to-output.
           perform 600-write-totals-report.
           
           goback.
           
       001-process-input.
           add 1                       to ws-total-records.
           perform 002-clear-ouput-buffer.
           perform 003-classify-record.
           perform 004-classify-store.
           perform 005-classify-type.

           read input-file 
                   at end move "y" to sw-eof.

       002-clear-ouput-buffer.
           move spaces to sales-output-line.
           move spaces to returns-output-line.
           move spaces to totals-output-line.
           
       003-classify-record.
           if sr-trans-code = ws-sales-code then
*      record is sales
               perform 101-process-sales-record
           else
           if sr-trans-code = ws-layaway-code then
*      record is layaway
               perform 102-process-layaway-record
           else
           if sr-trans-code = ws-return-code then
*      record is return
               perform 103-process-return-record
           end-if
           end-if
           end-if.

       004-classify-store.
            if sr-store-number = ws-store-01-code then
*      store 01
               add 1                   to ws-store-01-total
               add sr-trans-amount     to ws-store-01-amount 
           else
           if sr-store-number = ws-store-02-code then
*      store 02
               add 1                   to ws-store-02-total
               add sr-trans-amount     to ws-store-02-amount 
           else
           if sr-store-number = ws-store-03-code then
*      store 03
               add 1                   to ws-store-03-total
               add sr-trans-amount     to ws-store-03-amount 
            else
           if sr-store-number = ws-store-07-code then
*      store 07
               add 1                   to ws-store-07-total
               add sr-trans-amount     to ws-store-07-amount 
           end-if
           end-if
           end-if
           end-if.

       005-classify-type.
            if sr-payment-type = ws-ca-type then
*      payment type is CA
               add 1                   to ws-ca-total 
           else
           if sr-payment-type = ws-cr-type then
*      payment type is CR
               add 1                   to ws-cr-total 
           else
           if sr-payment-type = ws-db-type then
*      payment type is DB
               add 1                   to ws-db-total 
           end-if
           end-if
           end-if.

       101-process-sales-record.
           write sales-output-line from sales-record.
           add 1 to ws-sales-count.
           add sr-trans-amount         to ws-sales-amount.
           perform 201-increment-combined-total.

       102-process-layaway-record.
           write sales-output-line from sales-record.
           add 1 to ws-layaway-count.
           add sr-trans-amount         to ws-layaway-amount.
           perform 201-increment-combined-total.
           
       103-process-return-record.
           write returns-output-line from sales-record.

       201-increment-combined-total.
           add 1 to ws-combined-count.

       300-calculate-percentages.
           compute ws-ca-perc rounded =  ws-ca-total / ws-total-records * ws-a-hundo.
           compute ws-cr-perc rounded = ws-cr-total / ws-total-records * ws-a-hundo.
           compute ws-db-perc rounded = ws-db-total / ws-total-records * ws-a-hundo.

       500-move-to-output.
           move ws-ca-perc to ws-ca-percent.
           move ws-cr-perc to ws-cr-percent.
           move ws-db-perc to ws-db-percent.
           move ws-sales-amount to ws-total-sales-amount.
           move ws-layaway-amount to ws-total-layaway-amount.

       600-write-totals-report.
           write totals-output-line from name-line
               after advancing 1 line.
           write totals-output-line from counts-and-totals-heading
               after advancing 1 line.
           write totals-output-line from combined-count-record
               after advancing 2 lines.
           write totals-output-line from total-sales-records
               after advancing 1 line.
           write totals-output-line from total-layaway-records
               after advancing 1 line.
           write totals-output-line from store-totals-records
               after advancing 1 line.
           write totals-output-line from payment-type-percent
               after advancing 1 line.
         
       end program Program2.
