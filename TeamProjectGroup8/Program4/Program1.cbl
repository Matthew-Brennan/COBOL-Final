       identification division.
       program-id. Program4.
       author. Group8.
       date-written. 2018-04-15.
      *Description:
      *    Takes in validated data and determins amount of Retruns and
      *    the value of all the returns and the value of the tax owed
     

       environment division.
       configuration section.
       file-control.
           select input-file
               assign to "../../data/ReturnRecords_out.dat"
               organization is line sequential.

           select returns-file
               assign to "../../data/program-4-returns.out"
               organization is line sequential.

       data division.
       file section.
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-trans-code            pic x.
           05 il-trans-ammount         pic 9(5)v99.
           05 il-payment-type          pic xx.
           05 il-store-number          pic xx.
           05 il-invoice-number        pic x(9).
           05 il-sku-code              pic x(15).

       fd returns-file
           data record is returns-line
           record contains 80 characters.
          
       01 returns-line.
           05 rl-trans-code            pic x.
           05 filler                   pic x(10).
           05 rl-trans-ammount         pic $(5).99.
           05 filler                   pic x(5).
           05 rl-payment-type          pic xx.
           05 filler                   pic x(11).
           05 rl-store-number          pic xx.
           05 filler                   pic x(11).
           05 rl-invoice-number        pic x(9).
           05 filler                   pic x(6).
           05 rl-sku-code              pic x(15).
           

       working-storage section.
       01 ws-page-header.
           05 filler                   pic x(28)
               value spaces.
           05 filler                   pic x(14)
               value "RETURNS REPORT".
           05 filler                   pic x(30)
               value spaces.
           05 filler                   pic x(6)
               value "PAGE: ".
           05 ws-page-header-number    pic 99
               value 0.

       01 ws-detail-line.
           05 filler                   pic x(49)
              value "Trans Code Trans Amount Payment Type Store Number".
      *              ----+----1----+----2----+----3----+----4----+----5
           05 filler                   pic x(24)
              value " Invoice Number SKU Code".
      *              5----+----6----+----7---
       01 ws-totals-line.
           05 ws-tl-returns-line.
               10 filler               pic x(25)
                   value "TOTAL AMOUNT OF RETURNS: ".
               10 ws-tl-returns        pic 99
                   value 0.
           05 ws-tl-amount-line.
               10 filler               pic x(24)
                   value "TOTAL VALUE OF RETURNS: ".
               10 ws-tl-value          pic $(3)99.99
                   value 0.
           05 ws-tl-tax-line.
               10 filler               pic x(24)
                   value "TOTAL VALUE OF TAX: ".
               10 ws-tl-tax            pic $(3)99.99
                   value 0.

       01 ws-return-type-ID            pic x
           value "R".
       01 ws-page-count                pic 99
           value 0.
       01 ws-line-count                pic 99
           value 0.
       01 ws-line-per-page             pic 99
           value 20.
       01 ws-eof                       pic x
           value "N".
       01 ws-amount-of-returns         pic 99
           value 0.
       01 ws-total-ammount             pic 9(5)v99
           value 0.
       01 ws-total-tax                 pic 9(5)v99
           value 0.
       01 ws-tax-rate                  pic 9v99
           value 0.13.

       procedure division.
           open input input-file,
                output returns-file.

           read input-file at end move "Y" to ws-eof.

           perform 210-print-headers.
       

           perform 200-process-records until ws-eof equals "Y".
               
           perform 310-print-totals.
           



           close input-file
                 returns-file.
           stop run.

       100-print-headings.
           write returns-line from ws-page-header
               after advancing 1 line.
           write returns-line from ws-detail-line.
           

       110-print-headings-first-page.
           write returns-line from ws-page-header.
           write returns-line from ws-detail-line.
           

       200-process-records.
           if ws-line-count > ws-line-per-page
               perform 210-print-headers
           end-if

           if il-trans-code equals ws-return-type-ID
               perform 220-process-lines
           end-if.
        
           read input-file at end move "Y" to ws-eof.

       210-print-headers.
           add 1 to ws-page-count
               move ws-page-count          to ws-page-header-number
           set ws-line-count               to zero

           if ws-page-count > 1
               perform 100-print-headings
           else if ws-page-count = 1
                       perform 110-print-headings-first-page
                   end-if
           end-if.

       220-process-lines.
           
               add 1 to ws-line-count
               add 1 to ws-amount-of-returns.
               perform 300-calculate-totals.
               perform 400-move-values.

       

       300-calculate-totals.
           add il-trans-ammount to ws-total-ammount.

       310-print-totals.
           compute ws-total-tax
               rounded = (ws-total-ammount * ws-tax-rate).

           move ws-amount-of-returns      to ws-tl-returns.
           move ws-total-ammount          to ws-tl-value.
           move ws-total-tax              to ws-tl-tax.


           write returns-line from ws-tl-returns-line after
             advancing 2 lines.
           write returns-line from ws-tl-amount-line.
           write returns-line from ws-tl-tax-line.

           

           
       400-move-values.
           move spaces                     to returns-line.
           move il-trans-code              to rl-trans-code.
           move il-trans-ammount           to rl-trans-ammount. 
           move il-payment-type            to rl-payment-type.
           move il-store-number            to rl-store-number.
           move il-invoice-number          to rl-invoice-number.
           move il-sku-code                to rl-sku-code.

           write returns-line before advancing 1 lines.

       end program Program4.