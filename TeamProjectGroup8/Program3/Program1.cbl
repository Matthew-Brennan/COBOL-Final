       identification division.
       program-id. Program3.
       author. Group8.
       date-written. 2018-04-15.
      *Description:
      *    Takes in validated data and determins amount of sales and
      *    layaways as well as their cash value. it also determins
      *    the percentage of cash credit and debit transactions,
      *    the total value of tax, and the store with the most
      *    transactions

       environment division.
       configuration section.
       file-control.
           select input-file
               assign to "../../data/SalesRecords_out.dat"
               organization is line sequential.

           select returns-file
               assign to "../../data/program-3.out"
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
           05 ws-tl-sales-line.
               10 filler               pic x(26)
                   value "TOTAL AMOUNT OF SALES: ".
               10 ws-tl-sales          pic 99
                   value 0.
               10 filler               pic x(26)
                   value " TOTAL VALUE OF SALES: ".
               10 ws-tl-sales-value    pic $(5)99.99
                   value 0.
           05 ws-tl-layaways-line.
               10 filler               pic x(26)
                   value "TOTAL AMOUNT OF LAYAWAYS: ".
               10 ws-tl-layaways       pic 99
                   value 0.
               10 filler               pic x(26)
                   value " TOTAL VALUE OF LAYAWAYS: ".
               10 ws-tl-layaway-value  pic $(5)99.99
                   value 0.
           05 ws-tl-cash-line.
               10 filler               pic x(29)
                   value "PERCENT OF CASH PAYMENTS:   %".
               10 ws-tl-cash           pic z9.99
                   value 0.
           05 ws-tl-credit-line.
               10 filler               pic x(29)
                   value "PERCENT OF CREDIT PAYMENTS: %".
               10 ws-tl-credit         pic z9.99
                   value 0.
           05 ws-tl-debit-line.
               10 filler               pic x(29)
                   value "PERCENT OF DEBIT PAYMENTS:  %".
               10 ws-tl-debit          pic z9.99
                   value 0.
           05 ws-tl-tax-line.
               10 filler               pic x(24)
                   value "TOTAL VALUE OF TAX: ".
               10 ws-tl-tax            pic $(3)99.99
                   value 0.
           05 ws-tl-most-transactions-line.
               10 filler               pic x(41)
                   value "STORE NUMBER WITH THE MOST TRANSACTIONS: ".
               10 ws-tl-store-number   pic xx.

       01 ws-store-table.
           05 ws-store occurs 4 times.
               10 ws-store-number      pic xx.
               10 ws-number-per-store  pic 99.
                   


       01 ws-return-type-ID            pic x
           value "R".
       01 ws-sale-type-ID              pic x
           value "S".
       01 ws-layaway-type-ID           pic x
           value "L".
       01 ws-cash-ID                   pic xx
           value "CA".
       01 ws-credit-ID                 pic xx
           value "CR".
       01 ws-debit-ID                  pic xx
           value "DB".

       01 ws-page-count                pic 99
           value 0.
       01 ws-line-count                pic 99
           value 0.
       01 ws-line-per-page             pic 99
           value 20.
       01 ws-eof                       pic x
           value "N".
       01 ws-amount-of-sales           pic 99
           value 0.
       01 ws-amount-of-layaway         pic 99
           value 0.
       01 ws-total-sales-value         pic 9(5)v99
           value 0.
       01 ws-total-layaway-value       pic 9(5)v99
           value 0.
       01 ws-total-tax                 pic 9(5)v99
           value 0.
       01 ws-tax-rate                  pic 9v99
           value 0.13.
       01 ws-num-cash-payments         pic 999
           value 0.
       01 ws-num-credit-payments       pic 999
           value 0.
       01 ws-num-debit-payments        pic 999
           value 0.
       01 ws-payment-percentage        pic 99v99
           value 0.
       01 ws-total-transactions        pic 999
           value 0.
       01 ws-store-01                  pic xx.
       01 ws-store-02                  pic xx.
       01 ws-store-03                  pic xx.
       01 ws-store-07                  pic xx.

       procedure division.
           open input input-file,
                output returns-file.

           read input-file at end move "Y" to ws-eof.

           perform 210-print-headers.
           perform 230-process-table.

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
           if ws-line-count >= ws-line-per-page
               perform 210-print-headers
           end-if

           if il-trans-code is not equal ws-return-type-ID
               perform 220-process-lines
           end-if.
        
           read input-file at end move "Y" to ws-eof.

       210-print-headers.
           add 1                           to ws-page-count
               move ws-page-count          to ws-page-header-number
           set ws-line-count               to zero

           if ws-page-count > 1
               perform 100-print-headings
           else if ws-page-count = 1
                       perform 110-print-headings-first-page
                   end-if
           end-if.

       220-process-lines.
           add 1                           to ws-line-count.
               
           perform 300-calculate-totals.
           perform 400-move-values.

      *Set up the values for the store table
       230-process-table.
           move "01"                       to ws-store-number(1).
           move "02"                       to ws-store-number(2).
           move "03"                       to ws-store-number(3).
           move "07"                       to ws-store-number(4).

           move zero                       to ws-number-per-store(1).
           move zero                       to ws-number-per-store(2).
           move zero                       to ws-number-per-store(3).
           move zero                       to ws-number-per-store(4).

       300-calculate-totals.

      *Determin the current transaction type
           if il-trans-code equals ws-sale-type-ID
               add il-trans-ammount        to ws-total-sales-value
               add 1                       to ws-amount-of-sales
           
           else if il-trans-code equals ws-layaway-type-ID
                   add il-trans-ammount    to ws-total-layaway-value
                   add 1                   to ws-amount-of-layaway
               end-if
           end-if.

      *Determin the current type of transaction
           if il-payment-type equals ws-cash-ID
               add 1                       to ws-num-cash-payments

           else if il-payment-type equals ws-credit-ID
                   add 1                   to ws-num-credit-payments

               else if il-payment-type equals ws-debit-ID
                       add 1               to ws-num-debit-payments

                   end-if
               end-if
           end-if.

      *Increment number of transactions for each store
           if il-store-number equals ws-store-number(1)
               add 1                       to ws-number-per-store(1)
           else if il-store-number equals ws-store-number(2)
                   add 1                   to ws-number-per-store(2)
                
                else if il-store-number equals ws-store-number(3)
                       add 1               to ws-number-per-store(3)
               
                    
                   else if il-store-number equals ws-store-number(4)
                           add 1           to ws-number-per-store(4)

                        end-if
                    end-if
                end-if
           end-if.
    

       310-print-totals.
           add ws-amount-of-layaway        to ws-amount-of-sales
               giving ws-total-transactions

           compute ws-total-tax
               rounded = ((ws-total-sales-value + ws-total-layaway-value
               )* ws-tax-rate).

      *Move numeric values to alphanumeric storage for use with MAX
           move ws-number-per-store(1)     to ws-store-01.
           move ws-number-per-store(2)     to ws-store-02.
           move ws-number-per-store(3)     to ws-store-03.
           move ws-number-per-store(4)     to ws-store-07.

      *Determin which store had the highest number of transactions
           if ws-number-per-store(4) equals
              Function MAX (ws-store-01 ws-store-02
              ws-store-03 ws-store-07)
               move ws-store-number(4)     to ws-tl-store-number

           else if ws-number-per-store(3) equals
                 Function MAX (ws-store-01 ws-store-02
                 ws-store-03 ws-store-07)
                   move ws-store-number(3) to ws-tl-store-number

               else if ws-number-per-store(2) equals
                    Function MAX (ws-store-01 ws-store-02
                    ws-store-03 ws-store-07)
                       move ws-store-number(2)
                                           to ws-tl-store-number

                   else if ws-number-per-store(3) equals
                        Function MAX (ws-store-01 ws-store-02
                        ws-store-03 ws-store-07)
                           move ws-store-number(3)
                                           to ws-tl-store-number
               
                       end-if
                   end-if
               end-if
           end-if.

       


      *compute the percentage of cash transactions
           compute ws-payment-percentage rounded =
               ws-num-cash-payments / ws-total-transactions * 100.
           move ws-payment-percentage      to ws-tl-cash.

      *compute the percentage of credit transactions
           compute ws-payment-percentage rounded =
               ws-num-credit-payments / ws-total-transactions * 100.
           move ws-payment-percentage      to ws-tl-credit.

      *compute the percentage of cash transactions
           compute ws-payment-percentage rounded =
               ws-num-debit-payments / ws-total-transactions * 100.
           move ws-payment-percentage      to ws-tl-debit.

           move ws-amount-of-sales         to ws-tl-sales.
           move ws-total-sales-value       to ws-tl-sales-value.
           move ws-amount-of-layaway       to ws-tl-layaways.
           move ws-total-layaway-value     to ws-tl-layaway-value.
           move ws-total-tax               to ws-tl-tax.


           write returns-line from ws-tl-sales-line after
             advancing 1 lines.
           write returns-line from ws-tl-layaways-line.
           write returns-line from ws-tl-cash-line.
           write returns-line from ws-tl-credit-line.
           write returns-line from ws-tl-debit-line.
           write returns-line from ws-tl-tax-line.
           write returns-line from ws-tl-most-transactions-line.

           

           
       400-move-values.
           move spaces                     to returns-line.
           move il-trans-code              to rl-trans-code.
           move il-trans-ammount           to rl-trans-ammount. 
           move il-payment-type            to rl-payment-type.
           move il-store-number            to rl-store-number.
           move il-invoice-number          to rl-invoice-number.
           move il-sku-code                to rl-sku-code.

           write returns-line before advancing 1 lines.

       end program Program3.