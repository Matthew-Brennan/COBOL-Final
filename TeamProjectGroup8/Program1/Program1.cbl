       identification division.
       program-id. VALIDATOR as "project1_VALIDATION.Program1".
       author. Group8.
       date-written. 2018-04-15.
      *Description:
      *    Takes in a data file with preset content,
      *    and sorts data into invalid or valid content. 
      *    All invalid data must be accounted for with
      *    a matching error report.
       environment division.

       input-output section.
       file-control.
           select input-file  assign to "../../data/project1.dat"
               organization is line sequential.
           select error-log   assign to "../../data/errors.dat"
               organization is line sequential.
           select invalid-log assign to "../../data/invalid.dat"
               organization is line sequential.
           select valid-log   assign to "../../data/valid.dat"
               organization is line sequential.

       data division.
       file section.

       fd input-file
          record contains 5714 characters
          data record is input-rec.
      *Contains our input line
       01 input-rec.
           05 input-code             pic x.
           05 input-amount           pic 9(5)V99.
           05 input-type             pic xx.
           05 input-storenum         pic xx.
           05 input-invoicenum       pic x(9).
           05 input-sku              pic x(15).

      *Used to write out to our error file
       fd error-log
           record contains 0 characters
           data record is prt-error.

       01 prt-error.
           05 filler                 pic x(1).
           05 error-loc              pic Z(3).
           05 filler                 pic x(5).
           05 error-rec              pic x(36).
           05 filler                 pic x(2).
           05 error-desc             pic x(25).

      *Used to write out to our invalid file
       fd invalid-log
           record contains 0 characters
           data record is prt-inval.

       01 prt-inval.
           05 invalid-code           pic x(36).

      *Used to write out to our valid file
       fd valid-log
           record contains 0 characters
           data record is prt-valid.

       01 prt-valid.
           05 valid-code             pic x(36).
       

       working-storage section.

       01 sw-eof                     pic x value 'n'.

       01 ws-printed-headers         pic x value 'n'.

       01 ws-record-counter          pic 999
           value 0.

       01 ws-error-counter           pic 9
           value 0.
       01 ws-total-errors            pic 9(3)
           value 0.

       01 ws-input-sec.
           05 ws-input-code          pic x.
               88 88-code-validate   value 'S', 'R', 'L'.
           05 ws-input-amount        pic 9(5)V99.
               88 88-amt-validate    value 0 thru 99999.99.
           05 ws-input-type          pic xx.
               88 88-type-validate   value 'CA', 'CR', 'DB'.
           05 ws-input-storenum      pic xx.
               88 88-store-validate  value '01', '02', '03', '07'.
           05 ws-input-invoicenum.
               10 invoice-ident      pic xx.
                   88 88-inv-ident   value 'AA' thru 'ZZ'.
               10 invoice-dash       pic x.
               10 invoice-num        pic 9(6).
                   88 88-inv-num     value 0 thru 999999.
           05 ws-input-sku           pic x(15).
               88 88-sku-alpha       value '1' thru '9', 'A' thru 'Z'.

       01 ws-error-flags.
           05 ws-code-flag           pic x value 'n'.
           05 ws-amt-flag            pic x value 'n'.
           05 ws-paytype-flag        pic x value 'n'.
           05 ws-storenum-flag       pic x value 'n'.
           05 ws-invoice-flag        pic x value 'n'.
           05 ws-sku-flag            pic x value 'n'.
           05 ws-invalid-flag        pic x value 'n'.

       01 ws-header-error-log-l1.
           05 filler                 pic x(1)
               value spaces.
           05 filler                 pic x(6)
               value "RECORD".
           05 filler                 pic x(17)
               value spaces.
           05 filler                 pic x(6)
               value "RECORD".
           05 filler                 pic x(22)
               value spaces.
           05 filler                 pic x(6)
               value "RECORD".

       01 ws-header-error-log-l2.
           05 filler                 pic x(1)
               value spaces.
           05 filler                 pic x(6)
               value " LINE".
           05 filler                 pic x(16)
               value spaces.
           05 filler                 pic x(8)
               value "CONTENTS".
           05 filler                 pic x(21)
               value spaces.
           05 filler                 pic x(6)
               value "ERRORS".

       01 ws-header-error-log-lines.
           05 filler                 pic x(1)
               value spaces.
           05 filler                 pic x(6)
               value "------".
           05 filler                 pic x(17)
               value spaces.
           05 filler                 pic x(6)
               value "------".
           05 filler                 pic x(22)
               value spaces.
           05 filler                 pic x(6)
               value "------".

       01 ws-error-log-padding.
           05 filler                 pic x(1)
               value spaces.
           05 filler                 pic x(46)
               value spaces.
           05 ws-error-log-message   pic x(25)
               value spaces.

       01 ws-error-log-footer.
           05 filler                 pic x(1)
               value spaces.
           05 filler                 pic x(5)
               value spaces.
           05 ws-error-footer-msg    pic x(24)
               value "TOTAL ERRONEOUS RECORDS:".
           05 filler                 pic x(2)
               value spaces.
           05 ws-error-footer-count  pic Z(3)
               value 0.

       01 ws-error-log-content.
           05 ws-prt-error-transcode pic x(20)
               value "BAD TRANSACTION CODE".
           05 ws-prt-error-transamt  pic x(21)
               value "BAD TRANSACTION VALUE".
           05 ws-prt-error-paytype   pic x(16)
               value "BAD PAYMENT TYPE".
           05 ws-prt-error-storenum  pic x(16)
               value "BAD STORE NUMBER".
           05 ws-prt-error-invoice   pic x(16)
               value "BAD INVOICE DATA".
           05 ws-prt-error-sku       pic x(12)
               value "BAD SKU".

       procedure division.

           open input input-file,
               output error-log, invalid-log, valid-log.

           read input-file into input-rec.

           perform until sw-eof = 'y'
               add 1 to ws-record-counter
               perform 000-perform-validation
           end-perform.
           if ws-printed-headers equals 'y' then
               perform 600-print-error-footer
           end-if.
           
           close input-file,
                 error-log, invalid-log, valid-log.
           stop run.

       000-perform-validation.
           perform 700-reset-flags.
           move input-rec to ws-input-sec.
           if not 88-code-validate then
               set ws-code-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           if not 88-amt-validate or not ws-input-amount is numeric then
               set ws-amt-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           if not 88-type-validate then
               set ws-paytype-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           if not 88-store-validate then
               set ws-storenum-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           if not 88-inv-ident or not 88-inv-num or not invoice-num
             is numeric then
               set ws-invoice-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           if not 88-sku-alpha then
               set ws-sku-flag to 'y'
               set ws-invalid-flag to 'y'
           end-if.
           perform 100-decide-output.
           read input-file into input-rec
               at end move 'y' to sw-eof.
       
       100-decide-output.
           if ws-invalid-flag equals 'y' then
               add 1 to ws-total-errors
               if ws-printed-headers = 'n' then
                   perform 400-print-error-headers
                   set ws-printed-headers to 'y'
               end-if
               perform 300-print-invalid-content
           else
               perform 200-print-valid-content
           end-if.
           

       200-print-valid-content.
           write prt-valid from input-rec.

       300-print-invalid-content.
           write prt-inval from input-rec.
           perform 500-print-error-content.

       400-print-error-headers.
           write prt-error from ws-header-error-log-l1.
           write prt-error from ws-header-error-log-l2.
           write prt-error from ws-header-error-log-lines.
           write prt-error from spaces.

       500-print-error-content.
           move ws-record-counter to error-loc.
           move input-rec to error-rec.
           inspect ws-error-flags tallying ws-error-counter for all 'y'.
           subtract 1 from ws-error-counter.
           STRING ws-error-counter delimited by size
                  " ERRORS FOUND" delimited by size
           into error-desc.
           write prt-error.
           if ws-code-flag equals 'y' then
               move ws-prt-error-transcode to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.
           if ws-amt-flag equals 'y' then
               move ws-prt-error-transamt to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.
           if ws-paytype-flag equals 'y' then
               move ws-prt-error-paytype to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.
           if ws-storenum-flag equals 'y' then
               move ws-prt-error-storenum to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.
           if ws-invoice-flag equals 'y' then
               move ws-prt-error-invoice to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.
           if ws-sku-flag equals 'y' then
               move ws-prt-error-sku to ws-error-log-message
               write prt-error from ws-error-log-padding
           end-if.

           move spaces to prt-error.
           write prt-error after advancing 1.
           move spaces to ws-error-log-message.

       600-print-error-footer.
           move ws-total-errors to ws-error-footer-count.
           write prt-error from ws-error-log-footer after advancing 1.

       700-reset-flags.
           set ws-error-flags to 'nnnnnnn'.
           set ws-error-counter to 0.

       end program VALIDATOR.