(define-module (vulgar components)
  #:use-module (srfi srfi-19)
  #:use-module (util)
  #:export ())

(define-public (display-calendar-header! date)
  (let* ((day   (number->string (date-day   date)))
         (month (number->string (date-month date)))
         (year  (number->string (date-year  date))))
    ;; BSD cal only supports setting highlighted day explicitly for
    ;; testing the functionality. This seems to at least give me
    ;; an (almost) working display, albeit ugly.
    (if (file-exists? "/usr/bin/ncal")
        (system*  "ncal" "-3" "-H" (format #f "~a-~a-~a"
                                          year month day)
                  month year)
        (system* "cal" "-3" day month year))))

