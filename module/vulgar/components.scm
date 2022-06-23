(define-module (vulgar components)
  :use-module (datetime)
  :use-module (hnh util)
  :export (display-calendar-header!))

(define (display-calendar-header! date)
  (let ((day   (number->string (day   date)))
        (month (number->string (month date)))
        (year  (number->string (year  date))))
    ;; BSD cal only supports setting highlighted day explicitly for
    ;; testing the functionality. This seems to at least give me
    ;; an (almost) working display, albeit ugly.
    (if (file-exists? "/usr/bin/ncal")
        (system* "ncal" "-3" "-H" (date->string date)
                 month year)
        (system* "cal" "-3" day month year))))

