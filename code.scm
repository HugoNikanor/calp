(add-to-load-path (dirname (current-filename)))

(use-modules (srfi srfi-19)
             (srfi srfi-26)
             (vcalendar))

(define (extract field)
  (cut get-attr <> field))

(define (localize-date date)
  (time-utc->date (date->time-utc date)
                  (date-zone-offset (current-date))))

;;; This function borrowed from web-ics (calendar util) 
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

(define STR-YELLOW "\x1b[0;33m")
(define STR-RESET "\x1b[m")

(define (date-today? input-date)
  (let* ((date (current-date))
         (now (make-date 0 0 0 0
                         (date-day date)
                         (date-month date)
                         (date-year date)
                         (date-zone-offset date)))
         (then (make-date 0 0 0 0
                          (1+ (date-day date))
                          (date-month date)
                          (date-year date)
                          (date-zone-offset date))))
    (and (time<=? (date->time-utc now)
                  (date->time-utc input-date))
         (time<=? (date->time-utc input-date)
                  (date->time-utc then)))))


(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))

(define-syntax-rule (for-each-in lst proc)
  (for-each proc lst))

