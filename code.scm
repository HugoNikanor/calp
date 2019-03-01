(define-module (code)
  #:export (extract localize-date sort* drop-time! copy-date
                    drop-time %date<=? date-today? color-if
                    for-each-in STR-YELLOW STR-RESET))

(use-modules (srfi srfi-19)
             (srfi srfi-19 setters)
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

(define (drop-time! date)
  (set! (hour date) 0)
  (set! (minute date) 0)
  (set! (second date) 0)
  (set! (nanosecond date) 0)
  date)

(define (copy-date date)
  (let* ((date-type (@@ (srfi srfi-19) date))
         (access (lambda (field) ((record-accessor date-type field) date))))
    (apply make-date (map access (record-type-fields date-type)))))

(define (drop-time date)
  (let ((new-date (copy-date date)))
    (drop-time! new-date)))

(define (%date<=? a b)
  (time<=? (date->time-utc a)
           (date->time-utc b)))

(define (date-today? input-date)
  (let* ((date (current-date))
         (now (drop-time date))
         (then (copy-date now)))
    (set! (day then)
          (1+ (day then)))
    (and (%date<=? now input-date)
         (%date<=? input-date then))))


(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))

(define-syntax-rule (for-each-in lst proc)
  (for-each proc lst))

