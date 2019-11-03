(define-module (output ical)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (vcomponent)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  )

(define opt-spec
  '((from (value #t) (single-char #\f))
    (to (value #t) (single-char #\t))))

(define (value-format key value)
  ;; TODO remove once key's are normalized to symbols.
  (case (string->symbol key)
    ((DTSTART DTEND)
     (time->string value "~Y~m~dT~H~M~SZ"))
    ((DURATION)
     #; (time->string value "~H~M~S")
     (let ((s (time-second value)))
       (format #f "~a~a~a"
               (floor/ s 3600)
               (floor/ (modulo s 3600) 60)
               (modulo s 60))
       ))
    (else value)))

(define (escape-chars str)
  (with-output-to-string
    (lambda ()
      (string-for-each (lambda (ch)
                         (case ch
                           ((#\, #\\) => (lambda (c) (display "\\") (display c)))
                           (else (display ch)))
                         ) str))))

(define (component->ical-string component)
  (format #t "BEGIN:~a~%" (type component))
  (for-each (lambda (kv)
              (let ((key (car kv))
                    (vline (cdr kv)))
                ;; key;p1=v;p3=10:value
                (format #t "~a~:{;~a=~@{~a~^,~}~}:~a~%"
                        key (properties vline)
                        (escape-chars (value-format key (value vline)))
                        )))
            (attributes component))
  (for-each component->ical-string (children component))
  (format #t "END:~a~%" (type component))

  )

(define (print-header)
  (format #t
"BEGIN:VCALENDAR
PRODID:~a
VERSION:2.0
CALSCALE:GREGORIAN
"
"Hugo"
))


(define (print-footer)
  (format #t "END:VCALENDAR~%"))

(define-public (ical-main calendars events args)
  (define opts (getopt-long args opt-spec))
  (define start (parse-freeform-date (option-ref opts 'from "2019-04-15")))
  (define end   (parse-freeform-date (option-ref opts 'to   "2019-05-10")))

  (print-header)

  (stream-for-each
   component->ical-string
   (filter-sorted-stream (lambda (ev) ((in-date-range? start end)
                                  (time-utc->date (attr ev 'DTSTART))))
                         events))

  (print-footer))
