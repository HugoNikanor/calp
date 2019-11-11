(define-module (output ical)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (util)
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
  (case key
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
                           ((#\, #\; #\\) => (lambda (c) (display "\\") (display c)))
                           ((#\newline) (display "\\n"))
                           (else (display ch)))
                         ) str))))

(define (component->ical-string component)
  (format #t "BEGIN:~a~%" (type component))
  (hash-for-each (lambda (key vline)
                   ;; key;p1=v;p3=10:value
                   (format #t "~a~:{;~a=~@{~a~^,~}~}:~a~%"
                           key (properties vline)
                           ;; TODO wrap lines
                           (escape-chars (value-format key (value vline)))))
            (attributes component))
  (for-each component->ical-string (children component))
  (format #t "END:~a~%" (type component))

  )

(define (print-header)
  (format #t
"BEGIN:VCALENDAR
PRODID:-//hugo//Calparse 0.5//EN
VERSION:2.0
CALSCALE:GREGORIAN
"
))


(define (print-footer)
  (format #t "END:VCALENDAR~%"))

(define-public (ical-main calendars events args)


  (define opts (getopt-long args opt-spec))

  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end   (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                      [else (normalize-date* (set (date-month start) = (+ 1)))]))

  (print-header)

  (stream-for-each
   component->ical-string
   (filter-sorted-stream (lambda (ev) ((in-date-range? start end)
                                  (time-utc->date (attr ev 'DTSTART))))
                         events))

  (print-footer))
