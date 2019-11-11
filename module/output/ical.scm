(define-module (output ical)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  )

(define opt-spec
  '((from (value #t) (single-char #\f))
    (to (value #t) (single-char #\t))))

(define (value-format key vline)
  (catch 'wrong-type-arg
    (lambda ()
     (case key
       ((DTSTART DTEND)
        (time->string (value vline) (if (prop vline 'TZID)
                                        "~Y~m~dT~H~M~S"
                                        "~Y~m~dT~H~M~SZ" )))
       ((DURATION X-HNH-DURATION)
     #; (time->string value "~H~M~S")
        (let ((s (time-second (value vline))))
          (format #f "~a~a~a"
                  (floor/ s 3600)
                  (floor/ (modulo s 3600) 60)
                  (modulo s 60))
          ))
       ((RRULE) (value vline))

       (else (escape-chars (value vline)))))
    (lambda (err caller fmt args call-args)
      (format (current-error-port)
              "WARNING: ~k~%" fmt args)
      (with-output-to-string (lambda () (display (value vline))))
      )))

(define (escape-chars str)
  (with-output-to-string
    (lambda ()
      (string-for-each (lambda (ch)
                         (case ch
                           ((#\, #\; #\\) => (lambda (c) (display "\\") (display c)))
                           ((#\newline) (display "\\n"))
                           (else (display ch)))
                         ) str))))

(define wrap-len 70 #; (floor/ 75 2)
  )

(define (ical-line-fold string)
  (cond [(< wrap-len (string-length string))
         (format #f "~a\r\n ~a"
                 (string-take string wrap-len)
                 (ical-line-fold (string-drop string wrap-len)))]
        [else string]))

(define (component->ical-string component)
  (format #t "BEGIN:~a\r\n" (type component))
  (hash-for-each (lambda (key vline)
                   ;; key;p1=v;p3=10:value

                   (display
                    (ical-line-fold
                     (format #f "~a~:{;~a=~@{~a~^,~}~}:~a"
                             key (properties vline)
                             ;; TODO wrap lines
                             (value-format key vline))))
                   (display "\r\n"))
            (attributes component))
  (for-each component->ical-string (children component))
  (format #t "END:~a\r\n" (type component))

  )

(define (print-header)
  (format #t
"BEGIN:VCALENDAR\r
PRODID:-//hugo//Calparse 0.5//EN\r
VERSION:2.0\r
CALSCALE:GREGORIAN\r
"
))


(define (print-footer)
  (format #t "END:VCALENDAR\r\n"))

(define-public (ical-main calendars events args)
  (define opts (getopt-long args opt-spec))

  (define start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                      [else (start-of-month (current-date))]))
  (define end   (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                      [else (normalize-date* (set (date-month start) = (+ 1)))]))

  (print-header)

  (let ((tzs (make-hash-table)))
    (for cal in calendars
         (for tz in (filter (lambda (e) (eq? 'VTIMEZONE (type e))) (children cal))
              (hash-set! tzs (attr tz 'TZID) tz)))

    (hash-for-each (lambda (key component) (component->ical-string component))
                   tzs))

  ;; TODO this contains repeated events multiple times
  (stream-for-each
   component->ical-string
   (filter-sorted-stream (lambda (ev) ((in-date-range? start end)
                                  (time-utc->date (attr ev 'DTSTART))))
                         events))

  (print-footer))
