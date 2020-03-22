(define-module (output ical)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  )


;; Format value depending on key type.
;; Should NOT emit the key.
(define (value-format key vline)
  (with-throw-handler 'wrong-type-arg
    (lambda ()
      (case key
        ((DTSTART DTEND RECURRENCE-ID)
         (with-output-to-string
           (lambda ()
             (case (and=> (prop vline 'VALUE) car)
               [(DATE) (display (date->string (as-date (value vline))
                                              "~Y~m~d"))]
               [(DATE-TIME)
                (display (datetime->string (value vline) "~Y~m~dT~H~M~S"))
                (let ((tz (and=> (prop vline 'TZID) car)))
                  (when (and tz (string= tz "UTC"))
                    (display #\Z)))]
               [else
                (error "Unknown VALUE type")]))))
        ((DURATION X-HNH-DURATION)
         #; (time->string value "~H~M~S")
         (let ((s (second (value vline))))
           (format #f "~a~a~a"
                   (floor/ s 3600)
                   (floor/ (modulo s 3600) 60)
                   (modulo s 60))
           ))
        ((RRULE) (value vline))

        (else (escape-chars (value vline)))))
    (lambda (err caller fmt args call-args)
      (format (current-error-port)
              "WARNING: key = ~a, caller = ~s, call-args = ~s~%~k~%" key caller call-args fmt args)
      (with-output-to-string (lambda () (display (value vline))))
      )))

(define (escape-chars str)
  (with-output-to-string
    (lambda ()
      (string-for-each
       (lambda (ch)
         (case ch
           ((#\, #\; #\\) => (lambda (c) (display "\\") (display c)))
           ((#\newline) (display "\\n"))
           (else (display ch))))
       str))))

;; Fold long lines to limit width.
;; Since this works in characters, but ics works in bytes
;; this will overshoot when faced with multi-byte characters.
;; But since the line wrapping is mearly a recomendation it's
;; not a problem.
;; Setting the wrap-len to slightly lower than allowed also help
;; us not overshoot.
(define* (ical-line-fold string #:key (wrap-len 70))
  (cond [(< wrap-len (string-length string))
         (format #f "~a\r\n ~a"
                 (string-take string wrap-len)
                 (ical-line-fold (string-drop string wrap-len)))]
        [else string]))

(define (component->ical-string component)
  (format #t "BEGIN:~a\r\n" (type component))
  (hash-for-each
   ;; Special cases depending on key.
   ;; Value formatting is handled in @code{value-format}.
   (match-lambda*
     ;; Handled below
     [('X-HNH-ALTERNATIVES _) 'noop]

     [(key vline)
      (display
       (ical-line-fold
        ;; Expected output: key;p1=v;p3=10:value
        (format #f "~a~:{;~a=~@{~a~^,~}~}:~a"
                key (properties vline)
                (value-format key vline))))
      (display "\r\n")])
   (attributes component))
  (for-each component->ical-string (children component))
  (format #t "END:~a\r\n" (type component))

  ;; If we have alternatives, splice them in here.
  (cond [(attr component 'X-HNH-ALTERNATIVES)
         => (lambda (alts) (map component->ical-string alts))]))

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

;; list x list x list x time x time → 
(define-public (ical-main calendars regular-events repeating-events start end)
  (print-header)

  (let ((tzs (make-hash-table)))
    (for cal in calendars
         (for tz in (filter (lambda (e) (eq? 'VTIMEZONE (type e))) (children cal))
              (hash-set! tzs (attr tz 'TZID) tz)))

    (hash-for-each (lambda (key component) (component->ical-string component))
                   tzs))

  ;; TODO add support for running without a range limiter, emiting all objects.
  (for-each
   component->ical-string
   (filter-sorted (lambda (ev) ((in-date-range? start end)
                           (as-date (attr ev 'DTSTART))))
                  regular-events))

  ;; TODO RECCURENCE-ID exceptions
  ;; We just dump all repeating objects, since it's much cheaper to do it this way than
  ;; to actually figure out which are applicable for the given date range.
  (for-each component->ical-string repeating-events)

  (print-footer))
