(define-module (output ical)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (util app)
  :use-module (vcomponent)
  :use-module (vcomponent datetime)
  :use-module (srfi srfi-1)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime zic)
  :use-module (glob)
  :use-module (vcomponent recurrence)
  )

;; Format value depending on key type.
;; Should NOT emit the key.
(define (value-format key vline)
  (define (handle-value value)
    (catch #t #; 'wrong-type-arg
      (lambda ()
        (case key
          ((DTSTART DTEND RECURRENCE-ID DTSTAMP
                    LAST-MODIFIED EXDATE)
           (with-output-to-string
             (lambda ()
               (case (and=> (prop vline 'VALUE) car)
                 [(DATE) (display (date->string (as-date value)
                                                "~Y~m~d"))]
                 [else              ; (DATE-TIME)
                  (display (datetime->string value "~Y~m~dT~H~M~S"))
                  (let ((tz (and=> (prop vline 'TZID) car)))
                    (when (and tz (string= tz "UTC"))
                      (display #\Z)))]))))

          [(TZOFFSETFROM TZOFFSETTO)
           (with-output-to-string
             (lambda ()
               (display (if (time-zero? (timespec-time value))
                            '+ (timespec-sign value)))
               (display (time->string (timespec-time value) "~H~M"))
               (when (not (zero? (second (timespec-time value))))
                 (display (time->string (timespec-time value) "~S")))))]

          ((DURATION X-HNH-DURATION)
           #; (time->string value "~H~M~S")
           (let ((s (second value)))
             (format #f "~a~a~a"
                     (floor/ s 3600)
                     (floor/ (modulo s 3600) 60)
                     (modulo s 60))
             ))
          [(RRULE)
           ((@ (vcomponent recurrence internal)
               recur-rule->rrule-string) value)]

          (else
           (escape-chars value)
           )))
      (lambda (err caller fmt args call-args)
        (define fallback-string
          (with-output-to-string (lambda () (display value))))
        (warning "key = ~a, caller = ~s, call-args = ~s~%~k~%Falling back to ~s"
                 key caller call-args fmt args
                 fallback-string)
        fallback-string
        )))

  (if (list? (value vline))
      (format #f "~{~a~^,~}"
              (map handle-value (value vline)))
      (handle-value (value vline))))

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

(define (generate-uuid)
  ((@ (rnrs io ports) call-with-port)
   ((@ (ice-9 popen) open-input-pipe) "uuidgen")
   (@ (ice-9 rdelim) read-line)))

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

(define-public (component->ical-string component)
  (format #t "BEGIN:~a\r\n" (type component))
  (hash-for-each
   ;; Special cases depending on key.
   ;; Value formatting is handled in @code{value-format}.
   (match-lambda*
     ;; Handled below
     [('X-HNH-ALTERNATIVES _) 'noop]

     ;; Remove from output
     [('X-HNH-FILENAME _) 'noop]

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
         => (lambda (alts) (hash-map->list (lambda (_ comp) (component->ical-string comp))
                                      alts))]))

;; TODO place these somewhere better
(define *prodid* "-//hugo//Calparse 0.9//EN")

;; TODO tzid prop on dtstart vs tz field in datetime object
;; how do we keep these two in sync?
(define (write-event-to-file event calendar-path)
  (define cal (make-vcomponent 'VCALENDAR))

  (set! (attr cal 'PRODID) *prodid*
        (attr cal 'VERSION) "2.0"
        (attr cal 'CALSCALE) "GREGORIAN")

  (add-child! cal event)

  (awhen (prop (attr* event 'DTSTART) 'TZID)
         ;; TODO this is broken
         (add-child! cal (zoneinfo->vtimezone (getf 'zoneinfo) it)))

  (unless (attr event 'UID)
    (set! (attr event 'UID)
      (generate-uuid)))

  (with-output-to-file (glob (format #f "~a/~a.ics"
                                     calendar-path
                                     (attr event 'UID)))
    (lambda () (component->ical-string cal))))



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


(define (get-tz-names events)
  (lset-difference
   equal? (lset-union
           equal? '("dummy")
           (filter-map
            (lambda (vline) (and=> (prop vline 'TZID) car))
            (filter-map (extract* 'DTSTART)
                        events)))
   '("dummy" "local")))

(define-public (print-components-with-fake-parent events)

  ;; The events are probably sorted before, but until I can guarantee
  ;; that we sort them again here. We need them sorted from earliest
  ;; and up to send the earliest to zoneinfo->vtimezone
  (set! events (sort* events date/-time<=? (extract 'DTSTART)))

  (print-header)

  (let ((tz-names (get-tz-names events)))
    (for-each component->ical-string
              ;; TODO we realy should send the earliest event from each timezone here.
              (map (lambda (name) (zoneinfo->vtimezone (getf 'zoneinfo) name (car events)))
                   tz-names)))

  (for-each component->ical-string events)

  (print-footer))


(define-method (print-all-events)
  (print-components-with-fake-parent
   (append (getf 'fixed-events)
           ;; TODO RECCURENCE-ID exceptions
           ;; We just dump all repeating objects, since it's much cheaper to do
           ;; it this way than to actually figure out which are applicable for
           ;; the given date range.
           (getf 'repeating-events))))

(define-method (print-events-in-interval start end)
  (print-components-with-fake-parent
   (append (fixed-events-in-range start end)
           ;; TODO RECCURENCE-ID exceptions
           ;; We just dump all repeating objects, since it's much cheaper to do
           ;; it this way than to actually figure out which are applicable for
           ;; the given date range.
           (getf 'repeating-events))))
