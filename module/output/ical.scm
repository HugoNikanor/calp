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
  :use-module (vcomponent geo)
  :use-module (output types)
  :use-module (output common)
  )


;; Format value depending on key type.
;; Should NOT emit the key.
(define (value-format key vline)

  (define writer
    ;; fields which can hold lists need not be considered here,
    ;; since they are split into multiple vlines when we parse them.
    (cond
     [(and=> (prop vline 'VALUE) string->symbol) => get-writer]
     [(memv key '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID
                         CREATED DTSTAMP LAST-MODIFIED
                         ACKNOWLEDGED EXDATE))
      (get-writer 'DATE-TIME)]

     [(memv key '(TRIGGER DURATION))
      (get-writer 'DURATION)]

     [(memv key '(FREEBUSY))
      (get-writer 'PERIOD)]

     [(memv key '(CALSCALE METHOD PRODID COMMENT DESCRIPTION
                        LOCATION SUMMARY TZID TZNAME
                        CONTACT RELATED-TO UID

                        CATEGORIES RESOURCES

                        VERSION))
      (get-writer 'TEXT)]

     [(memv key '(TRANSP
               CLASS
               PARTSTAT
               STATUS
               ACTION))
      (lambda (p v) ((get-writer 'TEXT) p (symbol->string v)))]

     [(memv key '(TZOFFSETFROM TZOFFSETTO))
      (get-writer 'UTC-OFFSET)]

     [(memv key '(ATTACH TZURL URL))
      (get-writer 'URI)]

     [(memv key '(PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE))
      (get-writer 'INTEGER)]

     [(memv key '(GEO))
      (lambda (_ v)
        (define fl (get-writer 'FLOAT))
        (format #f "~a:~a"
                (fl (geo-latitude v))
                (fl (geo-longitude v))))]

     [(memv key '(RRULE))
      (get-writer 'RECUR)]

     [(memv key '(ORGANIZER ATTENDEE))
      (get-writer 'CAL-ADDRESS)]

     [(x-property? key)
      (get-writer 'TEXT)]

     [else
      (warning "Unknown key ~a" key)
      (get-writer 'TEXT)]))

  (catch #t #; 'wrong-type-arg
    (lambda ()
      (writer ((@@ (vcomponent base) get-vline-parameters) vline)
              (value vline)))
    (lambda (err caller fmt args call-args)
      (define fallback-string
        (with-output-to-string (lambda () (display value))))
      (warning "key = ~a, caller = ~s, call-args = ~s~%~k~%Falling back to ~s"
               key caller call-args fmt args
               fallback-string)
      fallback-string)))


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



(define (vline->string vline)
  (define key (vline-key vline))
  (ical-line-fold
   ;; Expected output: key;p1=v;p3=10:value
   (string-append
    (symbol->string key)
    (string-concatenate
     (map (match-lambda
            [(? (compose internal-field? car)) ""]
            [(key values ...)
             (string-append
              ";" (symbol->string key)
              (string-join (map (compose (get-writer 'TEXT) ->string) values)
                           "," 'infix))])
          (properties vline)))
    ":" (value-format key vline))))

(define-public (component->ical-string component)
  (format #t "BEGIN:~a\r\n" (type component))
  ;; TODO this leaks internal information,
  ;; write a better API for vcomponent.
  (hash-for-each
   ;; Special cases depending on key.
   ;; Value formatting is handled in @code{value-format}.
   (match-lambda*

     [(? (compose internal-field? car)) 'noop]

     [(key (vlines ...))
      (for vline in vlines
           (display (vline->string vline))
           (display "\r\n"))]

     [(key vline)
      (display (vline->string vline))
      (display "\r\n")])
   (attributes component))
  (for-each component->ical-string (children component))
  (format #t "END:~a\r\n" (type component))

  ;; If we have alternatives, splice them in here.
  (cond [(attr component 'X-HNH-ALTERNATIVES)
         => (lambda (alts) (hash-map->list (lambda (_ comp) (component->ical-string comp))
                                      alts))]))

;; TODO tzid prop on dtstart vs tz field in datetime object
;; how do we keep these two in sync?
(define (write-event-to-file event calendar-path)
  (define cal (make-vcomponent 'VCALENDAR))

  (set! (attr cal 'PRODID) (@ (global) *prodid*)
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
PRODID:~a\r
VERSION:2.0\r
CALSCALE:GREGORIAN\r
" (@ (global) *prodid*)
))


(define (print-footer)
  (format #t "END:VCALENDAR\r\n"))



(define-public (print-components-with-fake-parent events)

  ;; The events are probably sorted before, but until I can guarantee
  ;; that we sort them again here. We need them sorted from earliest
  ;; and up to send the earliest to zoneinfo->vtimezone
  (set! events (sort* events date/-time<=? (extract 'DTSTART)))

  (print-header)

  (let ((tz-names (get-tz-names events)))
    (for-each component->ical-string
              ;; TODO we realy should send the earliest event from each timezone here,
              ;; instead of just the first.
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
