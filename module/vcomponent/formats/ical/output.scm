(define-module (vcomponent formats ical output)
  :use-module (hnh util exceptions)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (datetime zic)
  :use-module ((datetime instance) :select (zoneinfo))
  :use-module (glob)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-88)
  :use-module (vcomponent)
  :use-module (vcomponent datetime)
  :use-module (vcomponent geo)
  :use-module (vcomponent formats ical types)
  :use-module (vcomponent recurrence)
  :use-module (calp translation)
  :autoload (vcomponent util instance) (global-event-object)
  :export (component->ical-string
           print-components-with-fake-parent
           print-all-events
           print-events-in-interval
           ))

(define (prodid)
  (format #f "-//hugo//calp ~a//EN"
          (@ (calp) version)))


;; Format value depending on key type.
;; Should NOT emit the key.
(define (value-format key vline)

  (define writer
    ;; fields which can hold lists need not be considered here,
    ;; since they are split into multiple vlines when we parse them.
    (cond
     ;; TODO parameters return? One or many‽
     [(and=> (param vline 'VALUE) (compose string->symbol car)) => get-writer]
     [(memv key '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID
                         CREATED DTSTAMP LAST-MODIFIED
                         ACKNOWLEDGED EXDATE))
      (get-writer 'DATE-TIME)]

     [(memv key '(TRIGGER DURATION))
      (get-writer 'DURATION)]

     [(memv key '(FREEBUSY))
      (get-writer 'PERIOD)]

     [(memv key '(CATEGORIES RESOURCES))
      (lambda (p v)
        (string-join (map (lambda (v) ((get-writer 'TEXT) p v))
                          v)
                     ","))]

     [(memv key '(CALSCALE METHOD PRODID COMMENT DESCRIPTION
                        LOCATION SUMMARY TZID TZNAME
                        CONTACT RELATED-TO UID

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
      (warning (G_ "Unknown key ~a") key)
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
(define* (ical-line-fold string key: (wrap-len 70))
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
              ";" (symbol->string key) "="
              (string-join (map (compose escape-chars ->string) values)
                           "," 'infix))])
          (parameters vline)))
    ":" (value-format key vline))))

(define (component->ical-string component)
  (format #t "BEGIN:~a\r\n" (type component))
  (for-each
   ;; Special cases depending on key.
   ;; Value formatting is handled in @code{value-format}.
   (match-lambda

     [(? (compose internal-field? car)) 'noop]

     [(key vlines ...)
      (for vline in vlines
           (display (vline->string vline))
           (display "\r\n"))]

     [(key . vline)
      (display (vline->string vline))
      (display "\r\n")])
   (properties component))
  (for-each component->ical-string (children component))
  (format #t "END:~a\r\n" (type component))

  ;; If we have alternatives, splice them in here.
  (cond [(prop component '-X-HNH-ALTERNATIVES)
         => (lambda (alts) (hash-map->list (lambda (_ comp)
                                        (unless (eq? component comp)
                                          (component->ical-string comp)))
                                      alts))]))



(define (print-header)
  (format #t
"BEGIN:VCALENDAR\r
PRODID:~a\r
VERSION:2.0\r
CALSCALE:GREGORIAN\r
" (prodid)
))


(define (print-footer)
  (format #t "END:VCALENDAR\r\n"))

(define (get-tz-names events)
  (lset-difference
   equal? (lset-union
           equal? '("dummy")
           (filter-map
            (lambda (vline) (and=> (param vline 'TZID) car))
            (filter-map (extract* 'DTSTART)
                        events)))
   '("dummy" "local")))


(define (print-components-with-fake-parent events)

  ;; The events are probably sorted before, but until I can guarantee
  ;; that we sort them again here. We need them sorted from earliest
  ;; and up to send the earliest to zoneinfo->vtimezone
  (set! events (sort* events date/-time<=? (extract 'DTSTART)))

  (print-header)

  (when (provided? 'zoneinfo)
    (let ((tz-names (get-tz-names events)))
      (for-each component->ical-string
                ;; TODO we realy should send the earliest event from each timezone here,
                ;; instead of just the first.
                (map (lambda (name) (zoneinfo->vtimezone
                                      (zoneinfo)
                                      name (car events)))
                     tz-names))))

  (for-each component->ical-string events)

  (print-footer))


(define (print-all-events)
  (print-components-with-fake-parent
   (append (get-fixed-events global-event-object)
           ;; TODO RECCURENCE-ID exceptions
           ;; We just dump all repeating objects, since it's much cheaper to do
           ;; it this way than to actually figure out which are applicable for
           ;; the given date range.
           (get-repeating-events global-event-object))))

(define (print-events-in-interval start end)
  (print-components-with-fake-parent
   (append (fixed-events-in-range start end)
           ;; TODO RECCURENCE-ID exceptions
           ;; We just dump all repeating objects, since it's much cheaper to do
           ;; it this way than to actually figure out which are applicable for
           ;; the given date range.
           (get-repeating-events global-event-object))))
