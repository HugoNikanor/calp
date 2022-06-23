(define-module (vcomponent recurrence internal)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)           ; better keywords
  :use-module ((vcomponent base) :select (prop))
  :use-module (ice-9 i18n)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module (ice-9 format)
  :use-module (hnh util)
  :use-module (datetime)

  :replace (count)
  :export (repeating?

           make-recur-rule
           freq until interval bysecond byminute byhour
           byday bymonthday byyearday byweekno bymonth bysetpos
           wkst

           recur-rule->rrule-string
           recur-rule->rrule-sxml

           weekdays
           intervals
           ))

(define weekdays
  (weekday-list sun))

(define intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))


;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (prop ev 'RRULE)
      (prop ev 'RDATE)
      (prop ev '-X-HNH-ALTERNATIVES)))

;; weekday := [0, 7)

;; Immutable, since I easily want to be able to generate the recurence set for
;; the same event multiple times.
(define-immutable-record-type <recur-rule>
  (make-recur-rule% freq until count interval bysecond byminute byhour
                    byday bymonthday byyearday byweekno bymonth bysetpos
                    wkst)
  recur-rule?
  (freq freq) ; 'SECONDLY | 'MINUTELY | 'HOURLY | 'DAILY | 'WEEKLY | 'MONTHLY | 'YEARLY
  (until until)                         ; <date> | <datetime>
  (count count)                         ; 𝐙₊
  (interval interval)                   ; 𝐙₊
  (bysecond bysecond)                   ; (list [0, 60])
  (byminute byminute)                   ; (list [0, 59])
  (byhour byhour)                       ; (list [0, 23])
  (byday byday)                         ; (list (cons [#f | 𝐙] weekday)
  (bymonthday bymonthday)               ; (list [-31, 31] \ { 0 })
  (byyearday byyearday)                 ; (list [-366, 366] \ { 0 })
  (byweekno byweekno)                   ; (list [-53, 53] \ { 0 })
  (bymonth bymonth)                     ; (list [-12, 12] \ { 0 })
  (bysetpos bysetpos)                   ; (list [-366, 366] \ { 0 })
  (wkst wkst)                           ; weekday
  )



;; Interval and wkst have default values, since those are assumed
;; anyways, and having them set frees us from having to check them at
;; the use site.
(define* (make-recur-rule
          key:
          freq until count (interval 1) bysecond byminute byhour
          byday bymonthday byyearday byweekno bymonth bysetpos
          (wkst monday))
  ;; TODO possibly validate fields here
  ;; to prevent creation of invalid rules.
  ;; This was made apparent when wkst was (incorrectly) set to MO,
  ;; which later crashed generate-recurrence-set.
  (make-recur-rule% freq until count interval bysecond byminute byhour
                    byday bymonthday byyearday byweekno bymonth bysetpos
                    wkst))

;; only print fields with actual values.
(set-record-type-printer!
 <recur-rule>
 (lambda (r port)
   (define (get f)
     ((record-accessor <recur-rule> f) r))
   (with-output-to-string
     (lambda ()
       (display "#<<recur-rule>" port)
       (for field in (record-type-fields <recur-rule>)
            (awhen (get field)
                   (format port " ~a=~a" field it)))
       (display ">" port)))))


(define (byday->string pair)
  (let ((off day (car+cdr pair)))
    (string-append
     (or (and=> off number->string) "")
     (string-upcase
      (week-day-name day 2
                     locale: (make-locale (list LC_TIME) "C"))))))


(define (field->string field value)
  (case field
    [(wkst)
     (string-upcase
      (week-day-name value 2
                     locale: (make-locale (list LC_TIME) "C")))]
    [(byday)
     (string-join (map byday->string value) ",")]
    [(freq count interval)
     (format #f "~a" value)]
    [(until)
     (if (date? value)
         (date->string value "~Y~m~d")
         (datetime->string value "~Y~m~dT~H~M~S~Z"))]
    [else (format #f "~{~a~^,~}" value)]))

(define (map-fields proc rrule)
  (define (get f)
    ((record-accessor <recur-rule> f) rrule))
  (filter-map
   (lambda (field)
     (if (not (get field))
         #f (proc field (get field))))
   (record-type-fields <recur-rule>)))

(define (recur-rule->rrule-string rrule)
  (string-join
   (map-fields
    (lambda (field value)
      (string-append
       (string-upcase (symbol->string field))
       "=" (field->string field value)))
    rrule)
   ";"))

(define (recur-rule->rrule-sxml rrule)
  (map-fields
   (lambda (field value)
     (cond [(string-ci=? "UNTIL" (symbol->string field))
            `(until
              ,(if (date? value)
                   (date->string value "~Y-~m-~d")
                   (datetime->string
                    value "~Y-~m-~dT~H:~M:~S~Z")))]
           [(string-ci=? "BYDAY" (symbol->string field))
            (map (lambda (v)
                   `(,(downcase-symbol field)
                     ,(byday->string v)))
                 value)
            ]
           [(string-ci=? "BY" (substring (symbol->string field)
                                         0 2))
            (map (lambda (v)
                   `(,(downcase-symbol field)
                     ,v))
                 value)]
           [else
            `(,(downcase-symbol field)
              ,(field->string field value))]))
   rrule))
