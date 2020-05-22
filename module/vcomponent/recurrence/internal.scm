(define-module (vcomponent recurrence internal)
  #:export (repeating? format-recur-rule make-recur-rule)

  #:use-module (srfi srfi-88)           ; better keywords
  #:use-module ((vcomponent base) :select (attr))
  #:use-module (datetime util)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (util)
  )

;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (attr ev 'RRULE)
      (attr ev 'RDATE)))

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

(export freq until interval bysecond byminute byhour
        byday bymonthday byyearday byweekno bymonth bysetpos
        wkst)
(export! count)

(define*-public (make-recur-rule
                 key:
                 freq until count interval bysecond byminute byhour
                 byday bymonthday byyearday byweekno bymonth bysetpos
                 wkst)
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
  (let* (((off . day) pair))
    (string-append
     (or (and=> off number->string) "")
     (string-upcase
      (week-day-name day 2
                     locale: (make-locale (list LC_TIME) "C"))))))

(use-modules (ice-9 i18n)
             (datetime)
             (srfi srfi-1))

(define-public (recur-rule->rrule-string rrule)
  (define (get f)
    ((record-accessor <recur-rule> f) rrule))
  (string-join
   (filter-map
    (lambda (field)
      (if (not (get field))
          #f
          (string-append
           (string-upcase (symbol->string field))
           "="
           (case field
             [(wkst)
              (string-upcase
               (week-day-name (get field) 2
                              locale: (make-locale (list LC_TIME) "C")))]
             [(byday)
              (string-join (map byday->string (get field)) ",")]
             [(freq count interval)
              (format #f "~a" (get field))]
             [(until)
              (let ((o (get field)))
                (if (date? o)
                    (date->string o "~Y~m~d")
                    (datetime->string o "~Y~m~dT~H~M~S~Z")
                    ))]
             [else (format #f "~{~a~^,~}" (get field))]))))
    (record-type-fields <recur-rule>))
   ";"))




(define-public weekdays
  (weekday-list sun))

(define-public intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))
