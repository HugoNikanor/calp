(define-module (vcomponent recurrence internal)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)           ; better keywords
  :use-module ((vcomponent base) :select (prop))
  :use-module (ice-9 i18n)
  :use-module (ice-9 format)
  :use-module (ice-9 pretty-print)
  :use-module (hnh util)
  :use-module (hnh util object)
  :use-module ((hnh util type) :select (list-of pair-of false?))
  :use-module (datetime)
  :use-module (sxml namespaced)
  :use-module ((calp namespaces) :select (xcal))

  :replace (count)
  :export (repeating?

           recur-rule
           freq until interval bysecond byminute byhour
           byday bymonthday byyearday byweekno bymonth bysetpos
           wkst

           freq-placeholder

           recur-rule->rrule-string
           recur-rule->rrule-sxml

           weekdays
           intervals
           ))

(define weekdays
  (weekday-list sun))

(define freq-placeholder (gensym))

(define intervals
  `(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY
             ,freq-placeholder))


;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (prop ev 'RRULE)
      (prop ev 'RDATE)
      (prop ev '-X-HNH-ALTERNATIVES)))

(define-syntax-rule (in-range? x start end)
  (<= start x end))

(define (recur-rule-constructor-factory primitive-constructor type-checker)
  ;; Interval and wkst have default values, since those are assumed
  ;; anyways, and having them set frees us from having to check them at
  ;; the use site.
  (lambda* (key: freq until count (interval 1) bysecond byminute
                 byhour byday bymonthday byyearday byweekno bymonth
                 bysetpos (wkst monday))
    ;; Allow `(cons #f day)' to be written as just `day'.
    (let ((byday* (if byday
                      (map (lambda (day)
                             (if (number? day)
                                 (cons #f day)
                                 day))
                           byday)
                      #f)))
      ;; TODO possibly check that until and count are mutually exclusive
      (type-checker
       freq until count interval bysecond byminute byhour
       byday* bymonthday byyearday byweekno bymonth bysetpos
       wkst)
      (primitive-constructor
       freq until count interval bysecond byminute byhour
       byday* bymonthday byyearday byweekno bymonth bysetpos
       wkst))))

(define (serialize-recur-rule record)
  `(recur-rule
    ,@(when (freq record) `(freq: ,(serialize (freq record))))
    ,@(when (until record) `(until: ,(serialize (until record))))
    ,@(when (count record) `(count: ,(serialize (count record))))
    ,@(when (interval record) `(interval: ,(serialize (interval record))))
    ,@(when (bysecond record) `(bysecond: ,(serialize (bysecond record))))
    ,@(when (byminute record) `(byminute: ,(serialize (byminute record))))
    ,@(when (byhour record) `(byhour: ,(serialize (byhour record))))
    ,@(when (byday record) `(byday: ,(serialize (byday record))))
    ,@(when (bymonthday record) `(bymonthday: ,(serialize (bymonthday record))))
    ,@(when (byyearday record) `(byyearday: ,(serialize (byyearday record))))
    ,@(when (byweekno record) `(byweekno: ,(serialize (byweekno record))))
    ,@(when (bymonth record) `(bymonth: ,(serialize (bymonth record))))
    ,@(when (bysetpos record) `(bysetpos: ,(serialize (bysetpos record))))
    ,@(when (wkst record) `(wkst: ,(serialize (wkst record))))))

;;; Both interval and wkst are optional by the standard.
;;; We however default those to 1 and monday in the constructor
;;; saving us from checking at the use site.
(define-type (recur-rule
              constructor: recur-rule-constructor-factory
              serializer: serialize-recur-rule)
  (freq       type: (memv intervals))
  (until      type: (or false? date? datetime?))
  (count      type: (or false? (and integer? positive?)))
  (interval   type: (and integer? positive?))
  (bysecond   type: (or false? (list-of (in-range? 0 60))))
  (byminute   type: (or false? (list-of (in-range? 0 59))))
  (byhour     type: (or false? (list-of (in-range? 0 23))))
  (byday      type: (or false? (list-of (pair-of (or false? integer?)
                                                 (memv weekdays)))))
  (bymonthday type: (or false? (list-of (and (not zero?) (in-range? -31 31)))))
  (byyearday  type: (or false? (list-of (and (not zero?) (in-range? -366 366)))))
  (byweekno   type: (or false? (list-of (and (not zero?) (in-range? -53 53)))))
  (bymonth    type: (or false? (list-of (and (not zero?) (in-range? -12 12)))))
  (bysetpos   type: (or false? (list-of (and (not zero?) (in-range? -366 366)))))
  (wkst       type: (memv weekdays)))


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
     (cond ((get field)
            => (lambda (v) (proc field v)))
           (else #f)))
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
  (concatenate
   (map-fields
    (lambda (field value)
      (cond [(string-ci=? "UNTIL" (symbol->string field))
             (list
              ((xml xcal 'until)
               (if (date? value)
                   (date->string value "~Y-~m-~d")
                   (datetime->string
                    value "~Y-~m-~dT~H:~M:~S~Z"))))]

            [(string-ci=? "BYDAY" (symbol->string field))
             (map (xml xcal (downcase-symbol field))
                  (map byday->string value))]

            [(string-ci=? "BY" (substring (symbol->string field)
                                          0 2))
             (map (xml xcal (downcase-symbol field))
                  (map number->string value))]

            [else
             (list
              ((xml xcal (downcase-symbol field))
               (field->string field value)))]))

    rrule)))
