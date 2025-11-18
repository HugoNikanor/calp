(define-module (vcomponent type recurrence internal)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)           ; better keywords
  :use-module ((vcomponent) :select (prop1))
  :use-module (ice-9 i18n)
  :use-module (ice-9 pretty-print)
  :use-module (hnh util)
  :use-module (hnh util object)
  :use-module ((hnh util type) :select (list-of pair-of false?))
  :use-module (datetime)
  :replace (count)
  :export (repeating?

           recur-rule
           recur-rule?
           freq freq*
           until      until*
           count      count*
           interval   interval*
           bysecond   bysecond*
           byminute   byminute*
           byhour     byhour*
           byday      byday*
           bymonthday bymonthday*
           byyearday  byyearday*
           byweekno   byweekno*
           bymonth    bymonth*
           bysetpos   bysetpos*
           wkst       wkst*

           weekdays
           intervals

           weekday->symbol

           byday->string
           ))

(define weekdays
  (weekday-list sun))

(define (weekday->symbol day)
  (vector-ref #(SU MO TU WE TH FR SA) day))

(define freq-placeholder (gensym))

(define intervals
  `(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY
             ,freq-placeholder))


;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (prop1 ev 'RRULE)
      (prop1 ev 'RDATE)
      ;; TODO TODO doesn't exist
      (prop1 ev '-X-HNH-ALTERNATIVES)))

(define-syntax-rule (in-range? x start end)
  (<= start x end))

(define (recur-rule-constructor-factory primitive-constructor type-checker)
  ;; Interval and wkst have default values, since those are assumed
  ;; anyways, and having them set frees us from having to check them at
  ;; the use site.
  (lambda* (key: (freq freq-placeholder) until count (interval 1)
                 bysecond byminute
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
     (symbol->string (weekday->symbol day)))))



