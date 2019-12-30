(define-module (vcomponent recurrence internal)
  #:export (repeating? format-recur-rule make-recur-rule)

  #:use-module (srfi srfi-88)           ; better keywords
  #:use-module ((vcomponent base) :select (attr))
  #:use-module (util)
  )

;; EXDATE is also a property linked to recurense rules
;; but that property alone don't create a recuring event.
(define (repeating? ev)
  "Does this event repeat?"
  (or (attr ev 'RRULE)
      (attr ev 'RDATE)))

;; Immutable, since I easily want to be able to generate the recurence set for
;; the same event multiple times.
(define-quick-record recur-rule
  (public: freq until count interval bysecond byminute byhour
           byday bymonthday byyearday byweekno bymonth bysetpos
           wkst)

  ;; TODO make this part of define-quick-record.
  ;; Only "hard" part would be to provide type hints for fields for
  ;; string conversions.
  (printer:
   (lambda (r port)
     (define (get f)
       ((record-accessor <recur-rule> f) r))
     (with-output-to-string
       (lambda ()
         (display "#<<recur-rule>" port)
         (for field in (record-type-fields <recur-rule>)
              (awhen (get field)
                     (display " " port)
                     (display field port)
                     (display "=" port)
                     (display
                      (case field
                        ((until) ((@ (srfi srfi-19 util) time->string) it))
                        (else it))
                      port)))
         (display ">" port))))))

;; @begin{example}
;; (mkrule (freq HOURLY) (wkst MO) (interval 1))
;; @end
;; TODO field validation here, like in parse-recurrence-rule.
;; NOTE this shadows built in constructor generated in define-quick-record
(define-syntax make-recur-rule
  (syntax-rules ()
    ((_ (key val) ...)
     ((record-constructor <recur-rule> (quote (key ...)))
      val ...))))

(define-public weekdays
  '(SU MO TU WE TH FR SA))

(define-public intervals
  '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY))
