(define-module (vcomponent recurrence parse)
  :duplicates (last)                   ; Replace @var{count}

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (datetime)
  :use-module (srfi srfi-26)
  :use-module (vcomponent recurrence internal)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (ice-9 match)

  :export (rfc->datetime-weekday
            parse-recurrence-rule))


;; transform into weekday objects from
(define (rfc->datetime-weekday symbol)
  (case symbol
    [(SU) sun]
    [(MO) mon]
    [(TU) tue]
    [(WE) wed]
    [(TH) thu]
    [(FR) fri]
    [(SA) sat]
    [else => (lambda (d)
               (scm-error 'misc-error "rfc->datetime-weekday"
                          "No such day ~a (~s)"
                          (list d (symbol->string d))
                          #f))]))

;; @example
;; <weekday> ∈ weekdays
;; <weekdaynum> ::= [[±] <num>] <weekday> ;; +3MO
;; (<weekadynum>, ...)
;; @end example

;;; weekdaynum can contain ±
;;; only used in bywdaylist
;;; only present with by BYDAY

;; Returns a pair, where the @code{car} is the offset
;; and @code{cdr} is the day symbol.
;; The @code{car} may be @code{#f}.
;; str → (<num> . <symb>)
(define (parse-day-spec str)
  (let* ((numerical-characters (append '(#\+ #\-) (map integer->char (iota 10 #x30))))
         (numbers letters (span (cut memv <> numerical-characters)
                                (string->list str))))
    (cons (string->number (list->string numbers))
          (rfc->datetime-weekday (apply symbol letters)))))

(define* (string->number/throw string optional: (radix 10))
  (or (string->number string radix)
      (scm-error 'wrong-type-arg
                 "string->number/throw"
                 "Can't parse ~s as number in base ~a"
                 (list string radix) (list string radix))))

;; RFC 5545, Section 3.3.10. Recurrence Rule, states that the UNTIL value MUST have
;; the same type as the DTSTART of the event (date or datetime). I have seen events
;; in the wild which didn't follow this. I consider that an user error.
(define* (parse-recurrence-rule str optional: (datetime-parser parse-ics-datetime))
  (define result
    (fold
     (lambda (kv o)
       (let ((key (car kv))
             (val (cadr kv)))
         (let-lazy
          ((symb (string->symbol val))
           ;; NOTE until MUST have the same value type as DTSTART
           ;; on the object. Idealy we would save that type and
           ;; check it here. That however is impractical since we
           ;; might encounter the RRULE field before the DTSTART
           ;; field.
           (date (if (= 8 (string-length val))
                     (parse-ics-date val)
                     (parse-ics-datetime val)))
           (day (rfc->datetime-weekday (string->symbol val)))
           (days (map parse-day-spec (string-split val #\,)))
           (num  (string->number/throw val))
           (nums (map string->number/throw (string-split val #\,))))

          ;; It's an error to give BYHOUR and smaller for pure dates.
          ;; 3.3.10. p 41
          (case (string->symbol key)
            ((UNTIL)      (until      o date))
            ((COUNT)      (count      o num))
            ((INTERVAL)   (interval   o num))
            ((FREQ)       (freq       o symb))
            ((WKST)       (wkst       o day))
            ((BYSECOND)   (bysecond   o nums))
            ((BYMINUTE)   (byminute   o nums))
            ((BYHOUR)     (byhour     o nums))
            ((BYMONH)     (bymonth    o nums))
            ((BYDAY)      (byday      o days))
            ((BYMONTHDAY) (bymonthday o nums))
            ((BYYEARDAY)  (byyearday  o nums))
            ((BYSETPOS)   (bysetpos   o nums))
            ((BYWEEKNO)   (byweekno   o nums))
            (else o)))))

     ;; obj
     (recur-rule freq: (@ (vcomponent recurrence internal) freq-placeholder))

     ;; ((key val) ...)
     (map (cut string-split <> #\=)
          (string-split str #\;))))

  (when (eq? (@ (vcomponent recurrence internal) freq-placeholder)
             (freq result))
    (scm-error 'wrong-type-arg
               "parse-recurrence-rule"
               "A valid for `freq' is required, but none supplied"
               '() #f))
  result)
