(define-module (vcomponent recurrence parse)
  #:duplicates (last)                   ; Replace @var{count}

  #:export (parse-recurrence-rule)

  #:use-module (srfi srfi-1)
  #:use-module (datetime)
  #:use-module (srfi srfi-26)
  #:use-module (vcomponent recurrence internal)
  #:use-module (hnh util)
  #:use-module (hnh util exceptions)
  #:use-module (ice-9 match))


;; transform into weekday objects from
(define-public (rfc->datetime-weekday symbol)
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

(define-macro (quick-case key . cases)
  (let ((else-clause (or (assoc-ref cases 'else)
                         '(scm-error 'misc-error "quick-case"
                                     "Guard failed"
                                     #f #f))))
    `(case ,key
       ,@(map (match-lambda
                ((key guard '=> body ...)
                 `((,key) (if (not ,guard)
                              (begin (warning
                                      "RRULE guard failed for key ~a~%    guard: ~a : ~s"
                                      (quote ,key)
                                      (quote ,guard)
                                      (map (lambda (o) (if (procedure? o)
                                                      (procedure-name o)
                                                      o))
                                           (list ,@guard)))
                                     ,@else-clause)
                              (begin ,@body))))
                ((key body ...)
                 `((,key) (begin ,@body)))
                (('else body ...)
                 `(else ,@body)))
              cases))))

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
  (fold
   (lambda (kv o)
     (let* (((key val) kv))
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
        (quick-case (string->symbol key)
          (UNTIL (set (until o) date))

          (COUNT    (<= 0 num) => (set (count o) num))
          (INTERVAL (<= 0 num) => (set (interval o) num))

          (FREQ (memv symb intervals) => (set (freq o) symb))
          (WKST (memv day weekdays)  => (set (wkst o) day))

          ;; Always positive
          (BYSECOND (every (lambda (n) (<= 0 n 60)) nums) => (set (bysecond o) nums))
          (BYMINUTE (every (lambda (n) (<= 0 n 59)) nums) => (set (byminute o) nums))
          (BYHOUR   (every (lambda (n) (<= 0 n 23)) nums) => (set (byhour   o) nums))
          (BYMONTH  (every (lambda (n) (<= 1 n 12)) nums) => (set (bymonth  o) nums))

          ;; May be negative
          (BYDAY (every (lambda (p) (memv (cdr p) weekdays)) days) => (set (byday o) days))

          (BYMONTHDAY (every (lambda (n) (and (!= n 0) (<= -31  n 31)))  nums) => (set (bymonthday o) nums))
          (BYYEARDAY  (every (lambda (n) (and (!= n 0) (<= -366 n 366))) nums) => (set (byyearday  o) nums))
          (BYSETPOS   (every (lambda (n) (and (!= n 0) (<= -366 n 366))) nums) => (set (bysetpos   o) nums))
          (BYWEEKNO   (every (lambda (n) (and (!= n 0) (<= -53  n 53)))  nums) => (set (byweekno   o) nums))

          (else o)))))

   ;; obj
   (make-recur-rule)

   ;; ((key val) ...)
   (map (cut string-split <> #\=)
        (string-split str #\;))))
