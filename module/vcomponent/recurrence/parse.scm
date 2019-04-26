(define-module (vcomponent recurrence parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)
  #:use-module ((vcomponent datetime) #:select (parse-datetime))
  #:duplicates (last)                   ; Replace @var{count}
  #:use-module (vcomponent recurrence internal)
  #:use-module (util)
  #:use-module (ice-9 match)
  #:export (parse-recurrence-rule))


;; @example
;; <weekday> ∈ weekdays
;; <weekdaynum> ::= [[±] <num>] <weekday> ;; +3MO
;; (<weekadynum>, ...)
;; @end example

;; Returns a pair, where the @code{car} is the offset
;; and @code{cdr} is the day symbol.
;; The @code{car} may be @code{#f}.
(define (parse-day-spec str)
  (let* ((numchars (append '(#\+ #\-) (map integer->char (iota 10 #x30))))
         (num symb (span (cut memv <> numchars)
                         (string->list str))))
    (cons (string->number (list->string num))
          (apply symbol symb))))

(define-macro (quick-case key . cases)
  (let ((else-clause (or (assoc-ref cases 'else)
                         '(error "Guard failed"))))
    `(case ,key
       ,@(map (match-lambda
                ((key guard '=> body ...)
                 `((,key) (if (not ,guard)
                              (begin ,@else-clause)
                              (begin ,@body))))
                ((key body ...)
                 `((,key) (begin ,@body)))
                (('else body ...)
                 `(else ,@body)))
              cases))))

(define (parse-recurrence-rule str)
  (fold
   (lambda (kv o)
     (let* (((key val) kv))
       (let-lazy
        ((symb (string->symbol val))
         (date (date->time-utc (parse-datetime val)))
         (days (map parse-day-spec (string-split val #\,)))
         (num  (string->number val))
         (nums (map string->number (string-split val #\,))))

        (quick-case (string->symbol key)
          (UNTIL (set! (until o) date))

          (COUNT    (<= 0 num) => (set! (count o) num))
          (INTERVAL (<= 0 num) => (set! (interval o) num))

          (FREQ (memv symb intervals) => (set! (freq o) symb))
          (WKST (memv symb weekdays)  => (set! (wkst o) symb))

          (BYSECOND (every (lambda (n) (<= 0 n 60)) nums) => (set! (bysecond o) nums))
          (BYMINUTE (every (lambda (n) (<= 0 n 59)) nums) => (set! (byminute o) nums))
          (BYHOUR   (every (lambda (n) (<= 0 n 23)) nums) => (set! (byhour   o) nums))
          (BYMONTH  (every (lambda (n) (<= 1 n 12)) nums) => (set! (byweekno o) nums))

          (BYDAY (every (lambda (p) (memv (cdr p) weekdays)) days) => (set! (byday o) days))

          (BYMONTHDAY (every (lambda (n) (and (!= n 0) (<= -31  n 31)))  nums) => (set! (bymonthday o) nums))
          (BYYEARDAY  (every (lambda (n) (and (!= n 0) (<= -366 n 366))) nums) => (set! (byyearday  o) nums))
          (BYSETPOS   (every (lambda (n) (and (!= n 0) (<= -366 n 366))) nums) => (set! (bysetpos   o) nums))
          (BYWEEKNO   (every (lambda (n) (and (!= n 0) (<= -53  n 53)))  nums) => (set! (byweekno   o) nums))
          (else o)))))

   ;; obj
   (make-recur-rule 1 'MO)

   ;; ((key val) ...)
   (map (cut string-split <> #\=)
        (string-split str #\;))))
