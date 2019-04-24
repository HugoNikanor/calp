(define-module (vcomponent recurrence parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)
  #:use-module ((vcomponent datetime) #:select (parse-datetime))
  #:duplicates (last)                   ; Replace @var{count}
  #:use-module (vcomponent recurrence internal)
  #:use-module (util)
  #:use-module (exceptions)
  #:use-module (ice-9 curried-definitions)
  #:export (parse-recurrence-rule))


(define (printerr fmt . args)
  (apply format (current-error-port)
          fmt args))

(define (parse-recurrence-rule str)
  (catch-multiple
    (lambda () (%build-recur-rules str))

    [unfulfilled-constraint
     (cont obj key val . rest)
     (printerr "ERR ~a [~a] doesn't fulfill constraint of type [~a], ignoring~%"
               err val key)
     (cont #f)]

    [invalid-value
     (cont obj key val . rest)
     (printerr "ERR ~a [~a] for key [~a], ignoring.~%"
               err val key)
     (cont #f)]))

(define-macro (quick-case key obj . cases)
  `(case ,key
     ,@(map (lambda (c)
              (let* (((symb val pred) c))
                `((,symb)
                  (set! (,(symbol-downcase symb) ,obj)
                        (let ((v ,val))
                          (if (,pred v) v
                              (throw-returnable
                               'unfulfilled-constraint
                               ,obj (quote ,key) ,val)))))))
            cases)))

(define-syntax all-in
  (syntax-rules ()
    ((_ var rules ...)
     (cut every (lambda (var) (and rules ...)) <>))))

(define (string->number-list val delim)
  (map string->number (string-split val delim)))

(define (string->symbols val delim)
  (map string->symbol (string-split val delim)))

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

(define (%build-recur-rules str)
  (fold
   (lambda (kv obj)
     (let* (((key val) kv))
       (let-lazy
        ((symb (string->symbol val))
         (date (date->time-utc (parse-datetime val)))
         (days (map parse-day-spec (string-split val #\,)))
         (num  (string->number val))
         (nums (string->number-list val #\,)))

        (quick-case (string->symbol key) obj
                    (FREQ     symb (cut memv <> intervals)) ; Required
                    (UNTIL    date identity)
                    (COUNT    num  (cut <= 0 <>))
                    (INTERVAL num  (cut <= 0 <>))
                    (BYSECOND nums (all-in n (<= 0 n 60)))
                    (BYMINUTE nums (all-in n (<= 0 n 59)))
                    (BYHOUR   nums (all-in n (<= 0 n 23)))

                    (BYDAY days
                           (lambda (p*)
                             (map (lambda (p)
                                    (let* (((n . s) p))
                                      (memv s weekdays)))
                                  p*)))

                    (BYMONTHDAY nums (all-in n (<=  -31 n  31) (!= n 0)))
                    (BYYEARDAY  nums (all-in n (<= -366 n 366) (!= n 0)))
                    (BYWEEKNO   nums (all-in n (<=  -53 n  53) (!= n 0)))
                    (BYMONTH    nums (all-in n (<=    1 n  12)))
                    (BYSETPOS   nums (all-in n (<= -366 n 366) (!= n 0)))

                    (WKST symb (cut memv <> weekdays))))))
   ;; obj
   (make-recur-rule 1 'MO)

   ;; ((key val) ...)
   (map (cut string-split <> #\=)
        (string-split str #\;))))
