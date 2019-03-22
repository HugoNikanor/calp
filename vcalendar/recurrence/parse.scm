(define-module (vcalendar recurrence parse)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)           ; Datetime
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-26)
  #:use-module ((vcalendar datetime) #:select (parse-datetime))
  #:duplicates (last)                   ; Replace @var{count}
  #:use-module (vcalendar recurrence internal)
  #:use-module (util)
  #:use-module (exceptions)
  #:use-module (ice-9 curried-definitions)
  #:export (build-recur-rules)

  )



(define (build-recur-rules str)
  "Takes a RECUR value (string), and returuns a <recur-rule> object"
 (catch #t
  (lambda () (%build-recur-rules str))
  (lambda (err cont obj key val . rest)
    (let ((fmt (case err
                 ((unfulfilled-constraint)
                  "ERR ~a [~a] doesn't fulfill constraint of type [~a], ignoring~%")
                 ((invalid-value)
                  "ERR ~a [~a] for key [~a], ignoring.~%")
                 (else "~a ~a ~a"))))
      (format #t fmt err val key))
    (cont obj))))

(eval-when (expand)
 (define ((handle-case stx obj) key val proc)
   (with-syntax ((skey (datum->syntax
                        stx (symbol-downcase (syntax->datum key)))))
     #`((#,key)
        (let ((v #,val))
          (cond ((not v) (throw-returnable 'invalid-value #,obj (quote #,key) v))
                ((#,proc #,val) (set! (skey #,obj) v))
                (else (set! (skey #,obj)
                            (throw-returnable 'unfulfilled-constraint
                                              #,obj (quote #,key) v)))))))))


;; A special form of case only useful in build-recur-rules above.
;; Each case is on the form (KEY val check-proc) where:
;; `key` is what should be matched against, and what is used for the setter
;; `val` is the value to bind to the loop object and
;; `check` is something the object must conform to
(define-syntax quick-case
  (lambda (stx)
   (syntax-case stx ()
     ((_ var-key obj (key val proc) ...)
      #`(case var-key
          #,@(map (handle-case stx #'obj)
                  #'(key ...)
                  #'(val ...)
                  #'(proc ...))
        (else obj))))))

(define-syntax all-in
  (syntax-rules ()
    ((_ var rules ...)
     (cut every (lambda (var) (and rules ...)) <>))))

(define (string->number-list val delim)
  (map string->number (string-split val delim)))

(define (string->symbols val delim)
  (map string->symbol (string-split val delim)))

(define (%build-recur-rules str)
  (fold
   (lambda (kv obj)
     (let* (((key val) kv)
            ;; Lazy fields for the poor man.
            (symb (lambda () (string->symbol val)))
            (date (lambda () (parse-datetime val)))
            (num  (lambda () (string->number val)))
            (nums (lambda () (string->number-list val #\,))))
       (quick-case (string->symbol key) obj
                   (FREQ     (symb) (cut memv <> intervals)) ; Requirek
                   (UNTIL    (date) identity)
                   (COUNT    (num)  (cut <= 0 <>))
                   (INTERVAL (num)  (cut <= 0 <>))
                   (BYSECOND (nums) (all-in n (<= 0 n 60)))
                   (BYMINUTE (nums) (all-in n (<= 0 n 59)))
                   (BYHOUR   (nums) (all-in n (<= 0 n 23)))

                   ;; TODO
                   ;; <weekday> ∈ weekdays
                   ;; <weekdaynum> ::= [[±] <num>] <weekday> ;; +3MO
                   ;; (<weekadynum>, ...)
                   ;; (BYDAY (string-split val #\,))

                   (BYMONTHDAY (nums) (all-in n (<=  -31 n  31) (!= n 0)))
                   (BYYEARDAY  (nums) (all-in n (<= -366 n 366) (!= n 0)))
                   (BYWEEKNO   (nums) (all-in n (<=  -53 n  53) (!= n 0)))
                   (BYMONTH    (nums) (all-in n (<=    1 n  12)))
                   (BYSETPOS   (nums) (all-in n (<= -366 n 366) (!= n 0)))

                   (WKST (symb) (cut memv <> weekdays))
                   )))

   ;; obj
   (make-recur-rule 1 'MO)

   ;; ((key val) ...)
   (map (cut string-split <> #\=)
        (string-split str #\;))))
