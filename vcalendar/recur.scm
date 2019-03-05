(define-module (vcalendar recur)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (util))


(define-immutable-record-type <recur-rule>
  (make-recur-rules freq until count interval)
  recur-rule?
  (freq     get-freq     set-freq)
  (until    get-until    set-until)
  (count    get-count    set-count)
  (interval get-interval set-interval))


;; (build-recur-rules "FREQ=HOURLY") ; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=3") ; => #<<recur-rule> freq: HOURLY until: #f count: 3 interval: #f>
;; (build-recur-rules "FREQ=ERR;COUNT=3") ; => #<<recur-rule> freq: #f until: #f count: 3 interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=err") ; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>
;; (build-recur-rules "FREQ=HOURLY;COUNT=-1") ; => #<<recur-rule> freq: HOURLY until: #f count: #f interval: #f>

(define (build-recur-rules str)
 (catch-let
  (lambda () (%build-recur-rules str))

  ((unknown-key
    (lambda (err cont obj key . rest)
      (format #t "ERR Invalid key [~a] while parsing recurence rule, ignoring.~%" key)
      (cont obj)))

   (unfulfilled-constraint
    (lambda (err cont obj key val . rest)
      (let ((default-value (case key
                             ((INTERVAL) 1)
                             (else #f))))
        (format #t "ERR Value [~a] doesn't fulfill constraint of type [~a], defauting to [~a].~%"
                val key default-value)
        (cont default-value))))

   (invalid-value
    (lambda (err cont obj key val . rest)
      (format #t "ERR Invalid value [~a] for key [~a], ignoring.~%" val key)
      (cont obj))))))

(define (string->number-list val delim)
  (map string->number (string-split val delim)))

(define (%build-recur-rules str)
  (fold-lists
   (lambda ((key val) obj)
     (quick-case (string->symbol key) obj
                 (FREQ  (string->symbol val) (cut memv <> '(SECONDLY MINUTELY HOURLY DAILY WEEKLY MONTHLY YEARLY)))
                 (UNTIL (string->date val) identity)
                 (COUNT (string->number val) (cut <= 0 <>))
                 (INTERVAL (string->number val) (cut <= 0 <>))
                 (BYSECOND (string->number-list val #\,) (cut every (cut <= 0 <> 60) <>))
                 (BYMINUTE (string->number-list val #\,) (cut every (cut <= 0 <> 59) <>))
                 (BYHOUR (string->number-list val #\,) (cut every (cut <= 0 <> 23) <>))
                 ))
   (make-recur-rules #f #f #f 1)
   (map (cut string-split <> #\=)
        (string-split str #\;))))


;;; A special form of case only useful in build-recur-rules above.
;;; Each case is on the form (KEY val check-proc) where:
;;; `key` is what should be matched against, and what is used for the setter
;;; `val` is the value to bind to the loop object and
;;; `check` is something the object must conform to

(define (syntax-helper obj parent-expr expr )
  "Helper function for quick-case below"
  (with-syntax ((obj (datum->syntax parent-expr obj)))
    (syntax-case expr ()
      ((key val proc)
       (with-syntax ((setter (datum->syntax parent-expr (make-setter (syntax->datum (syntax key))))))
         #'((key)
            (cond ((not val) (call/cc (lambda (cont) (throw 'invalid-value cont obj (quote key) val))))
                  ((proc val) (setter obj val))
                  (else (setter obj (call/cc (lambda (cont) (throw 'unfulfilled-constraint cont obj (quote key) val))))))))))))

(define-syntax quick-case 
  (lambda (x)
    (syntax-case x ()
      ((_ var-key obj (key val proc) ...)
       (let* ((make-setter (lambda (symb) (symbol-append 'set- (symbol-downcase symb))))
              (cc (lambda (lst) (map (cut syntax-helper (syntax->datum (syntax obj)) x <>)
                                lst))))
         #`(case var-key
             #,@(cc #'((key val proc) ...))
             (else (call/cc (lambda (cont) (throw 'unknown-key cont obj var-key))))))))))
