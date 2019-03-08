(define-module (vcalendar output)
  #:use-module (vcalendar)
  #:use-module (util)
  #:use-module (srfi srfi-26)
  #:export (print-vcomponent
            color-if
            STR-YELLOW STR-RESET))

(define STR-YELLOW "\x1b[0;33m")
(define STR-RESET "\x1b[m")

(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))

(define* (print-vcomponent comp #:optional (depth 0))
  (let ((kvs (map (lambda (key) (cons key (attr comp key)))
                  (attributes comp))))
    (format #t "~a <~a> :: ~:a~%"
            (make-string depth #\:)
            (type comp) comp)
    (for-each-in kvs
                 (lambda (kv)
                   (let ((key (car kv))
                         (value (cdr kv)))
                     (format #t "~a ~20@a: ~a~%"
                             (make-string depth #\:)
                             key value))))
    (for-each-in (children comp)
                 (cut print-vcomponent <> (1+ depth)))))
