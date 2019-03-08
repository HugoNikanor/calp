(define-module (code)
  #:export (extract sort* color-if
                    for-each-in STR-YELLOW STR-RESET
                    print-vcomponent))

(use-modules (srfi srfi-19)
             (srfi srfi-19 util)
             (srfi srfi-26)
             (vcalendar))

(define (extract field)
  (cut get-attr <> field))

;;; This function borrowed from web-ics (calendar util) 
(define* (sort* items comperator #:optional (get identity))
  "A sort function more in line with how python's sorted works"
  (sort items (lambda (a b)
                (comperator (get a)
                            (get b)))))

(define STR-YELLOW "\x1b[0;33m")
(define STR-RESET "\x1b[m")


(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))


(define* (print-vcomponent comp #:optional (depth 0))
  (let ((kvs (map (lambda (key) (cons key (get-attr comp key)))
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

