(define-module (vcomponent control)
  #:use-module (util)
  #:use-module (vcomponent)
  #:export (with-replaced-attrs))


(eval-when (expand load)                ; No idea why I must have load here.
  (define href (make-procedure-with-setter hash-ref hash-set!))

  (define (set-temp-values! table component kvs)
    (for-each (lambda (kv)
                (let* (((key val) kv))
                  (when (attr component key)
                    (set! (href table key) (attr component key))
                    (set! (attr component key) val))))
              kvs))

  (define (restore-values! table component keys)
    (for-each (lambda (key)
                (and=> (href table key)
                       (lambda (val)
                         (set! (attr component key) val))))
              keys)))

;;; TODO with-added-attributes

(define-syntax with-replaced-attrs
  (syntax-rules ()
    [(_ (component (key val) ...)
        body ...)

     (let ((htable (make-hash-table 10)))
       (dynamic-wind
         (lambda () (set-temp-values! htable component (quote ((key val) ...))))  ; In guard
         (lambda () body ...)
         (lambda () (restore-values! htable component (quote (key ...))))))]))  ; Out guard

