(define-module (vcomponent util control)
  :use-module (hnh util)
  :use-module (vcomponent)
  :export (with-replaced-properties))


(eval-when (expand load)                ; No idea why I must have load here.
  (define href (make-procedure-with-setter hash-ref hash-set!))

  (define (set-temp-values! table component kvs)
    (for-each (lambda (kv)
                (let ((key (car kv))
                      (val (cadr kv)))
                  (when (prop component key)
                    (set! (href table key) (prop component key))
                    (set! (prop component key) val))))
              kvs))

  (define (restore-values! table component keys)
    (for-each (lambda (key)
                (and=> (href table key)
                       (lambda (val)
                         (set! (prop component key) val))))
              keys)))

;; TODO what is this even used for?
(define-syntax with-replaced-properties
  (syntax-rules ()
    [(_ (component (key val) ...)
        body ...)

     (let ((htable (make-hash-table 10)))
       (dynamic-wind
         (lambda () (set-temp-values! htable component (quote ((key val) ...))))  ; In guard
         (lambda () body ...)
         (lambda () (restore-values! htable component (quote (key ...))))))]))  ; Out guard

