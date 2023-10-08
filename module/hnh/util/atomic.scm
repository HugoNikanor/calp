(define-module (hnh util atomic)
  :use-module (srfi srfi-18)
  :export (with-mutex))

(define-syntax with-mutex
  (syntax-rules ()
    ((_ mutex body ...)
     (dynamic-wind
       (lambda () (mutex-lock! mutex))
       (lambda () body ...)
       (lambda () (mutex-unlock! mutex))))))
