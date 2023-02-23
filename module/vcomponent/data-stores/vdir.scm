(define-module (vcomponent data-stores vdir)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module ((srfi srfi-88) :select ())
  :export ())

(define-class <vdir-data-store> (<calendar-data-store>)
  )

(define-method (get-all (this <vdir-data-store>))
  '())

(define-method (get-by-uid (this <vdir-data-store>) (uid <string>))
  #f
  )

;; (define (get-in-date-interval ))
