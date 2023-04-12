(define-module (calp)
  :export (version prodid))

;; Update me on new release
(define version "0.6.1")

(define (prodid)
  (format #f "-//hugo//calp ~a//EN"
          (@ (calp) version)))
