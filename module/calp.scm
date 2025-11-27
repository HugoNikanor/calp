(define-module (calp)
  :export (calp-version prodid))

;; Update me on new release
(define calp-version "0.6.1")

(define (prodid)
  (format #f "-//hugo//calp ~a//EN"
          (@ (calp) calp-version)))
