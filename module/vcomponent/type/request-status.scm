(define-module (vcomponent type request-status)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (srfi srfi-88)
  :export (request-status
           request-status?
           statcode statcode*
           statdesc statdesc*
           extdata extdata*))

(define-type (request-status)
  (statcode type: (or (tuple-of exact-integer? exact-integer?)
                      (tuple-of exact-integer? exact-integer? exact-integer?)))
  (statdesc type: string?)
  (extdata type: (or false? string?)))
