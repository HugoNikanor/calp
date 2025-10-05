(define-module (hnh util optional)
  :use-module (srfi srfi-88)
  :use-module (hnh util object)
  :use-module (ice-9 curried-definitions)
  :export (optional?
           just just? just*
           nothing nothing?
           from-just))

(define-type (just
              constructor: (lambda (c _) c)
              serializer: (lambda (o) `(just ,(from-just o))))
  from-just)

(define-type (nothing))

(define (optional? x)
  (or (just? x)
      (nothing? x)))


(define ((just* optional) f)
  (if (just? optional)
      (just (f (from-just optional)))
      (nothing)))
