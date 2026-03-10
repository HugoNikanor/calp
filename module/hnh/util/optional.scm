(define-module (hnh util optional)
  :use-module (srfi srfi-88)
  :use-module (hnh util object)
  :use-module (hnh util serialize)
  :use-module (hnh util type)
  :use-module (hnh util destructure)
  :use-module (ice-9 curried-definitions)
  :export (optional?
           just just? just*
           nothing nothing?
           from-just unjust))

(define-type (just
              constructor: (lambda (c _) c)
              serializer: (lambda (o) `(just ,(serialize (from-just o))))
              no-destructure?: #t)
  from-just)

(define-type (nothing))

(define-matcher (just x)
  (define-values (inner-predicates inner-values inner-captures)
    (get-expander #'x))

  (values
   (lambda (expr)
     (cons #`(just? #,expr)
           (inner-predicates #`(from-just #,expr))))
   (lambda (expr) (inner-values #`(from-just #,expr)))
   inner-captures))


(define (optional? x)
  (or (just? x)
      (nothing? x)))


(define-syntax-rule (optional-of x p)
  (or (nothing? x)
      (and (just? x)
           (expand-validator (from-just x) p))))


(define ((just* optional) f)
  (if (just? optional)
      (just (f (from-just optional)))
      (nothing)))


(define-syntax-rule (unjust optional dflt)
  (if (just? optional)
      (from-just optional)
      dflt))
