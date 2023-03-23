;;; Commentary:
;;; A state monad similar to (and directly influenced by) the one found in in
;;; Haskell
;;; Each procedure can either explicitly take the state as a curried last
;;; argument, or use the `do' notation, which handles that implicitly.
;;; Each procedure MUST return two values, where the second value is the state
;;; value which will be chained.
;;;
;;; Code borrowed from guile-dns
;;; Code:

(define-module (hnh util state-monad)
  :use-module (ice-9 curried-definitions)
  :replace (do mod)
  :export (with-temp-state
           <$> return get get* put put* sequence lift))

(define-syntax do
  (syntax-rules (<- let =)
    ((_ (a ...) <- b rest ...)
     (lambda state-args
       (call-with-values (lambda () (apply b state-args))
                         (lambda (a* . next-state)
                           (apply (lambda (a ...)
                                    (apply (do rest ...)
                                           next-state))
                                  a*)))))
    ((_ a <- b rest ...)
     (lambda state-args
       (call-with-values (lambda () (apply b state-args))
                         (lambda (a . next-state)
                           (apply (do rest ...)
                                  next-state)))))

    ((_ a = b rest ...)
     (let ((a b))
       (do rest ...)))

    ((_ a)
     (lambda state (apply a state)))
    ((_ a rest ...)
     (lambda state
       (call-with-values (lambda () (apply a state))
                         (lambda (_ . next-state)
                           (apply (do rest ...)
                            next-state)))))))


(define (with-temp-state state* op)
  (do old <- (get*)
      (apply put* state*)
      ret-value <- op
      (apply put* old)
      (return ret-value)))


(define (<$> f y)
  (do tmp <- y
      (return (f tmp))))

(define ((return x) . y)
  (apply values x y))

(define ((get*) . state)
  "Like @code{get}, but always returns a list"
  (values state state))

(define ((get) fst . state)
  "If state contains a single variable return that, otherwise, return a list of all variables in state"
  (if (null? state)
      (values fst fst)
      (apply values (cons fst state) fst state)))

(define ((put . new-state) fst . old-state)
  (if (null? old-state)
      (apply values fst new-state)
      (apply values (cons fst old-state) new-state)))

;; Like put, but doesn't return anything (useful)
(define ((put* . new-state) . _)
  (apply values #f new-state))

(define (mod proc)
  (do
    a <- (get)
    (put (proc a))))

;; ms must be a list of continuations
(define (sequence ms)
  (if (null? ms)
    (return '())
    (do
      fst <- (car ms)
      rest <- (sequence (cdr ms))
      (return (cons fst rest)))))


(define (lift proc . arguments)
  (do xs <- (sequence arguments)
      (return (apply proc xs))))
