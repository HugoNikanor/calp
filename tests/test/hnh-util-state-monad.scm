(define-module (test hnh-util-state-monad)
  :use-module (srfi srfi-64)
  :use-module (hnh util state-monad))


(call-with-values (lambda () ((return 1) 2))
  (lambda (value state)
    (test-equal "Return returns the value unmodified" 1 value)
    (test-equal "Return also returns the state as a second value" 2 state)))

(test-equal "Get returns the current state as primary value, while kepping the state"
  '(state state)
  (call-with-values (lambda () ((get) 'state)) list))

;; Return value of put untested, since it's undefined
(test-equal "Put replaces the old state with a new one, and return old one"
  '(old-state new-state)
  (call-with-values (lambda () ((put 'new-state) 'old-state))
    list))

(test-equal "A simple do is effectively a `values' call"
  '(value initial-state)
  (call-with-values (lambda () ((do (return 'value)) 'initial-state))
    list))

(test-equal "Let statement in do"
  '(10 state)
  (call-with-values (lambda () ((do x = 10
                                  (return x))
                                'state))
                    list))

;; TODO let statement with multiple binds
;; (do let (a b) = (values 10 20) ...)

(test-equal "Set and get through do, along with <- in do."
  '(5 1)
  (call-with-values (lambda () ((do old <- (get)
                               (put (1+ old))
                               (return 5))
                           0))
    list))



(test-equal "<$> Updates stuff before being removed from the monad context"
    '(11 10)
 (call-with-values (lambda ()
                     ((do x <- (<$> 1+ (get))
                          (return x))
                      10))
   list))

(test-equal "Sequence should update the state accordingly"
  3
  (call-with-values
      (lambda ()
        ((sequence
           (list (mod 1+)
                 (mod 1+)
                 (mod 1+)))
         0))
    (lambda (_ st) st)))

(test-equal "Sequence should also act as map on the primary value"
  '((0 1 2) 3)
  (call-with-values
      (lambda ()
        ((sequence
           (list (mod 1+)
                 (mod 1+)
                 (mod 1+)))
         0))
    list))

(test-equal "Get returns a single value when only a single value is in the state"
  '(1 1) (call-with-values (lambda () ((get) 1))
           list))

(test-equal "Get returns a list of values when multiple items are in the state"
  '((1 2 3) 1 2 3)
  (call-with-values (lambda ()  ((get) 1 2 3))
    list))

(test-equal "Get with multiple values"
  '((1 2) 1 2)
  (call-with-values (lambda () ((get) 1 2))
    list))

(test-equal "Get with multiple values in do"
  '((1 2) 1 2)
  (call-with-values (lambda ()
                     ((do (a b) <- (get)
                          (return (list a b)))
                      1 2))
    list))

((do (put 0)
     (with-temp-state
      (list 10)
      (do a <- (get)
          (return (test-equal "Temporary state is set"
                    10 a))
          (put 20)))
   a <- (get)
   (return (test-equal "Pre-temp state is restored" 0 a)))
 'init)


;; TODO test for do where the number of implicit arguments changes

(test-equal "Something" 30
            ((do (with-temp-state
                  '(10 20)
                  ;; todo (lift +)
                  (do (a b) <- (get)
                      (return (+ a b)))))
             0 1))


