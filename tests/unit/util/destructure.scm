(define-module (test destructure)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (hnh util destructure))

;;; test basic identifier binding
(test-equal 10 (destructure 10 (x x)))

;;; test basic literals:
(test-group "Self quoting literals"
  (test-equal 'exact-string
    (destructure "str"
      ("oth" 'another-string)
      ("str" 'exact-string)
      (_ 'catch-all)))

 (test-equal "Vector literals with vector literal value"
   'exact-vector
    (destructure #(1 2 3)
      (#(1 2) 'another-vector)
      (#(1 2 3) 'exact-vector)
      (_ 'catch-all)))

 (test-equal "Vector literal with non-literal value"
   'exact-vector
   (destructure (vector 1 2 3)
     (#(1 2) 'another-vector)
     (#(1 2 3) 'exact-vector)
     (_ 'catch-all))))

(test-equal "cons as base compound type" 3
  (destructure (cons 1 2)
    ((cons x y) (+ x y))))

(test-equal "identifier equivalence"
  'neq
  (destructure (cons 1 2)
    ((cons x x) 'eq)
    ((cons x y) 'neq)))

(test-equal "_ isn't bound (by binding different values to it)"
  'not-bound
  (destructure (cons 1 2)
    ((cons _ _) 'not-bound)
    (_ 'dflt)))

(test-equal "cons is recursive"
  (list 1 2 3 4)
  (destructure (cons (cons 1 2) (cons 3 4))
    ((cons (cons x y) (cons z w))
     (list x y z w))))

(test-equal "Binding equivalence is also deep (negative case)"
  'otherwise
  (destructure (cons (cons 1 2) (cons 3 4))
    ((cons (cons x y) (cons x z))
     'matching-cars)
    (_ 'otherwise)))


(test-equal "Binding equivalence is also deep (positive case)"
  'matching-cars
  (destructure (cons (cons 1 2) (cons 1 4))
    ((cons (cons x y) (cons x z))
     'matching-cars)
    (_ 'otherwise)))



;;; (we can now use cons in remaining types)




(test-group "(@ name pat)"
  (test-equal "Named pattern"
    (cons 1 2)
    (destructure (cons 1 2)
      ((@ p (cons x y)) p))))

(test-group "'sexp"
  (test-equal "Quoted data (positive)"
    'match
    (destructure '(a b c)
      ('(a b c) 'match)
      (_ 'no-match)))

  (test-equal "Quoted data (negative)"
    'no-match
    (destructure '(a b c)
      ('(a b d) 'match)
      (_ 'no-match))))

;;; TODO `sexp

(test-group "(and pat predicates ...)"
  (test-equal
      'desc
    (destructure (cons 2 1)
      ((and (cons x y)
            (< x y))
       'asc)
      ((and (cons x y)
            (> x y))
       'desc))))

(test-group "cons*"
  (test-equal "cons* only tail"
    'x
    (destructure 'x
      ((cons* x) x)))
  (test-equal "cons* many values"
    '(0 1 (2 3 4))
    (destructure (iota 5)
      ((cons* x y z) (list x y z)))))

(test-group "list"
  (test-equal "empty list"
    'empty
    (destructure '()
      ((list) 'empty)))

  (test-equal "List of exact length"
    'yes
    (destructure (iota 3)
      ((list x y) 'no)
      ((list x y z w) 'also-no)
      ((list x y z) 'yes)))

  (test-equal "Fully variable list"
    10
    (destructure (iota 10)
      ((list xs ...)
       (length xs))))

  (test-equal "Partially variable list"
    '(0 (1) 2)
    (destructure (iota 3)
      ((list a xs ... b)
       (list a xs b))))

  (test-equal "Variable list with complex inner pattern"
    "Hello, World!"
    (destructure (map cons
                      (iota 13)
                      (string->list "Hello, World!"))
      ((list (cons _ c) ...) (list->string c)))))

(test-group "vector"
  ;; We assume that the vector matcher shares basically all code with.
  (test-equal 3
    (destructure #(1 2 3)
      ((vector x y z) z)))

  (test-equal "Slices are returned in their \"native\" form"
    #(a b c)
    (destructure #(1 2 a b c 3)
      ((vector _ _ as ... _)
       as))))

'((hnh util destructure))
