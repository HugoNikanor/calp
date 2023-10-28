;;; Commentary:
;; Checks some prodecuders from (hnh util)
;;; Code:

(define-module (test hnh-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  )

(define (unreachable)
  (throw 'unreachable))


;;; Changed core bindings

(test-group "set!"
  (let ((x 10))
    (set! x 20)
    (test-eqv "Regular set! still works" 20 x))

  (test-group "Multiple set! at once works"
    (let ((x 10) (y 20))
      (set! x 20
            y 30)
      (test-eqv x 20)
      (test-eqv y 30)))

  (test-group "Set! is ordered"
    (let ((x 10))
      (set! x 20
            x (* x 2))
      (test-eqv x 40)))

  ;; TODO
  ;; (test-group "set! ="
  ;;   )

  )

;;; Nonscensical to test
;; (test-group "define-syntax"
;;   )

(test-group "when"
 (test-equal "when"
   1 (when #t 1))

 (test-equal "'() when #f"
   '() (when #f 1)))

(test-group "unless"
 (test-equal "unless"
   1 (unless #f 1))

 (test-equal "'() unless #t"
   '() (unless #t 1)))



;;; New bindings

(test-group "aif"
  (aif (+ 1 2)
       (test-eqv 3 it)
       (unreachable))

  (aif #f
       (unreachable)
       (test-assert #t)))

(test-group "awhen"
 (test-equal "awhen it"
   '(3 4 5)
   (awhen (memv 2 '(1 2 3 4 5))
          (cdr it)))

 (test-equal "awhen not"
   '()
   (awhen (memv 0 '(1 2 3 4 5))
          (cdr it))))

(test-group "for"
 (test-equal "for simple"
   (iota 10)
   (for x in (iota 10)
        x))

 (test-equal "for matching"
   (iota 12)
   (for (x c) in (zip (iota 12) (string->list "Hello, World"))
        x))

 (test-equal "for with improper list elements"
   `(3 7)
   (for (a . b) in '((1 . 2) (3 . 4))
        (+ a b)))

 (test-equal "for with longer improper list elements"
   '(1 2 4)
   (for (a b . c) in '((1 -1 . 1) (2 -2 . 2) (4 -4 . 4))
        (* c (+ 1 a b)))))

(test-group "begin1"
 (let ((value #f))
   (test-equal
       "begin1 return value"
     "Hello"
     (begin1 "Hello" (set! value "World")))
   (test-equal "begin1 side effects" "World" value))

 (let ((x 1))
   (test-eqv "begin1 set! after return"
     1 (begin1 x (set! x 10)))
   (test-eqv "Updates value"
     10 x)))

(test-group "print-and-return"
  (let ((p (open-output-string)))
    (let ((v (with-error-to-port p
               (lambda () (print-and-return (+ 1 2))))))
      (test-equal "Printed value"
        "3 [(+ 1 2)]\n" (get-output-string p))
      (test-eqv "Returned value"
        3 v))))

(test-group "swap"
  (test-equal
      '(3 2 1)
    ((swap list) 1 2 3)))

(test-group "set/r!"
 (test-equal
     "set/r! = single"
   #f
   (let ((x #t)) (set/r! x = not)))

 (test-error
     'syntax-error
   (test-read-eval-string "(set/r! x err not)")))

(test-group "label"
 (test-equal "procedure label"
   120
   ((label factorial (lambda (n)
                       (if (zero? n)
                           1 (* n (factorial (1- n))))))
    5)))

(test-group "sort*"
 ;; we can't test if sort*! destroys the list, since its only /allowed/ to do it,
 ;; not required.
 (test-equal "sort*!"
   '("a" "Hello" "Assparagus")
   (sort*! (list-copy '("Hello" "a" "Assparagus"))
           < string-length)))


(test-group "find-extreme"
  (test-error 'wrong-type-arg (find-extreme '()))

  (test-group "find-min"
   (call-with-values
       (lambda () (find-min (iota 10)))
     (lambda (extreme rest)
       (test-equal "Found correct minimum" 0 extreme)
       (test-equal
           "Removed \"something\" from the set"
         9
         (length rest)))))

  (test-group "find-max"
   (call-with-values
       (lambda ()
         (find-max
          '("Hello" "Test" "Something long")
          string-length))
     (lambda (extreme rest)
       (test-equal
           "Found the longest string"
         "Something long"
         extreme)
       (test-equal "Removed the string" 2 (length rest))
       (test-assert
           "Other members left 1"
         (member "Hello" rest))
       (test-assert
           "Other members left 2"
         (member "Test" rest))))))

(test-group "filter-sorted"
 (test-equal
     "Filter sorted"
   '(3 4 5)
   (filter-sorted (lambda (x) (<= 3 x 5)) (iota 10))))


(test-group "!="
 (test-assert "not equal"
   (!= 1 2)))

(test-group "init+last"
  (call-with-values
    (lambda () (init+last (iota 5)))
    (lambda (init last)
      (test-equal '(0 1 2 3) init)
      (test-equal 4 last))))

(test-group "take-to"
 (test-equal "Take to"
   '() (take-to '() 5)))

(test-group "string-take-to"
  (test-equal "Hello"
    (string-take-to "Hello, World!" 5)))

(test-group "string-first"
  (test-eqv #\H (string-first "Hello, World!")))

(test-group "string-last"
  (test-eqv #\! (string-last "Hello, World!")))

(test-group "as-symb"
  (test-eq "From string" 'hello (as-symb "hello"))
  (test-eq "From symbol" 'hello (as-symb 'hello))
  (test-eq "NOTE that others pass right through"
    '() (as-symb '())))


(test-group "enumerate"
 (test-equal "Enumerate"
   '((0 #\H) (1 #\e) (2 #\l) (3 #\l) (4 #\o) (5 #\,) (6 #\space) (7 #\W) (8 #\o) (9 #\r) (10 #\l) (11 #\d) (12 #\!))
   (enumerate (string->list "Hello, World!"))))


(test-group "unval"
 (test-equal "unval first"
   1
   ((unval (lambda () (values 1 2 3)))))

 (test-equal "unval other"
   2
   ((unval car+cdr 1)
    (cons 1 2))))


(test-group "flatten"
 (test-equal "flatten already flat"
   (iota 10)
   (flatten (iota 10)))

 (test-equal "flatten really deep"
   '(1)
   (flatten '(((((((((((((((1)))))))))))))))))

 (test-equal "flatten mixed"
   '(1 2 3 4 5)
   (flatten '((((((1(((((2((((3))))))4))))))))5))))

(test-group "let-lazy"
  'TODO)

(test-group "map/dotted"
 (test-equal "map/dotted without dot"
   '(1 2 3 4)
   (map/dotted 1+ '(0 1 2 3)))

 (test-equal "map/dotted with dot"
   '(1 2 3 . 4)
   (map/dotted 1+ '(0 1 2 . 3)))

 (test-equal "map/dotted direct value"
   1 (map/dotted 1+ 0)))

(test-group "assq-merge"
  (test-equal "assq merge"
    '((k 2 1) (v 2))
    (assq-merge '((k 1) (v 2)) '((k 2)))))


(test-group "kvlist->assq"
 (test-equal "kvlist->assq"
   '((a . 1) (b . 2))
   (kvlist->assq '(a: 1 b: 2)))

 (test-equal "kvlist->assq repeated key"
   '((a . 1) (b . 2) (a . 3))
   (kvlist->assq '(a: 1 b: 2 a: 3))))

(test-group "assq-limit"
  'TODO)


(test-group "group-by"
  ;; Extra roundabout tests since groups-by doesn't guarantee order of the keys
  (test-group "Two simple groups"
    (let ((groups (group-by even? (iota 10))))
      (test-assert (lset= eq? '(#f #t) (map car groups)))
      (test-assert (lset= = '(0 2 4 6 8) (assq-ref groups #t)))
      (test-assert (lset= = '(1 3 5 7 9) (assq-ref groups #f)))))

  (test-group "Identity groups"
    (let ((groups (group-by identity (iota 5))))
      (test-assert "Correct keys"
        (lset= = (iota 5) (map car groups)))
      (test-group "Correct amount in each group"
        (for-each (lambda (g) (test-equal 1 (length (cdr g)))) groups))))

  (test-equal "Null case"
    '()
    (group-by (lambda _ (unreachable)) '())))

(test-group "split-by"
  'TODO)


(test-group "span-upto"
  (test-group "Case 1"
   (call-with-values
       (lambda ()
         (span-upto
          2
          char-numeric?
          (string->list "123456")))
     (lambda (head tail)
       (test-equal '(#\1 #\2) head)
       (test-equal '(#\3 #\4 #\5 #\6) tail))))

  (test-group "Case 2"
   (call-with-values
       (lambda ()
         (span-upto
          2
          char-numeric?
          (string->list "H123456")))
     (lambda (head tail)
       (test-equal '() head)
       (test-equal '(#\H #\1 #\2 #\3 #\4 #\5 #\6) tail)))))

(test-group "cross-product"

  (test-equal "null case"
    '() (cross-product))

  (test-equal "Basic case"
      '((1 4)
        (1 5)
        (1 6)
        (2 4)
        (2 5)
        (2 6)
        (3 4)
        (3 5)
        (3 6))
   (cross-product
    '(1 2 3)
    '(4 5 6)))

  (test-equal "Single input list"
    '((1) (2) (3))
    (cross-product '(1 2 3)))

  (test-equal "More than two"
    '((1 3 5) (1 3 6)
      (1 4 5) (1 4 6)
      (2 3 5) (2 3 6)
      (2 4 5) (2 4 6))
    (cross-product
     '(1 2)
     '(3 4)
     '(5 6))))

(test-group "string-flatten"
  'TODO)

(test-group "intersperse"
  'TODO)

(test-group "insert-ordered"
  'TODO)

(test-group "-> (arrows)"
  (test-equal "->" 9 (-> 1 (+ 2) (* 3)))
  (test-equal "-> order dependant" -1 (-> 1 (- 2)))
  (test-equal "->> order dependant" 1 (->> 1 (- 2))))

(test-group "set"
  'TODO)

(test-group "set->"
  'TODO)

(test-group "downcase-symbol"
  'TODO)


(test-group "group"
  ;; TODO test failure when grouping isn't possible?
  (test-equal "Group"
    '((0 1) (2 3) (4 5) (6 7) (8 9))
    (group (iota 10) 2)))

(test-group "iterate"
  (test-equal 0 (iterate 1- zero? 10)))

(test-group "valued-map"
  'TODO)

(test-group "assoc-ref-all"
  (test-equal "assoc-ref-all"
    '(1 3) (assoc-ref-all '((a . 1) (b . 2) (a . 3)) 'a))
  (test-equal "assq-ref-all"
    '(1 3) (assq-ref-all '((a . 1) (b . 2) (a . 3)) 'a))
  (test-equal "assv-ref-all"
    '(1 3) (assv-ref-all '((a . 1) (b . 2) (a . 3)) 'a)))

(test-group "unique"
  'TODO)

(test-group "vector-last"
  (test-equal "vector-last"
    1 (vector-last #(0 2 3 1))))

(test-group "->string"
  (test-equal "5" (->string 5))
  (test-equal "5" (->string "5")))

(test-group "catch*"
  'TODO)

'((hnh util))
