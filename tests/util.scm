;;; Commentary:
;; Checks some prodecuders from (hnh util)
;;; Code:

(((hnh util) filter-sorted set/r!
  find-min find-max span-upto
  iterate ->string ->quoted-string
  begin1)
 ((hnh util path) path-append path-split)
 ((ice-9 ports) with-output-to-string)
 ((guile) set!)
 )

(test-equal "Filter sorted"
  '(3 4 5)
  (filter-sorted (lambda (x) (<= 3 x 5)) (iota 10)))

(test-equal "set/r! = single"
  #f
  (let ((x #t))
    (set/r! x = not)))

(test-error
 'syntax-error
 (test-read-eval-string "(set/r! x err not)"))


(call-with-values (lambda () (find-min (iota 10)))
  (lambda (extreme rest)
    (test-equal "Found correct minimum"
      0 extreme)
    (test-equal "Removed \"something\" from the set"
      9 (length rest))))


(call-with-values (lambda () (find-max '("Hello" "Test" "Something long") string-length))
  (lambda (extreme rest)
    (test-equal "Found the longest string" "Something long" extreme)
    (test-equal "Removed the string" 2 (length rest))
    (test-assert "Other members left 1" (member "Hello" rest))
    (test-assert "Other members left 2" (member "Test" rest))))


(test-error 'misc-error (find-extreme '()))

(call-with-values (lambda () (span-upto 2 char-numeric? (string->list "123456")))
  (lambda (head tail)
    (test-equal '(#\1 #\2) head)
    (test-equal '(#\3 #\4 #\5 #\6) tail)))

(call-with-values (lambda () (span-upto 2 char-numeric? (string->list "H123456")))
  (lambda (head tail)
    (test-equal '() head)
    (test-equal '(#\H #\1 #\2 #\3 #\4 #\5 #\6) tail)))


(let ((value #f))
  (test-equal "begin1 return value" "Hello"
              (begin1 "Hello" (set! value "World")))
  (test-equal "begin1 side effects" "World" value))


(test-equal 0 (iterate 1- zero? 10))

(test-equal "5" (->string 5))
(test-equal "5" (->string "5"))

(test-equal "5" (->quoted-string 5))
(test-equal "\"5\"" (->quoted-string "5"))

(test-equal "no slashes"                                      "home/user"  (path-append "home" "user"))
(test-equal "no slashes, absolute"                            "/home/user" (path-append "" "home" "user"))
(test-equal "slashes in one component, absolute"              "/home/user" (path-append "" "/home/" "user"))
(test-equal "slashes in one component, absolute due to first" "/home/user" (path-append "/home/" "user"))
(test-equal "Slashes in both"                                 "home/user"  (path-append "home/" "/user"))
(test-equal "root"                                            "/"          (path-append ""))

(test-equal '("usr" "lib" "test")    (path-split "usr/lib/test"))
(test-equal '("usr" "lib" "test")    (path-split "usr/lib/test/"))
(test-equal '("" "usr" "lib" "test") (path-split "/usr/lib/test"))
(test-equal '("" "usr" "lib" "test") (path-split "//usr////lib/test"))
