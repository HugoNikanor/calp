;;; Commentary:
;; Checks some prodecuders from (hnh util)
;;; Code:

(define-module (test util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :use-module (hnh util env)
  :use-module ((hnh util path)
               :select (path-append
                        path-split
                        file-hidden?
                        realpath
                        relative-to
                        filename-extension)))

(test-equal "when"
  1 (when #t 1))

(test-equal "'() when #f"
  '() (when #f 1))

(test-equal "unless"
  1 (unless #f 1))

(test-equal "'() unless #t"
  '() (unless #t 1))

(test-equal "awhen it"
  '(3 4 5)
  (awhen (memv 2 '(1 2 3 4 5))
         (cdr it)))

(test-equal "awhen not"
  '()
  (awhen (memv 0 '(1 2 3 4 5))
         (cdr it)))

(test-equal "for simple"
  (iota 10)
  (for x in (iota 10)
       x))

(test-equal "for matching"
  (iota 12)
  (for (x c) in (zip (iota 12) (string->list "Hello, World"))
       x))

(test-equal "procedure label"
  120
  ((label factorial (lambda (n)
                      (if (zero? n)
                          1 (* n (factorial (1- n))))))
   5))

;; we can't test if sort*! destroys the list, since its only /allowed/ to do it,
;; not required.
(test-equal "sort*!"
  '("a" "Hello" "Assparagus")
  (sort*! '("Hello" "a" "Assparagus")
          < string-length))

(test-assert "not equal"
  (!= 1 2))

(test-equal "Take to"
  '() (take-to '() 5))

(test-equal "Enumerate"
  '((0 #\H) (1 #\e) (2 #\l) (3 #\l) (4 #\o) (5 #\,) (6 #\space) (7 #\W) (8 #\o) (9 #\r) (10 #\l) (11 #\d) (12 #\!))
  (enumerate (string->list "Hello, World!")))

(test-equal "unval first"
  1
  ((unval (lambda () (values 1 2 3)))))

(test-equal "unval other"
  2
  ((unval car+cdr 1)
   (cons 1 2)))

(test-equal "flatten already flat"
  (iota 10)
  (flatten (iota 10)))

(test-equal "flatten really deep"
  '(1)
  (flatten '(((((((((((((((1)))))))))))))))))

(test-equal "flatten mixed"
  '(1 2 3 4 5)
  (flatten '((((((1(((((2((((3))))))4))))))))5)))

;; TODO test let-lazy

(test-equal "map/dotted without dot"
  '(1 2 3 4)
  (map/dotted 1+ '(0 1 2 3)))

(test-equal "map/dotted with dot"
    '(1 2 3 . 4)
  (map/dotted 1+ '(0 1 2 . 3)))

(test-equal "map/dotted direct value"
  1 (map/dotted 1+ 0))

(test-equal "assq merge"
  '((k 2 1) (v 2))
  (assq-merge '((k 1) (v 2)) '((k 2))))

(test-equal "kvlist->assq"
  '((a 1) (b 2))
  (kvlist->assq '(a: 1 b: 2)))


(test-equal "kvlist->assq repeated key"
  '((a 1) (b 2) (a 3))
  (kvlist->assq '(a: 1 b: 2 a: 3)))

;; TODO assq-limit ?

(test-equal "->" 9 (-> 1 (+ 2) (* 3)))
(test-equal "-> order dependant" -1 (-> 1 (- 2)))
(test-equal "->> order dependant" 1 (->> 1 (- 2)))

;; TODO set and set->

;; TODO and=>>

(test-equal "Group"
  '((0 1) (2 3) (4 5) (6 7) (8 9))
  (group (iota 10) 2))

;; TODO test failure when grouping isn't possible?

(test-equal "assoc-ref-all" '(1 3) (assoc-ref-all '((a . 1) (b . 2) (a . 3)) 'a))
(test-equal "assq-ref-all" '(1 3) (assq-ref-all '((a . 1) (b . 2) (a . 3)) 'a))
(test-equal "assv-ref-all "'(1 3) (assv-ref-all '((a . 1) (b . 2) (a . 3)) 'a))

(test-equal "vector-last"
  1 (vector-last #(0 2 3 1)))

;; TODO test catch*

(test-equal
  "Filter sorted"
  '(3 4 5)
  (filter-sorted (lambda (x) (<= 3 x 5)) (iota 10)))

(test-equal
  "set/r! = single"
  #f
  (let ((x #t)) (set/r! x = not)))

(test-error
  'syntax-error
  (test-read-eval-string "(set/r! x err not)"))

(call-with-values
  (lambda () (find-min (iota 10)))
  (lambda (extreme rest)
    (test-equal "Found correct minimum" 0 extreme)
    (test-equal
      "Removed \"something\" from the set"
      9
      (length rest))))

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
      (member "Test" rest))))

(test-error 'wrong-type-arg (find-extreme '()))

(call-with-values
  (lambda ()
    (span-upto
      2
      char-numeric?
      (string->list "123456")))
  (lambda (head tail)
    (test-equal '(#\1 #\2) head)
    (test-equal '(#\3 #\4 #\5 #\6) tail)))

(call-with-values
  (lambda ()
    (span-upto
      2
      char-numeric?
      (string->list "H123456")))
  (lambda (head tail)
    (test-equal '() head)
    (test-equal '(#\H #\1 #\2 #\3 #\4 #\5 #\6) tail)))

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
    10 x))

(test-equal 0 (iterate 1- zero? 10))

(test-equal "5" (->string 5))

(test-equal "5" (->string "5"))

(test-equal
  "no slashes"
  "home/user"
  (path-append "home" "user"))

(test-equal
  "no slashes, absolute"
  "/home/user"
  (path-append "" "home" "user"))

(test-equal
  "slashes in one component, absolute"
  "/home/user"
  (path-append "" "/home/" "user"))

(test-equal
  "slashes in one component, absolute due to first"
  "/home/user"
  (path-append "/home/" "user"))

(test-equal
  "Slashes in both"
  "home/user"
  (path-append "home/" "/user"))

(test-equal "root" "/" (path-append ""))

(test-equal
  '("usr" "lib" "test")
  (path-split "usr/lib/test"))

(test-equal
  '("usr" "lib" "test")
  (path-split "usr/lib/test/"))

(test-equal
  '("" "usr" "lib" "test")
  (path-split "/usr/lib/test"))

(test-equal
  '("" "usr" "lib" "test")
  (path-split "//usr////lib/test"))

(test-assert (file-hidden? ".just-filename"))
(test-assert (file-hidden? "/path/to/.hidden"))
(test-assert (not (file-hidden? "/visible/.in/hidden")))
(test-assert (not (file-hidden? "")))

;; TODO test realpath with .. and similar

(test-equal "Realpath for path fragment"
  "/home/hugo"
  (with-working-directory
   "/home"
   (lambda () (realpath "hugo"))))

(test-equal "Realpath for already absolute path"
  "/home/hugo"
  (with-working-directory
   "/tmp"
   (lambda () (realpath "/home/hugo"))))

(test-equal "Realpath for already absolute path"
  "/home/hugo"
  (with-working-directory
   "/tmp"
   (lambda () (realpath "/home/hugo"))))


(test-group "Relative to"

  (test-group "With relative child"
    (test-equal "/some/path" (relative-to "/some" "path")))

  ;; Relative parent just adds (getcwd) to start of parent,
  ;; but this is "hard" to test.
  ;; (test-group "With relative parent")

  (test-group "With absolute child"
    (test-error 'misc-error (relative-to "" "/some/path"))
    (test-equal "some/path" (relative-to "/" "/some/path"))
    (test-group "Without trailing slashes"
      (test-equal "path" (relative-to "/some" "/some/path"))
      (test-equal "../path" (relative-to "/some" "/other/path")))
    (test-group "With trailing slashes"
      (test-equal "path" (relative-to "/some" "/some/path/"))
      (test-equal "../path" (relative-to "/some" "/other/path/"))))

  (test-equal "/a/b" (relative-to "/a/b/c" "/a/b"))

  )


(test-equal "Extension of simple file"
  "txt" (filename-extension "file.txt"))

(test-equal "Extension of file with directory"
  "txt" (filename-extension "/direcotry/file.txt"))

(test-equal "Extension of file with multiple"
  "gz" (filename-extension "filename.tar.gz"))

(test-equal "Filename extension when none is present"
  "" (filename-extension "filename"))

(test-equal "Filename extension when none is present, but directory has"
  "" (filename-extension "config.d/filename"))

(test-equal "Filename extension of directory"
  "d" (filename-extension "config.d/"))


(test-equal "Extension of hidden file"
  "sh" (filename-extension ".bashrc.sh"))

(test-equal "Extension of hidden file without extension"
  "bashrc" (filename-extension ".bashrc"))
