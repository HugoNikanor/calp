(define-module (test text-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (text util))

(test-group "words/unwords"
 (test-equal '("A" "list" "of" "words")
   (words "A list of words"))

 (test-equal "A joined string"
   (unwords '("A" "joined" "string"))))

(test-group "lines/unlines"
 (test-equal '("A" "list of" "words")
   (lines "A\nlist of\nwords"))

 (test-equal "A joined\nstring"
   (unlines '("A joined" "string"))))

;;; TODO tests for "true" string length procedures
;;; Or better yet, remove them and format text properly.

(test-group "trim-to-width"
  (test-equal "Extending strings"
    "An extended string                      "
    (trim-to-width "An extended string" 40))
  (test-equal "Truncating strings"
    "This is a…"
    (trim-to-width "This is a rather long string" 10))
  (test-equal "Exact width"
    "Hello"
    (trim-to-width "Hello" 5)))


(test-group "Add enumeration punctuation"
  (test-equal '("")
    (add-enumeration-punctuation '()))
  (test-equal '("Single element")
    (add-enumeration-punctuation '("Single element")))
  (test-equal "Two elements"
    '("Hello" " " "&" " " "World")
    (add-enumeration-punctuation '("Hello" "World")))
  (test-equal "Three elements and different end"
    '("A" ", " "B" " " "and" " " "C")
    (add-enumeration-punctuation '("A" "B" "C")
                                 "and")))


'((text util))
