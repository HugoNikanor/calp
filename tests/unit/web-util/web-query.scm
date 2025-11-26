(define-module (test web-query)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((web query) :select (parse-query encode-query-parameters)))

(test-equal "Empty query gives empty assoc list"
  '() (parse-query ""))
(test-equal "Simple key-value query"
  '(key: "value") (parse-query "key=value"))

;; Slightly cumbersome check, since keys aren't ordered
(test-group
 "Simple key-value query, with multiple keys"
 (let ((kv-list (parse-query "k1=value&k2=1")))
   (test-equal "value" (and=> (memv k1: kv-list) cadr))
   (test-equal "1"     (and=> (memv k2: kv-list) cadr))))

(test-equal "Values are HTTP-decoded"
  '(key: " ") (parse-query "key=%20"))
(test-equal "Keys are HTTP-decoded"
  '(A: "test") (parse-query "%41=test"))

(test-equal "Query with only key, value becomes key"
  '(key: "key") (parse-query "key"))

(test-group
 "Some with only key"
 (let ((kv-list (parse-query "k1&k2=10")))
   (test-equal "k1" (and=> (memv k1: kv-list) cadr))
   (test-equal "10" (and=> (memv k2: kv-list) cadr))))

;; I don't know if HTTP allows this, but my code works like this
(test-equal "Value with equal in it"
  '(key: "=") (parse-query "key=="))


(test-group "encode-query-parameters"
  (test-equal "Null case" "" (encode-query-parameters '()))
  (test-equal "Single simple" "a=10" (encode-query-parameters '((a . 10))))
  (test-equal "Multi simple" "a=10&b=20" (encode-query-parameters '((a . 10) (b . 20))))
  (test-equal "Strings" "a=Hello" (encode-query-parameters '((a . "Hello"))))
  (test-equal "Strings are URI encoded" "a=Hello%20World" (encode-query-parameters '((a . "Hello World"))))
  (test-equal "Symbols are display and URI encoded"
    ;; #{Hello World}#
    "a=%23%7BHello%20World%7D%23" (encode-query-parameters `((a . ,(string->symbol "Hello World"))))))


'((web query))
