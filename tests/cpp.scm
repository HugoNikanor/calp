(((c lex) lex)
 ((c parse) parse-lexeme-tree))

(define run (compose parse-lexeme-tree lex))

(test-equal
    '(+ (post-increment (dereference C)) 3)
  (run "(*C)++ + 3"))


(test-equal
    '(+ (post-increment (dereference C)) 3)
  (run "*C++ + 3"))

(test-equal
    '(post-increment (dereference C))
  (run "*C++"))

(test-equal
    '(+ (post-increment C) (post-increment C))
  (run "C++ + C++"))

(test-equal
    '(+ (pre-increment C) (pre-increment C))
  (run "++C + ++C"))


(test-equal
    '(+ 2 (* 2 2))
  (run "2 + 2 * 2"))

(test-equal
    '(+ (* 2 2) 2)
  (run "2 * 2 + 2"))

(test-equal
    '(+ 2 2 2)
  (run "2+2+2"))

