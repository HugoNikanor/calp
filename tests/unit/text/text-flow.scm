(define-module (test text-flow)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (text flow))


(define str "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.")

(test-group "String lengths"
  (let ((final init (car+cdr (reverse (flow-text str)))))
    (test-assert (>= 70 (string-length final)))
    (test-assert (every (lambda (s) (= 70 (string-length s)))
                        init))))

;;; This could be fixed by actually supporting hyphenation
(test-equal "Justification with words longer than the line width"
  ;; This also shows bad spacing
  '("A    sentance   containing    the   word"
    ;; This line is longer that the expected line width.
    "pneumonoultramicroscopicsilicovolcanoconiosis"
    "is rather hard to justify properly")
  (flow-text "A sentance containing the word pneumonoultramicroscopicsilicovolcanoconiosis is rather hard to justify properly"
             width: 40)
  )


'((text flow))
