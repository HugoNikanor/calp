(define-module (test vulgar-color)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (vulgar color))

(test-equal "No color" "" (color-escape #f))

;;; Extra with-output-to-string so escape sequences are escaped in error diff.
(test-equal "Color"
  (with-output-to-string (lambda () (write "\x1b[38;2;16;32;48m")))
  (with-output-to-string (lambda () (write (color-escape "#102030")))))

'((vulgar color))
