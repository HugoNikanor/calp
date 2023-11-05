(define-module (test text-numbers-en)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (text numbers))

(test-equal "Fallback for non-existing language"
  "one hundred twenty-three"
  (number->string-cardinal 123 'missing-language))

'((text numbers))
