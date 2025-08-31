(define-module (test text-numbers-en)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (text numbers))

(test-equal "Fallback for non-existing language"
  "one hundred twenty-three"
  (number->string-cardinal 123 language: 'missing-language))

'((text numbers))
