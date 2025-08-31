(define-module (test text-numbers-en)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (text numbers))

(test-equal "one hundred twenty-three million, four hundred fifty-six thousand, seven hundred eighty-nine"
 (number->string-cardinal 123456789 language: 'en))
(test-equal "one hundred twenty-three million, four hundred fifty-six thousand, seven hundred eighty-ninth"
 (number->string-ordinal 123456789 language: 'en))

(test-group "each-string en"
 (test-equal "each"
   (each-string 1 language: 'en))
 (test-equal "every other"
   (each-string 2 language: 'en))
 (test-equal "every third"
   (each-string 3 language: 'en)))

'((text numbers)
  (text numbers en))
