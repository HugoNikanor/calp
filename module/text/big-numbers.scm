(define-module (text big-numbers)
  :use-module ((hnh util) :select (->>))
  :use-module (hnh util type)
  :export (number->exponential-form
           power-of-10?
           subscript supscript))

(define (integer-string-ref str n)
  (typecheck str string?)
  (typecheck n (and exact? integer?))
  (->> n
       number->string
       string->list
       (map char->integer)
       (map (lambda (x) (- x (char->integer #\0))))
       (map (lambda (i) (string-ref str i)))
       list->string))

(define (subscript n)
  (integer-string-ref "₀₁₂₃₄₅₆₇₈₉" n))

(define (supscript n)
  (integer-string-ref "⁰¹²³⁴⁵⁶⁷⁸⁹" n))

;;; Is the number a power of 10?
(define (power-of-10? x)
  (integer? (log10 (exact->inexact x))))

(define (number->exponential-form n)
  (typecheck n (and exact? integer? power-of-10?))
  (string-append "10" (supscript (inexact->exact (round (log10 n))))))
