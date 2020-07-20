(define-module (c operators))


;;; Simple operators are those which can be combined with '='
(define simple-operators
  `(+ - * / & ,(symbol #\|) ^ << >> % < > =))

;; apparently part of C
(define-public wordy-binary-operators
  '(bitand and_eq and bitor or_eq or xor_eq xor))

(define-public symbol-binary-operators
  (append (map (lambda (x) (symbol-append x '=)) simple-operators)
          `(&& ,(symbol #\| #\|) != ,(symbol #\,)
               -> ,(symbol #\.))
          simple-operators))

(define-public binary-operators
  (append symbol-binary-operators
          wordy-binary-operators))
