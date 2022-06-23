(define-module (c operators)
  :export (wordy-binary-operators
           symbol-binary-operators
           binary-operators))


;;; Simple operators are those which can be combined with '='
(define simple-operators
  `(+ - * / & ,(symbol #\|) ^ << >> % < > =))

;; apparently part of C
(define wordy-binary-operators
  '(bitand and_eq and bitor or_eq or xor_eq xor))

(define symbol-binary-operators
  (append (map (lambda (x) (symbol-append x '=)) simple-operators)
          `(&& ,(symbol #\| #\|) != ,(symbol #\,)
               -> ,(symbol #\.))
          simple-operators))

(define binary-operators
  (append symbol-binary-operators
          wordy-binary-operators))
