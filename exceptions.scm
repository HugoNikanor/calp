(define-module (exceptions)
  #:export (throw-returnable))

(define-syntax-rule (throw-returnable symb args ...)
  (call/cc (lambda (cont) (throw symb cont args ...))))
