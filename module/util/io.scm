(define-module (util io))

(define-public (open-input-port str)
  (if (string=? "-" str)
      (current-input-port)
      (open-input-file str)))

(define-public (open-output-port str)
  (if (string=? "-" str)
      (current-output-port)
      (open-output-file str)))
