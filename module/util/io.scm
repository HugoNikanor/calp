(define-module (util io)
  :use-module ((ice-9 rdelim) :select (read-line)))

(define-public (open-input-port str)
  (if (string=? "-" str)
      (current-input-port)
      (open-input-file str)))

(define-public (open-output-port str)
  (if (string=? "-" str)
      (current-output-port)
      (open-output-file str)))



(define-public (read-lines port)
  (with-input-from-port port
    (lambda ()
      (let loop ((line (read-line)))
        (if (eof-object? line)
            '() (cons line (loop (read-line))))))))
