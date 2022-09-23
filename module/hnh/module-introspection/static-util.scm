(define-module (hnh module-introspection static-util)
  :export (get-forms))

(define (get-forms port)
  (let loop ((done '()))
    (let ((form (read port)))
      (if (eof-object? form)
          done
          (loop (cons form done))))))
