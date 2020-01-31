(define-module (output general)
  )

;; Returns a color with good contrast to the given background color.
(define-public (calculate-fg-color c)
  (catch #t
    (lambda ()
      (define (str->num c n) (string->number (substring/shared c n (+ n 2)) 16))
      ;; (format (current-error-port) "COLOR = ~s~%" c)
      (let ((r (str->num c 1))
            (g (str->num c 3))
            (b (str->num c 5)))
        (if (< 1/2 (/ (+ (* 0.299 r)
                         (* 0.587 g)
                         (* 0.144 b))
                      #xFF))
            "#000000" "#e5e8e6")))
    (lambda args
      (format (current-error-port) "Error calculating foreground color?~%~s~%" args)
      "#FF0000"
      )))
