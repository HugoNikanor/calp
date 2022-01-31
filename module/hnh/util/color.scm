(define-module (hnh util color)
  )

;; Returns a color with good contrast to the given background color.
;; https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color/1855903#1855903
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
                         (* 0.114 b))
                      #xFF))
            "#000000" "#FFFFFF")))
    (lambda args
      (format (current-error-port) "Error calculating foreground color?~%~s~%" args)
      "#FF0000"
      )))
