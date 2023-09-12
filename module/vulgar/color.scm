(define-module (vulgar color)
  :export (color-if color-escape))

(define-public STR-RESET "\x1b[m")

(define-syntax-rule (color-if pred color body ...)
  (let ((pred-value pred))
    (format #f "~a~a~a"
            (if pred-value color "")
            (begin body ...)
            (if pred-value STR-RESET ""))))

(define (color-escape n)
  (cond ((not n) "")
        ((char=? #\# (string-ref n 0))
         (let* ((str (string-drop n 1))
                (rs (substring str 0 2))
                (gs (substring str 2 4))
                (bs (substring str 4 6)))
           (format #f "\x1b[38;2;~a;~a;~am"
                   (string->number rs 16)
                   (string->number gs 16)
                   (string->number bs 16))))))
