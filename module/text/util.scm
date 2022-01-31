;;; Commentary:
;;; Small utility functions which helps the rest of the text processing.
;;; Code:

(define-module (text util)
  :use-module ((hnh util) :select (define*-public intersperse) )
  )

(define-public (words str) (string-split str #\space))
(define-public (unwords list) (string-join list " " 'infix))

(define-public (lines str) (string-split str #\newline))
(define-public (unlines list) (string-join list "\n" 'infix))

;; Alternative string-length whith counts ANSI escapes as 0-length.
;; NOTE Some way to opt in and out of different features would be nice.
(define-public (true-string-length word)
  (let loop ((chars (string->list word)))
    (if (null? chars)
        0
        (let ((char (car chars)))
          (if (eqv? #\escape char)
              (loop (cdr (memv #\m chars)))
              (1+ (loop (cdr chars))))))))

(define*-public (true-string-pad str len optional: (chr #\space))
  (let ((strlen (true-string-length str)))
    (if (> strlen len)
        str
        (string-append (make-string (- len strlen) chr)
                       str))))


(define-public (trim-to-width str len)
  (let ((trimmed (string-pad-right str len)))
    (if (< (string-length trimmed)
           (string-length str))
        (string-append (string-drop-right trimmed 1)
                       "…")
        trimmed)))

;; TODO more options for infix strings
(define*-public (add-enumeration-punctuation
                 list optional: (final-delim "&"))
  (cond [(null? list) ""]
        [(= 1 (length list)) (car list)]
        [else
         (let* ((rev (reverse list))
                (tail (car rev))
                (rest (cdr rev)))
           (reverse (cons* tail " " final-delim " "
                           (intersperse ", " rest))))]))
