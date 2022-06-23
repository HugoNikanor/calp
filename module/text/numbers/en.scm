(define-module (text numbers en)
  :use-module (ice-9 format)
  :export (number->string-cardinal
           number->string-ordinal
           each-string))

(define (number->string-cardinal n)
  (format #f "~r" n))

(define (number->string-ordinal n)
  (format #f "~:r" n))

;; Allows extra args to handle eventual local changes.
(define (each-string count . _)
  (case count
        [(1) "each"]
        [(2) "every other"]
        [else (format #f "every ~:r" count)]))
