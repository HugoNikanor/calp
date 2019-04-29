(define-module (output text)
  #:use-module (srfi srfi-1)
  #:use-module (util)
  #:export (justify-line flow-text))

(define-public (words str) (string-split str #\space))
(define-public (unwords list) (string-join list " " 'infix))

(define-public (lines str) (string-split str #\newline))
(define-public (unlines list) (string-join list "\n" 'infix))

;; (str) -> str
(define* (justify-line-helper words #:key (width 70))
  (let* ((phrase-length (string-length (string-concatenate/shared words)))
         (needed-spaces (- width phrase-length))
         (slots (1- (length words)))
         (space-list
          (let loop ((n needed-spaces) (d slots))
            (unless (zero? d)
              (let ((v (round (/ n d))))
                (cons v (loop (- n v)
                              (1- d))))))))
    (string-concatenate/shared
     (merge words (map (lambda (n) (make-string n #\space))
                       space-list)
            (let ((f #t)) (lambda _ (mod/r! f not)))))))


;; Splits and justifies the given line to @var{#:width}.
;; Returns a list of justified strings.
;; str -> (str)
(define* (justify-line line #:key (width 70))
  (let recur ((lst (words line)))
    (let* ((head tail (span
                       (let ((w 0))
                         (lambda (word)    ; Take words until we are above the limit.
                           (< (mod/r! w = (+ 1 (string-length word)))
                              width)))
                       lst)))
      (cond ((null? tail) (list (unwords head))) ; Don't justify last line.
            ((null? head)
             ;; an empty head implies that we found a word longer
             ;; than our max width. Add it as is and continue
             ;; (while crying).
             (cons (car tail) (recur (cdr tail))))
            (else (cons (justify-line-helper head #:width width)
                        (recur tail)))))))

;; str -> (str)
(define* (flow-text str #:key (width 70))
  (flatten
   (map (lambda (line) (justify-line line #:width width))
        (lines str))))

(define-public (trim-to-width str len)
  (let ((trimmed (string-pad-right str len)))
    (if (< (string-length trimmed)
           (string-length str))
        (string-append (string-drop-right trimmed 1)
                       "…")
        trimmed)))
