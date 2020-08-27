;;; Commentary:
;;; @var{flow-text} take a (multiline) string, and justifies
;;; its contents. All linebreaks are treated as hard line breaks.
;;; Code:

(define-module (text flow)
  :use-module (calp util)
  :use-module (text util)
  :use-module (srfi srfi-1)
  )


;; str -> (str)
(define*-public (flow-text str #:key (width 70))
  (flatten
   (map (lambda (line) (justify-line line #:width width))
        (lines str))))



;; Splits and justifies the given line to @var{#:width}.
;; Returns a list of justified strings.
;; str -> (str)
(define* (justify-line line #:key (width 70))
  (let recur ((lst (words line)))
    (let* ((head tail (span
                       (let ((w 0))
                         (lambda (word)    ; Take words until we are above the limit.
                           (< (set/r! w = (+ 1 (true-string-length word)))
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

;; (str) -> str
(define* (justify-line-helper words #:key (width 70))
  (let* ((phrase-length (true-string-length (string-concatenate/shared words)))
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
            (let ((f #t)) (lambda _ (set/r! f = not)))))))

