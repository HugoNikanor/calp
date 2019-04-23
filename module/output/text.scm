(define-module (output text)
  #:use-module (util)
  #:export (justify-line flow-text))

(define-public (words str) (string-split str #\space))
(define-public (unwords list) (string-join list " " 'infix))

(define-public (lines str) (string-split str #\newline))
(define-public (unlines list) (string-join list "\n" 'infix))

(define* (add-some list amount item #:optional flipflop?)
  (cond ((zero? amount) list)
        ((null? list) '())
        (else
         (cons (if flipflop? item (car list))
               (add-some (if flipflop? list (cdr list))
                         (if flipflop? (1- amount) amount)
                         item (not flipflop?))))))

;; (str) -> str
(define* (justify-line-helper words #:key (width 70))
  (let* ((len (1- (apply + (map (compose 1+ string-length) words))))
         (to-add (- width len))
         (slots (1- (length words)))
         (sp-per (ceiling-quotient to-add slots)))

    (unwords (add-some words to-add (make-string (1- sp-per) #\space)))))


;; Splits and justifies the given line to @var{#:width}.
;; Returns a list of justified strings.
;; str -> (str)
(define* (justify-line line #:key (width 70))
  (let recur ((lst (words line)))
    (let* ((head tail (take-drop-while
                       (let ((w 0))
                         (lambda (word)    ; Take words until we are above the limit.
                           (< (mod! w = (+ 1 (string-length word)))
                              width)))
                       lst)))
      (if (null? tail)
          (list (unwords head)) ; Don't justify last line.
          (cons (justify-line-helper head #:width width)
                (recur tail))))))

;; str -> str
(define* (flow-text str #:key (width 70) (height 10))
  (unlines
   (take-to
    (flatten
     (map (lambda (line) (justify-line line #:width width))
          (lines str)))
    height)))
