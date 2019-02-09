(use-modules (srfi srfi-1)
             (srfi srfi-8) ; receive
             )

(define (nlist? l)
  "Returns #t if l is a pair that is not a list."
  (and (pair? l)
       (not (list? l))))

(define (flatten tree)
  "Flattens tree, should only return propper lists."
  (cond ((null? tree) '())
        ((list? tree)
         (if (null? (cdr tree))
             (flatten (car tree))
             (let ((ret (cons (flatten (car tree))
                              (flatten (cdr tree)))))
               (if (nlist? ret)
                   (list (car ret) (cdr ret))
                   ret))))
        (else tree)))


(define (map-lists f lst)
  "Map f over lst, if (car lst) is a list, pass the list to f. If (car list)
isn't a list, pass the rest of lst to f."
  (cond ((null? lst) '())
        ((list? (car lst)) (cons (f (car lst))
                                 (map-lists f (cdr lst))))
        (else (f lst))))

(define (beautify tree)
  "Takes a prefix tree and turns some characters to strings."
  (define (helper branch)
    (receive (head tail)
        (span char? branch)
      (cons (list->string head)
            (beautify tail))))
  (if (or (null? tree)
          (not (list? tree)))
      tree
      (cons (beautify (car tree))
            (map-lists helper (cdr tree)))))
