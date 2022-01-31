(define-module (module-introspection)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :export (get-forms
           uniq
           unique-symbols
           find-module-declaration
           ))


(define (get-forms port)
  (let loop ((done '()))
    (let ((form (read port)))
      (if (eof-object? form)
          done
          (loop (cons form done))))))


(define (uniq lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) lst)
        ((and (pair? lst)
              (eqv? (car lst) (cadr lst)))
         (uniq (cons (car lst) (cddr lst))))
        (else (cons (car lst)
                    (uniq (cdr lst))))))


(define (unique-symbols tree)
  (uniq
   (sort* (filter symbol? (flatten-tree tree))
          string<? symbol->string)))


(define (flatten-tree tree)
  (cond ((null? tree) '())
        ((pair? tree)
         (append (flatten-tree (car tree))
                 (flatten-tree (cdr tree))))
        (else (list tree))))


(define (find-module-declaration forms)
  (and=> (find (lambda (form)
                 (cond ((null? form) #f)
                       ((not (pair? form)) #f)
                       (else (eq? 'define-module (car form)))))
               forms)
         cadr))
