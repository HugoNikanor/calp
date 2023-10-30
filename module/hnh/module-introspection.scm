(define-module (hnh module-introspection)
  :use-module (srfi srfi-1)
  :use-module (hnh util)
  :export (unique-symbols
           find-module-declaration
           module-declaration?
           get-forms
           ))


(define (unique-symbols tree)
  (uniq
   (sort* (filter symbol? (flatten tree))
          string<? symbol->string)))

(define (module-declaration? form)
  (cond ((null? form) #f)
        ((not (pair? form)) #f)
        (else (eq? 'define-module (car form)))))

(define (find-module-declaration forms)
  (and=> (find module-declaration? forms)
         cadr))

(define (get-forms port)
  (let loop ((done '()))
    (let ((form (read port)))
      (if (eof-object? form)
          done
          (loop (cons form done))))))
