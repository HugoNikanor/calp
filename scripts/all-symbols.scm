#!/usr/bin/guile \
-e main -s
!#

;;; Commentary:
;;;
;;; Scripts which finds unused imports in each file.
;;; Uses Guile's module system reflection to find what is imported,
;;; but simple looks at all unique symbols in the source file for what
;;; is used, which might lead to some discrepancies.
;;;
;;; Code:

(add-to-load-path (string-append (dirname (dirname (current-filename))) "/module"))

(use-modules (hnh util)
             (srfi srfi-1))


(define (get-forms port)
  (let loop ((done '()))
    (let ((form (read port)))
      (if (eof-object? form)
          done
          (loop (cons form done))))))

(define (flatten-tree tree)
  (cond ((null? tree) '())
        ((pair? tree)
         (append (flatten-tree (car tree))
                 (flatten-tree (cdr tree))))
        (else (list tree))))



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

(define (find-module-declaration forms)
  (cadr 
  (find (lambda (form)
          (cond ((null? form) #f)
                ((not (pair? form)) #f)
                (else (eq? 'define-module (car form)))))
        forms)))


;;; Module use high scores
;;; $ grop -Ho '#\?:use-module' -R module | uniq -c | sort -n

(define (main args)
  (define filename (cadr args))
  (define forms (reverse (call-with-input-file filename get-forms)))
  (define symbs (unique-symbols forms))
  ;; (format #t "~y" (find-module-declaration forms))
  ;; (format #t "~a~%" symbs)

  (format #t "=== ~a ===~%" filename)
  (for-each (lambda (mod)

              (define used-symbols
                (map (lambda (symb)
                       (if (memv symb symbs)
                         #f symb))
                     (module-map (lambda (key value) key) mod)))

              (define used-count (count not used-symbols))
              (define total-count (length (module-map list mod)))

              (format #t "~a/~a  ~a~%    used ~s~%  unused ~s~%"
                      used-count total-count (module-name mod)
                      (lset-difference eq?  (module-map (lambda (key value) key) mod) (filter identity used-symbols))
                      (filter identity used-symbols)
                      ))
            (remove (lambda (mod)
                      (member (module-name mod)
                              '((guile)
                                (guile-user)
                                (srfi srfi-1)
                                )))
                    (module-uses (resolve-module
                                   (find-module-declaration
                                     forms)))))
  (newline))
