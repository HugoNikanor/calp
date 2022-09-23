;;; Commentary:
;;;
;;; Scripts which finds unused imports in each file.
;;; Uses Guile's module system reflection to find what is imported,
;;; but simple looks at all unique symbols in the source file for what
;;; is used, which might lead to some discrepancies.
;;;
;;; Code:

(define-module (scripts module-imports)
  :use-module ((srfi srfi-1) :select (lset-difference))
  :use-module ((rnrs lists) :select (remp filter partition))
  :use-module ((hnh module-introspection) :select (module-declaration? unique-symbols))
  :use-module ((hnh module-introspection static-util) :select (get-forms))
  :use-module ((hnh module-introspection module-uses) :select (module-uses*))
  :export (main)
  )

(define %summary "List imports, and how many are used.")
(define %synopsis "module-imports filename")

;;; Module use high scores
;;; $ grep -Ho '#\?:use-module' -R module | uniq -c | sort -n

(define (main . args)
  (define filename (car args))
  (define-values (module-declaration-list forms)
    (partition module-declaration?
               (reverse (call-with-input-file filename get-forms))))

  ;; All symbols in source file, which are not in module declaration.
  ;; Otherwise all explicitly imported symbols would be marked as
  ;; used.
  (define symbs (unique-symbols forms))
  ;; (format #t "~y" (find-module-declaration forms))
  ;; (format #t "~a~%" symbs)

  ;; TODO parameterize this to a command line argument
  (define skip-list '((guile)
                      (guile-user)
                      (srfi srfi-1)
                      ))


  (define modules
    (if (null? module-declaration-list)
        (map resolve-interface
             (remp (lambda (mod) (member mod skip-list))
                   (module-uses* forms)))
        (remp (lambda (mod) (member (module-name mod) skip-list))
              (module-uses (resolve-module
                            (cadr (car module-declaration-list)))))))

  (format #t "=== ~a ===~%" filename)
  (for-each (lambda (mod)

              ;; all symbols imported from module
              (define all-symbols (module-map (lambda (key value) key) mod))

              ;; Thes subset of all imported symbols from module which are used
              (define used-symbols
                (filter (lambda (symb) (memv symb symbs))
                        all-symbols))

              (define used-count (length used-symbols))
              (define total-count (length (module-map list mod)))

              (format #t "~a/~a  ~a~%    used ~s~%  unused ~s~%"
                      used-count total-count (module-name mod)
                      used-symbols
                      (lset-difference eq? all-symbols used-symbols)))
            modules)
  (newline))
