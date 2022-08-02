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
(add-to-load-path (dirname (current-filename)))

(use-modules ((srfi srfi-1) :select (lset-difference))
             ((rnrs lists) :select (remp filter partition))
             ((module-introspection) :select (module-declaration? unique-symbols))
             ((static-util) :select (get-forms))
             ((module-uses) :select (module-uses*))
             )


;;; Module use high scores
;;; $ grep -Ho '#\?:use-module' -R module | uniq -c | sort -n

(define (main args)
  (define filename (cadr args))
  (define-values (module-declaration-list forms)
    (partition module-declaration?
               (reverse (call-with-input-file filename get-forms))))

  ;; All symbols in source file, which are not in module declaration.
  ;; Otherwise all explicitly imported symbols would be marked as
  ;; used.
  (define symbs (unique-symbols forms))
  ;; (format #t "~y" (find-module-declaration forms))
  ;; (format #t "~a~%" symbs)

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
