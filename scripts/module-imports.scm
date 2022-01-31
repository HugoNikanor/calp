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

(use-modules (hnh util)
             (srfi srfi-1)
             (module-introspection))


;;; Module use high scores
;;; $ grop -Ho '#\?:use-module' -R module | uniq -c | sort -n

(define (main args)
  (define filename (cadr args))
  (define forms (reverse (call-with-input-file filename get-forms)))
  ;; All symbols in source file
  (define symbs (unique-symbols forms))
  ;; (format #t "~y" (find-module-declaration forms))
  ;; (format #t "~a~%" symbs)

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
