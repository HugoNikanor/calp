#!/usr/bin/guile \
-s
!#

;;; Commentary:
;;; Script for finding all top level `config' forms. Run this from the
;;; project root.
;;; Code:


(add-to-load-path "module")
(add-to-load-path "scripts")

(use-modules
  (hnh util)
  (ice-9 ftw)
  (ice-9 match)
  (srfi srfi-1)
  (srfi srfi-88)

  (all-modules)
  (module-introspection)
  ((static-util)
   :select (get-forms))

  ((calp translation)
   :select (translate))
  )

;; TODO split this into separate read and write stages
;; TODO and add texinfo output (besides ini output)
(for (filename module-name)
  in (all-files-and-modules-under-directory "module")
  (define forms (call-with-input-file filename get-forms))
  (define configurations
    (filter (lambda (form)
              (and (list? form) (not (null? form))
                   (eq? 'define-config (car form))))
            forms))
  (unless (null? configurations)
    (format #t "~%[~{~a~^ ~}]" module-name)
    (for-each (match-lambda
                (('define-config name default kvs ...)
                 (cond ((memv description: kvs)
                        => (match-lambda
                             ((description: ('_ desc) rest ...)
                              (format #t "~%; ~a"
                                      (gettext desc "calp")))
                             ((description: desc rest ...)
                              (format #t "~%; ~a" desc)))))
                 (format #t "~%~a = ~s~%"
                         name default)))
              configurations)))
