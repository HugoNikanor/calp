#!/usr/bin/guile \
-s
!#

;;; Commentary:
;;; Script for finding all top level `config' forms. Run this from the
;;; project root.
;;; Code:


(add-to-load-path (string-append (dirname (dirname (current-filename))) "/module"))

(use-modules
  (hnh util)
  (ice-9 ftw)
  (ice-9 match)
  (srfi srfi-1)
  (srfi srfi-88)

  (hnh module-introspection all-modules)
  (hnh module-introspection)
  ((hnh util io)
   :select (read-all ensure-newline))

  ((calp translation)
   :select (translate))
  )

(format #t ";;;~%")
(format #t ";;; Found configurable options in the program~%")
(format #t ";;;~%")

;; TODO split this into separate read and write stages
;; TODO Add extra output formats
;; - Texinfo
;; - actual configuration files
(for (filename module-name)
  in (all-files-and-modules-under-directory "module")
  (define forms (call-with-input-file filename (lambda (p) (read-all read p))))
  (define configurations
    (filter (lambda (form)
              (and (list? form) (not (null? form))
                   (eq? 'define-config (car form))))
            forms))
  (unless (null? configurations)
    (newline)
    (format #t "[~{~a~^ ~}]~%" module-name)
    (for-each (match-lambda
                (('define-config name default kvs ...)
                 (cond ((memv description: kvs)
                        => (match-lambda
                             ((description: (_ desc) rest ...)
                              (format #t ";; ~a~%"
                                      (gettext desc "calp")))
                             ((description: desc rest ...)
                              (format #t ";; ~a~%" desc)))))
                 (format #t "~a = ~s~%"
                         name default)))
              configurations)))

(newline)
