#!/usr/bin/guile \
-s
!#

;;; Commentary:
;;; Script for finding all top level `config' forms. Run this from the
;;; project root.
;;; Code:


(add-to-load-path "module")

(use-modules 
  (hnh util) 
  (ice-9 ftw) 
  (ice-9 match)
  (srfi srfi-1)
  )

(define (read-multiple)
  (let loop ((done '()))
    (let ((sexp (read)))
      (if (eof-object? sexp)
          (reverse done)
          (loop (cons sexp done))))))

(define remove-stat
  (match-lambda
    ((name state) name)
    ((name stat children ...)
     (cons name (map remove-stat children)))))

(define (f tree)
  (let loop ((rem tree) (path '()))
    (cond [(string? rem)
           (string-join (reverse (cons rem path)) "/" 'infix)]
          [(null? rem)
           '()]
          [else
           (map (lambda (branch)
                  (loop branch (cons (car rem) path)))
                (cdr rem))])))


((@ (ice-9 pretty-print) pretty-print)
 (filter
  (lambda (form)
    (and (list? form) (not (null? form))
         (eq? 'define-config (car form))))
  (concatenate
   (map (lambda (filename) (with-input-from-file filename read-multiple))
        (flatten (f (remove-stat (file-system-tree "module"))))))))

;; expected result =>
#;
((config debug)
 (config edit-mode)
 (config summary-filter)
 (config description-filter)
 (config
   tz-dir
   "Directory in which zoneinfo files can be found")
 (config
   tz-list
   "List of default zoneinfo files to be parsed")
 (config default-week-start "First day of week")
 (config
   calendar-files
   "Which files to parse. Takes a list of paths or a single string which will be globbed."))
