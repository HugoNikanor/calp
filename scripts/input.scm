#!/usr/bin/guile -s
!#

;;; Commentary:
;;; Script for checking keyname sequences for different keyboard keys.
;;; `c' to clear screen, `q' to quit.
;;; Code:

(add-to-load-path "module")
(use-modules (vulgar))

(define chrlist '())

(with-vulgar
  (lambda ()
    (while #t
           (cls)
           (format #t "~s~%> " chrlist)
           (force-output )
           (let ((char (read-char)))
             (when (eof-object? char)
               (break))
             (set! chrlist (append chrlist (list char)))
             (case char
               ((#\c) (set! chrlist '()))
               ((#\q) (break)))))))
