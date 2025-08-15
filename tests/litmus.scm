#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

# here=$(dirname $(realpath $0))
# . "$(dirname "$here")/env"

root=$(dirname "$(dirname "$(realpath "$0")")")
eval "$(env __PRINT_ENVIRONMENT=1 "${root}/calp")"

exec $GUILE -e main -s "$0" "$@"
!#

(use-modules (calp server webdav)
             (calp server socket)
             (ice-9 threads)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-88))

;;; Commentary:
;;; Runs the external WebDAV test framework litmus [1], pointing it
;;; to a new instance of our webdav server.
;;;
;;; [1]: https://notroj.github.io/litmus/
;;;
;;; Code:



(define (start-server out)
  (begin-thread
   (with-error-to-file "webdav.log"
     (lambda ()
       (run-at-any-port
        webdav-handler
        min-port: 8102
        msg-port: out)))))


(define (main args)
  (define-values (in out) (car+cdr (pipe)))
  (define scm (start-server out))
  (define uri-base (read-line in))
  (define suffix
    (if (null? (cdr args))
        ""
        (string-append "/" (cadr args))))
  (system* "litmus" (string-append uri-base suffix))

  (cancel-thread scm))
