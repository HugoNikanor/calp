#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

# here=$(dirname $(realpath $0))
# . "$(dirname "$here")/env"

root=$(dirname "$(dirname "$(realpath "$0")")")
eval "$(env __PRINT_ENVIRONMENT=1 "${root}/calp")"

exec $GUILE -e main -s "$0" "$@"
!#

(use-modules ((calp server webdav) :select (webdav-handler))
             ((hnh util randport) :select (randport))
             ((web server) :select (run-server))
             ((ice-9 threads) :select (begin-thread cancel-thread))
             (srfi srfi-1)
             (srfi srfi-88)

             ((scheme base) :select (string->utf8))

             (rnrs io ports)
             (oop goops)
             (calp webdav resource)
             (calp webdav resource virtual)
             (calp webdav resource file)

             (calp webdav builder)
             )

;;; Commentary:
;;; Runs the external WebDAV test framework litmus [1], pointing it
;;; to a new instance of our webdav server.
;;;
;;; [1]: https://notroj.github.io/litmus/
;;;
;;; Code:




(define (main args)
  (define root-resource
    (build-webdav-resource-tree
     `(virtual
       (("virtual"
         (virtual
          content: ,(string->utf8 "Hello, World\n")))
        ("files"
         ;; TODO the temp directory should be created by us
         (file path: "/home/hugo/tmp2"))))))

  (define-values (port socket)
    (randport "127.0.0.1" start: 8102))

  (define server-thread
   (begin-thread
    (with-error-to-file "webdav.log"
      (lambda ()
        (run-server (webdav-handler root-resource) 'http `(socket: ,socket))))))

  ;; Start the litmus test suite

  (define suffix
    (if (null? (cdr args))
        ""
        (string-append "/" (cadr args))))
  ;; Tiny wait to give the server thread chance to start properly
  (usleep 1000)

  (let ((tests (getenv "TESTS")))
    (when (or (not tests) (string-null? tests))
      ;; Everything except "http". Those we skip since they are part
      ;; of the Guile provided server, and it hangs trying to do a
      ;; Expect: 100-continue
      (setenv "TESTS" (string-join '("basic" "copymove" "props" "locks")))))

  (system* "litmus" (format #f "http://localhost:~a~a"
                            port suffix))

  (cancel-thread server-thread))
