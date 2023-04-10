#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

here=$(dirname $(realpath $0))
. "$(dirname "$here")/env"

exec $GUILE -e main -s "$0" "$@"
!#

(use-modules (calp server webdav)
             (ice-9 threads)
             (ice-9 rdelim)
             (web server)
             (srfi srfi-1)
             (srfi srfi-88))

;;; Commentary:
;;; Runs the external WebDAV test framework litmus [1], pointing it
;;; to a new instance of our webdav server.
;;;
;;; [1]: http://webdav.org/neon/litmus/
;;;
;;; Code:


;;; NOTE this "page" is borrowed from (calp server server).
;;; Possibly rewrite so that module actually works as a module,
;;; And import it here


;; NOTE The default make-default-socket is broken for IPv6.
;; A patch has been submitted to the mailing list. 2020-03-31
;;
;; This sets up the socket manually, and sends that to @code{http-open}.
(define* (make-default-socket/fixed family addr port)
  (let ((sock (socket family SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock family addr port)
    sock))

(define* (setup-socket key:
                       (host #f)
                       (family AF_INET)
                       (addr (if host (inet-pton family host)
                                 INADDR_LOOPBACK))
                       (port 8080))
  (make-default-socket/fixed family addr port))



(define (start-server out)
  (begin-thread
   (let loop ((port 8102))
     (catch 'system-error
       (lambda ()
         (let ((socket (setup-socket port: port)))
           (format out "http://localhost:8102~%")
           (force-output out)
           (with-error-to-file "webdav.log"
             (lambda ()
               (run-server webdav-handler 'http
                           `(socket: ,socket))))
           (format #t "Server closed~%")))
       (lambda (err proc fmt args data)
         (if (= EADDRINUSE (car data))
             (loop (1+ port))
             (apply throw err proc fmt args data)))))))


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
