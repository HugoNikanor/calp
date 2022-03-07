;;; Commentary:
;; Checks that HTTP server can start correctly, and that at least some
;; endpoints return correct information.
;;
;; NOTE This test, when ran in as `tests/run-tests --only web-server.scm'
;; segfaults on Guile 2.2.7, but not on Guile 3.0.8. This doesn't happen
;; when it's run as one of all tests.
;;; Code:

(define-module (test web-server)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((calp server routes) :select (make-make-routes))
  :use-module ((web server) :select (run-server))
  :use-module ((ice-9 threads)
               :select (call-with-new-thread cancel-thread))
  :use-module ((web client) :select (http-get))
  :use-module ((hnh util) :select (let*))
  :use-module ((web response) :select (response-code response-location))
  :use-module ((web uri) :select (build-uri uri-path))
  :use-module ((guile)
               :select (socket
                        inet-pton
                        bind
                        make-socket-address
                        setsockopt
                        AF_INET
                        PF_INET
                        SOL_SOCKET
                        SO_REUSEADDR
                        SOCK_STREAM
                        current-error-port))
  :use-module ((ice-9 format) :select (format))
  :use-module ((web response) :select (build-response)))

(define host "127.8.9.5")

(define sock (socket PF_INET SOCK_STREAM 0))

(setsockopt sock SOL_SOCKET SO_REUSEADDR 1)

(define-values
  (port sock)
  (let ((addr (inet-pton AF_INET host)))
    (let loop ((port 8090))
      (catch 'system-error
             (lambda ()
               (bind sock
                     (make-socket-address AF_INET addr port))
               (values port sock))
             (lambda (err proc fmt args data)
               (if (and (not (null? data))
                        ;; errno address already in use
                        (= 98 (car data)))
                 (loop (1+ port))
                 ;; rethrow
                 (throw err fmt args data)))))))

(define server-thread
  (call-with-new-thread
    (lambda ()
      (catch #t
             (lambda ()
               (run-server
                 (make-make-routes)
                 'http
                 `(socket: ,sock)))
             (lambda args
               (format #f "~s~%" args)
               (test-assert "Server Crashed" #f)))
      ;; This test should always fail, but should never be run
      (test-assert "Server returned unexpectedly" #f))))

(let* ((response
         _
         (catch 'system-error
                (lambda ()
                  (http-get
                    (build-uri 'http host: host port: port)))
                (lambda (err proc fmt args data)
                  (format
                    (current-error-port)
                    "~a (in ~a) ~?~%"
                    err
                    proc
                    fmt
                    args)
                  (values (build-response code: 500) #f)))))
  (test-eqv
    "Basic connect"
    200
    (response-code response)))

(let* ((response
         body
         (http-get
           (build-uri
             'http
             host:
             host
             port:
             port
             path:
             "/today"
             query:
             "view=week&date=2020-01-04"))))
  (test-eqv
    "Redirect"
    302
    (response-code response))
  (test-equal
    "Fully specified redirect position"
    "/week/2020-01-04.html"
    (uri-path (response-location response))))

(cancel-thread server-thread)
