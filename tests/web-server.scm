;;; Commentary:
;; Checks that HTTP server can start correctly, and that at least some
;; endpoints return correct information.
;;
;; NOTE This test, when ran in as `tests/run-tests --only web-server.scm'
;; segfaults on Guile 2.2.7, but not on Guile 3.0.8. This doesn't happen
;; when it's run as one of all tests.
;;; Code:

(((calp server routes) make-make-routes)
 ((web server) run-server)
 ((ice-9 threads) call-with-new-thread cancel-thread)
 ((web client) http-get)
 ((hnh util) let*)
 ((web response) response-code response-location)
 ((web uri) build-uri uri-path)
 ((guile)
  socket inet-pton bind make-socket-address setsockopt
  AF_INET PF_INET SOL_SOCKET SO_REUSEADDR SOCK_STREAM
  )
 )


(define host "127.8.9.5")
(define sock (socket PF_INET SOCK_STREAM 0))
(setsockopt sock SOL_SOCKET SO_REUSEADDR 1)

(define-values (port sock)
 (let ((addr (inet-pton AF_INET host)))
   (let loop ((port 8090))
     (catch 'system-error
       (lambda ()
         (bind sock (make-socket-address AF_INET addr port))
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
      (run-server (make-make-routes) 'http `(socket: ,sock))
      ;; This test should always fail, but should never be run
      (test-assert "Server returned unexpectedly" #f))))

(let* ((response body (http-get (build-uri 'http host: host port: port))))
  (test-eqv "Basic connect" 200 (response-code response)))

(let* ((response body (http-get (build-uri 'http host: host port: port
                                           path: "/today"
                                           query: "view=week&date=2020-01-04"))))
  (test-eqv "Redirect"
    302 (response-code response))
  (test-equal "Fully specified redirect position"
    "/week/2020-01-04.html" (uri-path (response-location response))))

(cancel-thread server-thread)
