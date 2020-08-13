(((server routes) make-make-routes)
 ((web server) run-server)
 ((ice-9 threads) call-with-new-thread cancel-thread)
 ((web client) http-get)
 ((util) let*)
 ((web response) response-code response-location)
 ((web uri) build-uri uri-path)
 ((guile) AF_INET))

;; TODO find some free address.
(define port 8090)
(define host "127.8.9.5")

(define server-thread
  (call-with-new-thread
   (lambda ()
     (run-server (make-make-routes)
                 'http
                 `(family: ,AF_INET
                           host: ,host
                           port: ,port
                           ))
     ;; This test should always fail, but should never be run
     (test-assert "Server returned unexpectedly" #f)
     )))

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
