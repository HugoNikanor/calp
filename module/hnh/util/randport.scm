(define-module (hnh util randport)
  :use-module (srfi srfi-88)
  :export (randport))

;; Setup and bind a network socket on an arbitrary port on the host system
;; Returns 2 values:
;; - the port number bound to
;; - a socket ready for listening on that port
(define* (randport base-address key: (start 8080))
  (define sock (socket PF_INET SOCK_STREAM 0))

  ;; Allows quicker re-use of the port?
  (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)

  (define addr (cond ((string? base-address) (inet-pton AF_INET base-address))
                     ((number? base-address) base-address)
                     (else (scm-error 'misc-error "randport"
                                      "base-adress doesn't look like an IP address: ~s"
                                      (list base-address) '()))))

  (let loop ((port start))
    (catch 'system-error
      (lambda ()
        (bind sock (make-socket-address AF_INET addr port))
        (values port sock))
      (lambda (err proc fmt args data)
        (if (and (not (null? data))
                 (= EADDRINUSE (car data)))
            (loop (1+ port))
            ;; rethrow
            (throw err fmt args data))))))
