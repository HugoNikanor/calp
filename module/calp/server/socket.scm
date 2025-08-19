(define-module (calp server socket)
  :use-module (srfi srfi-88)
  :use-module (web server)
  :export (setup-socket)
  )

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


