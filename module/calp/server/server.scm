(define-module (calp server server)
  :use-module (hnh util)
  :use-module (web server)
  :use-module ((calp server routes) :select (make-make-routes))
  :use-module (ice-9 threads)
  :use-module (srfi srfi-88)

  :export (start-server))

(define handler (make-make-routes))

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

(define (start-server open-params)
  (run-server handler
              'http
              (append open-params
                      `(socket: ,(apply setup-socket open-params)))
              1)
  ;; NOTE at first this seems to work, but it quickly deteriorates.
  ;; (for i in (iota 16)
  ;;      (begin-thread
  ;;       (let lp ((state (list 0)))
  ;;         (lp (serve-one-client handler impl server state)))))
  ;; (pause)
  )


