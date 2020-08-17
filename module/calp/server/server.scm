(define-module (calp server server)
  :use-module (util)
  :use-module (web server)
  :use-module ((calp server routes) :select (make-make-routes))
  :use-module (ice-9 threads))

;; NOTE The default make-default-socket is broken for IPv6.
;; A patch has been submitted to the mailing list. 2020-03-31
(module-set!
 (resolve-module '(web server http))
 'make-default-socket
 (lambda (family addr port)
   (let ((sock (socket family SOCK_STREAM 0)))
     (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
     (bind sock family addr port)
     sock)))

(define handler (make-make-routes))

;; (define impl (lookup-server-impl 'http))
;; (define server (open-server impl open-params))


(define-public (start-server open-params)
  (run-server handler 'http open-params 1)
  ;; NOTE at first this seems to work, but it quickly deteriorates.
  ;; (for i in (iota 16)
  ;;      (begin-thread
  ;;       (let lp ((state (list 0)))
  ;;         (lp (serve-one-client handler impl server state)))))
  ;; (pause)
  )


