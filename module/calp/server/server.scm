(define-module (calp server server)
  :use-module (hnh util)
  :use-module (web server)
  :use-module ((calp server routes) :select (make-make-routes))
  :use-module (ice-9 threads)
  :use-module (srfi srfi-88)
  :use-module (calp server socket)

  :export (start-server))

;;; TODO Do I really want this hardcoded here?
(define handler (make-make-routes))


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


