;;; Commentary:
;;; Starts a repl server on some form of address.
;;; Code:

(define-module (calp repl)
  :use-module (system repl server)
  :use-module (ice-9 regex)
  :use-module ((calp util hooks) :select (shutdown-hook))
  :use-module (calp util exceptions)
  )

(define-public (repl-start address)
  (define lst (string->list address))
  (format (current-error-port)
          "Starting REPL server at ~a~%" address)
  (spawn-server
   (case (cond [(memv (car lst) '(#\. #\/)) 'UNIX]
               [(string-match "(\\d{1,3}\\.){3}\\d{1,3}(:\\d+)?" address) 'IPv4]
               ;; IPv6 is as of Gulie 2.2 not supported by make-tcp-server-socket.
               ;; This might be the same problem as I encountered in my html server.
               [else 'UNIX])
     [(UNIX)
      (add-hook! shutdown-hook (lambda () (catch 'system-error (lambda () (delete-file address))
                                       (lambda (err proc fmt . args)
                                         (warning "Failed to unlink ~a" address args)
                                         err))))
      (make-unix-domain-server-socket path: address)]
     [(IPv4) (apply (case-lambda
                      [() (error "Empty address?")]
                      [(address)      (make-tcp-server-socket host: address)]
                      [(address port) (make-tcp-server-socket host: address port: port)])
                    (string-split address #\:))]
     ;; currently impossible
     [(IPv6) (error "How did you get here?")]))

  ;; TODO setup repl environment here


  )
