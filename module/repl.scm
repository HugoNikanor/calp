(define-module (repl)
  :use-module (system repl server)
  :use-module (ice-9 regex))

(define-public (runtime-dir)
  (or (getenv "XDG_RUNTIME_DIR")
      "/tmp"))

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
     ;; TODO created unix sockets are newer cleaned up
     [(UNIX)
      (make-unix-domain-server-socket path: address)]
     [(IPv4) (apply (case-lambda
                      [() (error "Empty address?")]
                      [(address)      (make-tcp-server-socket host: address)]
                      [(address port) (make-tcp-server-socket host: address port: port)])
                    (string-split address #\:))]
     ;; currently impossible
     [(IPv6) (error "How did you get here?")])))
