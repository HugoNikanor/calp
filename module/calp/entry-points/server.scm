(define-module (calp entry-points server)
  :use-module (calp util)
  :use-module (calp util options)
  :use-module (calp util config)

  :use-module (srfi srfi-1)

  :use-module (ice-9 getopt-long)

  :use-module ((calp server server) :select (start-server))

  :export (main))


(define options
  '((port (value #t) (single-char #\p)
          (description "Bind to TCP port, defaults to " (i 8080) "."
                       (br) "Can also be set through the config variable "
                       (i "port") "."))
    (addr (value #t)

          (description "Address to use, defaults to " (i "0.0.0.0")
                       " for IPv4, and " (i "[::]") " for IPv6.")
          )
    ;; numbers as single-char doesn't work.
    (six (description "Use IPv6."))
    (four (description "Use IPv4."))
    (sigusr (description "Reload events on SIGUSR1"))
    (help (single-char #\h)
          (description "Print this help."))))


(define-config port 8080
  description: "Port to which the web server should bind.")

(define-public (main args)

  (define opts (getopt-long args (getopt-opt options)))
  (define addr (option-ref opts 'addr #f))
  (define port (or (and=> (option-ref opts 'port #f)
                          string->number)
                   (get-config 'port)))
  (define family
    (cond [(option-ref opts 'six  #f) AF_INET6]
          [(option-ref opts 'four #f) AF_INET]
          [(and addr (string-contains addr ":")) AF_INET6]
          [(and addr (string-contains addr ".")) AF_INET]
          [else AF_INET6]))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  ;; update address if it was left blank. A bit clumsy since
  ;; @var{addr} & @var{family} depend on each other.
  ;; placed after load-calendars to keep Guile 2.2 compability.
  (set! addr
    (if addr addr
        (if (eqv? family AF_INET6)
            "::" "0.0.0.0")))

  (when (option-ref opts 'sigusr #f)
    (display "Listening for SIGUSR1\n" (current-error-port))
    ;; NOTE this uses the main thread, and does therefore block HTTP requests
    ;; while reloading. However, it appears to not cause any race conditions.
    (sigaction SIGUSR1
      (lambda _
        (display "Received SIGUSR1, reloading calendars\n"
                 (current-error-port))
        ((@ (vcomponent util instance) reload)))))



  (format #t "Starting server on ~a:~a~%I'm ~a, runing from ~a~%"
          addr port
          (getpid) (getcwd))

  (catch 'system-error
    (lambda ()
      (start-server (list family: family port: port host: addr)))

    ;; probably address already in use
    (lambda (err proc fmt args errno)
      (format (current-error-port) "~a: ~?~%"
              proc fmt args))))
