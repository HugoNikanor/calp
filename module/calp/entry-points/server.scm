(define-module (calp entry-points server)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (calp util config)

  :use-module (srfi srfi-1)

  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (calp translation)
  :use-module (sxml simple)

  :use-module ((calp server server) :select (start-server))

  :export (main))


(define options
  `((port (value #t) (single-char #\p)
          (description ,(xml->sxml (_ "<group>Bind to TCP port, defaults to <i>8080</i>.
<br/>Can also be set through the config variable
<i>port</i>.</group>"))))
    (addr (value #t)
          (description ,(xml->sxml (_ "<group>Address to use, defaults to <i>0.0.0.0</i> for IPv4,
and <i>[::]</i> for IPv6</group>"))))
    ;; numbers as single-char doesn't work.
    (six (description ,(_ "Use IPv6.")))
    (four (description ,(_ "Use IPv4.")))
    (sigusr (description ,(_ "Reload events on SIGUSR1")))
    (help (single-char #\h)
          (description ,(_ "Print this help.")))))


(define-config port 8080
  description: (_ "Port to which the web server should bind."))

(define (main args)

  (define opts (getopt-long args (getopt-opt options)))
  (define addr (option-ref opts 'addr #f))
  (define port% (cond ((option-ref opts 'port #f) => string->number)
                     (else (port))))
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
  (unless addr
    (set! addr (if (eqv? family AF_INET6)
                   "::" "0.0.0.0")))

  (when (option-ref opts 'sigusr #f)
    (format (current-error-port) (_ "Listening for SIGUSR1~%"))
    ;; NOTE this uses the main thread, and does therefore block HTTP requests
    ;; while reloading. However, it appears to not cause any race conditions.
    (sigaction SIGUSR1
      (lambda _
        (format (current-error-port) (_ "Received SIGUSR1, reloading calendars~%"))
        ((@ (vcomponent util instance) reload)))))



  ;; Arguments are
  ;; IP-address which we bind to
  ;; Port which we listen to
  ;; PID of this process
  ;; PWD of this process
  (format #t (_ "Starting server on ~a:~a~%I'm ~a, runing from ~a~%")
          addr port%
          (getpid) (getcwd))

  (catch 'system-error
    (lambda ()
      (start-server (list family: family port: port% host: addr)))

    ;; probably address already in use
    (lambda (err proc fmt args errno)
      (format (current-error-port) "~a: ~?~%"
              proc fmt args))))
