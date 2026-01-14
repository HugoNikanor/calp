(define-module (calp entry-points server)
  :use-module (hnh util)
  :use-module (hnh util options)
  :use-module (calp util config)

  :use-module (srfi srfi-1)

  :use-module (ice-9 getopt-long)
  :use-module (ice-9 format)
  :use-module (calp translation)
  :use-module (sxml simple)

  :use-module ((calp server routes) :select (make-make-routes))
  :use-module (hnh util randport)
  :use-module ((web server) :select (run-server))

  :export (%summary main))

(define %summary
  (G_ "starts an HTTP server which dynamically loads and
displays events. The endpoints <i>/month/{date}.html</i> &amp; <i>/week/{date}.html</i> runs
the same output code as <b>html</b>. While the <i>/calendar/{uid}.ics</i> emits text/calendar."))

(define options
  `((port (value #t) (single-char #\p)
          (description ,(xml->sxml (G_ "<group>Bind to TCP port, defaults to <i>8080</i>.
<br/>Can also be set through the config variable
<i>port</i>.</group>"))))
    (addr (value #t)
          (description ,(xml->sxml (G_ "<group>Address to use, defaults to <i>0.0.0.0</i> for IPv4,
and <i>[::]</i> for IPv6</group>"))))
    ;; numbers as single-char doesn't work.
    (six (description ,(G_ "Use IPv6.")))
    (four (description ,(G_ "Use IPv4.")))
    (sigusr (description ,(G_ "Reload events on SIGUSR1")))
    (randport (description ,(G_ "Start server on a random port, using --port as minimum.")))
    (help (single-char #\h)
          (description ,(G_ "Print this help.")))))


(define-config port "8080"
  description: (G_ "Port to which the web server should bind.")
  ;; TODO converter?
  ;; TODO ensure string (or exact integer with  converter)
  )

(define (main args)

  (define opts (getopt-long args (getopt-opt options)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (when (option-ref opts 'sigusr #f)
    (format (current-error-port) (G_ "Listening for SIGUSR1~%"))
    ;; NOTE this uses the main thread, and does therefore block HTTP requests
    ;; while reloading. However, it appears to not cause any race conditions.

    ;; TODO re-implement dynamic reloading
    ;; (sigaction SIGUSR1
    ;;   (lambda _
    ;;     (format (current-error-port) (G_ "Received SIGUSR1, reloading calendars~%"))
    ;;     ((@ (vcomponent util instance) reload))))
    )


  (define addrinfos
    (getaddrinfo (option-ref opts 'addr "localhost")
                 (option-ref opts 'port (port))
                 (logior AI_PASSIVE AI_CANONNAME)
                 (cond ((option-ref opts 'six #f) AF_INET6)
                       ((option-ref opts 'four #f) AF_INET)
                       (else AF_UNSPEC))
                 SOCK_STREAM))

  (when (null? addrinfos)
    (format (current-error-port) (G_ "No available addresses for given configuration~%"))
    ;; TODO error code
    (throw 'return))

  ;; TODO bind on all found addresses instead of only first found
  (define addrinfo (car addrinfos))

  (define-values (addr sock)
    (catch 'system-error
      (lambda ()
        (if (option-ref opts 'randport #f)
            (randport2 addrinfo)
            (let ((sock (socket (addrinfo:fam addrinfo)
                                (addrinfo:socktype addrinfo)
                                0)))
              (bind sock (addrinfo:addr addrinfo))
              (values (addrinfo:addr addrinfo) sock))))

      ;; probably address already in use
      (lambda (err proc fmt args errno)
        (format (current-error-port) "~a: ~?, when binding ~s~%"
                proc fmt args
                addrinfo)
        ;; TODO error code
        (throw 'return))))

  ;; Arguments are
  ;; IP-address which we bind to
  ;; Port which we listen to
  ;; PID of this process
  ;; PWD of this process
  (format #t (G_ "Starting server on http://~a:~a~%I'm ~a, runing from ~a~%")
          (cond ((addrinfo:canonname addrinfo)
                 => (lambda (can)
                      ;; Literal IPv6 addresses may appear in the canonical name field...
                      (if (string-contains can ":")
                          (format #f "[~a]" can)
                          can)))
                ((eqv? AF_INET6 (sockaddr:fam addr))
                 (format #f "[~a]" (inet-ntop AF_INET6 addr)))
                (else (inet-ntop (sockaddr:fam addr) (sockaddr:addr addr))))
          (sockaddr:port addr)
          (getpid) (getcwd))

  (run-server (make-make-routes)
              'http
              (list socket: sock)))
