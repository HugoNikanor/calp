(define-module (entry-points server)
  :use-module (util)
  :use-module (util options)
  :use-module (util exceptions)

  :use-module (srfi srfi-1)

  :use-module (ice-9 getopt-long)
  :use-module (ice-9 regex) #| regex here due to bad macros |#

  :use-module ((web server) :select (run-server))

  ;; :use-module (vcomponent)
  ;; :use-module (vcomponent search)
  ;; :use-module (datetime)
  ;; :use-module (output html)
  ;; :use-module (output ical)

  :use-module ((server routes) :select (make-make-routes))

  :export (main)
  )






(define options
  '((port (value #t) (single-char #\p)
          (description "Bind to TCP port, defaults to " (i 8080) "."))
    (addr (value #t)
          (description "Address to use, defaults to " (i "0.0.0.0")
                       " for IPv4, and " (i "::") " for IPv6.")
          )
    ;; numbers as single-char doesn't work.
    (six (description "Use IPv6."))
    (four (description "Use IPv4."))
    (help (single-char #\h)
          (description "Print this help."))))


(define-public (main args)

  (define opts (getopt-long args (getopt-opt options)))
  (define port (string->number (option-ref opts 'port "8080")))
  (define addr (option-ref opts 'addr #f))
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

  (format #t "Starting server on ~a:~a~%I'm ~a, runing from ~a~%"
          addr port
          (getpid) (getcwd))

  (catch 'system-error
    (lambda ()
     (run-server (make-make-routes)
                 'http
                 `(family: ,family
                           port: ,port
                           host: ,addr)
                 0))
    ;; probably address already in use
    (lambda (err proc fmt args errno)
      (format (current-error-port) "~a: ~?~%"
              proc fmt args))))
