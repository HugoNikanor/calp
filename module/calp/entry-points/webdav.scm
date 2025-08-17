(define-module (calp entry-points webdav)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((calp server webdav) :select (webdav-handler))
  :use-module (web server)
  :use-module (hnh util options)
  :use-module (calp translation)
  :use-module (sxml simple)
  :use-module (ice-9 getopt-long)
  :use-module (hnh util options)
  :use-module ((calp load-config) :select (load-config))
  :use-module (calp util config)
  :use-module ((hnh util randport) :select (randport))
  :use-module (oop goops)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  :use-module ((calp webdav builder) :select (build-webdav-resource-tree))
  :export (main))



;; Resource tree declaration, MUST be set in the user supplied configuration file
(define-config webdav-resources #f
  ;; TODO better type checking
  ;; pre: (ensure list?)
  )


(define options
  `((port (value #t) (single-char #\p)
          (description ,(G_ "TCP port to bind to.
If --randport is also given, this is used as the minimum port to attempt to bind to.
")))
    (randport (value port-file)
              (description ,(G_ "Start the server at a random port.
The port number will be written to the file given as an argument.")))
    (config (value file)
            (description ,(G_ "Configuration file describing resource tree.")))
    (help (single-char #\h)
          (description ,(G_ "Print this help.")))))


(define (main args)
  (define opts (getopt-long args (getopt-opt options)))
  (define port (cond ((option-ref opts 'port #f) => string->number)
                     (else 8080)))

  (when (option-ref opts 'help #f)
    (print-arg-help options)
    (throw 'return))

  (define config-file (option-ref opts 'config #f))
  (unless config-file
    ;; TODO output that `--config <file>` is required
    (throw 'return))

  ;; TODO fail if no configuration file is given
  (load-config config-file)

  (unless (webdav-resources)
    ;; TODO output that (@ (calp entry-points webdav) webdav-resources) must be set by the config file
    (throw 'return))

  (define root-resource (build-webdav-resource-tree (webdav-resources)))

  (run-server
   (webdav-handler root-resource) 'http
   (cond ((option-ref opts 'randport #f)
          => (lambda (randport-target)
               (let ((port-number socket (randport "127.0.0.1" start: port)))
                 (with-output-to-file randport-target
                   (lambda () (format #t "~a~%" port-number)))
                 `(socket: ,socket))))
         (else  `(port: ,port)))))
