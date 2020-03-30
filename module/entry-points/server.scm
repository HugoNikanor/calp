(define-module (entry-points server)
  :export (main)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (parameters)
  )

(use-modules* (web (server request response uri))
              (output (html))
              (server (util macro))
              (sxml (simple))
              (ice-9 (match control rdelim curried-definitions ftw
                            getopt-long
                            iconv regex #| regex here due to bad macros |#  ))
              (srfi (srfi-1 srfi-88)))

(use-modules (datetime)
             (datetime util))

(define (file-extension name)
  (car (last-pair (string-split name #\.))))

(define (sxml->html-string sxml)
  (with-output-to-string
    (lambda () (display "<!doctype html>\n") (sxml->xml sxml))))

(define (directory-table dir)
  `(table
    (thead
     (tr (th "") (th "Name") (th "Perm")))
    (tbody
     ,@(map (lambda (kv)
              (let* (((k stat) kv))
                `(tr (td ,(case (stat:type stat)
                            [(directory) "📁"]
                            [(regular) "📰"]
                            [else "🙃"]))
                     (td (a (@ (href "/" ,dir ,k)) ,k))
                     (td ,(number->string (stat:perms stat) 8)))))
            (cddr (file-system-tree dir))))))


(define (make-make-routes calendar events)
  (make-routes

   (GET "/week/:start-date.html" (start-date)
        (let* ((start-date (parse-iso-date start-date)))

          (return '((content-type text/html))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: calendar
                                     events: events
                                     start-date: start-date
                                     end-date: (date+ start-date (date day: 6))
                                     next-start: (lambda (d) (date+ d (date day: 7)))
                                     prev-start: (lambda (d) (date- d (date day: 7)))
                                     render-calendar: render-calendar
                                     ))))))

   (GET "/month/:start-date.html" (start-date)
        (let* ((start-date (parse-iso-date start-date)))

          (return '((content-type text/html))
                  (with-output-to-string
                    (lambda ()
                      (html-generate calendars: calendar
                                     events: events
                                     start-date: start-date
                                     end-date: (date- (month+ start-date)
                                                      (date day: 1))
                                     next-start: month+
                                     prev-start: month-
                                     render-calendar: render-calendar-table
                                     pre-start: (start-of-week start-date)
                                     post-end: (end-of-week start-date)
                                     ))))))

   (GET "/static" ()
        (return
         '((content-type text/html))
         (sxml->html-string
          (directory-table "static/"))))

   (GET "/static/:filename.css" (filename)
        (return
         `((content-type text/css))
         (call-with-input-file (string-append "static/" filename ".css")
           read-string)))

   (GET "/static/:filename.js" (filename)
        (return
         `((content-type text/javascript))
         (call-with-input-file (string-append "static/" filename ".js")
           read-string)))

   (GET "/count" ()
        ;; (sleep 1)
        (return '((content-type text/plain))
                (string-append (number->string state) "\n")
                (1+ state)))

   ))

(define options
  '((port (value #t) (single-char #\p))
    (addr (value #t))
    (family (value #t)
            (predicate ,(lambda (v) (memv (string->symbol (string-upcase v))
                                  '(INET INET4 INET6)))))))

(define-public (main args)

  (define opts (getopt-long args options))
  (define port (option-ref opts 'port 8080))
  (define family (case (string->symbol (string-upcase (option-ref opts 'family "INET6")))
                   [(INET INET4) AF_INET]
                   [(INET6) AF_INET6]
                   [else (error "That address family is not supported")]) )
  ;; TODO the guile methods wants the ip address in numeric form. This is currently extra impossible
  (define addr (option-ref opts 'addr 0))

  (define-values (c e)
    (load-calendars
     calendar-files: (cond [(option-ref opts 'file #f) => list]
                           [else (calendar-files)]) ))

  (format #t "Starting server on ~a:~a~%I'm ~a, runing from ~a~%"
          (inet-ntop family addr) port
          (getpid) (getcwd))

  (run-server (make-make-routes c e)
              'http
              `(port: ,port
                addr: ,addr)
              0))
