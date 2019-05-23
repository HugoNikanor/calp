(define-module (server)
  :use-module (util))

(use-modules* (web (server request response uri))
              (output (html))
              (server (util macro))
              (sxml (simple))
              (ice-9 (match control rdelim curried-definitions ftw
                            getopt-long
                            iconv regex #| regex here due to bad macros |#  ))
              (srfi (srfi-1 srfi-19 srfi-88)))

(use-modules (srfi srfi-19 util))

(define (file-extension name)
  (car (last-pair (string-split name #\.))))

(define (sxml->xml-string sxml)
  (with-output-to-string
    (lambda () (sxml->xml sxml))))

(define (directory-table dir)
  `(table
    (thead
     (tr (th "Name") (th "Type") (th "Perm")))
    (tbody
     ,@(map (lambda (kv)
              (let* (((k stat) kv))
                `(tr (td (a (@ (href ,dir ,k)) ,k))
                     (td ,(stat:type stat))
                     (td ,(number->string (stat:perms stat) 8)))))
            (cddr (file-system-tree dir))))))


(define (make-make-routes calendar events)
  (make-routes

   (GET "/" (y m) ; m in [1, 12]
        (let* ((cd (current-date))
               (start (if m
                          (date year: 2019 day: 1 month: (string->number m))
                          (current-date)))
               (end (set (date-month start) = (+ 1))))

          (return '((content-type text/html))
                  (with-output-to-string
                    (lambda () (html-generate calendar events start end))))))

   (GET "/static" ()
        (return
         '((content-type text/html))
         (sxml->xml-string
          (directory-table "static/"))))

   (GET "/static/:filename" (filename)
        (return
         `((content-type ,(case (string->symbol (file-extension filename))
                            ((js) 'text/javascript)
                            ((css) 'text/css))))
         (call-with-input-file (string-append "static/" filename) read-string)))

   (GET "/count" ()
        ;; (sleep 1)
        (return '((content-type text/plain))
                (string-append (number->string state) "\n")
                (1+ state)))

   ))

(define options
  '((port (value #t) (single-char #\p))
    (addr (value #t))))

(define-public (server-main c e args)

  (define opts (getopt-long args options))
  (define port (option-ref opts 'port 8080))
  (define addr (inet-aton (option-ref opts 'addr "127.0.0.1")))

  (format #t "Starting server on ~a:~a~%I'm ~a, runing from ~a~%"
          (inet-ntoa addr) port
          (getpid) (getcwd))

  (run-server (make-make-routes c e)
              'http
              `(port: ,port
                addr: ,addr)
              0))
