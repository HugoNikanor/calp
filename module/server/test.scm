(add-to-load-path "..")

(use-modules (util)
             (web server)

             (web response)
             (web request)
             (web uri)
             (ice-9 control)
             (ice-9 regex)
             (server util)
             (server macro)

             (ice-9 iconv)

             (sxml simple)
             (ice-9 ftw))

(define *name* "")

(define (form-page)
  `(div
    (p "hello" ,*name*)
    (form (@ (action "/form")
             (method POST))
          (input (@ (type text)
                    (name name)))
          (input (@ (type submit))))))

(define (sxml->xml-string sxml)
  (with-output-to-string
    (lambda () (sxml->xml sxml))))

(define routes
  (make-routes

   (GET "/" (name)
        (return
         '((content-type text/plain))
         (format #f "No root page, ~a~%" name)))

   (GET "/form" ()
        (return
         '((content-type text/html))
         (sxml->xml-string (form-page))))

   (POST "/form" ()
         (when (memv 'application/x-www-form-urlencoded (assoc-ref r:headers 'content-type))
           (apply (lambda* (#:key name #:allow-other-keys)
                    (format #t "*name* := [~a] Received [~a]~%" *name* name)
                    (set! *name* name))
                  (parse-query (uri-decode (bytevector->string body "UTF-8")))))
         (return (build-response
                  #:code 303
                  #:headers `((location . ,(string->uri-reference "/form"))))
                 ""))


   (GET "/ls" ()
        (return
         '((content-type text/html))
         (sxml->xml-string
          `(table
            (thead
             (th (td "Name") (td "Type") (td "Perm")))
            (tbody
             ,@(map (lambda (kv)
                      (let* (((k stat) kv))
                        `(tr (td ,k)
                             (td ,(stat:type stat))
                             (td ,(number->string (stat:perms stat) 8)))))
                    (cddr (file-system-tree "." (lambda (p _) (string=? p "."))))))))))


   (GET "/ls/:file" (file)
        (return '((content-type text/plain))
                (call-with-input-file (string-append "./" file)
                  (@ (ice-9 rdelim) read-string))))))

(run-server routes)
