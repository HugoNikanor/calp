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
             (srfi srfi-88)

             (sxml simple)
             (ice-9 ftw)
             (ice-9 rdelim)
             )

(define (form-page name)
  `(div
    (p "Hello " ,name)
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
         (sxml->xml-string (form-page state))))

   (POST "/form" (name)
         (return (build-response
                  #:code 303
                  #:headers `((location . ,(string->uri-reference "/form"))))
                 "" name))


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
                (call-with-input-file file read-string)))))

(run-server routes 'http '() "Default Name")

