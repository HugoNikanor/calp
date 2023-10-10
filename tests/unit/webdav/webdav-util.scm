(define-module (test webdav-test)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (web uri)
  :use-module (calp webdav util)
   )

(test-equal "Parse-dav-line"
  `(1 2 access-control ,(string->uri "http://example.com/uri"))
  (parse-dav-line "1, 2, access-control, <http://example.com/uri>"))

(test-equal "write-dav-line"
  "1, 2, access-control, <http://example.com/uri>"
  (call-with-output-string
    (lambda (port)
      (write-dav-line
       `(1 2 access-control ,(string->uri "http://example.com/uri"))
       port))))

'((calp webdav util))
