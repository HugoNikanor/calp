(define-module (test webdav-server)
  :use-module (srfi srfi-1)
  ;; :use-module (ice-9 threads)

  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp server webdav)
  :use-module (calp webdav resource)
  :use-module (calp webdav href)
  :use-module ((calp webdav property) :select (propstat))
  :use-module (calp webdav resource virtual)
  :use-module (calp namespaces)
  :use-module (oop goops)
  :use-module (web request)
  :use-module (web response)
  :use-module (web uri)
  :use-module (sxml simple)
  ;; :use-module (sxml xpath)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module ((scheme base) :select (string->utf8 utf8->string))
  :use-module (datetime)
  )

;;; Commentary:
;;; Tests that handlers for all HTTP Methods works correctly.
;;; Note that these tests don't have as goal to check that resources and
;;; properties work correctly. See (test webdav) and (test webdav-tree) for that.
;;;
;;; The namespaces http://ns.example.com/properties is intentionally given
;;; different prefixes everywhere, to ensure that namespaces are handled correctly.
;;; Code:

(define prop-ns (string->symbol "http://ns.example.com/properties"))

(define root-resource (make <virtual-resource> collection?: #t))
(create-resource! root-resource "a" '() (string->utf8 "Contents of A"))
(create-resource! root-resource "b" '() (string->utf8 "Contents of B"))

;; (define (xml->sxml* port)
;;   (xml->sxml port namespaces: `((d . ,(symbol->string webdav))
;;                                 (y . ,(symbol->string prop-ns)))))

(define-syntax-rule (run-op op)
  (catch 'http
    (lambda ()
      (call-with-values (lambda () op)
        (case-lambda
          ((response body) (values response body))
          ((response)      (values response "")))))
    (lambda* (_ error-code optional: (body "") content-type)
      (values (build-response code: error-code
                              headers: (when content-type
                                         `((content-type . content-type))))
              body))))

(define (un-namespace xml)
  (typecheck xml xml-element?)
  (namespaced-sxml->sxml xml `((,webdav . d)
                               (,prop-ns . y))))



(test-group "run-propfind"
  (test-group "Working, depth 0"
    (let* ((request (build-request
                     (string->uri "http://localhost/")
                     method: 'PROPFIND
                     headers: '((depth . 0))
                     validate-headers?: #f))
           (head body (run-op (run-propfind root-resource '() request #f))))
      (test-equal 207 (response-code head))
      (test-equal '(application/xml)
        (response-content-type head))
      (test-assert (xml-element? body))
      (let (#; (body* (with-output-to-string (lambda () (xml->sxml* body))))
            )
        (test-equal "Resource gets returned as expected"
          ((xml webdav 'multistatus)
           ((xml webdav 'response)
            ((xml webdav 'href) "/")
            ((xml webdav 'propstat)
             ((xml webdav 'prop)
              ((xml webdav 'supportedlock))
              ((xml webdav 'resourcetype) ((xml webdav 'collection)))
              ((xml webdav 'getcontentlength) "0")
              ((xml webdav 'creationdate) (datetime->string (current-datetime)
                                                            "~Y-~m-~dT~H:~M:~SZ")))
             ((xml webdav 'status) "HTTP/1.1 200 OK"))))
          body))))

  (test-group "Depth: infinity"
    (let* ((request (build-request
                     (string->uri "http://localhost/")
                     method: 'PROPFIND
                     headers: '((depth . infinity))
                     ; validate-headers?: #f
                     ))
           (head body (run-op (run-propfind root-resource '() request #f))))
      (test-equal 207 (response-code head))
      (test-equal '(application/xml) (response-content-type head))
      (test-assert (xml-element? body))
      (let (#;(body* (with-output-to-string (lambda () (xml->sxml* body))))
            )
        (test-equal
            (list ((xml webdav 'href) "/")
                  ((xml webdav 'href) "/a")
                  ((xml webdav 'href) "/b"))
          (map
           (lambda (child)
             (find-child ((xml webdav 'href)) (xml-element-children child)))
           (filter (lambda (child) (tag-matches? child 'response webdav))
                   (xml-element-children body)))))))

  (test-group "With body"
    (let ((request (build-request (string->uri "http://localhost/")
                                  method: 'PROPFIND
                                  headers: '((depth . 0))
                                  validate-headers?: #f))
          (request-body "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propfind xmlns=\"DAV:\">
  <prop><resourcetype/></prop>
</propfind>"))
      (let ((head body (run-op (run-propfind root-resource '() request request-body))))
        (test-equal 207 (response-code head))
        (test-equal '(application/xml) (response-content-type head))
        (test-assert (xml-element? body))
        (let (#;(body* (with-output-to-string (lambda () (xml->sxml* body))))
              )
          (test-equal "We only get what we ask for"
            ((xml webdav 'multistatus)
             ((xml webdav 'response)
              ((xml webdav 'href) "/")
              ((xml webdav 'propstat)
               ((xml webdav 'prop)
                ((xml webdav 'resourcetype) ((xml webdav 'collection))))
               ((xml webdav 'status) "HTTP/1.1 200 OK"))))
            body))


          ;; (test-equal "We only get what we ask for"
          ;;   '((d:prop (d:resourcetype (d:collection))))
          ;;   ;; TODO better query language
          ;;   (filter (lambda (x) (and (tag-matches? x 'response webdav)
          ;;                       (and=> (find-child ((xml webdav 'propstat))
          ;;                                          (xml-element-children x))
          ;;                              (lambda (propstat)
          ;;                                (and=> (find-child ((xml webdav 'href))
          ;;                                                   (xml-element-children propstat))
          ;;                                       (lambda (href) (equal? '("HTTP/1.1 200 OK")
          ;;                                                         (xml-element-children href))))))))
          ;;           (xml-element-children body))


          ;;   ((sxpath '(// d:response    ; ;
          ;;   (d:propstat (// d:status (equal? "HTTP/1.1 200 OK"))) ; ;
          ;;   // d:prop))                 ; ;
          ;;   body*))))))
          ))))



(test-group "run-proppatch"
  (let ((request (build-request (string->uri "http://localhost/a")
                                method: 'PROPPATCH))
        (request-body (format #f "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propertyupdate xmlns=\"DAV:\" xmlns:x=\"~a\">
  <set>
    <prop>
      <displayname>New Displayname</displayname>
      <x:test><x:content/></x:test>
    </prop>
  </set>
  <!-- TODO test remove? -->
</propertyupdate>" prop-ns)))
    (let ((response body (run-op (run-proppatch root-resource '("a") request request-body))))
      (test-equal 207 (response-code response))
      (test-equal '(application/xml) (response-content-type response))
      (test-assert (xml-element? body))
      (with-output-to-string (lambda () (namespaced-sxml->xml body)))
      ))

  (let ((response body (run-op (run-propfind
                                root-resource
                                '("a")
                                (build-request (string->uri "http://localhost/a")
                                               method: 'PROPFIND
                                               headers: '((depth . 0))
                                               validate-headers?: #f)
                                (format #f "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propfind xmlns=\"DAV:\" xmlns:z=\"~a\">
  <prop>
    <displayname/>
    <z:test/>
  </prop>
</propfind>" prop-ns)))))
    (test-equal "Excpected code" 207 (response-code response))
    (test-equal "Expected content type" '(application/xml) (response-content-type response))
    (test-assert "XML response body" (xml-element? body))

    ;; (format (current-error-port) "Here~%")
    ;; ;; The crash is after here
    ;; (body (current-error-port))

    ;; TODO better query language
    ;; TODO re-write and re-enable these tests
    #;
    (let* (#; (body* (with-output-to-string (lambda () (xml->sxml* body))))
    (properties ((sxpath '(// d:response
    (d:propstat (// d:status (equal? "HTTP/1.1 200 OK")))))
    body*)))
    ;; ((@ (ice-9 format) format) (current-error-port) "Properties: ~y~%" properties)
    (test-equal "Native active property is properly updated"
    '("New Displayname")
    ((sxpath '(// d:displayname *text*)) properties))
    (test-equal "Custom property is correctly stored and preserved"
    '((y:test (y:content)))
    ((sxpath '(// y:test)) properties))))

  ;; TODO test proppatch atomicity
  )



(test-group "run-options"
  (let ((head body (run-op (run-options root-resource '() #f))))
    (test-equal "found options head"
      (build-response
       code: 200
       headers: `((dav . (1 3))
                  (allow . (GET HEAD PUT MKCOL PROPFIND OPTIONS DELETE COPY MOVE))))
      head)
    (test-equal "found options body"
      "" body))

  (let ((head _ (run-op (run-options root-resource '("nonexistant") #f))))
    (test-equal "missing options"
      (build-response code: 404)
      head)))



(test-group "run-get"
  (let ((head body (run-op (run-get root-resource '("a")
                                    (build-request
                                     (string->uri "http://localhost/a")
                                     method: 'GET)))))
    (test-equal "Contents of A" (utf8->string body))))



(test-group "run-put"
  (test-group "Update existing resource"
    (run-op
     (run-put root-resource '("a")
              (build-request (string->uri "http://localhost/a")
                             method: 'PUT
                             port: (open-output-string))
              (string->utf8 "New Contents of A")))

    (let ((head body (run-op (run-get root-resource '("a")
                                      (build-request
                                       (string->uri "http://localhost/a")
                                       method: 'GET)))))
      (test-equal "Put updates subsequent gets"
        "New Contents of A" (utf8->string body))))

  (test-group "Create new resource"
    (run-op (run-put root-resource '("c")
                     (build-request (string->uri "http://localhost/c")
                                    method: 'PUT
                                    port: (open-output-string))
                     (string->utf8 "Created Resource C")))
    (let ((head body (run-op (run-get root-resource '("c")
                                      (build-request
                                       (string->uri "http://localhost/c")
                                       method: 'GET)))))
      (test-equal "Put creates new resources"
        "Created Resource C" (utf8->string body)))))



;;; Run DELETE
(test-group "run-delete"
  'TODO)




(test-group "run-mkcol"
  (run-op (run-mkcol root-resource '("a" "b")
                     (build-request (string->uri "http://localhost/a/b")
                                    method: 'MKCOL)
                     #f))
  (let* ((request (build-request
                   (string->uri "http://localhost/")
                   method: 'PROPFIND
                   headers: '((depth . infinity))
                   validate-headers?: #f))
         (head body (run-op (run-propfind root-resource '() request #f))))
    (test-equal 207 (response-code head))
    (test-equal '(application/xml) (response-content-type head))
    (test-assert (xml-element? body))
    (let ((body* (un-namespace body)))
      ;; TODO re-enable this test
      'TODO
      #;
      (test-equal "Check that all created resources now exists"
        '("/" "/a" "/a/b" "/b" "/c")
        (sort* ((sxpath '(// d:href *text*)) body*)
               string<)))))


;;; TODO test MKCOL indempotence



;;; Run COPY
(test-group "run-copy"
  (let* ((root-resource (make <virtual-resource> collection?: #t))
         (a (create-resource! root-resource "a" '() (string->utf8 "Content of A"))))
    (set-property! a ((xml prop-ns 'test) "prop-value"))
    ;; Extra child added to ensure deep copy works
    (create-resource! a "d" '()  (string->utf8 "Content of d"))

    (test-group "cp /a /c"
      (let ((response _ (run-op (run-copy root-resource '("a")
                                          (build-request
                                           (string->uri "http://example.com/a")
                                           headers: `((destination
                                                       . ,(string->uri "http://example.com/c"))))))))
        ;; Created
        (test-eqv "Resource was reported created"
          201 (response-code response)))

      (let ((c (lookup-resource root-resource '("c"))))
        (test-assert "New resource present in tree" c)
        (test-equal "Content was correctly copied"
          "Content of A" (utf8->string (content c '())))
        (test-equal "Property was correctly copied"
          (propstat 200
                    (list ((xml prop-ns 'test)
                            "prop-value")))
          (get-property c ((xml prop-ns 'test))))
        (test-assert "Copy remainied a collection?"
          (collection? c)))

      (let ((d (lookup-resource root-resource '("c" "d"))))
        (test-assert "Deep copy worked" d)
        (test-equal "Deep copy content transfered"
          "Content of d" (utf8->string (content d '())))
        (test-assert "Deep copy stayed a non-collection?"
          (not (collection? d))))
      )

    (test-group "cp --no-clobber /c /a"
      (let ((response _ (run-op (run-copy root-resource '("c")
                                          (build-request
                                           (string->uri "http://example.com/c")
                                           headers: `((destination
                                                       . ,(string->uri "http://example.com/a"))
                                                      (overwrite . #f)))))))
        (test-eqv "Resource collision was reported"
          412 (response-code response))))

    ;; Copy recursive collection, and onto child of self.
    #;
    (test-group "cp -r / /c"
      (let ((response
             (run-copy root-resource '()
                       (build-request
                        (string->uri "http://example.com/")
                        headers: `((destination . ,(string->uri "http://example.com/c")))))))
        (test-eqv "Check that reported replaced"
          204 (response-code response))
        (test-equal "Check that recursive resources where created"
          '("/" "/a" "/a/d" "/c"
            ;; New resources. Note that /c/c doesn't create an infinite loop
            "/c/a" "/c/a/d" "/c/c")
          (map car
           (sort* (map (lambda (p) (cons (href->string (car p)) (cdr p)))
                       (all-resources-under root-resource '()))
                  string< car)))

        ;; TODO we should also check that /c is a copy of the root resource,
        ;; instead of the old /c resource.
        ;; Do this by setting some properties
        ))))



;;; Run MOVE
(test-group "run-move"
  (let ((root-resource (make <virtual-resource> collection?: #t)))
    (create-resource! root-resource "a" '()  (string->utf8 "Content of A"))
    (let ((a (lookup-resource root-resource '("a"))))
      (set-property! a ((xml prop-ns 'test) "prop-value")))

    (test-group "mv /a /c"
      (let ((response body (run-op (run-move root-resource '("a")
                                          (build-request
                                           (string->uri "http://example.com/a")
                                           headers: `((destination
                                                       . ,(string->uri "http://example.com/c"))))))))
        ;; Created
        (test-eqv "Resource was reported created"
          201 (response-code response))
        (test-equal "No error message was sent"
          "" body))
      ;; TODO check that old resource is gone
      )))



;;; Run REPORT

'((calp server webdav))
