(define-module (test webdav-server)
  ;; :use-module (srfi srfi-1)
  ;; :use-module (ice-9 threads)

  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp server webdav)
  :use-module (calp webdav resource)
  :use-module ((calp webdav property) :select (propstat))
  :use-module (calp webdav resource virtual)
  :use-module (calp namespaces)
  :use-module (oop goops)
  :use-module (web request)
  :use-module (web response)
  :use-module (web uri)
  :use-module (sxml simple)
  :use-module (sxml xpath)
  :use-module (sxml namespaced)
  :use-module (hnh util)
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

(define root-resource (make <virtual-resource> name: "*root*"))
(add-resource! root-resource "a" "Contents of A")
(add-resource! root-resource "b" "Contents of B")

;;; Connect output of one procedure to input of another
;;; Both producer and consumer should take exactly one port as argument
(define (connect producer consumer)
  ;; (let ((in out (car+cdr (pipe))))
  ;;   (let ((thread (begin-thread (consumer in))))
  ;;     (producer out)
  ;;     (join-thread thread)))

  (call-with-input-string
      (call-with-output-string producer)
    consumer))

(define (xml->sxml* port)
  (xml->sxml port namespaces: `((d . ,(symbol->string webdav))
                                (y . ,(symbol->string prop-ns)))))



(test-group "run-propfind"
  (test-group "Working, depth 0"
   (let* ((request (build-request
                    (string->uri "http://localhost/")
                    method: 'PROPFIND
                    headers: '((depth . 0))
                    validate-headers?: #f))
          (head body (run-propfind root-resource '() request #f)))
     (test-equal 207 (response-code head))
     (test-equal '(application/xml)
       (response-content-type head))
     (test-assert (procedure? body))
     (let ((body* (connect body xml->sxml*)))
       ;; Arbitrarily chosen resource
       (test-equal "Resource gets returned as expected"
           '((d:resourcetype (d:collection)))
         ((sxpath '(// d:response
                       (d:propstat (// d:status (equal? "HTTP/1.1 200 OK")))
                       // d:resourcetype))
          body*)))))

  (test-group "Depth: infinity"
    (let* ((request (build-request
                     (string->uri "http://localhost/")
                     method: 'PROPFIND
                     headers: '((depth . infinity))
                     validate-headers?: #f))
           (head body (run-propfind root-resource '() request #f)))
      (test-equal 207 (response-code head))
      (test-equal '(application/xml) (response-content-type head))
      (test-assert (procedure? body))
      (let ((body* (connect body xml->sxml*)))
        (test-equal
            '("/" "/a" "/b")
          (sort* ((sxpath '(// d:href *text*)) body*)
                 string<)))))

  (test-group "With body"
    (let ((request (build-request (string->uri "http://localhost/")
                                  method: 'PROPFIND
                                  headers: '((depth . 0))
                                  validate-headers?: #f))
          (request-body "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propfind xmlns=\"DAV:\">
  <prop><resourcetype/></prop>
</propfind>"))
      (let ((head body (run-propfind root-resource '() request request-body)))
        (test-equal 207 (response-code head))
        (test-equal '(application/xml) (response-content-type head))
        (test-assert (procedure? body))
        (let ((body* (connect body xml->sxml*)))
          (test-equal "We only get what we ask for"
            '((d:prop (d:resourcetype (d:collection))))
            ((sxpath '(// d:response
                          (d:propstat (// d:status (equal? "HTTP/1.1 200 OK")))
                          // d:prop))
             body*)))))))



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
    (let ((response body (run-proppatch root-resource '("a") request request-body)))
      (test-equal 207 (response-code response))
      (test-equal '(application/xml) (response-content-type response))
      (test-assert (procedure? body))
      ;; Commit the changes
      (call-with-output-string body)
      ))

  (let ((response body (run-propfind
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
</propfind>" prop-ns))))
    (test-equal 207 (response-code response))
    (test-equal '(application/xml) (response-content-type response))
    (test-assert (procedure? body))

    ;; (format (current-error-port) "Here~%")
    ;; ;; The crash is after here
    ;; (body (current-error-port))

    (let* ((body* (connect body xml->sxml*))
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
  (let ((head body (run-options root-resource #f #f)))
    (test-equal "options head"
      (build-response
       code: 200
       headers: `((dav . (1))
                  (allow . (GET HEAD PUT MKCOL PROPFIND OPTIONS DELETE COPY MOVE))))
      head)
    (test-equal "options body"
      "" body)))



(test-group "run-get"
  (let ((head body (run-get root-resource '("a")
                            (build-request
                             (string->uri "http://localhost/a")
                             method: 'GET)
                            'GET)))
    (test-equal "Contents of A" body)))



(test-group "run-put"
  (test-group "Update existing resource"
    (run-put root-resource '("a")
             (build-request (string->uri "http://localhost/a")
                            method: 'PUT
                            port: (open-output-string))
             "New Contents of A")

    (let ((head body (run-get root-resource '("a")
                              (build-request
                               (string->uri "http://localhost/a")
                               method: 'GET)
                              'GET)))
      (test-equal "Put updates subsequent gets"
        "New Contents of A" body)))

  (test-group "Create new resource"
    (run-put root-resource '("c")
             (build-request (string->uri "http://localhost/c")
                            method: 'PUT
                            port: (open-output-string))
             "Created Resource C")
    (let ((head body (run-get root-resource '("c")
                              (build-request
                               (string->uri "http://localhost/c")
                               method: 'GET)
                              'GET)))
      (test-equal "Put creates new resources"
        "Created Resource C" body))))



;;; Run DELETE
(test-group "run-delete"
  'TODO)




(test-group "run-mkcol"
  (run-mkcol root-resource '("a" "b")
             (build-request (string->uri "http://localhost/a/b")
                            method: 'MKCOL)
             "")
  (let* ((request (build-request
                   (string->uri "http://localhost/")
                   method: 'PROPFIND
                   headers: '((depth . infinity))
                   validate-headers?: #f))
         (head body (run-propfind root-resource '() request #f)))
    (test-equal 207 (response-code head))
    (test-equal '(application/xml) (response-content-type head))
    (test-assert (procedure? body))
    (let ((body* (connect body xml->sxml*)))
      (test-equal "Check that all created resources now exists"
        '("/" "/a" "/a/b" "/b" "/c")
        (sort* ((sxpath '(// d:href *text*)) body*)
               string<)))))


;;; TODO test MKCOL indempotence



;;; Run COPY
(test-group "run-copy"
  (let ((root-resource (make <virtual-resource> name: "*root*")))
    (add-resource! root-resource "a" "Content of A")
    (let ((a (lookup-resource root-resource '("a"))))
      (set-property! a ((xml prop-ns 'test) "prop-value"))
      ;; Extra child added to ensure deep copy works
      (add-resource! a "d" "Content of d"))

    (test-group "cp /a /c"
      (let ((response _
                      (run-copy root-resource '("a")
                                (build-request
                                 (string->uri "http://example.com/a")
                                 headers: `((destination
                                             . ,(string->uri "http://example.com/c")))))))
        ;; Created
        (test-eqv "Resource was reported created"
          201 (response-code response)))

      (let ((c (lookup-resource root-resource '("c"))))
        (test-assert "New resource present in tree" c)
        (test-equal "Content was correctly copied"
          "Content of A" (content c))
        (test-equal "Property was correctly copied"
          (propstat 200
                    (list ((xml prop-ns 'test)
                            "prop-value")))
          (get-property c ((xml prop-ns 'test))))))

    (test-group "cp --no-clobber /c /a"
      (let ((response _
                      (run-copy root-resource '("c")
                                (build-request
                                 (string->uri "http://example.com/c")
                                 headers: `((destination
                                             . ,(string->uri "http://example.com/a"))
                                            (overwrite . #f))))))
        ;; collision
        (test-eqv "Resource collision was reported"
          412 (response-code response))))

    ;; Copy recursive collection, and onto child of self.
    (test-group "cp -r / /c"
      (let ((response _
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
  (let ((root-resource (make <virtual-resource> name: "*root*")))
    (add-resource! root-resource "a" "Content of A")
    (let ((a (lookup-resource root-resource '("a"))))
      (set-property! a ((xml prop-ns 'test) "prop-value")))

    (test-group "mv /a /c"
      (let ((response _
                      (run-move root-resource '("a")
                                (build-request
                                 (string->uri "http://example.com/a")
                                 headers: `((destination
                                             . ,(string->uri "http://example.com/c")))))))
        ;; Created
        (test-eqv "Resource was reported created"
          201 (response-code response))
        ;; TODO check that old resource is gone
        ))))



;;; Run REPORT

'((calp server webdav))
