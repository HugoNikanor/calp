(define-module (test webdav-resource)
  :use-module ((calp namespaces) :select (webdav))
  :use-module ((calp webdav property)
               :select (propstat
                        property-getter
                        property-setter-generator
                        live-property?
                        ))
  :use-module (calp webdav resource base)
  :use-module (calp webdav resource virtual)
  :use-module (calp webdav resource)
  :use-module (datetime)
  :use-module (oop goops)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (sxml namespaced)
  )

(define get-live-property (@@ (calp webdav resource base) get-live-property))

(define dt
  (datetime year: 2010 month: 11 day: 12
            hour: 13 minute: 14 hour: 15))

(define resource (make <virtual-resource>
                   ;; local-path: '("")
                   name: "*root"
                   content: #vu8(1 2 3 4)
                   creation-time: dt))



(test-group "string->href"
  (test-equal "Root path becomes null"
    '() (string->href "/"))
  (test-equal "Trailing slashes are ignored"
    '("a" "b") (string->href "/a/b/")))

(test-group "href->string"
  (test-equal "Null case becomes root path"
    "/" (href->string '()))
  (test-equal "Trailing slashes are not added"
    "/a/b" (href->string '("a" "b"))))

(test-group "href-relative"
  (test-equal '("a" "b") (href-relative '() '("a" "b")))
  (test-equal '("b") (href-relative '("a") '("a" "b")))
  (test-equal '() (href-relative '("a" "b") '("a" "b")))

  (test-error 'misc-error
    (href-relative '("c") '("a" "b")))

  (test-error 'misc-error
    (href-relative '("c") '())))

(test-group "All live properties"
 (let ((props (live-properties resource)))
   (test-assert (list? props))
   (for-each (lambda (pair)
               (test-assert (xml-element? (car pair)))
               (test-assert (live-property? (cdr pair)))
               (test-assert (procedure? (property-getter (cdr pair))))
               (test-assert (procedure? (property-setter-generator (cdr pair)))))
             props)))


(define ns1 (string->symbol "http://example.com/namespace"))

((set-dead-property!! resource ((xml ns1 'test) "Content")))

(test-equal "Get dead property"
  (propstat 200 (list ((xml ns1 'test) "Content")))
  (get-dead-property resource ((xml ns1 'test))))

(test-equal "Get live property"
  (propstat 404 (list ((xml ns1 'test))))
  (get-live-property resource ((xml ns1 'test))))

(test-group "Dead properties"
  (test-equal "Existing property"
    (propstat 200 (list ((xml ns1 'test) "Content")))
    (get-property resource ((xml ns1 'test))))

  (test-equal "Missing property"
    (propstat 404 (list ((xml ns1 'test2))))
    (get-property resource ((xml ns1 'test2)))))

(test-group "Live Properties"

  ;; TODO these tests were written when displayname always returned 200, but have since changed to test for 404.
  ;; Change to another property which return 200
  (test-equal "Existing live property (through get-live-property)"
    (propstat 404 (list ((xml webdav 'displayname))))
    (get-live-property resource ((xml webdav 'displayname))))

  (test-equal "Existing live property (thrtough get-property)"
    (propstat 404 (list ((xml webdav 'displayname))))
    (get-property resource ((xml webdav 'displayname)))))

(test-group "lookup-resource"
  (let* ((root (make <virtual-resource>))
         (a (create-collection! root "a"))
         (b (create-collection! a "b"))
         (c (create-resource! b "c")))
    (test-eq "Lookup root"
      root (lookup-resource root '()))
    (test-eq "Lookup direct child"
      a (lookup-resource root '("a")))
    (test-eq "Lookup deep child"
      c (lookup-resource root '("a" "b" "c")))
    (test-assert "Lookup missing"
      (not (lookup-resource root '("a" "d" "c"))))))

'((calp webdav resource)
  (calp webdav property)
  (calp webdav resource virtual)
  (calp webdav resource base))
