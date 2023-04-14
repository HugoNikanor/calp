(define-module (test webdav)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-1)
  :use-module (sxml namespaced)
  :use-module (oop goops)
  :use-module (calp namespaces)
  :use-module ((hnh util) :select (sort*))
  :use-module (datetime)

  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  )

;;; NOTE these tests don't check that XML namespaces work correctly, but only as
;;; far as not checking that the correct namespace is choosen. They should fail if
;;; namespacing gets completely broken.

;;; TODO tests for a missing resource?

(define (swap p) (xcons (car p) (cdr p)))

(define dt #2010-11-12T13:14:15)

(define resource (make <virtual-resource>
                   ;; local-path: '("")
                   name: "*root"
                   content: #vu8(1 2 3 4)
                   creation-time: dt))

(define (sort-propstats propstats)
  (map
   (lambda (propstat)
     (make-propstat (propstat-status-code propstat)
                    (sort* (propstat-property propstat)
                           string< (compose symbol->string xml-element-tagname car))
                    (propstat-error propstat)
                    (propstat-response-description propstat)))
   (sort* propstats < propstat-status-code))
  )

;; (test-equal "/" (href->string (href resource)))
(test-equal "Basic propstat"
    (propstat 200 (list (list (xml webdav 'getcontentlength) 4)))
    (getcontentlength resource))


(define (sort-symbols symbs)
  (sort* symbs string<=? symbol->string))



;;; NOTE propstat's return order isn't stable, making this test possibly fail
(let ((ps (list (propstat 200 (list `(,(xml webdav 'displayname) "Displayname")))
                (propstat 200 (list `(,(xml webdav 'getcontenttype) "text/plain"))))))
  (test-equal "Propstat merger"
    (list (propstat 200
                    (list (list (xml webdav 'getcontenttype) "text/plain")
                          (list (xml webdav 'displayname) "Displayname"))))
    (merge-propstats ps)))



(test-group "All live properties"
 (let ((props (live-properties resource)))
   (test-assert (list? props))
   (for-each (lambda (pair)
               ;; (test-assert (xml-element? (car pair)))
               (test-assert (live-property? (cdr pair)))
               (test-assert (procedure? (property-getter (cdr pair))))
               (test-assert (procedure? (property-setter-generator (cdr pair)))))
             props)))

(test-group "\"All\" live properties"
  (let ((most (propfind-most-live-properties resource)))
    (test-equal "Correct amount of keys" 10 (length most))
    (for-each (lambda (propstat)
                (test-assert "Propstat is propstat" (propstat? propstat))
                (test-equal (format #f "Propstat well formed: ~a" (propstat-property propstat))
                  1 (length (propstat-property propstat)))
                (test-assert "Propstat child is xml"
                  (xml-element? (caar (propstat-property propstat)))))
              most)

    (test-equal "Correct keys"
      '(creationdate displayname getcontentlanguage getcontentlength
                     getcontenttype getetag getlastmodified
                     lockdiscovery resourcetype supportedlock)
      (sort-symbols (map (compose xml-element-tagname caar propstat-property) most)))))



(define ns1 (string->symbol "http://example.com/namespace"))

(set-dead-property! resource `(,(xml ns1 'test) "Content"))

(test-equal "Get dead property"
  (propstat 200 (list (list (xml ns1 'test) "Content")))
  (get-dead-property resource (xml ns1 'test)))

(test-equal "Get live property"
  (propstat 404 (list (list (xml ns1 'test))))
  (get-live-property resource (xml ns1 'test)))

(test-group "Dead properties"
  (test-equal "Existing property"
    (propstat 200 (list (list (xml ns1 'test) "Content")))
    (get-property resource (xml ns1 'test)))

  (test-equal "Missing property"
    (propstat 404 (list (list (xml ns1 'test2))))
    (get-property resource (xml ns1 'test2)))

  (test-equal "All dead properties"
    (list (propstat 200 (list (list (xml ns1 'test) "Content"))))
    (propfind-all-dead-properties resource)))

(test-group "Live Properties"

  ;; TODO these tests were written when displayname always returned 200, but have since changed to test for 404.
  ;; Change to another property which return 200
  (test-equal "Existing live property (through get-live-property)"
    (propstat 404 `((,(xml webdav 'displayname))))
    (get-live-property resource (xml webdav 'displayname)))

  (test-equal "Existing live property (thrtough get-property)"
    (propstat 404 `((,(xml webdav 'displayname))))
    (get-property resource (xml webdav 'displayname)))
  )

(test-equal "propfind-selected-properties"
  (list (propstat 404 `((,(xml webdav 'displayname)))))
  (propfind-selected-properties resource (list (xml webdav 'displayname))))

(test-group "parse-propfind"
  (test-group "propname"
    (let ((props (parse-propfind `(,(xml webdav 'propfind)
                                   (,(xml webdav 'propname)))
                                resource)))


     (test-group "Propfind should NEVER fail for an existing resource"
       (test-equal 1 (length props))
       (test-equal 200 (propstat-status-code (car props))))

     (test-assert "Propstat objects are returned" (propstat? (car props)))
     (for-each (lambda (el)
                 (test-assert "Base is list" (list? el))
                 (test-eqv "List only contains head el" 1 (length el))
                 #;
                 (test-assert (format #f "Head is an xml tag: ~a" el)
                   (xml-element? (car el))))
               (propstat-property (car props)))

     #;
     (test-equal "Correct property keys"
       (sort-symbols (cons* 'test 'is-virtual webdav-keys))
       (sort-symbols (map (compose xml-element-tagname car)
                          (propstat-property (car props)))))

     (test-group "No property should contain any data"
       (for-each (lambda (el)
                   (test-eqv (format #f "Propname property: ~s" el)
                     1 (length el)))
                 (propstat-property (car props))))))


  (test-group "direct property list"
    (let ((props (parse-propfind `((xml webdav 'propfind)
                                   (,(xml webdav 'prop)
                                    (,(xml webdav 'displayname))))
                                 resource)))
      (test-equal "Simple lookup"
        (list (propstat 404 (list (list (xml webdav 'displayname)
                                        ))))
        props)))

  ;; TODO test that calendar properties are reported by propname
  ;; TODO test that non-native caldav propreties aren't reported by allprop

  (test-group "allprop"
    (let ((props (parse-propfind `(,(xml webdav 'propfind)
                                   (,(xml webdav 'allprop)))
                                 resource)))


      (test-equal "Propfind result"
        (list
         (propstat 200
                   (list (list (xml webdav 'creationdate)
                               (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         (list (xml webdav 'getcontentlength)
                               4)
                         (list (xml webdav 'getcontenttype)
                               "application/binary")
                         (list (xml webdav 'getlastmodified)
                               "Thu, 01 Jan 1970 00:00:00 GMT")
                         (list (xml webdav 'lockdiscovery) '())
                         (list (xml webdav 'resourcetype)
                                        ; (list (xml webdav 'collection))
                               )
                         (list (xml webdav 'supportedlock) '())
                         (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list (list (xml webdav 'displayname))
                             (list (xml webdav 'getcontentlanguage))))
         (propstat 501
                   (list (list (xml webdav 'getetag))
                        )))
        (sort-propstats props))))


  (test-group "allprop with include"
    (let ((props (parse-propfind `((xml webdav 'propfind)
                                   (,(xml webdav 'allprop))
                                   (,(xml webdav 'include)))
                                 resource)))


      (test-equal "Include NOTHING"
        (list
         (propstat 200
                   (list (list (xml webdav 'creationdate)
                               (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         (list (xml webdav 'getcontentlength)
                               4)
                         (list (xml webdav 'getcontenttype)
                               "application/binary")
                         (list (xml webdav 'getlastmodified)
                               "Thu, 01 Jan 1970 00:00:00 GMT")
                         (list (xml webdav 'lockdiscovery) '())
                         (list (xml webdav 'resourcetype)
                                        ; (list (xml webdav 'collection))
                               )
                         (list (xml webdav 'supportedlock) '())
                         (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list (list (xml webdav 'displayname))
                             (list (xml webdav 'getcontentlanguage))))
         (propstat 501
                   (list (list (xml webdav 'getetag))
                         )))
        (sort-propstats props)))


    (let ((props (parse-propfind `(,(xml webdav 'propfind)
                                   (,(xml webdav 'allprop))
                                   (,(xml webdav 'include)
                                    (,(xml virtual-ns 'isvirtual))))
                                 resource)))

      (test-equal "Include isvirtual"
        (list
         (propstat 200
                   (list (list (xml webdav 'creationdate) (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         (list (xml webdav 'getcontentlength) 4)
                         (list (xml webdav 'getcontenttype) "application/binary")
                         (list (xml webdav 'getlastmodified) "Thu, 01 Jan 1970 00:00:00 GMT")
                         (list (xml virtual-ns 'isvirtual) "true")
                         (list (xml webdav 'lockdiscovery) '())
                         (list (xml webdav 'resourcetype))
                         (list (xml webdav 'supportedlock) '())
                         (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list (list (xml webdav 'displayname))
                             (list (xml webdav 'getcontentlanguage))))
         (propstat 501
                   (list (list (xml webdav 'getetag))
                     )))
        (sort-propstats props)))))




;;; Setting properties

;;; We already use set-dead-property! above, but for testing get we need set,
;;; and for testing set we need get, and get is more independent, so we start there.



(test-group "Propstat -> namespaced sxml"
  (test-equal "Simple"
    `(,(xml webdav 'propstat)
      (,(xml webdav 'prop) (,(xml webdav 'displayname) "test"))
      (,(xml webdav 'status) "HTTP/1.1 200 OK"))
    (propstat->namespaced-sxml (propstat 200 `((,(xml webdav 'displayname) "test")) )))

  ;; TODO populated error field

  (test-equal "With response description"
    `(,(xml webdav 'propstat)
      (,(xml webdav 'prop) (,(xml webdav 'displayname) "test"))
      (,(xml webdav 'status) "HTTP/1.1 403 Forbidden")
      (,(xml webdav 'responsedescription) "Try logging in"))
    (propstat->namespaced-sxml (propstat 403 `((,(xml webdav 'displayname) "test"))
                                         responsedescription: "Try logging in"))))




;;; TODO what am I doing here?

(test-equal
    (list (propstat 200
                    `((,(xml webdav 'getcontentlength) 4)
                      (,(xml webdav 'getlastmodified) "Thu, 01 Jan 1970 00:00:00 GMT")
                      (,(xml webdav 'resourcetype))))
          (propstat 404
                    `((,(xml webdav 'checked-in))
                      (,(xml webdav 'checked-out))
                      (,(xml (string->symbol "http://apache.org/dav/props/") 'executable)))))
  (let ((request (xml->namespaced-sxml
                  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<propfind xmlns=\"DAV:\">
  <prop>
    <getcontentlength/>
    <getlastmodified/>
    <executable xmlns=\"http://apache.org/dav/props/\"/>
    <resourcetype/>
    <checked-in/>
    <checked-out/>
  </prop>
</propfind>")))

    (sort-propstats (parse-propfind (caddr request) resource))))



(test-group "lookup-resource"
  (let* ((root (make <virtual-resource> name: "*root*"))
         (a (add-collection! root "a"))
         (b (add-collection! a "b"))
         (c (add-resource! b "c" "~~Nothing~~")))
    (test-eq "Lookup root"
      root (lookup-resource root '()))
    (test-eq "Lookup direct child"
      a (lookup-resource root '("a")))
    (test-eq "Lookup deep child"
      c (lookup-resource root '("a" "b" "c")))
    (test-assert "Lookup missing"
      (not (lookup-resource root '("a" "d" "c"))))))




(test-group "mkcol"
  (let ((root (make <virtual-resource> name: "*root*")))
    (add-collection! root "child")
    (test-eqv "Child got added" 1 (length (children root)))))
