(define-module (test webdav-propfind)
  :use-module ((hnh util) :select (sort*))
  :use-module ((calp namespaces) :select (webdav))
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (calp webdav property)
  :use-module (calp webdav propfind)
  :use-module (calp webdav resource virtual)
  :use-module (datetime)
  :use-module (oop goops)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (sxml namespaced)
  )


(define (sort-symbols symbs)
  (sort* symbs string<=? symbol->string))

;;; Propstats are unsorted, we just sort them here for easier
;;; equivalence checks in the tests.
(define (sort-propstats propstats)
  (map
   (lambda (pr)
     (typecheck pr propstat?)
     (modify pr propstat-property*
             (lambda (it)
              (sort* it
                     string< (compose symbol->string xml-element-tagname)))))
   (sort* propstats < propstat-status-code)))



(define dt (datetime year: 2020 month: 11 day: 12
                     hour: 13 minute: 14 second: 15))

(define resource (make <virtual-resource>
                   ;; local-path: '("")
                   name: "*root"
                   content: #vu8(1 2 3 4)
                   creation-time: dt))

(define ns1 (string->symbol "http://example.com/namespace"))



(test-group "\"All\" live properties"
  (let ((most (propfind-most-live-properties resource)))
    (test-equal "Correct amount of keys" 10 (length most))
    (for-each (lambda (propstat)
                (test-assert "Propstat is propstat" (propstat? propstat))
                (test-equal (format #f "Propstat well formed: ~a" (propstat-property propstat))
                  1 (length (propstat-property propstat)))
                (test-assert "Propstat child is xml"
                  (xml-element? (car (propstat-property propstat)))))
              most)

    (test-equal "Correct keys"
      '(creationdate displayname getcontentlanguage getcontentlength
                     getcontenttype getetag getlastmodified
                     lockdiscovery resourcetype supportedlock)
      (sort-symbols (map (compose xml-element-tagname car propstat-property)
                         most)))))


(test-equal "propfind-selected-properties"
  (list (propstat 404 (list ((xml webdav 'displayname)))))
  (propfind-selected-properties resource (list ((xml webdav 'displayname)))))

(test-group "exec-propfind"
  (test-group "propname"
    (let ((props (exec-propfind ((xml webdav 'propfind)
                                  ((xml webdav 'propname)))
                                resource)))


     (test-group "Propfind should NEVER fail for an existing resource"
       (test-equal 1 (length props))
       (test-equal 200 (propstat-status-code (car props))))

     (test-assert "Propstat objects are returned" (propstat? (car props)))
     (for-each (lambda (el)
                 (test-assert "Each entry is an xml element" (xml-element*? el))
                 (test-eqv "The enrties lack children" 0 (length (xml-element-children el))))
               (propstat-property (car props)))

     #;
     (test-equal "Correct property keys"
       (sort-symbols (cons* 'test 'is-virtual webdav-keys))
       (sort-symbols (map (compose xml-element-tagname car)
                          (propstat-property (car props)))))
     ))


  (test-group "direct property list"
    (let ((props (exec-propfind ((xml webdav 'propfind)
                                  ((xml webdav 'prop)
                                   ((xml webdav 'displayname))))
                                 resource)))
      (test-equal "Simple lookup"
        (list (propstat 404 (list ((xml webdav 'displayname)))))
        props)))

  ;; TODO test that calendar properties are reported by propname
  ;; TODO test that non-native caldav propreties aren't reported by allprop

  (test-group "allprop"
    (let ((props (exec-propfind ((xml webdav 'propfind)
                                  ((xml webdav 'allprop)))
                                 resource)))


      (test-equal "Propfind result"
        (list
         (propstat 200
                   (list ((xml webdav 'creationdate)
                          (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         ((xml webdav 'getcontentlength)
                          "4")
                         ((xml webdav 'getcontenttype)
                          "application/binary")
                         ((xml webdav 'getlastmodified)
                          "Thu, 01 Jan 1970 00:00:00 GMT")
                         ((xml webdav 'lockdiscovery))
                         ((xml webdav 'resourcetype)
                                        ; (list (xml webdav 'collection))
                          )
                         ((xml webdav 'supportedlock))
                         ;; (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list ((xml webdav 'displayname))
                             ((xml webdav 'getcontentlanguage))))
         (propstat 501
                   (list ((xml webdav 'getetag)))))
        (sort-propstats props))))


  (test-group "allprop with include"
    (let ((props (exec-propfind ((xml webdav 'propfind)
                                  ((xml webdav 'allprop))
                                  ((xml webdav 'include)))
                                 resource)))


      (test-equal "Include NOTHING"
        (list
         (propstat 200
                   (list ((xml webdav 'creationdate)
                          (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         ((xml webdav 'getcontentlength)
                          "4")
                         ((xml webdav 'getcontenttype)
                          "application/binary")
                         ((xml webdav 'getlastmodified)
                          "Thu, 01 Jan 1970 00:00:00 GMT")
                         ((xml webdav 'lockdiscovery))
                         ((xml webdav 'resourcetype)
                                        ; (list (xml webdav 'collection))
                          )
                         ((xml webdav 'supportedlock))
                         ;; (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list ((xml webdav 'displayname))
                             ((xml webdav 'getcontentlanguage))))
         (propstat 501
                   (list ((xml webdav 'getetag))
                         )))
        (sort-propstats props)))


    (let ((props (exec-propfind ((xml webdav 'propfind)
                                  ((xml webdav 'allprop))
                                  ((xml webdav 'include)
                                   ((xml virtual-ns 'isvirtual))))
                                 resource)))

      (test-equal "Include isvirtual"
        (list
         (propstat 200
                   (list ((xml webdav 'creationdate) (datetime->string dt "~Y-~m-~dT~H:~M:~SZ"))
                         ((xml webdav 'getcontentlength) "4")
                         ((xml webdav 'getcontenttype) "application/binary")
                         ((xml webdav 'getlastmodified) "Thu, 01 Jan 1970 00:00:00 GMT")
                         ((xml virtual-ns 'isvirtual) "true")
                         ((xml webdav 'lockdiscovery))
                         ((xml webdav 'resourcetype))
                         ((xml webdav 'supportedlock))
                         ;; (list (xml ns1 'test) "Content")
                         ))
         (propstat 404 (list ((xml webdav 'displayname))
                             ((xml webdav 'getcontentlanguage))))
         (propstat 501 (list ((xml webdav 'getetag)))))
        (sort-propstats props)))))


(test-equal
    (list (propstat 200
                    (list ((xml webdav 'getcontentlength) "4")
                          ((xml webdav 'getlastmodified) "Thu, 01 Jan 1970 00:00:00 GMT")
                          ((xml webdav 'resourcetype))))
          (propstat 404
                    (list ((xml webdav 'checked-in))
                          ((xml webdav 'checked-out))
                          ((xml (string->symbol "http://apache.org/dav/props/") 'executable)))))
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

    (sort-propstats (exec-propfind (xml-document-root request) resource))))

(test-equal "All dead properties"
  (list #;
   (propstat 200 (list (list (xml ns1 'test) "Content") ;
   )))
  (propfind-all-dead-properties resource))

'((calp webdav propfind))
