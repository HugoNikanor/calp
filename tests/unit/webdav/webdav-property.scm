(define-module (test webdav-property)
  :use-module ((calp namespaces) :select (webdav))
  :use-module ((calp webdav property)
               :select (propstat merge-propstats propstat->namespaced-sxml))
  :use-module (calp webdav resource)
  :use-module (calp webdav resource href)
  :use-module (calp webdav resource virtual)
  :use-module (datetime)
  :use-module (oop goops)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (sxml namespaced)
  )

(define dt
  (datetime year: 2010 month: 11 day: 12
            hour: 13 minute: 14 hour: 15))

(define resource (make <virtual-resource>
                   ;; local-path: '("")
                   name: "*root"
                   content: #vu8(1 2 3 4)
                   creation-time: dt))



;; (test-equal "/" (href->string (href resource)))
(test-equal "Basic propstat"
    (propstat 200 (list ((xml webdav 'getcontentlength) "4")))
    (getcontentlength resource))


;;; NOTE propstat's return order isn't stable, making this test possibly fail
(let ((ps (list (propstat 200 (list ((xml webdav 'displayname) "Displayname")))
                (propstat 200 (list ((xml webdav 'getcontenttype) "text/plain"))))))
  (test-equal "Propstat merger"
    (list (propstat 200
                    (list ((xml webdav 'getcontenttype) "text/plain")
                          ((xml webdav 'displayname) "Displayname"))))
    (merge-propstats ps)))

(test-group "Propstat -> namespaced sxml"
  (test-equal "Simple"
    ((xml webdav 'propstat)
     ((xml webdav 'prop) ((xml webdav 'displayname) "test"))
     ((xml webdav 'status) "HTTP/1.1 200 OK"))
    (propstat->namespaced-sxml (propstat 200 (list ((xml webdav 'displayname) "test")))))

  ;; TODO populated error field

  (test-equal "With response description"
    ((xml webdav 'propstat)
     ((xml webdav 'prop) ((xml webdav 'displayname) "test"))
     ((xml webdav 'status) "HTTP/1.1 403 Forbidden")
     ((xml webdav 'responsedescription) "Try logging in"))
    (propstat->namespaced-sxml (propstat 403 (list ((xml webdav 'displayname) "test"))
                                         responsedescription: "Try logging in"))))

'((calp webdav property))
