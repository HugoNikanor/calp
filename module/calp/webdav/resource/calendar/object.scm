(define-module (calp webdav resource calendar object)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (calp webdav resource)
  :use-module ((vcomponent formats ical) :prefix #{ics:}#)
  :use-module ((vcomponent formats xcal) :prefix #{xcs:}#)
  :use-module ((vcomponent) :prefix vcs-)
  :use-module ((calp namespaces) :select (webdav))
  :use-module (calp webdav property)
  :use-module (sxml namespaced)

  :export (<calendar-object-resource>
           calendar-object-resource?
           component)
  )

;;; content%

(define-class <calendar-object-resource> (<resource>)
  ;; TODO typecheck
  (component getter: component
             init-keyword: component:))



(define-method (collection? (_ <calendar-object-resource>))
  #f)



(define-method (children (_ <calendar-object-resource>))
  '())

(define (calendar-object-resource? x)
 (is-a? x <calendar-object-resource>))

(define-method (content (self <calendar-object-resource>) content-type)
  (case content-type
    ((text/calendar)
     (call-with-output-string (lambda (port) (ics:serialize (content% self) port))))
    ((application/calendar+xml)
     (call-with-output-string (lambda (port) (xcs:serialize (content% self) port))))
    ;; ((text/html))
    ;; ((application/xhtml+xml))
    ;; ((application/calendar+json))
    (else (content self 'text/calendar))
    )
  )

(define-method (creationdate (self <calendar-object-resource>))
  (propstat 200
            `((,(xml webdav 'creationdate)
               (-> (content self)
                   (prop 'CREATED)
                   ;; TODO timezone
                   (datetime->string "~Y-~m-~dT~H:~M:~SZ"))))))


(define-method (getcontentlength (self <calendar-object-resource>))
  ;; TODO which representation should be choosen to calculate length?
  (propstat 501 `((,(xml webdav 'getcontentlength)))))



(define-method (getcontenttyype (self <calendar-object-resource>))
  ;; TODO different representations
  (propstat 200 `((,(xml webdav 'getcontentlength)
                   "text/calendar"))))


(define-method (getlastmodified (self <calendar-object-resource>))
  (propstat 200
            `((,(xml webdav 'getlastmodified)
               (string->datetime (prop (content self) 'LAST-MODIFIED)
                                 "~Y~m~dT~H~M~S")))))
