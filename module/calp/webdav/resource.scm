(define-module (calp webdav resource)
  :use-module ((srfi srfi-1) :select (find remove last append-map drop-while concatenate))
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :use-module ((hnh util) :select (unless))
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (rnrs bytevectors)
  :use-module (hnh util)
  :use-module (hnh util env)
  :use-module (datetime)
  :export (<resource>
           resource?
           parent

           get-property
           set-property!!
           set-property!
           remove-property!!
           remove-property!

           dead-properties
           live-properties
           get-dead-property
           set-dead-property!!
           remove-dead-property!!

           content
           set-content!

           creation-date
           content-language
           content-length
           content-type
           display-name
           etag
           last-modified

           children
           collection?

           get-child-by-name!
           move-resource!
           move-resource-implementation!
           copy-resource!
           remove-self!
           on-child-removed
           mount-resource!

           create-collection!
           create-resource!
           create-collection-copy!
           create-resource-copy!

           ;; properties defined by the RFC
           creationdate       set-creationdate!       remove-creationdate!
           displayname        set-displayname!        remove-displayname!
           getcontentlanguage set-getcontentlanguage! remove-getcontentlanguage!
           getcontentlength   set-getcontentlength!   remove-getcontentlength!
           getcontenttype     set-getcontenttype!     remove-getcontenttype!
           getetag            set-getetag!            remove-getetag!
           getlastmodified    set-getlastmodified!    remove-getlastmodified!
           lockdiscovery      set-lockdiscovery!      remove-lockdiscovery!
           resourcetype       set-resourcetype!       remove-resourcetype!
           supportedlock      set-supportedlock!      remove-supportedlock!

           ;; List of those properties
           webdav-properties

           ;; Lookup utilities
           lookup-resource
           all-resources-under

           ))

(define-class <resource> ()
  (parent accessor: parent
          init-keyword: parent:
          init-value: #f))


(define-method (initialize (self <resource>) args)
  (next-method)
  (typecheck (parent self) (or false? resource?) "<resource>.parent"))

(define-method (equal? (a <resource>) (b <resource>))
  (equal? (parent a) (parent b)))

(define (resource? x)
  (is-a? x <resource>))

(define-generic children)
(define-generic collection?)

(define-method (content (resource <resource>))
  (if (collection? resource)
      (throw 'http 403)
      (throw 'http 500 (format #f "The given resource type failed to implement content: ~s" resource))))

(define-method (set-content! (r <resource>) c)
  (set-content! r c '()))
(define-method (set-content! (_ <resource>) c h)
  (throw 'http 405))

(define-method (content-length (self <resource>))
  (cond ((content self)
         bytevector? => bytevector-length)
        (else #f)))



(define* (lookup-resource resource path key: create?)
  (cond ((null? path) resource)
        ((get-child-by-name! resource (car path) create?)
         => (lambda (resource) (lookup-resource resource (cdr path)
                                           create?: create?)))
        (else #f)))

;; Returns a flat list of this resource, and all its decendants
(define* (all-resources-under resource optional: (path-prefix '()))
  (cons (cons path-prefix resource)
        (concatenate
         (map (lambda (c) (all-resources-under (cdr c) (append path-prefix (list (car c)))))
              (children resource)))))



;; Only tagname and namespaces are checked on the <xml-element> for the {get,set}-property

;;; All get-*-property methods return propstat elements

(define (lookup-live-property resource xml-el)
  (typecheck resource resource?)
  (typecheck xml-el xml-element?)

  (and=> (find (lambda (p)
                 (and (xml-element? (car p))
                      (equal? (xml-element-hash-key (car p))
                              (xml-element-hash-key xml-el))))
               (live-properties resource))
         cdr))

(define (get-live-property resource xml-el)
  (typecheck resource resource?)
  (typecheck xml-el xml-element?)

  (cond ((lookup-live-property resource xml-el)
         => (lambda (prop) ((property-getter prop) resource)))
        (else (propstat 404 (list xml-el)))))

;; Return a promise which performs the set operation.
;; Pre-conditions can cause this function to throw
(define (set-live-property!! resource value)
  (typecheck resource resource?)
  (typecheck value xml-element?)

  (cond ((lookup-live-property resource value)
         ;; NOTE this drops any (xml) attributes from the value object
         => (lambda (prop) (apply (property-setter-generator prop)
                             resource (xml-element-children value))))
        (else #f)))

(define-generic get-dead-property)

;; Return a list of xml elements, where each entry is a property
(define-generic dead-properties)

(define-generic set-dead-property!!)

(define-generic remove-dead-property!!)

;; Returns a promise, which when evaluated, attempts to physically set
;; the property. This procedure might fail due to pre-conditions,
;; and the actuall fail might also fail
(define (set-property!! resource value)
  (or (set-live-property!! resource value)
      (set-dead-property!! resource value)))

(define (set-property! resource value)
  ((set-property!! resource value)))

(define (remove-live-property!! resource xml-tag)
  (typecheck xml-tag xml-element?)
  (typecheck resource resource?)

  (cond ((lookup-live-property resource xml-tag)
         => (lambda (prop) ((property-remover-generator prop) resource)))
        (else #f)))

(define (remove-property!! resource xml-tag)
  (or (remove-live-property!! resource xml-tag)
      (remove-dead-property!! resource xml-tag)))

(define (remove-property! resource xml-tag)
  ((remove-property!! resource xml-tag)))



(define (get-property resource xml-tag)
  (typecheck xml-tag xml-element?)
  (typecheck resource resource?)

  (cond ((get-dead-property resource xml-tag)
         propstat-200? => identity)
        (else (get-live-property resource xml-tag))))

;; Return an alist from xml-element objects without children,
;; to generic procedures returning that value.
;; SHOULD be extended by children, which append their result to this result
;; @example
;; (define-method (live-properties (self <specific-resource>)
;;   (append (next-method)
;;           specific-resource-properties))
;; @end example
(define-method (live-properties (self <resource>))
  (map (lambda (pair) (cons ((xml webdav (car pair)))
                       (cdr pair)))
       webdav-properties))




(define-method (creation-date (_ <resource>)) #f)

(define-method (creationdate (self <resource>))
  (cond ((creation-date self)
         => (lambda (cd)
              (propstat
               200 (list ((xml webdav 'creationdate)
                          (datetime->string cd "~Y-~m-~dT~H:~M:~S~Z"))))))
        (else (propstat 404 (list ((xml webdav 'creationdate)))))))

(define-method (set-creationdate! (self <resource>) _) (throw 'protected-property))
(define-method (remove-creationdate! (self <resource>) _) (throw 'protected-property))

(define-method (display-name (_ <resource>)) #f)

(define-method (displayname (self <resource>))
  (cond ((display-name self)
         => (lambda (name) (propstat 200 (list ((xml webdav 'displayname)
                                           name)))))
        (else (propstat 404 (list ((xml webdav 'displayname)))))))

(define-method (set-displayname! (_ <resource>)) (throw 'protected-property))
(define-method (remove-displayname! (_ <resource>)) (throw 'protected-property))

(define-method (content-language (_ <resource>)) #f)

(define-method (set-getcontentlanguage! (_ <resource>) v) (throw 'protected-property))
(define-method (remove-getcontentlanguage! (_ <resource>)) (throw 'protected-property))
(define-method (getcontentlanguage (self <resource>))
  (cond ((content-language self)
         => (lambda (lang) (propstat 200 (list ((xml webdav 'getcontentlanguage) lang)))))
        (else (propstat 404 (list ((xml webdav 'getcontentlanguage)))))))


(define-method (remove-getcontentlength! (self <resource>)) (throw 'protected-property))
(define-method (set-getcontentlength! (self <resource>) _) (throw 'protected-property))
(define-method (getcontentlength (self <resource>))
  (propstat 200
            (list
             ((xml webdav 'getcontentlength)
              (number->string
               (content-length self))))))


(define-method (content-type (_ <resource>)) #f)

(define-method (remove-getcontenttype! (self <resource>)) (throw 'protected-property))
(define-method (set-getcontenttype! (self <resource>) _) (throw 'protected-property))
(define-method (getcontenttype (self <resource>))
  (cond ((content-type self)
         => (lambda (type)
              (propstat 200 (list ((xml webdav 'getcontenttype) type)))))
        (else
         (propstat 404 (list ((xml webdav 'getcontenttype)))))))


(define-method (etag (_ <resource>)) #f)

(define (remove-getetag! _) (throw 'protected-property))
(define (set-getetag! r _) (throw 'protected-property))
(define-method (getetag (self <resource>))
  (cond ((etag self)
         => (lambda (tag)
              (propstat 200 (list ((xml webdav 'getetag) tag)))))
        (else
         (propstat 404 (list ((xml webdav 'getetag)))))))


(define-method (last-modified (_ <resource>)) #f)

(define-method (remove-getlastmodified! (self <resource>) _) (throw 'protected-property))
(define-method (set-getlastmodified! (self <resource>) _) (throw 'protected-property))
(define-method (getlastmodified (self <resource>))
  (cond ((last-modified self)
         => (lambda (dt)
              (propstat
               200
               (list ((xml webdav 'getlastmodified)
                      (datetime->http-date dt))))))
        (else (propstat 404 (list ((xml webdav 'getlastmodified)))))))

(define (remove-lockdiscovery! _) (throw 'protected-property))
(define (set-lockdiscovery! r _) (throw 'protected-property))
(define-method (lockdiscovery (self <resource>))
  (propstat #; 200 404 (list ((xml webdav 'lockdiscovery)))))


(define-method (remove-resourcetype! (self <resource>)) (throw 'protected-property))
(define-method (set-resourcetype! (self <resource>) _) (throw 'protected-property))
(define-method (resourcetype (self <resource>))
  (propstat 200 (list (apply (xml webdav 'resourcetype)
                             (when (collection? self)
                               (list ((xml webdav 'collection))))))))


(define (remove-supportedlock! _) (throw 'protected-property))
(define (set-supportedlock! r _) (throw 'protected-property))
(define-method (supportedlock (self <resource>))
  (propstat 200 (list ((xml webdav 'supportedlock)))))

;; Dirty macro to quickly generate  live property definitions
(define-macro (xx . symbs)
  `(list ,@(map (lambda (symb)
                  `(cons (quote ,symb)
                        (make-live-property
                         ,symb
                         ,(string->symbol (format #f "set-~a!" symb))
                         ,(string->symbol (format #f "remove-~a!" symb)))))
               symbs)))

(define webdav-properties
  (xx creationdate
      displayname
      getcontentlanguage
      getcontentlength
      getcontenttype
      getetag
      getlastmodified
      lockdiscovery
      resourcetype
      supportedlock))



;;; Additional functions
;;; lookup-resource
;;;
;;; No delete by path exists, instead do something like
;;;     (and=> (lookup-resource root path) delete-self!)
;;; No PUT by path exists for the same reason:
;;;     (and=> (lookup-resource root path create?: #t)
;;;            (lambda (resource) (set! (content resource) payload)))

(define-generic remove-self!)
(define-generic create-collection!)
(define-method (create-collection! resource name)
  (create-collection! resource name '() #f))
(define-generic create-resource!)

(define-method (create-collection-copy!
                (source <resource>) (destination <resource>) name depth)

  (typecheck depth (memv '(0 infinity)))

  (let ((resource (create-collection! destination name)))
    (for-each (lambda (prop) (set-property! resource prop))
              (dead-properties source))

    (case depth
      ((0) 'noop)
      ((infinity)
       (for (name . child) in (children source)
            (create-collection-copy! child resource name 'infinity))))))


(define-method (create-resource-copy!
                (source <resource>) (destination <resource>) name)
  (let ((resource (create-resource! destination name)))
    (set-content! resource (content source) '())
    (for-each (lambda (prop) (set-property! resource prop))
              (dead-properties source))))

(define-method (copy-onto! (source <resource>) (destination <resource>) name depth)
  (define p (parent destination))
  (remove-self! destination)
  (if (collection? source)
      (create-collection-copy! source p name depth)
      (create-resource-copy! source p name)))

(define* (copy-resource! source destination name key: (depth 'infinity) (overwrite? #t))
  (typecheck source resource?)
  (typecheck destination resource?)
  (typecheck name string?)
  (typecheck depth (memv '(0 infinity)))

  (cond ((not (collection? destination))
         (throw 'http 412 "Destination resource is not a collection"))
        ((and overwrite? (get-child-by-name! destination name))
         => (lambda (old)
              (copy-onto! source old name depth)
              'replaced))
        ((get-child-by-name! destination name)
         'collision)
        ((collection? source)           ; target doesn't exist
         (create-collection-copy! source destination name depth)
         'created)
        (else       ; target doesn't exist, source is a non-collection
         (create-resource-copy! source destination name)
         'created)))


(define* (move-resource! source destination name key: overwrite?)
  (define return-code 'created)
  (cond ((and overwrite? (get-child-by-name! destination name))
         => (lambda (old)
              (remove-self! old)
              (set! return-code 'replaced)))
        ((get-child-by-name! destination name)
         (throw 'http 412)))
  (move-resource-implementation! source destination name)
  return-code)


(define-method (move-resource-implementation! (source <resource>) (destination <resource>) name)
  (copy-resource! source destination name)
  (remove-self! source))


(define-method (get-child-by-name! (resource <resource>) (name <string>))
  (get-child-by-name! resource name #f))

;; Default implementation for finding children.
;; Specific resource types are free to implement faster lookup methods.
(define-method (get-child-by-name! (resource <resource>) (name <string>) create?)
  (cond ((find (lambda (p) (string=? name (car p)))
               (children resource))
         => cdr)
        (create? (create-resource! resource name))
        (else #f)))


(define-generic on-child-removed)

(define-method (mount-resource! (resource <resource>) (parent <resource>) name)
  (throw 'http "Can't mount that resource in that location"))
