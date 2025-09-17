(define-module (calp webdav resource virtual)
  :use-module (oop goops)
  :use-module (datetime)
  :use-module (rnrs bytevectors)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module (hnh util lens)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :export (<virtual-resource>
           virtual-resource?
           virtual-ns
           ;; content
           isvirtual
           make-resource
           )
  )

(define virtual-ns (string->symbol "http://example.com/virtual"))

(define-class <virtual-resource> (<resource>)
  (collection? init-keyword: collection?: accessor: collection? init-value: #f)
  (content* init-value: #vu8()
           init-keyword: content:
           accessor: content*)
  (creation-time init-form: (current-datetime)
                 init-keyword: creation-time:
                 getter: creation-time)

  (dead-properties
   ;; Table, where keys are the result of xml-element-hash-key
   ;; And values are xml elements.
   init-form: (table)
   accessor: dead-properties%)
  )

(define-method (initialize (self <virtual-resource>) args)
  (next-method)
  (typecheck (content* self) bytevector? "<virtual-resource>.content*")
  (typecheck (creation-time self) datetime? "<virtual-resource>.creation-time"))
  ;; (typecheck (dead-properties self) (list-of xml-element?) "<resource>.dead-properties")

(define-method (setup-new-collection! (this <virtual-resource>) (parent <resource>))
  (set! (collection? this) #t))

(define (virtual-resource? x)
  (is-a? x <virtual-resource>))

(define-method (write (self <virtual-resource>) port)
  (format port "#<<virtual-resource> name=~s, creation-time=~s, content=~s>"
          (name self)
          (creation-time self)
          (content self)))

(define (make-resource name . args)
  (apply make <virtual-resource> name: name args))

(define-method (live-properties (self <virtual-resource>))
  (append
   (next-method)
   (list (cons ((xml virtual-ns 'isvirtual))
               (make-live-property isvirtual set-isvirtual!)))))

(define-method (content (self <virtual-resource>))
  (content* self))

(define-method (set-content! (self <virtual-resource>) data)
  (set! (content* self) data))

(define-method (is-collection? (self <virtual-resource>))
  (collection? self))

(define-method (creationdate (self <virtual-resource>))
  (propstat 200
            (list
             ((xml webdav 'creationdate)
              (-> (creation-time self)
                  (datetime->string "~Y-~m-~dT~H:~M:~SZ"))))))


(define-method (getcontenttype (self <virtual-resource>))
  (propstat 200
            (list
             ((xml webdav 'getcontenttype)
              "application/binary"))))

(define-method (isvirtual (self <virtual-resource>))
  (propstat 200
            (list
             ((xml virtual-ns 'isvirtual)
              "true"))))


(define-method (set-isvirtual! (self <virtual-resource>) _)
  (throw 'protected-resource "isvirtual"))



(define-method (get-dead-property (resource <virtual-resource>) xml-el)
  (typecheck xml-el xml-element?)

  (cond ((table-get (dead-properties% resource)
                    (xml-element-hash-key xml-el))
         => (lambda (it) (propstat 200 (list it))))
        (else (propstat 404 (list xml-el)))))

(define-method (dead-properties (resource <virtual-resource>))
  (map cdr
       (table->list
        (dead-properties% resource))))

;;; TODO this should be moved to the <virtual> resource type,
;;; since this gives a false impression that setting dead properties
;;; on custom resource types works, while in actuality it just stores them
;;; to working memory, without ever serializing them anywhere
;;; same goes for get-dead-property
(define-method (set-dead-property!! (resource <virtual-resource>) value)
  (typecheck value xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      (set (dead-properties% resource)
           (table-focus (xml-element-hash-key value))
           value))))

(define-method (remove-dead-property!! (resource <virtual-resource>) xml-tag)
  (typecheck xml-tag xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      (table-remove (dead-properties% resource)
                    (xml-element-hash-key xml-tag)))))
