(define-module (calp webdav resource virtual)
  :use-module (oop goops)
  :use-module (datetime)
  :use-module (rnrs bytevectors)
  :use-module (hnh util)
  :use-module (sxml namespaced)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :export (<virtual-resource>
           virtual-resource?
           virtual-ns
           ;; content
           isvirtual
           )
  )

(define virtual-ns (string->symbol "http://example.com/virtual"))

(define-class <virtual-resource> (<resource>)
  (content* init-value: #vu8()
           init-keyword: content:
           accessor: content*)
  (creation-time init-form: (current-datetime)
                 init-keyword: creation-time:
                 getter: creation-time))

(define (virtual-resource? x)
  (is-a? x <virtual-resource>))

(define-method (write (self <virtual-resource>) port)
  (format port "#<<virtual-resource> name=~s, creation-time=~s, content=~s>"
          (name self)
          (creation-time self)
          (content self)))

(define-method (live-properties (self <virtual-resource>))
  (append
   (next-method)
   (list (cons (xml-element-hash-key (xml virtual-ns 'isvirtual)) (make-live-property isvirtual set-isvirtual!)))))

(define-method (content (self <virtual-resource>))
  (content* self))

(define-method (set-content! (self <virtual-resource>) data)
  (set! (content* self) data))

(define-method (creationdate (self <virtual-resource>))
  (propstat 200
            (list
             (list (xml webdav 'creationdate)
                   (-> (creation-time self)
                       (datetime->string "~Y-~m-~dT~H:~M:~SZ"))))))


(define-method (getcontenttype (self <resource>))
  (propstat 200
            (list
             (list (xml webdav 'getcontenttype)
                   "application/binary"))))

(define-method (isvirtual (self <virtual-resource>))
  (propstat 200
            (list
             (list (xml virtual-ns 'isvirtual)
                   "true"))))


(define-method (set-isvirtual! (self <virtual-resource>) _)
  (throw 'protected-resource "isvirtual"))
