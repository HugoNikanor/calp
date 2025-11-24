(define-module (calp webdav resource virtual)
  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (datetime)
  :use-module (rnrs bytevectors)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :export (<virtual-resource>
           virtual-resource?
           virtual-ns
           make-resource

           isvirtual set-isvirtual! remove-isvirtual!
           ))

(define virtual-ns (string->symbol "http://example.com/virtual"))

(define-class <virtual-resource> (<resource>)
  (collection? init-keyword: collection?:
               accessor: collection*?
               init-value: #f)
  (content* init-value: #vu8()
            init-keyword: content:
            accessor: content*)
  (creation-date init-form: (current-datetime)
                 init-keyword: creation-date:
                 getter: creation-date)

  (child-table init-keyword: children:
               init-form: (table)
               accessor: child-table)

  (display-name init-keyword: display-name:
                init-form: #f
                getter: display-name
                setter: set-display-name!)

  (content-type init-keyword: content-type:
                accessor: content-type*
                init-value: #f)

  (dead-properties
   ;; Table, where keys are the result of xml-element-hash-key
   ;; And values are xml elements.
   init-form: (table)
   accessor: dead-properties%
   init-keyword: dead-properties:)
  )

(define-method (initialize (self <virtual-resource>) args)
  (next-method)
  (typecheck (content* self) bytevector? "<virtual-resource>.content*")
  (typecheck (creation-date self) datetime? "<virtual-resource>.creation-date")

  (typecheck (child-table self) table? "<virtual-resource>.child-table")
  (typecheck (dead-properties% self) table? "<virtual-resource>.dead-properties")

  )

;;; TODO implement `equal?`.

(define-method (collection? (resource <virtual-resource>))
  (collection*? resource))

;; (define-method (setup-new-collection! (this <virtual-resource>) (parent <resource>))
;;   (set! (collection? this) #t))

(define (virtual-resource? x)
  (is-a? x <virtual-resource>))

(define-method (write (self <virtual-resource>) port)
  (format port "#<<virtual-resource> creation-date=~s, content=~s collection=~s>"
          (creation-date self)
          (catch 'decoding-error
            (lambda () (utf8->string (content* self)))
            (lambda _ (content* self)))
          (collection? self)))

(define (make-resource . args)
  (apply make <virtual-resource> args))

(define-method (live-properties (self <virtual-resource>))
  (append
   (list (cons ((xml virtual-ns 'isvirtual))
               (make-live-property isvirtual set-isvirtual! remove-isvirtual!)))
   (next-method)))

(define-method (content (self <virtual-resource>) _)
  (content* self))

(define-method (set-content! (self <virtual-resource>) data headers)
  (set! (content* self) data)
  #f)


(define-method (content-type (self <virtual-resource>))
  (content-type* self))

(define-method (set-displayname! (self <virtual-resource>) value)
  (lambda () (set-display-name! self value)))


(define-method (set-isvirtual! (self <virtual-resource>) _) (throw 'protected-property))
(define-method (remove-isvirtual! (_ <virtual-resource>)) (throw 'protected-property))
(define-method (isvirtual (self <virtual-resource>))
  (propstat 200 (list ((xml virtual-ns 'isvirtual) "true"))))



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

(define-method (set-dead-property!! (resource <virtual-resource>) value)
  (typecheck value xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      (set (dead-properties% resource)
           (table-focus (xml-element-hash-key value))
           (just value)))))

(define-method (remove-dead-property!! (resource <virtual-resource>) xml-tag)
  (typecheck xml-tag xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      (table-remove (dead-properties% resource)
                    (xml-element-hash-key xml-tag)))))


(define-method (children (resource <virtual-resource>))
  (modify (table->list (child-table resource))
          (lens-compose each car*) symbol->string))

(define-method (get-child-by-name! (resource <virtual-resource>) (name <string>) create?)
  (table-get (child-table resource) (string->symbol name)))

(define-method (remove-self! (resource <virtual-resource>))
  (when (parent resource)
    (on-child-removed (parent resource) resource))
  (set! (parent resource) #f)
  ;; This assumes that any children of us will be garbage collected.
  ;; This works for this run, since the tree is now gone.
  ;; However, consider the sittuation
  ;;     (make <virtual-resource>
  ;;       children: (list (cons "files" (lambda (p) (make <file-resource>
  ;;                                              path: "/home/hugo"
  ;;                                              parent: p))))))
  ;; Then the file resource would be garbage collected and removed
  ;; from the resource tree. However, at next program start all files
  ;; would still be there, since it would just get re-mounted.
  ;; This might however be expected behaviour, since nested virtual
  ;; resources probably also work that way, at least if they originate from
  ;; a configuration file.
  )

;; Local function to <virtual-resource> module
;; Creates a new virtual resource identical to the old one
(define (copy-self resource depth)
  (make <virtual-resource>
    crollection?: (collection? resource)
    content: (content* resource)
    child-table: (child-table resource)
    dead-properties: (dead-properties% resource)
    display-name: (display-name resource)
    )
  )

;;; TODO this is broken, I think it simply doesn't create the new resource
#;
(define-method (move-resource-implementation!
                (source <virtual-resource>)
                (destination <virtual-resource>)
                name)
  (remove-self! source)
  (set! (parent source) destination)
  (set! (child-table source)
    (table-put (child-table source)
               (string->symbol name)
               source)))


(define-method (create-collection! (resource <virtual-resource>) name headers body)
  (when body (throw 'http 415))

  (set! (collection*? resource) #t)

  (define child
   (make <virtual-resource>
     parent: resource
     collection?: #t))

  (set! (child-table resource)
    (table-put (child-table resource)
               (string->symbol name)
               child))

  child)

(define-method (create-resource! (resource <virtual-resource>) name)
  (set! (collection*? resource) #t)
  (define child (make <virtual-resource> parent: resource))
  (set! (child-table resource)
    (table-put (child-table resource) (string->symbol name)
               child))
  child)


(define-method (on-child-removed (resource <virtual-resource>) (child <resource>))
  (cond ((find (lambda (p) (eq? child (cdr p)))
               (children resource))
         => (lambda (p)
              (set! (child-table resource)
                (table-remove (child-table resource)
                              (string->symbol (car p))))))))


(define-method (mount-resource!
                (resource <resource>) (parent-resource <virtual-resource>) name)
  (when (parent resource)
    (throw 'http 502 "Refusing to mount a resource with parent"))

  (set! (collection*? parent-resource) #t)

  (set! (child-table parent-resource)
    (table-put (child-table parent-resource) (string->symbol name) resource)))
