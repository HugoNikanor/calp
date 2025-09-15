(define-module (calp webdav resource base)
  :use-module ((srfi srfi-1) :select (find remove last append-map drop-while))
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
           ;; href
           href->string
           string->href
           href-relative
           ;; local-path
           name
           dead-properties
           ;; resource-children
           resource?
           children



           get-live-property
           get-dead-property
           get-property

           set-dead-property!!
           set-live-property!!
           set-property!!
           set-property!

           remove-dead-property!!
           remove-live-property!!
           remove-property!!
           remove-property!


           setup-new-resource!
           setup-new-collection!



           live-properties
           add-child!
           add-resource!
           add-collection!
           is-collection?

           content
           set-content!

           copy-resource
           copy-to-location!
           move-to-location!
           cleanup-resource
           delete-child!
           setup-new-resource!
           ;; prepare-for-add!

           creationdate
           displayname
           getcontentlanguage
           getcontentlength
           getcontenttype
           getetag
           getlastmodified
           lockdiscovery
           resourcetype
           supportedlock

           webdav-properties

           ;; absolute-path
           ;; find-resource
           lookup-resource
           all-resources-under

           ;; dereference

           make-live-property
           live-property?
           property-getter
           property-setter-generator
           property-remover-generator

           prepare-update-properties

           ))


(define-type (live-property)
  (property-getter            keyword: getter  type: procedure?)
  (property-setter-generator  keyword: setter  type: procedure?)
  (property-remover-generator keyword: remover type: (or false? procedure?)))


(define* (make-live-property getter setter-generator optional: remover-generator)
  (live-property getter: getter
                 setter: setter-generator
                 remover: remover-generator))


;; Collections are also resources, this is non-collection resources
(define-class <resource> ()
  ;; (href init-keyword: href: getter: href init-value: #f)
  ;; (local-path init-keyword: local-path: getter: local-path)

  ;; name is a part of its search path.
  ;; For example: the component located at /a/b
  ;; would have name="a", its parent name="b", and the root element
  ;; would have an unspecified name (probably the empty string, or "*root*")
  (name init-keyword: name: getter: name)

  (dead-properties
   ;; Map from (namespace . tagname) pairs to namespaced xml element
   ;; init-form: (make-hash-table)
   init-form: '()
   accessor: dead-properties%)

  ;; Attributes on data
  (displayname accessor: displayname* init-value: #f)
  (contentlanguage accessor: contentlanguage init-value: #f)

  ;; Direct children, used by @code{children} if not overwritten by child
  (resource-children init-value: '()
                     accessor: resource-children)

  ;; Table containing href -> resource mappings, saves us from recursivly searching children each time.
  (resource-cache init-value: (make-hash-table 0)
                  getter: resource-cache))


(define-method (initialize (self <resource>) args)
  (next-method)
  ;; TODO (name self) when name is unbound gives a really weird error
  (typecheck (name self)            string?             "<resource>.name")
  (typecheck (displayname* self)    (or false? string?) "<resource>.displayname")
  (typecheck (contentlanguage self) (or false? string?) "<resource>.contentlanguage")
  ;; (typecheck (dead-properties self) (list-of xml-element?) "<resource>.dead-properties")
  ;; (typecheck (resource-children self) ...)
  )

(define (resource? x)
  (is-a? x <resource>))


(define (href->string href)
  (typecheck href (list-of string?))

  (if (null? href)
      "/" (string-join href "/" 'prefix)))

(define (string->href s)
  (typecheck s string?)

  (remove string-null?
          (string-split s #\/)))

;; parent must be the head of child, elements in child after that is "free range"
(define (href-relative parent child)
  (typecheck parent (list-of string?))
  (typecheck child (list-of string?))

  (cond ((null? parent) child)
        ((null? child) (scm-error 'misc-error "href-relative" "Not a sub-href" '() #f))
        ((equal? (car parent) (car child))
         (href-relative (cdr parent) (cdr child)))
        (else (scm-error 'misc-error "href-relative" "Not a sub-href" '() #f))))

(define-method (children (self <resource>))
  (resource-children self))

;;; TODO merge content and set-content! into an accessor?
(define-method (content (self <resource>))
  (throw 'misc-error "content<resource>"
         "Base <resource> doesn't implement (getting) content, please override this method"
         '() #f))

(define-method (set-content! (self <resource>) content)
  (throw 'misc-error "set-content!<resource>"
         "Base <resource> doesn't implement (setting) content, please override this method"
         '() #f))

(define-method (content-length (self <resource>))
  (if (is-collection? self)
      0 ; TODO maybe return number of children
      (let ((c (content self)))
        (cond ((bytevector? c) (bytevector-length c))
              ((string? c) (string-length c))
              (else -1)))))

(define-method (write (self <resource>) port)
  (catch #t
    (lambda ()
      (display ; Make output atomic
       (call-with-output-string
         (lambda (port)
           (format port "#<~a name=~s"
                   (class-name (class-of self))
                   (name self))
           (cond ((displayname self)
                  propstat-200?
                  (lambda (name) (format port ", displayname=~s" name))))
           (format port ">")))
       port))
    (lambda _
      (format port "#<~a>" (class-name (class-of self))))))


(define (add-resource! self new-name content)
  (if (lookup-resource self (list new-name))
      (throw 'resource-exists)
      (let ((resource (make (class-of self) name: new-name)))
        (add-child! self resource collection?: #f)
        (set-content! resource content)
        resource)))

(define (add-collection! self new-name)
  (if (lookup-resource self (list new-name))
      (throw 'resource-exists)
      (let ((resource (make (class-of self) name: new-name)))
        (add-child! self resource collection?: #t)
        resource)))

(define (initialize-copied-resource! source copy)
  (for-each (lambda (tag) ((set-dead-property!! copy tag)))
            (dead-properties source))
  (set! (displayname* copy)    (displayname* source)
        (contentlanguage copy) (contentlanguage source))
  ;; (format (current-error-port) "Setting content! ~s (~s)~%" copy source)
  (when (content source)
    (set-content! copy (content source)))
  ;; resource-cache should never be copied
  )

(define-method (copy-resource (self <resource>) include-children?)
  (copy-resource self include-children? #f))

(define-method (copy-resource (self <resource>) include-children? new-name)
  (let ((resource (make (class-of self) name: (or new-name (name self)))))
    (initialize-copied-resource! self resource)
    (when include-children?
      (for-each (lambda (c) (add-child! resource c))
                (map (lambda (c) (copy-resource c #t))
                     (children self))))
    resource))

;; source and target-parent should be resource instances
;; new-name a string
;; include-children? and overwrite? booleans
(define* (copy-to-location! source target-parent
                            key:
                            (new-name (name source))
                            include-children?
                            overwrite?
                            )
  (let ((copy (make (class-of source) name: new-name))
        ;; Take copy if child list. If we run `cp -r / /c` then;
        ;; (at least when /c already exists) our child list gets
        ;; updated, leading to an infinite loop if we use
        ;; `(children source)` directly below.
        (children-before (children source)))
    (let ((status (add-child! target-parent copy
                              ;; (is-collection? copy) doesn't work for
                              ;; all types, since it's not quite yet
                              ;; added (for example: <file-resoure>
                              ;; checks if the target resource is a
                              ;; directory on the file system).
                              collection?: (is-collection? source)
                              overwrite?: overwrite?)))
      (case status
        ((created replaced)
         (initialize-copied-resource! source copy)
         (when include-children?
           (for-each (lambda (c) (copy-to-location!
                             c copy
                             include-children?: #t))
                     children-before))
         status)
        ((collision) 'collision)))))

(define* (move-to-location! source-parent source target-parent
                            key:
                            (new-name (name source))
                            overwrite?)
  (let ((status (copy-to-location! source target-parent
                                   new-name: new-name
                                   include-children?: #t
                                   overwrite?: overwrite?)))
    (case status
      ((created replaced)
       (delete-child! source-parent source)
       status)
      ((collision) 'collision))))


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

;;; TODO should {get,set}{,-{dead,live}}-property really be methods?
;;; - Live properties are defined by lookup-live-property, which isn't a
;;;   method, which in turn calls live-properties, which MUST be a method.
;;; - Dead properties may have a reason. For example, file resources might
;;;   want to store them directly in xattrs, ignoring its built in hash-table.
;;; - The combined should always just dispatch to either one

(define-method (get-live-property (resource <resource>) xml-el)
  (typecheck xml-el xml-element?)

  (cond ((lookup-live-property resource xml-el)
         => (lambda (prop) ((property-getter prop) resource)))
        (else (propstat 404 (list xml-el)))))

(define-method (get-dead-property (resource <resource>) xml-el)
  (typecheck xml-el xml-element?)

  (cond ((find-child xml-el (dead-properties% resource))
         => (lambda (it) (propstat 200 (list it))))
        (else (propstat 404 (list xml-el)))))

;;; Return a list of xml elements, where each entry is a property
(define-method (dead-properties (resource <resource>))
  (dead-properties% resource))

(define-method (set-dead-property!! (resource <resource>) value)
  (typecheck value xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      ;; TODO replace this with lens
      (let loop ((rem (dead-properties% resource)))
        (cond ((null? rem)
               ;; Append
               (list value))
              ((equal? (xml-element-hash-key value)
                       (xml-element-hash-key (car rem)))
               ;; Replace
               (cons value (cdr rem)))
              (else (loop (cdr rem))))))))

;; Return a promise which performs the set operation.
;; Pre-conditions can cause this function to throw
(define-method (set-live-property!! (resource <resource>) value)
  (typecheck value xml-element?)
  (cond ((lookup-live-property resource value)
         ;; NOTE this drops any (xml) attributes from the value object
         => (lambda (prop) (apply (property-setter-generator prop)
                             resource (xml-element-children value))))
        (else #f)))

;; Returns a promise, which when evaluated, attempts to physically set
;; the property. This procedure might fail due to pre-conditions,
;; and the actuall fail might also fail
(define (set-property!! resource value)
  (or (set-live-property!! resource value)
      (set-dead-property!! resource value)))

(define (set-property! resource value)
  ((set-property!! resource value)))

;;; The remove-* procedures still take "correct" namespaced sxml (so an
;;; xml-element object inside a list). These extra lists are a bit of a waste,
;;; But allows remove-* to have the same signature as set-*

(define-method (remove-dead-property!! (resource <resource>) xml-tag)
  (typecheck xml-tag xml-element?)
  (lambda ()
    (set! (dead-properties% resource)
      (remove (lambda (el)
                (equal? (xml-element-hash-key el)
                        (xml-element-hash-key xml-tag)))
              (dead-properties% resource)))))

(define-method (remove-live-property!! (resource <resource>) xml-tag)
  (typecheck xml-tag xml-element?)

  (cond ((lookup-live-property resource xml-tag)
         => (lambda (prop)
              (cond ((property-remover-generator prop)
                     => (lambda (f) (f resource)))
                    (else (throw 'irremovable-live-property)))))
        (else #f)))

(define (remove-property!! resource xml-tag)
  (or (remove-live-property!! resource xml-tag)
      (remove-dead-property!! resource xml-tag)))

(define (remove-property! resource xml-tag)
  ((remove-property!! resource xml-tag)))



;; xml-tag should be just the tag element, without a surounding list
(define-method (get-property (resource <resource>) xml-tag)
  (typecheck xml-tag xml-element?)

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

(define-method (setup-new-resource! (this <resource>) (parent <resource>))
  'noop)

(define-method (setup-new-collection! (this <resource>) (parent <resource>))
  'noop)

(define (add-child* this child collection?)
  (setup-new-resource! child this)
  (when collection?
    (setup-new-collection! child this))
  (set! (resource-children this)
    (cons child (resource-children this))))

(define* (add-child! this child
                     key:
                     overwrite?
                     (collection? (is-collection? child)))
  (let ((existing (lookup-resource this (list (name child)))))
    (cond ((and overwrite? existing)
           (delete-child! this existing)
           (add-child* this child collection?)
           'replaced)
          (existing 'collision)
          (else
           (add-child* this child collection?)
           'created))))


;; Free any aditional system resources held by this object.
;; For example, file resources will remove the underlying file here.
(define-method (cleanup-resource (this <resource>))
  'noop)

(define-method (delete-child! (this <resource>) (child <resource>))
  (set! (resource-children this)
    (delq1! child (children this)))
  (for-each (lambda (grandchild)
              (delete-child! child grandchild))
            (children child))
  (cleanup-resource child))



;;; TODO rename to simply @code{collection?}
(define-method (is-collection? (self <resource>))
  (not (null? (resource-children self))))




(define-method (creationdate (self <resource>))
  (propstat 501 (list ((xml webdav 'creationdate)))))

(define-method (set-creationdate! (self <resource>) _)
  (throw 'protected-resource "creationdate"))

(define-method (displayname (self <resource>))
  (cond ((displayname* self)
         => (lambda (name)
              (propstat 200 (list ((xml webdav 'displayname)
                                   name)))))
        (else
         (propstat 404 (list ((xml webdav 'displayname)))))))

(define-method (set-displayname! (self <resource>) value)
  (lambda () (set! (displayname* self) value)))

(define-method (getcontentlanguage (self <resource>))
  (cond ((contentlanguage self)
         => (lambda (lang) (propstat 200 (list ((xml webdav 'getcontentlanguage) lang)))))
        (else (propstat 404 (list ((xml webdav 'getcontentlanguage)))))))

(define-method (set-getcontentlanguage! (self <resource>) value)
  (lambda () (set! (contentlanguage self) value)))

(define-method (getcontentlength (self <resource>))
  (propstat 501 (list ((xml webdav 'getcontentlength)))))

(define-method (getcontentlength (self <resource>))
  (propstat 200
            (list
             ((xml webdav 'getcontentlength)
              (number->string
               (content-length self))))))

(define-method (set-getcontentlength! (self <resource>) _)
  (throw 'protected-resource "getcontentlength"))

(define-method (getcontenttype (self <resource>))
  (propstat 501 (list ((xml webdav 'getcontenttype)))))

(define-method (set-getcontenttype! (self <resource>) _)
  (throw 'protected-resource "getcontenttype"))

(define-method (getetag (self <resource>))
  ;; TODO
  (propstat 501 (list ((xml webdav 'getetag)))))

(define-method (set-getetag! (self <resource>) _)
  (throw 'protected-resource "getetag"))

(define-method (getlastmodified (self <resource>))
  (propstat 200 (list ((xml webdav 'getlastmodified)
                       (with-locale1
                        LC_TIME "C"
                        (lambda ()
                          (datetime->string (unix-time->datetime 0) "~a, ~d ~b ~Y ~H:~M:~S GMT")))))))

(define-method (set-getlastmodified! (self <resource>) _)
  (throw 'protected-resource "getlastmodified"))

(define-method (lockdiscovery (self <resource>))
  (propstat 200 (list ((xml webdav 'lockdiscovery)))))

(define-method (set-lockdiscovery! (self <resource>) _)
  (throw 'protected-resource "lockdiscovery"))

(define-method (resourcetype (self <resource>))
  (propstat 200 (list (apply (xml webdav 'resourcetype)
                             (when (is-collection? self)
                               (list ((xml webdav 'collection))))))))

(define-method (set-resourcetype! (self <resource>) _)
  (throw 'protected-resource "resourcetype"))

(define-method (supportedlock (self <resource>))
  (propstat 200 (list ((xml webdav 'supportedlock)))))

(define-method (set-supportedlock! (self <resource>) _)
  (throw 'protected-resource "supportedlock"))

(define webdav-properties
  `((creationdate       . ,(make-live-property creationdate set-creationdate!))
    (displayname        . ,(make-live-property displayname set-displayname!))
    (getcontentlanguage . ,(make-live-property getcontentlanguage set-getcontentlanguage!))
    (getcontentlength   . ,(make-live-property getcontentlength set-getcontentlength!))
    (getcontenttype     . ,(make-live-property getcontenttype set-getcontenttype!))
    (getetag            . ,(make-live-property getetag set-getetag!))
    (getlastmodified    . ,(make-live-property getlastmodified set-getlastmodified!))
    (lockdiscovery      . ,(make-live-property lockdiscovery set-lockdiscovery!))
    (resourcetype       . ,(make-live-property resourcetype set-resourcetype!))
    (supportedlock      . ,(make-live-property supportedlock set-supportedlock!))))



;;; TODO remove! This is a remnant of the old mount system
;; (define-method (dereference (self <resource>))
;;   self)

(define (find-resource resource path)
  ;; Resource should be a <resource> (or something descended from it)
  ;; path should be a list of strings
  (cond ((null? path) resource)
        ((string-null? (car path))
         ;; resource
         (find-resource resource (cdr path)))
        ((find (lambda (r) (string=? (car path) (name r)))
               (children resource))
         => (lambda (r) (find-resource r (cdr path))))
        (else #f)))

;; Lookup up a given resource first in the cache,
;; Then in the tree
;; and finaly fails and returns #f
(define (lookup-resource root-resource path)
  (find-resource root-resource path)
  #;
  (or (hash-ref (resource-cache root-resource) path)
      (and=> (find-resource root-resource path)
             (lambda (resource)
               (hash-set! (resource-cache root-resource) path resource)
               resource))))

(define* (all-resources-under* resource optional: (prefix '()))
  (define s (append prefix (list (name resource))))
  (cons (cons s resource)
        (append-map (lambda (c) (all-resources-under* c s))
                    (children resource))))

;; Returns a flat list of this resource, and all its decendants
(define* (all-resources-under resource optional: (prefix '()))
  (cons (cons prefix resource)
        (append-map (lambda (c) (all-resources-under* c prefix))
                    (children resource))))
