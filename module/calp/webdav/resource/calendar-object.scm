(define-module (calp webdav resource calendar-object)
  :use-module (srfi srfi-1)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (vcomponent formats)
  :use-module (vcomponent data-stores common)
  :export (<calendar-object-resource>
           calendar-object-resource?
           make-resource

           ;; calendar-home-set
           ;; set-calendar-home-set!
           ;; remove-calendar-home-set!
           ))

(define-class <calendar-object-resource> (<resource>)
  (data-store init-keyword: store: getter: data-store)
  (href init-keyword: href: getter: href)
  )

;;; TODO make-resource


;;; TODO
;;; - this should report supporting MKCALENDAR (but must fail when attempted) ??
;;; - is should report DAV: calendar-access

(define-method (collection? (_ <calendar-object-resource>)) #f)

(define-method (children (_ <calendar-object-resource>)) '())

(define-method (set-content! (resource <calendar-object-resource>) content headers)
  ;; check headers for content type
  ;; - text/calendar
  ;; - application/calendar+xml
  ;; - (application/xml)?

  ;; Parse as the given type
  ;; validate the semantics of the object by checkif if it
  ;; - is part of the supported-calendar-component-set?
  ;; - contains proper timezone info?
  ;; - only contains one calendar resource?
  )


;;; TODO gather this set from the module system
(define content-types
  (alist->table
   (list
    (cons 'text/calendar             (@ (vcomponent formats ical) format))
    (cons 'application/calendar+xml  (@ (vcomponent formats xcal) format))
    (cons 'application/calendar+json (@ (vcomponent formats jcal) format))
    )))

(define-method (content-type (resource <calendar-object-resource>) headers)
  "text/calendar")

(define-method (content (resource <calendar-object-resource>) headers)
  ;; Retrive content from underlying store
  ;; Ensure that METHOD is unset
  ;; Serialize it depending on the `Accept` header.
  ;; Generate appropriate etags
  ;; Set appropirate content-type
  ;; send off content

  ;; Sort accept by quality (undefined = 1)
  ;; Find first content type which matches what we provide
  ;; if no match, return default

  (define content-type
    (or
     (find (lambda (ct) (table-get content-types ct))
           (map car
                (sort*
                 (or (assoc-ref headers 'accept) '((*/*)))
                 > (lambda (p) (or (assoc-ref (cdr p) 'q) 1000)))))
     'text/calendar))

  (values
   (call-with-output-string
     (lambda (port)
       ((serializer (table-get content-types content-type))
        (get-by-href (data-store resource) (href resource))
        port)))
   (symbol->string content-type))
  )

(define-method (dead-properties (_ <calendar-object-resource>))
  '())

(define-method (get-dead-property (_ <calendar-object-resource>) value)
  (propstat 404 (list value)))

;;; TODO DAV:supported-report-set
