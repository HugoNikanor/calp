(define-module (calp webdav resource calendar-object)
  :use-module (srfi srfi-1)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (vcomponent media-type)
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
  ;; TODO
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

;;; TODO be sure to set a "Vary: accept" header on the resoponse

;; TODO does an HTTP response header informing the client of
;; alternative representations exist? E.g. when being sent text/calendar,
;; the client is informed of the existance of application/calendar+xml
;; and so on.

(define provided-content-types
  (let ()
    (define (get type) (module-ref (resolve-interface `(vcomponent media-type ,@type))
                                   'format))
    ;; TODO gather this set from the module system
    `((text
       (calendar . ,(get '(text calendar))))
      (application
       (calendar+xml . ,(get '(application calendar+xml)))
       (calendar+json . ,(get '(application calendar+json)))))))

(define (parse-media-type-identifier-symbol symb)
  ;; TODO possibly assert that return is of exactly length 2
  (map string->symbol (string-split (symbol->string symb) #\/)))

(define (negotiate-content-type client-accept)
  ;; (typecheck client-accept (or false? (list-of (pair-of symbol? (alist-of symbol? any-type)))))

  ;; NOTE this ignores any */*, meaning that one of those with q higher than something else
  ;; will never be used.
  ;; NOTE this uses the caputured provided-content-types, which is an
  ;; alist of major types to an alist of minor types to media-type format
  ;; objects. Each sub-alist MUST have at least one element.
  ;; Both the inner and outeor sub-list are sorted in the order the server prefers them.
  (or (let loop ((accepted-media-types
                  (map parse-media-type-identifier-symbol
                       (map car       ; we don't care about the q any longer
                            (sort*
                             (or client-accept '())
                             > (lambda (p) (or (assoc-ref (cdr p) 'q) 1000)))))))
        (and (not (null? accepted-media-types))
             (let ((accepted-media-type (car accepted-media-types)))
              (and=> (assoc-ref provided-content-types (car accepted-media-type))
                     (lambda (content-type-group)
                       (if (eq? '* (cadr accepted-media-type))
                           (cdar content-type-group)
                           (assoc-ref content-type-group (cadr accepted-media-type))))))))

      (cdr (car (cdr (car provided-content-types))))))

(define-method (content-type (resource <calendar-object-resource>) headers)
  (media-type (negotiate-content-type (assoc-ref headers 'accept))))

(define-method (content (resource <calendar-object-resource>) headers)
  (define media-format
    (negotiate-content-type (assoc-ref headers 'accept)))

  (values
   (call-with-output-string
     (lambda (port)
       ((serializer media-format)
        (get-by-href (data-store resource) (href resource))
        port)))
   (media-type media-format)))

(define-method (dead-properties (_ <calendar-object-resource>))
  '())

(define-method (get-dead-property (_ <calendar-object-resource>) value)
  (propstat 404 (list value)))

;;; TODO DAV:supported-report-set

;;; TODO REPORT calendar-multiget
