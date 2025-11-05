(define-module (calp webdav resource calendar-collection)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource calendar-object)
  :use-module (calp webdav property)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (hnh util)
  :use-module (calp namespaces)
  :use-module (vcomponent data-stores common)
  :use-module ((web uri) :select (string->uri))
  :use-module ((vcomponent formats) :select (serializer))
  :export (<calendar-collection-resource>
           calendar-collection-resource?
           make-resource

           ;; calendar-home-set
           ;; set-calendar-home-set!
           ;; remove-calendar-home-set!
           ))

(define-class <calendar-collection-resource> (<resource>)
  (data-store init-keyword: data-store:
              getter: data-store
              )
  )

(define (callendar-collection-resource? x)
  (is-a? x <calendar-collection-resource>))

(define-method (collection? (_ <calendar-collection-resource>)) #t)

(define-method (content (resource <calendar-collection-resource>) _)
  (format #f "I'm a calendar collection!~%My internal store is ~s~%"
          (data-store resource)))

(define-method (children (resource <calendar-collection-resource>))
  (map (lambda (href)
         (cons href
               (make <calendar-object-resource>
                 store: (data-store resource)
                 href: href)))
   (list-entries/shallow (data-store resource))))

(define-method (get-child-by-name! (resource <calendar-collection-resource>)
                                   name create?)
  (cond ((get-by-href (data-store resource) name)
         => (lambda (child)
              (make <calendar-object-resource>
                store: (data-store resource)
                href: name)))
        ;; TODO handle `create?`
        (else #f)))


(define-method (remove-self! (resource <calendar-collection-resource>))
  'TODO
  (throw 'http 501)
  )

(define-method (on-child-removed (resource <calendar-collection-resource>)
                                 (child <resource>))
  'TODO
  (throw 'http 501)
)

;;; Dead properties could be stored in XML properties on the calendar object

(define-method (dead-properties (_ <calendar-collection-resource>))
  '())

(define-method (get-dead-property (_ <calendar-collection-resource>) value)
  (propstat 404 (list value)))

(define-method (create-collection! (resource <calendar-collection-resource>)
                                   name headers body)
  ;; Calendar collections are allowed to contain other collections, as long as they aren't calendar collections. It's however easier to just ban all collections under calendar collections.
  (throw 'http 403 "Calendar collections may not contain sub-collections."))

(define-method (create-resource! (resource <calendar-collection-resource>) name)
  'TODO
  (throw 'http 501))


(define (make-resource store-declaration)
  ;; TODO support other forms of store declaration than uri strings
  (make <calendar-collection-resource>
    data-store: (-> store-declaration string->uri store-uri->store)))




(define-method (live-properties (resource <calendar-collection-resource>))
  (append
   (next-method)
   (list
    (cons ((xml caldav 'calendar-description))
          (make-live-property calendar-description set-calendar-description! remove-calendar-description!))
    (cons ((xml caldav 'calendar-timezone))
          (make-live-property calendar-timezone set-calendar-timezone! remove-calendar-timezone!))
    (cons ((xml caldav 'supported-calendar-component-set))
          (make-live-property supported-calendar-component-set
                              set-supported-calendar-component-set!
                              remove-supported-calendar-component-set!))
    (cons ((xml caldav 'supported-calendar-data))
          (make-live-property supported-calendar-data
                              set-supported-calendar-data!
                              remove-supported-calendar-data!))

    ;; These are omitted, since we don't need to set any limits.
    ;; However, once live-properties can indicate the absence of a
    ;; registered property, add them with corresponding methods, which
    ;; dispatch to the underlying store.
    ;; - CALDAV:max-resource-size
    ;; - CALDAV:min-date-time
    ;; - CALDAV:max-date-time
    ;; - CALDAV:max-instances
    ;; - CALDAV:max-attendees-per-instance

    (cons ((xml caldav 'supported-collation-set))
          (make-live-property supported-collation-set
                              set-supported-collation-set!
                              remove-supported-collation-set!)))

   ))

(define-method (display-name (resource <calendar-collection-resource>))
  (store-displayname (data-store resource)))

(define-method (set-displayname! (resource <calendar-collection-resource>) value)
  (set-store-displayname! (data-store resource) (xml-text-content value)))

(define-method (remove-displayname! (resource <calendar-collection-resource>))
  (remove-store-displayname! (data-store resource)))

;;; TODO color property?

(define-method (calendar-description (resource <calendar-collection-resource>))
  ;; NOTE xml:lang MUST be preserved
  (cond ((store-description (data-store resource))
         => (lambda (desc)
              (propstat 200 (list ((xml caldav 'calendar-description)
                                   desc)))))
        (else (propstat 404 (list ((xml caldav 'calendar-description)))))))

(define-method (set-calendar-description! (resource <calendar-collection-resource>) value)
  (set-store-description! (data-store resource)
                          (xml-text-content value)))

(define-method (remove-calendar-description! (resource <calendar-collection-resource>))
  (remove-store-description! (data-store resource)))

;;; TODO these should support a content-type parameter on the XML object, to allow xCal usage
(define-method (calendar-timezone (resource <calendar-collection-resource>))
  (cond ((store-calendar-timezone (data-store resource))
         => (lambda (tz)
              (propstat
               200 (list ((xml caldav 'calendar-timezone)
                          (call-with-output-string
                            (lambda (port)
                              ((serializer (@ (vcomponent formats ical) format))
                               tz port))))))))
        (else
         (propstat 404 (list ((xml caldav 'calendar-timezone)))))))

(define-method (set-calendar-timezone! (resource <calendar-collection-resource>) _)
  (throw 'http 501))
(define-method (remove-calendar-timezone! (resource <calendar-collection-resource>))
  (throw 'http 501))

(define-method (supported-calendar-component-set (_ <calendar-collection-resource>))
  (propstat 200
            (list
             ((xml caldav 'supported-calendar-component-set)
              ((xml caldav 'comp `((name . "VEVENT"))))))))

(define-method (set-supported-calendar-component-set! (r <calendar-collection-resource>) _)
  (throw 'protected-property))
(define-method (remove-supported-calendar-component-set! (_ <calendar-collection-resource>))
  (throw 'protected-property))

;;; Note that this is the format used on-wire, and works for ALL data
;;; stores, since we parse the data in this layer.
(define-method (supported-calendar-data (_ <calendar-collection-resource>))
  (propstat 200
            (list
             (apply (xml caldav 'supported-calendar-data)
                    (map (lambda (ct)
                           ((xml caldav 'calendar-data `((content-type . ,ct)
                                                         (version . "2.0")))))
                         ;; TODO automatically generate this list from the module system
                         '("text/calendar"
                           "application/calendar+xml"
                           "application/calendar+json"
                           ))))))
(define-method (set-supported-calendar-data! (r <calendar-collection-resource>) _)
  (throw 'protected-property))
(define-method (remove-supported-calendar-data! (_ <calendar-collection-resource>))
  (throw 'protected-property))

;;; TODO also support this on objects?
(define-method (supported-collation-set (_ <calendar-collection-resource>))
  (propstat 200
            (list
             (apply (xml caldav 'supported-collation-set)
                    (map (xml caldav 'supported-collation)
                         ;; TODO automatically generate this list from actuall provided collations
                         `("i;ascii-casemap" "i;octet"
                           ;; RFC 5051
                           "i;unicode-casemap"
                           ))))))

(define-method (set-supported-collation-set! (r <calendar-collection-resource>) _)
  (throw 'protected-property))
(define-method (remove-supported-collation-set! (_ <calendar-collection-resource>))
  (throw 'protected-property))



(define-method (resourcetype (_ <calendar-collection-resource>))
  (propstat 200 (list ((xml webdav 'resourcetype)
                       ((xml webdav 'collection))
                       ((xml caldav 'calendar))))))

