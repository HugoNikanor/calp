;;; Commentary:
;;; This is a fake resource type for the calendaring REPORTs.
;;; Many of the reports uses a system very similar to PROPFIND for
;;; specifying which data to be returned, but allonging the
;;; <C:calendar-data /> pseudo-property. This resource type simply
;;; wraps a single vcomponent (assumed to be a vcalendar object with only
;;; one logical entry inside) into the WebDAV resource system, and adds
;;; the "property" C:calendar-data.
;;; Code:
(define-module (calp webdav resource virtual-calendar-object)
  :use-module (oop goops)
  :use-module (calp webdav resource)
  :use-module (calp webdav property)
  :use-module ((calp namespaces) :select (caldav))
  :use-module (sxml namespaced)
  :use-module ((vcomponent data-stores report-canonical)
               :select (execute-calendar-data))
  :use-module ((vcomponent media-type) :select (serializer media-type resolve-media-type))
  :use-module (calp webdav resource virtual)
  :use-module (hnh util type)
  :use-module ((vcomponent) :select (vcalendar?))
  :export (<virtual-calendar-object-resource>))


;;; TODO document reasons for the following:
;;; - TODO make-resource explicitly omitted
;;; - TODO a bunch of the "required" methods also omitted

(define-class <virtual-calendar-object-resource>
   ;; TODO should we extend <calendar-object-resource> instead?
  (<virtual-resource>)

  (component init-keyword: component:
             getter: component))

(define-method (initialize (self <virtual-calendar-object-resource>) args)
  (next-method)
  (typecheck (component self) vcalendar?))

(define-method (set-calendar-data! (self <virtual-calendar-object-resource>) _)
  (throw 'protected-property))
(define-method (remove-calendar-data! (self <virtual-calendar-object-resource>) _)
  (throw 'protected-property))
(define-method (calendar-data (self <virtual-calendar-object-resource>) calendar-data)
  (define media-format
   (resolve-media-type
    (or (attribute calendar-data 'content-type) "text/calendar")))
  (propstat 200
            (list
             ((xml caldav 'calendar-data `((content-type . ,(media-type media-format))
                                           (version . "2.0")))
              (call-with-output-string
                (lambda (port)
                  ((serializer media-format)
                   (execute-calendar-data (component self) calendar-data)
                   port)))))))

(define-method (live-properties (self <virtual-calendar-object-resource>))
  (append
   (list (cons ((xml caldav 'calendar-data))
               (make-live-property
                calendar-data set-calendar-data! remove-calendar-data!)))))
