(define-module (calp webdav resource calendar-home)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (hnh util)
  :use-module (calp webdav resource calendar-collection)
  :export (<calendar-home-resource>
           calendar-home-resource?
           make-resource

           ;; calendar-home-set
           ;; set-calendar-home-set!
           ;; remove-calendar-home-set!
           ))


(define-class <calendar-home-resource> (<virtual-resource>))

(define (calendar-home-resoruce? x)
  (is-a? x <calendar-home-resource>))

(define (make-resource . args)
  (apply make <calendar-home-resource> args))

(define-method (collection? (_ <calendar-home-resource>)) #t)

(define-method (content-length (_ <calendar-home-resource>))
  0)

;;; Radicale has Allow: MKCALENDAR and DAV: calendar-access on *all* resources.

;;; TODO report calendar-access in the DAV header of OPTIONS

;;; TODO support MKCALENDAR



;; o  MUST support WebDAV ACL [RFC3744] with the additional privilege
;;    defined in Section 6.1 of this document;




;; o  MUST advertise support on all calendar collections and calendar
;;    object resources for the calendaring reports in the DAV:supported-
;;    report-set property, as defined in Versioning Extensions to WebDAV
;;    [RFC3253].
