;;; Commentary:
;;; Principal resources per RFC 3744, specialised for unix accounts.
;;; Code:

(define-module (calp webdav resource principal)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  ;; :use-module (calp webdav resource virtual)
  :use-module (calp webdav property)
  :use-module (calp namespaces)
  :use-module (oop goops)
  :use-module (sxml namespaced)
  :use-module (hnh util)
  :export (<principal-resource>
           principal-resource?
           make-resource

           calendar-home-set
           set-calendar-home-set!
           remove-calendar-home-set!
           ))


(define-class <principal-resource> (<virtual-resource>)
  ;; TODO underlying principal source
  ;; This would be some form of database which manages principals,
  ;; such as an ldap server.
  )

(define (principal-resource? x)
  (is-a? x <principal-resource>))

(define (make-resource . args)
  (apply make <principal-resource> args))

(define-method (collection? (_ <principal-resource>)) #t)

;; (define-method (children (resource <principal-resource>))
;;   ;; TODO
;;   '())

;;; Place where MKCALENDAR is supported
(define-method (calendar-home-set (_ <principal-resource>))
  (propstat
   200
   (list
    ((xml caldav 'calendar-home-set)
     ;; TODO this REALLY shouldn't be hard coded
     ((xml webdav 'href) "http://localhost:8888/users/hugo/calendars")))))

(define-method (set-calendar-home-set! (_ <principal-resource>))
  (throw 'protected-property))

(define-method (remove-calendar-home-set! (_ <principal-resource>))
  (throw 'protected-property))

(define-method (live-properties (self <principal-resource>))
  (append
   (list (cons ((xml caldav 'calendar-home-set))
               (make-live-property calendar-home-set
                                   set-calendar-home-set!
                                   remove-calendar-home-set!)))
   (next-method)))

(define-method (collection (resource <principal-resource>))
  "Listing users not supported\n")

(define-method (dead-properties (_ <principal-resource>))
  '())

(define-method (get-dead-property (_ <principal-resource>) value)
  (propstat 404 (list value)))

;; (define-method (set-dead-property!! (_ <principal-resource>) value))
;; (define-method (remove-dead-property!! (_ <principal-resource>) value))

;;; TODO implement privileges

;;; TODO implement properties
;;; DAV:owner
;;; DAV:group
;;; DAV:supported-privilege-set
;;; DAV:current-user-privilege-set
;;; DAV:acl
;;; DAV:acl-restrictions
;;; DAV:inherited-acl-set
;;; DAV:principal-collection-set

;;; TODO advertice access-control to OPTIONS requests


;;; TODO add webdav:principal to resourcetype

;;; TODO current-user-principal should return /users/<hugo> on all endpoints!

