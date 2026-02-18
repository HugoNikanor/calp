;;; Commentary:
;;; A "Calendar Home" is a directory in which the user is allowed
;;; to create new calendars through the MKCALENDAR method.
;;; This might eventually stop being a dedicated resource type, and
;;; instead simply become a composable attribute onto other stores.
;;; Code:
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

(define (with-output-to-bytevector codec thunk)
  (define-values (port get-bytevector)
    ((@ (rnrs io ports) open-bytevector-output-port)))
  (set-port-encoding! port codec)
  (with-output-to-port port thunk)
  (begin1
   (get-bytevector)
   (close-port port)))

(define-method (content-type (_ <calendar-home-resource>))
  "text/html; charset=UTF-8")

(define-method (content (resource <calendar-home-resource>) _)
  (with-output-to-bytevector
   "UTF-8"
   (lambda ()
     (display "<!DOCTYPE html>") (newline)
     ((@ (sxml html) sxml->html)
      `(html (@ (lang "en"))
             (head
              (title "Calendar list"))
             (body
              (h1 "Calendar list")
              (ul
               ,@(for (name . child) in (children resource)
                      `(li (a (@ (href ,name))
                              ,(or (display-name child) name)
                              " "
                              (code ,(class-name (class-of child)))
                              ;; ,(if (calendar-collection-resource? child)
                              ;;      (display-name child))
                              ))))))))))

;;; Radicale has Allow: MKCALENDAR and DAV: calendar-access on *all* resources.

;;; TODO report calendar-access in the DAV header of OPTIONS

;;; TODO support MKCALENDAR



;; o  MUST support WebDAV ACL [RFC3744] with the additional privilege
;;    defined in Section 6.1 of this document;




;; o  MUST advertise support on all calendar collections and calendar
;;    object resources for the calendaring reports in the DAV:supported-
;;    report-set property, as defined in Versioning Extensions to WebDAV
;;    [RFC3253].
