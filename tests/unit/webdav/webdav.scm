(define-module (test webdav)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-1)
  :use-module (oop goops)
  :use-module (calp namespaces)
  :use-module (datetime)

  :use-module (calp webdav property)
  :use-module (calp webdav resource)
  :use-module (calp webdav resource virtual)
  )

;;; NOTE these tests don't check that XML namespaces work correctly, but only as
;;; far as not checking that the correct namespace is choosen. They should fail if
;;; namespacing gets completely broken.

;;; TODO tests for a missing resource?





;;; TODO what am I doing here?




'((calp webdav resource virtual))
