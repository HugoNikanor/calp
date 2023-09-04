(define-module (vcomponent data-stores virtual)
  :use-module (oop goops)
  :use-module ((srfi srfi-88) :select ())
  :use-module (vcomponent data-stores common)
  :export (make-file-store))

(define-class <virtual-data-store> (<calendar-data-store>)
  )

(define-method (get-all (this <virtual-data-store>))
  #f)

(define-method (get-by-uid (this <virtual-data-store>)
                    (uid <string>))
  #f)


(define-method (color (this <virtual-data-store>))
  "")

(define-method (displayname (this <virtual-data-store>))
  "Virtual Calendar")
