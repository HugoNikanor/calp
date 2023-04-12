(define-module (vcomponent data-stores common)
  :use-module ((srfi srfi-88) :select ())
  :use-module (oop goops)
  :export (<calendar-data-store>
           ;; path
           get-all
           get-by-uid))


(define-class <calendar-data-store> ()
  ;; (path init-keyword: path:
  ;;       getter: path)
  )


;;; In (calp server routes)




;;; Load - Load store into memero
;;; Dump - Save store into "disk"


(define-method (get-all (this <calendar-data-store>))
  (scm-error 'not-implemented "get-all"
             "Get-all is not implemented for ~s"
             (class-of this)
             #f))

(define-method (get-by-uid (this <calendar-data-store>) (uid <string>))
  (scm-error 'not-implemented "get-by-uid"
             "Get-by-uid is not implemented for ~s"
             (class-of this)
             #f))


(define-method (color (this <calendar-data-store>))
  "")


(define-method (displayname (this <calendar-data-store>))
  "")
