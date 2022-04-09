(define-module (calp html config)
  :use-module (hnh util)
  :use-module (calp util config)
  :use-module (calp translation)
  )

(define-config debug #f
  description: (_ "Places the generated thingy in debug mode"))


;;; NOTE edit mode should preferably depend on login-status of the user
;;; but this works for the time being.
(define-config edit-mode #t
  description: (_ "Makes the document editable"))
