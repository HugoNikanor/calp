(define-module (calp html config)
  :use-module (calp util config))

(define-config debug #f
  description: (G_ "Places the generated thingy in debug mode"))


;;; NOTE edit mode should preferably depend on login-status of the user
;;; but this works for the time being.
(define-config edit-mode #t
  description: (G_ "Makes the document editable"))
