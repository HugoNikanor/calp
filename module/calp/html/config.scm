(define-module (calp html config)
  :use-module (hnh util)
  :use-module (calp util config)
  :use-module (calp translation)
  )

(define-public debug (make-parameter #f))
(define-config debug #f
  description: (_ "Places the generated thingy in debug mode")
  post: debug)


;;; NOTE edit mode should preferably depend on login-status of the user
;;; but this works for the time being.
(define-public edit-mode (make-parameter #t))
(define-config edit-mode #t
  description: (_ "Makes the document editable")
  post: edit-mode)

