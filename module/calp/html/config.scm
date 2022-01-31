(define-module (calp html config)
  :use-module (hnh util)
  :use-module (calp util config)
  )

(define-public debug (make-parameter #f))
(define-config debug #f
  description: "Places the generated thingy in debug mode"
  post: debug)


;;; NOTE edit mode should preferably depend on login-status of the user
;;; but this works for the time being.
(define-public edit-mode (make-parameter #t))
(define-config edit-mode #t
  description: "Makes the document editable"
  post: edit-mode)

