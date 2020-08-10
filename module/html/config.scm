(define-module (html config)
  :use-module (util)
  :use-module (util config)
  )

(define-public debug (make-parameter #f))
(define-config debug #f
  "Places the generated thingy in debug mode"
  post: debug)


;;; NOTE edit mode should preferably depend on login-status of the user
;;; but this works for the time being.
(define-public edit-mode (make-parameter #t))
(define-config edit-mode #t
  "Makes the document editable"
  post: edit-mode)

