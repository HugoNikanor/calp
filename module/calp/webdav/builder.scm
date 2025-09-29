;;; Commentary:
;;; Module for easily construct webdav trees from configuration files
;;; Code:
(define-module (calp webdav builder)
  :use-module (calp webdav resource)
  :use-module (hnh util)
  :use-module (ice-9 match)
  :export (build-webdav-resource-tree))

;; `(virtual
;;   (("files" (file path: ,(getenv "tmpdir")))
;;    ("virtual"
;;     (virtual
;;      content: ,(string->utf8 "Hello, World\n")))))

(define (build-webdav-resource-tree expr)
  (match expr
    ((type options ... (children ...))
     (let ((resource (apply
                      (module-ref (resolve-interface `(calp webdav resource ,type))
                                  'make-resource)
                      options)))
       (for (name declaration) in children
            (mount-resource! (build-webdav-resource-tree declaration)
                             resource name))
       resource))
    ((type options ...)
     (build-webdav-resource-tree `(,type ,@options ())))))
