;;; Commentary:
;;; Module for easily construct webdav trees from configuration files
;;; Code:
(define-module (calp webdav builder)
  :use-module ((calp webdav resource virtual)
               :select ((make-resource . make-virtual-resource)))
  :use-module ((calp webdav resource base) :select (lookup-resource))
  :use-module ((calp webdav resource) :select (mount-resource!))
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util type)
  :use-module (ice-9 match)
  :export (build-webdav-resource-tree)
  )

;;; Format:
;;; A list of records, where each record is of form
;;; (cons* absolute-path resource-type type-specific-arguments)

(define (build-webdav-resource-tree resources)
  (typecheck resources (list-of (pair-of* string? symbol? list?)))

  (define root-resource (make-virtual-resource "*root*"))

  (for (path-string type . data) in resources
       (match (path-split path-string)
         (("" location ... name)
          (define sub-resource
            (apply
             (module-ref (resolve-interface `(calp webdav resource ,type))

                         'make-resource)
             name data))

          (cond ((lookup-resource root-resource location)
                 => (lambda (direct-parent)
                      (mount-resource! direct-parent sub-resource)))
                (else
                 (scm-error 'misc-error "build-webdav-resource-tree"
                            "Parent path (~s) missing when mounting resource `~s`"
                            (list (path-join (cons "" location)) name)
                            '()))))

         (("")
          (scm-error 'misc-error "build-webdav-resource-tree"
                     "Can't modify root object"
                     '() '()))

         (_
          (scm-error 'misc-error "build-webdav-resource-tree"
                     "All paths must be absolute: ~s"
                     (list path-string)
                     '()))))


  root-resource)
