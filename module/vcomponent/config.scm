(define-module (vcomponent config)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (calp translation)
  :use-module (calp util config)
  :use-module ((vcomponent data-stores common) :select (calendar-data-store?))
  )

;;; TODO These belong to `calp`, they just happen to be here since they work with vcomponent related stuff

(define-config data-stores '()
  description: (G_ "Data stores from which to read events.")
  pre: (lambda (v)
         (and
          (expand-validator
           v (list-of (pair-of string? calendar-data-store?)))
          v)))

(define-config default-calendar ""
  description:
  (G_ "Default calendar to use for operations. Set to empty string to unset. If set, MUST be one of the keys to the data-stores alist.")
  pre: (ensure string?))

