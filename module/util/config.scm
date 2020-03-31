;;; Commentary:

;; This file should define all global configurable variables which
;; doesn't belong anywhere else. The config module should then import
;; this module, and set all configs as needed. The config module
;; should also be able to set configs gotten from other parts.

;;; Code:

(define-module (util config)
  :export (register-config!)
               )

(define-public (ensure pred?)
  (lambda (v)
    (unless (pred? v)
      (error (format #f "Value [~s] doesn't satisfy condition ~a"
               v (or (procedure-name pred?) ""))))
    v))

(define-macro (register-config! name default-value valid-value?)
  `(save-module-excursion
     (lambda ()
       (define mod (resolve-module '(util config all)))
       (set-current-module mod)
       (module-define! mod (quote ,name)
                       (make-parameter ,default-value
                                       ,valid-value?))
       (export ,name))
       ))
