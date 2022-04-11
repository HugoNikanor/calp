;;; Commentary:
;; Module intended to be included by user config, which exports some base
;; bindings basically everyone should want.
;;; Code:

(define-module (calp config-base))

(define cm (module-public-interface (current-module)))
(for-each (lambda (name) (module-use! cm (resolve-interface name)))
          `((ice-9 regex)
            (srfi srfi-1)
            (srfi srfi-88)
            (datetime)
            (vcomponent)
            (glob)
            ))
