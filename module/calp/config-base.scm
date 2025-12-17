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
            (calp util config)
            ;; (glob) is really slow (0.3s) to load,
            ;; Unclear if it's due to the C parser not expanding properly during macro expansion, or if it's hard to find the actual bindings.
            ;; (glob)
            ))
