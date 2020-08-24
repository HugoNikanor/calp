(define-module (calp util hooks)
  :export (shutdown-hook))

;; Run before program terminates
(define-once shutdown-hook
  (make-hook 0))
