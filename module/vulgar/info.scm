(define-module (vulgar info)
  :use-module (hnh util))

;; Sort-of backwards subprocess call since we want the current terminal to be
;; inherited by stty
(define-public (get-terminal-size)
 (let* (((rpipe . wpipe) (pipe)))
   (system (format #f "stty size > /proc/~s/fd/~s"
                   (getpid) (port->fdes wpipe)))
   (values (read rpipe)
           (read rpipe))))
