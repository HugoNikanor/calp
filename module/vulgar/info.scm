(define-module (vulgar info)
  :use-module (calp util))

(define-public (get-terminal-size)
 (let* (((rpipe . wpipe) (pipe)))
   (system (format #f "stty size > /proc/~s/fd/~s"
                   (getpid) (port->fdes wpipe)))
   (values (read rpipe)
           (read rpipe))))
