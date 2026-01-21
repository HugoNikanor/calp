(define-module (vulgar info)
  :use-module ((srfi srfi-1) :select (car+cdr))
  :use-module (srfi srfi-71)
  :export (get-terminal-size))

;; Sort-of backwards subprocess call since we want the current terminal to be
;; inherited by stty
(define (get-terminal-size)
 (let ((rpipe wpipe (car+cdr (pipe))))
   (if (= 0 (system (format #f "stty size > /proc/~s/fd/~s"
                        (getpid) (port->fdes wpipe))))
       (let* ((h (read rpipe))
              (w (read rpipe)))
         (close rpipe)
         (close wpipe)
         (values h w))
       (values 0 0))))
