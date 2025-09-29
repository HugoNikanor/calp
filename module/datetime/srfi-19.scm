(define-module (datetime srfi-19)
  :use-module ((datetime) :prefix #{dt:}#)
  :use-module ((srfi srfi-19) :prefix #{19:}#)
  :export (datetime->srfi-19-date
           date->srfi-19-date
           time->srfi-19-time))


(define (datetime->srfi-19-date dt)
  (let ((d (dt:datetime-date dt))
        (t (dt:datetime-time dt)))
   (19:make-date
    0
    (dt:second t) (dt:minute t) (dt:hour t)
    (dt:day d) (dt:month d) (dt:year d)
    0                                   ; TODO zone offset
    )))

(define (date->srfi-19-date d)
  ;; TODO zone offset
  (19:make-date 0 0 0 0 (dt:day d) (dt:month d) (dt:year d) 0))

(define (time->srfi-19-date t)
  (19:make-date 0 (dt:second t) (dt:minute t) (dt:hour t)
                0 0 0
                0))
