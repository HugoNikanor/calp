(define-module (vcomponent data-stores file)
  :use-module (oop goops)
  :use-module ((srfi srfi-88) :select ())
  :use-module ((calp) :select (prodid))
  :use-module (vcomponent data-stores common)
  :use-module ((vcomponent formats ical) :select (serialize deserialize))
  )

(define-class <file-data-store> (<calendar-data-store>)
  (path getter: path
        init-keyword: path:))

(define (make-file-store path)
  (make <file-store> path: path))

(define-method (get-all (this <file-data-store>))
  ;; X-WR-CALNAME ⇒ NAME
  ;; X-WR-CALDESC
  (call-with-input-file (path this)
    deserialize))

(define-method (get-by-uid (this <file-data-store>) (uid <string>))
  #f
  )

(define-method (queue-write (this <file-data-store>) vcomponent)
  )

(define-method (flush (this <file-data-store>))
  (with-atomic-output-to-file (path this)
    (lambda () (serialize (data this) (current-output-port))))
  )
