(define-module (calp html util)
  :use-module (hnh util))


(define-public (date-link date)
  ((@ (datetime) date->string) date "~Y-~m-~d"))


;; Generate an html id for an event.
;; TODO? same event placed multiple times, when spanning multiple cells
(define-public html-id
  (let ((id (make-object-property)))
    (lambda (ev)
      (or (id ev)
          (set/r! (id ev) (symbol->string (gensym "__html_id_")))))))
