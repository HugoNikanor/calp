(define-module (html util)
  :use-module (util))

;; Retuns an HTML-safe version of @var{str}.
(define-public (html-attr str)
  (define cs (char-set-adjoin char-set:letter+digit #\- #\_))
  (string-filter (lambda (c) (char-set-contains? cs c)) str))

(define-public (date-link date)
  ((@ (datetime) date->string) date "~Y-~m-~d"))



;; Generate an html id for an event.
;; TODO? same event placed multiple times, when spanning multiple cells
(define-public html-id
  (let ((id (make-object-property)))
   (lambda (ev)
     (or (id ev)
         (set/r! (id ev) (symbol->string (gensym "__html_id_")))))))
