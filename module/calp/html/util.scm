(define-module (calp html util)
  :use-module ((base64) :select (base64encode base64decode))
  :use-module (calp util))

;;; @var{html-attr} & @var{html-unattr} used to just strip any
;;; attributes not valid in css. That allowed a human reader to
;;; quickly see what data it was. The downside was that it was one
;;; way. The new base64 based system supports both an encode and a
;;; decode without problem.
;;;
;;; The encoded string substitutes { + => å, / => ä, = => ö } to be
;;; valid CSS selector names.

;; Retuns an HTML-safe version of @var{str}.
(define-public (html-attr str)
  (string-map (lambda (c)
                (case c
                  ((#\+) #\å)
                  ((#\/) #\ä)
                  ((#\=) #\ö)
                  (else c)))
              (base64encode str)))

(define-public (html-unattr str)
  (base64decode
    (string-map (lambda (c)
                  (case c
                    ((#\å) #\+)
                    ((#\ä) #\/)
                    ((#\ö) #\=)
                    (else c)))
                str)))


(define-public (date-link date)
  ((@ (datetime) date->string) date "~Y-~m-~d"))



;; Generate an html id for an event.
;; TODO? same event placed multiple times, when spanning multiple cells
(define-public html-id
  (let ((id (make-object-property)))
    (lambda (ev)
      (or (id ev)
          (set/r! (id ev) (symbol->string (gensym "__html_id_")))))))
