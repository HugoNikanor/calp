(define-module (calp html util)
  :use-module (hnh util)
  :use-module (calp translation)
  :export (date-link html-id calculate-fg-color))


(define (date-link date)
  ((@ (datetime) date->string) date "~Y-~m-~d"))


;; Generate an html id for an event.
;; TODO? same event placed multiple times, when spanning multiple cells
(define html-id
  (let ((id (make-object-property)))
    (lambda (ev)
      (or (id ev)
          (set/r! (id ev) (symbol->string (gensym "__html_id_")))))))

;; Returns a color with good contrast to the given background color.
;; https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color/1855903#1855903
(define (calculate-fg-color c)
  ;; TODO what errors can actually appear here?
  (catch #t
    (lambda ()
      (define (str->num c n) (string->number (substring/shared c n (+ n 2)) 16))
      ;; (format (current-error-port) "COLOR = ~s~%" c)
      (let ((r (str->num c 1))
            (g (str->num c 3))
            (b (str->num c 5)))
        (if (< 1/2 (/ (+ (* 0.299 r)
                         (* 0.587 g)
                         (* 0.114 b))
                      #xFF))
            "#000000" "#FFFFFF")))
    (lambda args
      (format (current-error-port) (_ "Error calculating foreground color?~%~s~%") args)
      "#FF0000"
      )))
