(define-module (calp html util)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util color)
  :use-module (calp translation)
  :use-module (vcomponent)
  :export (date-link html-id calculate-fg-color))


(define (date-link date)
  ((@ (datetime) date->string) date "~Y-~m-~d"))


;; Generate an html id for an event.
;; TODO? same event placed multiple times, when spanning multiple cells
(define html-id
  (let ((id (make-object-property)))
    (lambda (ev)
      (typecheck ev vevent?)
      (or (id ev)
          (set/r! (id ev) (symbol->string (gensym "__html_id_")))))))

;; Returns a color with good contrast to the given background color.
;; https://stackoverflow.com/questions/1855884/determine-font-color-based-on-background-color/1855903#1855903
;; TODO this be changed to return a color object!
(define (calculate-fg-color c)
  (typecheck c color?)
  (catch #t
    (lambda ()
      (if (< 1/2 (/ (+ (* 0.299 (rgba-r c))
                       (* 0.587 (rgba-g c))
                       (* 0.114 (rgba-b c)))
                    #xFF))
          "#000000" "#FFFFFF"))
    (lambda args
      (format (current-error-port) (G_ "Error calculating foreground color?~%~s~%") args)
      "#FF0000"
      )))
