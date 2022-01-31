;;; Commentary:

;; $ calp --repl /socket -- server
;; paste this when connected to the socket.

;;; Code:

(begin

 (use-modules
  (hnh util)
  ((srfi srfi-1) :select (partition))
  (srfi srfi-41)
  (vcomponent datetime)
  (datetime)
  ((calp html view calendar shared)
   :select (fix-event-widths! x-pos width))
  )

 (define events (get-event-set (@ (vcomponent instance) global-event-object)))

 (define evs (stream->list (events-between (current-date) (date+ (current-date) (date day: 1)) events)))

 (define-values (longevs shortevs) (partition long-event? evs))

 (fix-event-widths! shortevs event-length-key: (lambda (e) (event-length/day (current-date) e)))

 

 (use-modules (gnome gtk)
              (oop goops))

 (define window (make <gtk-window> type: 'toplevel))

 (define table (gtk-table-new 1 1 #f))

 (add window table)

 (define aligns
   (map (lambda (ev)
          (define xalign (exact->inexact (x-pos ev)))
          (define yalign (/ (time->decimal-hour (as-time (prop ev 'DTSTART))) 24))
          (define xscale (exact->inexact (/ (width ev) (- 1 (x-pos ev)))))
          (define yscale (exact->inexact (/ (- 1 yalign)
                                            (/ (datetime->decimal-hour (event-length ev)) 24))))
          (define align (gtk-alignment-new xalign yalign xscale yscale))
          ;; (define event-container (make <gtk-layout>))

          ;; (put event-container
          ;;      (make <gtk-label>
          ;;        label: (prop ev 'SUMMARY)
          ;;        wrap: #t
          ;;        wrap-mode: 'word)
          ;;      0 0)
          (define event-container (make <gtk-frame> label: (prop ev 'SUMMARY)))
          (add align event-container)

          align)
        shortevs))


 (for align in aligns
      (attach table align 0 1 0 1))

 (show-all window)

 (define thr ((@ (ice-9 threads) begin-thread) (gtk-main))))


