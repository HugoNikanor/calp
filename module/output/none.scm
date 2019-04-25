(define-module (output none)
  #:use-module (vcomponent group)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-19 util)
  #:use-module (util)
  #:export (none-main))

(define (none-main calendars events args)
  (define date (drop-time (current-date)))
  (group->event-list
   (stream-car
    ;; TODO reusing the same grouping causes it to lose events.
    ;; I currently have no idea why, but it's BAD.
    (get-groups-between (group-stream events)
                        date date))))
