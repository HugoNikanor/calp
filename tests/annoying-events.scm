(((srfi srfi-41 util) filter-sorted-stream)
 ((srfi srfi-41) stream stream->list stream-filter stream-take-while)
 ((vcomponent base) extract prop make-vcomponent)
 ((vcomponent datetime) event-overlaps?)
 ((datetime) date date+ date<)
 ((calp util) set!))

(define* (event key: summary dtstart dtend)
  (define ev (make-vcomponent 'VEVENT))
  (set! (prop ev 'SUMMARY) summary
        (prop ev 'DTSTART) dtstart
        (prop ev 'DTEND) dtend)
  ev)

(define start #2021-11-01)
(define end (date+ start (date day: 8)))

(define ev-set
  (stream
   (event                         ; should be part of the result
    summary: "A"
    dtstart: #2021-10-01
    dtend: #2021-12-01)
   (event                         ; should NOT be part of the result
    summary: "B"
    dtstart: #2021-10-10
    dtend: #2021-10-11)
   (event                         ; should also be part of the result
    summary: "C"
    dtstart: #2021-11-02
    dtend: #2021-11-03)))

;; (if (and (date< (prop ev 'DTSTART) start-date)
;;          (date<= (prop ev 'DTEND) end-date))
;;     ;; event will be picked, but next event might have
;;     (and (date< start-date (prop ev 'DTSTART))
;;          (date< end-date (prop ev 'DTEND)))
;;     ;; meaning that it wont be added, stopping filter-sorted-stream
;;     )

;; The naïve way to get all events in an interval. Misses C due to B being "in the way"

(test-equal "incorrect handling of non-contigious"
  '("A" #; "C")
  (map (extract 'SUMMARY)
       (stream->list
        (filter-sorted-stream
         (lambda (ev) (event-overlaps? ev start (date+ start (date day: 8))))
         ev-set))))

;; A correct way

(test-equal "correct handling of non-contigious"
  '("A" "C")
  (map (extract 'SUMMARY)
       (stream->list
        (stream-filter (lambda (ev) (event-overlaps? ev start end))
                       (stream-take-while (lambda (ev) (date< (prop ev 'DTSTART) end))
                                          ev-set)))))
