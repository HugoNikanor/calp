;;; TODO these aren't unit tests, but rather basic integration tests

(define-module (test annoying-events)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((srfi srfi-41 util)
   :select (filter-sorted-stream))
  :use-module ((srfi srfi-41)
   :select (stream
             stream->list
             stream-filter
             stream-take-while))
  :use-module ((vcomponent datetime) :select (instance-overlaps?))
  :use-module (datetime)
  :use-module ((hnh util) :select (set!))
  :use-module (vcomponent create)
  :use-module (vcomponent))


(define start (datetime year: 2021 month: 11 day: 01 tz: "UTC"))

(define end (datetime+ start (duration day: 8)))

;;; [--Event A-------------------------------]
;;;           [Event B]
;;;                          [Event C]
;;;                      [--Target Timespan--]

(define ev-set
  (stream
   (vevent                              ; should be part of the result
    summary: "A"
    dtstart: (date year: 2021 month: 10 day: 01)
    dtend: (date year: 2021 month: 12 day: 01))
   (vevent                          ; should NOT be part of the result
    summary: "B"
    dtstart: (date year: 2021 month: 10 day: 10)
    dtend: (date year: 2021 month: 10 day: 11))
   (vevent                         ; should also be part of the result
    summary: "C"
    dtstart: (date year: 2021 month: 11 day: 02)
    dtend: (date year: 2021 month: 11 day: 03))))

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
  (map (extract1 'SUMMARY)
       (stream->list
         (filter-sorted-stream
           (lambda (ev) (instance-overlaps? "UTC" ev start end))
           ev-set))))

(test-equal "correct handling of non-contigious"
  '("A" "C")
  (map (extract1 'SUMMARY)
       (stream->list
         (stream-filter
           (lambda (ev) (instance-overlaps? "UTC" ev start end))
           (stream-take-while
             (lambda (ev) (date< (prop1 ev 'DTSTART) (datetime-date end)))
             ev-set)))))



'((vcomponent)
  (vcomponent datetime))
