(define-module (vcomponent group)
  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:export (group-stream))

(define-stream (group-stream in-stream)
  (define (ein? day) (lambda (e) (event-contains? e (date->time-utc day))))

  (let loop ((days (day-stream (time-utc->date (attr (stream-car in-stream) 'DTSTART))))
             (stream in-stream))
    (if (stream-null? stream)
        stream-null
        (let* ((day (stream-car days))
               (tomorow (add-day (date->time-utc (drop-time day)))))
          (let ((head (stream-take-while (ein? day) stream))
                (tail
                 (filter-sorted-stream*
                  (lambda (e) (time<? tomorow (attr e 'DTEND)))
                  (lambda (e) (time<=? tomorow (attr e 'DTSTART)))
                  stream)))

            (stream-cons (cons day head)
                         (loop (stream-cdr days)
                               tail)))))))

(define-public (get-groups-between groups start-date end-date)
  (filter-sorted-stream
   ;; TODO in-date-range? drops the first date
   (compose (in-date-range? start-date end-date)
            car)
   groups))


(define-public (group->event-list group)
  (stream->list (cdr group)))
