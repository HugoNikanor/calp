(define-module (vcomponent group)
  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:export (group-stream get-groups-between))

;; TODO templetize this
(define-stream (group-stream in-stream)
  (define (ein? day) (lambda (e) (event-contains? e (date->time-utc day))))

  (let loop ((days (day-stream (time-utc->date (attr (stream-car in-stream) 'DTSTART))))
             (stream in-stream))
    (if (stream-null? stream)
        stream-null
        (let* ((day (stream-car days))
               (tomorow (date->time-utc (stream-car (stream-cdr days)))))

          (let ((head (stream-take-while (ein? day) stream))
                (tail
                 ;; This is a filter, instead of a stream-span together with head,
                 ;; since events can span multiple days.
                 ;; This starts with taking everything which end after the beginning
                 ;; of tommorow, and finishes with the rest when it finds the first
                 ;; object which begins tomorow (after midnight, exclusize).
                 (filter-sorted-stream*
                  (lambda (e) (time<? tomorow (attr e 'DTEND)))
                  (lambda (e) (time<=? tomorow (attr e 'DTSTART)))
                  stream)))


            (stream-cons (cons day head)
                         (loop (stream-cdr days)
                               tail)))))))

(define (get-groups-between groups start-date end-date)
  (filter-sorted-stream
   ;; TODO in-date-range? drops the first date
   (compose (in-date-range? start-date end-date)
            car)
   groups))


(define-public (group->event-list group)
  (stream->list (cdr group)))
