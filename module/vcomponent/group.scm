(define-module (vcomponent group)
  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:export (group-stream get-groups-between))

;; TODO templetize this
(define-stream (group-stream in-stream)
  (define (ein? day) (lambda (e) (event-contains? e day)))

  (if (stream-null? in-stream)
      stream-null
      (let loop ((days (day-stream (as-date (prop (stream-car in-stream) 'DTSTART))))
                 (stream in-stream))
        (let* ((day (stream-car days))
               (tomorow (stream-car (stream-cdr days))))

          (let ((head (stream-take-while (ein? day) stream))
                (tail
                 ;; This is a filter, instead of a stream-span together with head,
                 ;; since events can span multiple days.
                 ;; This starts with taking everything which end after the beginning
                 ;; of tommorow, and finishes with the rest when it finds the first
                 ;; object which begins tomorow (after midnight, exclusize).
                 (filter-sorted-stream*
                  (lambda (e) (date/-time<? tomorow
                                       (or (prop e 'DTEND)
                                           (if (date? (prop e 'DTSTART))
                                               (date+ (prop e 'DTSTART) (date day: 1))
                                               (prop e 'DTSTART)))))
                  (lambda (e) (date/-time<=? tomorow (prop e 'DTSTART)))
                  stream)))


            (stream-cons (cons day head)
                         (loop (stream-cdr days)
                               tail)))))))

(define (get-groups-between groups start-date end-date)

  (define good-part
    (filter-sorted-stream
     (compose (in-date-range? start-date end-date)
              car)
     groups))

  ;; NOTE slightly ugly hack. The first element in the return of group-stream shares
  ;; it's date component with the lowest dtstart in the event set. This means that a
  ;; group set might start after our start- (and end-!) date.
  ;; To combat this I simple create a bunch of dummy groups below.

  (cond [(stream-null? good-part)
         (list->stream
          (map (lambda (d) (cons d stream-null))
               (date-range start-date end-date)))]
        [(car (stream-car good-part))
         (lambda (d) (date< start-date d))
         => (lambda (d)
              (stream-append
               (list->stream
                (map (lambda (d) (cons d stream-null))
                     (date-range start-date
                                 (date- d (date day: 1)))))
               good-part))]
        [else good-part]))


(define-public (group->event-list group)
  (stream->list (cdr group)))
