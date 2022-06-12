(define-module (srfi srfi-41 util)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-71)
  #:use-module ((ice-9 sandbox) :select (call-with-time-limit))
  #:use-module (hnh util) ; find-min
  #:export (stream-car+cdr interleave-streams
                           stream-timeslice-limit))

(define (stream-car+cdr stream)
  (values (stream-car stream)
          (stream-cdr stream)))

;; Merges a number of totally ordered streams into a single
;; totally ordered stream.
;; ((≺, stream)) → (≺, stream)
(define (interleave-streams < streams)
  ;; Drop all empty streams
  (let ((streams (remove stream-null? streams)))
    ;; If all streams where empty, end the output stream
    (if (null? streams)
        stream-null
        (let* ((min other (find-extreme streams < stream-car))
               (m ms (stream-car+cdr min)))
          (stream-cons m (interleave-streams < (cons ms other)))))))

(define-public (stream-insert < item s)
  (interleave-streams < (list (stream item) s)))

;; Requires that stream is a total order in regards to what we filter
;; on. From there it knows that once it has found the first element
;; that satisfies our predicate all remaining elements satisfying pred
;; will be in direct succession.
;; Does have some drawbacks, concider an event between 2020-01-01 and 2020-12-31.
;; The collection is sorted on start time, and we want all events overlapping the
;; interval 2020-02-01 to 2020-02-29. We would get the long event, but then probably
;; stop because all regular small events in january.
(define-public (filter-sorted-stream pred stream)
  (stream-take-while
   pred (stream-drop-while
         (negate pred) stream)))


;; Simmilar to the regular @code{filter-sorted-stream}, but once an
;; element satisfies @code{keep-remaning?} then the remaining tail
;; of the stream is all assumed to be good.
(define-public (filter-sorted-stream* pred? keep-remaining? stream)
  (cond [(stream-null? stream) stream-null]
        [(keep-remaining? (stream-car stream)) stream]
        [(pred? (stream-car stream))
         (stream-cons (stream-car stream)
                      (filter-sorted-stream*
                       pred? keep-remaining?
                       (stream-cdr stream)))]
        [else (filter-sorted-stream* pred? keep-remaining?
                                     (stream-cdr stream))]))


;; returns all object in stream from the first object satisfying
;; start-pred, until the last object which sattisfies end-pred.
(define-public (get-stream-interval start-pred end-pred stream)
  (stream-take-while
   end-pred (stream-drop-while
             (negate start-pred)
             stream)))


;; Finds the first element in stream satisfying pred.
;; Returns #f if nothing was found
(define-public (stream-find pred stream)
  (cond ((stream-null? stream) #f)
        ((pred (stream-car stream)) (stream-car stream))
        (else (stream-find pred (stream-cdr stream)))))

(define-public (stream-remove pred stream)
  (stream-filter (negate pred) stream))

(define-public (stream->values stream)
  (apply values (stream->list stream)))


;; Natural numbers from 1 and up, each number repeated 7 times.
(define-public (repeating-naturals from repeats)
  (stream-unfold
   cdr                      ; map
   (const #t)               ; continue?
   (lambda (x)                   ; gen next
     (if (= (1- repeats) (car x))
         (cons 0 (1+ (cdr x)))
         (cons (1+ (car x)) (cdr x))))
   (cons 0 from)))

;; Returns two values. A stream of all the elements in stream
;; which satisfiy @var{pred}, and a stream of those elements
;; that don't. @var{pred} is called once per value in the
;; input stream.
(define-public (stream-partition pred stream)
  (let ((strm (stream-zip (stream-map pred stream)
                          stream)))
    (values
     (stream-map cadr (stream-filter car strm))
     (stream-map cadr (stream-remove car strm)))))

(define-public (stream-split idx stream)
  (stream-cons (stream-take idx stream)
               (stream-drop idx stream)))

(define-stream (stream-paginate% stream page-size)
  (stream-match (stream-split page-size stream)
                ((page . rest)
                 (if (stream-null? page)
                     stream-null
                     (stream-cons
                      page
                      (stream-paginate rest page-size))))))

(define*-public (stream-paginate stream optional: (page-size 10))
  (stream-paginate% stream page-size))


;; stream cons, but eval arguments beforehand.
(define (eager-stream-cons a b)
  (stream-cons a b))

;; Wrap a stream in time limits. Each element has at most @var{timeslice}
;; seconds to produce a value, otherwise the stream ends. Useful for finding the
;; "final" element matching a predicate in an infinite stream.
(define-stream (stream-timeslice-limit strm timeslice)
  (call-with-time-limit
   timeslice
   (lambda () (eager-stream-cons
          (stream-car strm)
          (stream-timeslice-limit (stream-cdr strm) timeslice)))
   (lambda _ stream-null)))

