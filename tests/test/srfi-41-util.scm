;;; Commentary:
;; Tests (srfi srfi-41 util).
;; Currently only tests stream-paginate.
;;; Code:

(define-module (test srfi-41-util)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-41)
  :use-module ((ice-9 sandbox) :select (call-with-time-limit)))

(test-equal "Finite stream"
  '((0 1 2) (3 4 5) (6 7 8) (9))
  (let ((strm (stream-paginate (stream 0 1 2 3 4 5 6 7 8 9) 3)))
    (map stream->list (stream->list strm))))

(test-equal "slice of infinite"
  '(1000 1001 1002 1003 1004 1005 1006 1007 1008 1009)
  (let ((strm (stream-paginate (stream-from 0))))
    (stream->list (stream-ref strm 100))))

(define unique-symbol (gensym))

(test-equal "time out on infinite 'empty' stream"
  unique-symbol
  ;; defined outside time limit since creation should always
  ;; succeed. Only reference is expected to fail.
  (let ((strm (stream-paginate
               ;; easy way to get stream which never finds
               ;; any elements.
               (stream-filter negative? (stream-from 0)))))
    (call-with-time-limit
     0.1
     (lambda () (stream-car strm))
     (lambda _ unique-symbol))))




(test-equal "stream insert"
  '(1 4 5 7 8)
  (stream->list (stream-insert < 5 (stream 1 4 7 8))))


(test-equal "Filter sorted stream"
  '(4 6 8)
  (stream->list (filter-sorted-stream even? (stream 1 3 4 6 8 9 11))))

(test-equal "Filter sorted stream (which actually is unsorted)"
  '(4 6 8)
  (stream->list (filter-sorted-stream even? (stream 1 3 4 6 8 9 11 12))))

;; TODO filter-sorted-stream*

(test-equal
    "Get stream interval"
    '(5 6 7 8 9)
    (stream->list (get-stream-interval (lambda (x) (< 4 x))
                                       (lambda (x) (< x 10))
                                       (stream 1 2 3 4 5 6 7 8 9 10 11 12))))



(test-equal "stream find" 2 (stream-find even? (stream-from 1)))


(test-equal
    "repeating naturals"
    '(1 1 1 2 2 2 3 3 3 4)
    (stream->list 10 (repeating-naturals 1 3)))


;; sleep will return early if a singal arrives, this just resumes sleeping until
;; the wanted time is hit.
;; Might sleep longer since sleep always returns a whole number of seconds remaining
(define (true-sleep n)
  (let loop ((remaining n))
    (unless (zero? remaining)
      (loop (sleep remaining)))))

(let ((strm (stream-map (lambda (x) (when (zero? (modulo x 4)) (true-sleep 1)) x) (stream-from 1))))
  (let ((strm (stream-timeslice-limit strm 0.1)))
    (test-equal "time limited stream"
      '(1 2 3)
      (stream->list strm))))
