(((srfi srfi-41 util) stream-paginate)
 ((srfi srfi-41) stream->list stream-ref stream-from
  stream-filter stream-car stream)
 ((ice-9 sandbox) call-with-time-limit)
 )

(test-equal "Finite stream"
  '((0 1 2) (3 4 5) (6 7 8) (9))
  (let ((strm
         (stream-paginate (stream 0 1 2 3 4 5 6 7 8 9)
                          3)))
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
