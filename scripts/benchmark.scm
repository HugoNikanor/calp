(add-to-load-path "module")
(use-modules (calp util)
             (calp util app)
             (calp util config)
             (vcomponent)
             (vcomponent group)
             (vcomponent datetime)
             (datetime)
             (srfi srfi-41)
             (srfi srfi-41 util)
             )

(load "config.scm")
((@ (vcomponent) init-app) (get-config 'calendar-files))

(define all-events (getf 'event-set))
(define groups (group-stream all-events))

(define start #2020-06-01)
(define end #2020-07-01)

(define (run-filter)
  (stream->list (filter-sorted-stream (lambda (e) (event-overlaps? e start end))
                                      all-events)))

(define (run-grouped)
  (map group->event-list
       (stream->list (get-groups-between groups start end))))

(define (calc-time start end)
  (exact->inexact
    (+ (- (car end)
          (car start))
       (/ (- (cdr end)
             (cdr start))
          #e1e6))))

(run-filter)
(run-grouped)

(format #t "Starting timings~%")
(define t1 (gettimeofday))
(do ((i 1 (1+ i)))
  ((> i 1000))
  (run-grouped))
(format #t "end first timings~%")
(define t2 (gettimeofday))
(do ((i 1 (1+ i)))
  ((> i 1000))
  (run-filter))
(format #t "end second timings~%")
(define t3 (gettimeofday))

(format #t "t1 = ~a~%t2 = ~a~%"
        (calc-time t1 t2)
        (calc-time t2 t3))

(values (calc-time t1 t2)
        (calc-time t2 t3))
