(define-module (calp html view calendar shared)
  :use-module (calp util)
  :use-module (srfi srfi-1)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (event-length
                        overlapping?
                        event-length/clamped))
  :use-module ((vcomponent datetime output)
               :select (format-summary))
  :use-module (calp util tree)
  :use-module (datetime)
  :use-module (calp html config)
  :use-module ((calp html components)
               :select (btn tabset))
  :use-module ((calp html vcomponent)
               :select (make-block) )
  )



(define-public x-pos (make-object-property))
(define-public width (make-object-property))


;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
(define*-public (fix-event-widths! lst key: event-length-key (event-length-comperator date/-time>?))
  ;; The tree construction is greedy. This means
  ;; that if  a smaller  event preceeds a longer
  ;; event it would capture  the longer event to
  ;; only find  events which  also overlaps  the
  ;; smaller event.

  ;; @var{x} is how for left in the container we are.
  (let inner ((x 0)
              (tree (make-tree overlapping?
                               (sort* lst event-length-comperator event-length-key
                                      ))))
    (unless (null? tree)
      (let ((w (/ (- 1 x)
                  (+ 1 (length-of-longst-branch (left-subtree tree))))))
        (set! (width (car tree)) w
              (x-pos (car tree)) x)
        (inner (+ x w) (left-subtree tree))
        (inner x (right-subtree tree))))))


(define-public (lay-out-long-events start end events)
  (fix-event-widths! events event-length-key: event-length
                     event-length-comperator: date/-time>)
  (map (lambda (e) (create-top-block start end e))
       events))

;; date{,time}-difference works in days, and days are simply multiplied by 24 to
;; get hours.  This means that a day is always assumed to be 24h, even when that's
;; wrong. This might lead to some weirdness when the timezon switches (DST), but it
;; makes everything else behave MUCH better.
(define-public (create-top-block start-date end-date ev)

  (define total-length
    (* 24 (days-in-interval start-date end-date)))

  (define top (* 100 (x-pos ev)))
  (define height (* 100 (width ev)))
  (define left ; start time
    (* 100
       (let* ((dt (datetime date: start-date))
              (diff (datetime-difference
                     (datetime-max dt (as-datetime (prop ev 'DTSTART)))
                     dt)))
         (/ (datetime->decimal-hour diff start-date) total-length))))

  ;; Set length of event, which makes end time
  (define width*
    (* 100
       (/ (datetime->decimal-hour
           (as-datetime (event-length/clamped start-date end-date ev))
           start-date)
          total-length)))

  (define style
    (if (edit-mode)
        (format #f "top:calc(var(--editmode)*~,3f%);height:calc(var(--editmode)*~,3f%);left:~,3f%;width:~,3f%;"
                top height left width*)
        (format #f "top:~,3f%;height:~,3f%;left:~,3f%;width:~,3f%;"
                top height left width*)))

  (make-block
   ev `((class
          ,(when (date/-time< (prop ev 'DTSTART) start-date)
             " continued")
          ,(when (and (prop ev 'DTEND)
                      (date/-time< (date+ end-date (date day: 1)) (prop ev 'DTEND)))
             " continuing"))
        (style ,style))))
