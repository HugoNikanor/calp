(define-module (calp html view calendar shared)
  :use-module (hnh util)
  :use-module (hnh util tree)
  :use-module (hnh util type)
  :use-module (srfi srfi-1)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (instance-length
                        overlapping?
                        instance-length/clamped))
  :use-module (datetime)
  :use-module (calp html config)
  :use-module ((calp html vcomponent)
               :select (make-block format-summary))
  :use-module (ice-9 format)
  :use-module (calp translation)

  :export (fix-event-widths!
           lay-out-long-events
           create-top-block
           ))


(define-public x-pos (make-object-property))
(define-public width (make-object-property))

;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
(define* (fix-event-widths! lst key: event-length-key (event-length-comperator date/-time>?))
  (typecheck lst (list-of vevent?))
  (typecheck event-length-key procedure?)
  (typecheck event-length-comperator procedure?)
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
        (set! (width (tree-node tree)) w
              (x-pos (tree-node tree)) x)
        (inner (+ x w) (left-subtree tree))
        (inner x (right-subtree tree))))))


(define (lay-out-long-events start end events)
  (typecheck start date?)
  (typecheck end date?)
  (typecheck events (list-of (tuple-of string? string? vevent?)))
  (fix-event-widths! (map caddr events)
                     event-length-key: instance-length
                     event-length-comperator: datetime>)
  (map (lambda (e) (create-top-block start end e))
       events))

;; date{,time}-difference works in days, and days are simply multiplied by 24 to
;; get hours.  This means that a day is always assumed to be 24h, even when that's
;; wrong. This might lead to some weirdness when the timezon switches (DST), but it
;; makes everything else behave MUCH better.
(define (create-top-block start-date end-date entry)
  (typecheck start-date date?)
  (typecheck end-date date?)
  (typecheck entry (tuple-of string? string? vevent?))

  (define ev (list-ref entry 2))

  (define total-length
    (* 24 (days-in-interval start-date end-date)))

  (define top (* 100 (x-pos ev)))
  (define height (* 100 (width ev)))
  (define left ; start time
    (* 100
       (let* ((dt (datetime date: start-date))
              (diff (datetime-difference
                     (datetime-max dt (as-datetime (prop1 ev 'DTSTART)))
                     dt)))
         (/ (datetime->decimal-hour diff start-date) total-length))))

  ;; Set length of event, which makes end time
  (define width*
    (* 100
       (/ (datetime->decimal-hour
           (instance-length/clamped start-date end-date ev)
           start-date)
          total-length)))

  (define style
    (if (edit-mode)
        (format #f "top:calc(var(--editmode)*~,3f%);height:calc(var(--editmode)*~,3f%);left:~,3f%;width:~,3f%;"
                top height left width*)
        (format #f "top:~,3f%;height:~,3f%;left:~,3f%;width:~,3f%;"
                top height left width*)))

  (make-block
   (list-ref entry 0)
   (list-ref entry 1)
   (list-ref entry 2)
   `((class
       ,(when (date/-time< (prop1 ev 'DTSTART) start-date)
          " continued")
       ,(when (and (prop% ev 'DTEND)
                   (date/-time< (date+ end-date (date day: 1)) (prop1 ev 'DTEND)))
          " continuing"))
     (style ,style))))
