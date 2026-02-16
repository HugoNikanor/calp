(define-module (calp html view calendar shared)
  :use-module (hnh util)
  :use-module (hnh util tree)
  :use-module (hnh util type)
  :use-module (srfi srfi-1)
  :use-module (vcomponent)
  :use-module ((vcomponent datetime)
               :select (instance-length
                        instance-start-datetime
                        instances-overlap?
                        instance-length/clamped))
  :use-module (datetime)
  :use-module (calp html config)
  :use-module ((calp html vcomponent)
               :select (make-block format-summary))
  :use-module (ice-9 format)
  :use-module (calp translation)

  :export (fix-event-widths!
           lay-out-long-events
           ))


(define-public x-pos (make-object-property))
(define-public width (make-object-property))

;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
;; TODO event-length-key is ALWAYS called with instance-length/clamped.
;; Possibly just take the clamping intervals instead
(define* (fix-event-widths! reference-zone lst key: event-length-key)
  (typecheck reference-zone string?)
  (typecheck lst (list-of vevent?))
  (typecheck event-length-key procedure?)
  ;; The tree construction is greedy. This means
  ;; that if  a smaller  event preceeds a longer
  ;; event it would capture  the longer event to
  ;; only find  events which  also overlaps  the
  ;; smaller event.

  ;; @var{x} is how for left in the container we are.
  (let inner ((x 0)
              (tree (make-tree (lambda (a b) (instances-overlap? reference-zone a b))
                               (sort* lst datetime> event-length-key))))
    (unless (null? tree)
      (let ((w (/ (- 1 x)
                  (+ 1 (length-of-longst-branch (left-subtree tree))))))
        (set! (width (tree-node tree)) w
              (x-pos (tree-node tree)) x)
        (inner (+ x w) (left-subtree tree))
        (inner x (right-subtree tree))))))


(define (lay-out-long-events reference-zone start end events)
  (typecheck start date?)
  (typecheck end   date?)
  (typecheck events (list-of (tuple-of string? string? vevent?)))
  (fix-event-widths! reference-zone (map caddr events)
                     event-length-key: (lambda (e) (instance-length/clamped
                                               (datetime date: start
                                                         tz: reference-zone)
                                               (datetime date: (date+ end (date day: 1))
                                                         tz: reference-zone)
                                               reference-zone
                                               e)))
  (map (lambda (e) (create-top-block reference-zone start end e))
       events))

;; date{,time}-difference works in days, and days are simply multiplied by 24 to
;; get hours.  This means that a day is always assumed to be 24h, even when that's
;; wrong. This might lead to some weirdness when the timezon switches (DST), but it
;; makes everything else behave MUCH better.
(define (create-top-block reference-zone start-date end-date entry)
  (typecheck reference-zone string?)
  (typecheck start-date date?)
  (typecheck end-date   date?)
  (typecheck entry (tuple-of string? string? vevent?))

  (define ev (list-ref entry 2))

  ;; TODO shouldn't this depend on the timezone?
  (define total-length
    (* 24 (days-in-interval start-date end-date)))

  (define event-continued?
    (not
     (datetime</zoneinfo (datetime date: start-date tz: reference-zone)
                         (instance-start-datetime reference-zone ev))))

  (define top (* 100 (x-pos ev)))
  (define height (* 100 (width ev)))
  (define left ; start time
    (if event-continued?
        0
        (* (/ 100 total-length)
         (datetime->decimal-hour
          (datetime-difference/zoneinfo
           (instance-start-datetime reference-zone ev)
           (datetime date: start-date tz: reference-zone))))))

  ;; Set length of event, which makes end time
  (define width*
    (* (/ 100 total-length)
       (datetime->decimal-hour
          (instance-length/clamped (datetime date: start-date
                                             tz: reference-zone)
                                   (datetime date: (date+ end-date (date day: 1))
                                             tz: reference-zone)
                                   reference-zone
                                   ev))))

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
       ,(when event-continued?
          " continued")
       ,(when (datetime</zoneinfo
               (datetime date: (date+ end-date (date day: 1)) tz: reference-zone)
               (datetime+ (instance-start-datetime reference-zone ev)
                          (instance-length ev)))
          " continuing"))
     (style ,style)))
  )
