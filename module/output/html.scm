(define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util tree)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (output general)

  #:use-module (ice-9 getopt-long)

  #:use-module (parameters)
  #:use-module (config))

(define-stream (group-stream in-stream)
  (define (ein? day) (lambda (e) (event-in? e (date->time-utc day))))

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

(define x-pos (make-object-property))
(define width (make-object-property))

;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
(define (fix-event-widths! start-of-day lst)
  ;; The tree construction is greedy. This means
  ;; that if  a smaller  event preceeds a longer
  ;; event it would capture  the longer event to
  ;; only find  events which  also overlaps  the
  ;; smaller event.

  ;; @var{x} is how for left in the container we are.
  (let inner ((x 0)
              (tree (make-tree overlapping?
                               (sort* lst time>? (lambda (e) (event-length/day e start-of-day))))))
    (if (null? tree) #f
        (let ((w (/ (- 1 x)
                    (+ 1 (length-of-longst-branch (left-subtree tree))))))
          (set! (width (car tree)) w
                (x-pos (car tree)) x)
          (inner (+ x w) (left-subtree tree))
          (inner x (right-subtree tree))))))

;; This should only be used on time intervals, never on absolute times.
;; For that see @var{date->decimal-hour}.
(define (time->decimal-hour time)
  (exact->inexact (/ (time-second time)
                     3600)))

(define (html-attr str)
  (define cs (char-set-adjoin char-set:letter+digit #\- #\_))
  (string-filter (lambda (c) (char-set-contains? cs c)) str))

(define (vevent->sxml day ev)
  (define time (date->time-utc day))
  (define style
    (format #f "left:~,3f%;width:~,3f%;top:~,3f%;height:~,3f%;"

            (* 100 (x-pos ev))          ; left
            (* 100 (width ev))          ; width

            ;; top
            (if (in-day? day (attr ev 'DTSTART))
                (* 100/24
                   (time->decimal-hour
                    (time-difference (attr ev 'DTSTART)
                                     (start-of-day* (attr ev 'DTSTART)))))
                0)

            ;; height
            (* 100/24 (time->decimal-hour (event-length/day ev time)))))

  `(div (@ (class "event CAL_" ,(html-attr (let ((l (attr (parent ev) 'NAME)))
                                             (if (pair? l) (car l) l)))
             ,(if (time<? (attr ev 'DTSTART) time)
                  " continued" "")
             ,(if (time<? (add-day time) (attr ev 'DTEND))
                  " continuing" ""))
           (style ,style))
        ,((summary-filter) ev (attr ev 'SUMMARY))))

(define (lay-out-day day)
  (let* (((date . events) day))
    ;; (format (current-error-port) "Processing ~a~%" (date->string date))
    (fix-event-widths! (date->time-utc date) (stream->list events))
    `(div (@ (class "day"))
          (div (@ (class "meta"))
               (span (@ (class "dayname")) ,(date->string date "~a"))
               (span (@ (class "daydate")) ,(date->string date "~Y-~m-~d")))
          (div (@ (class "events"))
               ,@(map (lambda (time)
                        `(div (@ (class "clock "
                                   ,(string-append "clock-" time))) " "))
                      (map number->string (iota 12 0 2)))
               ,@(stream->list (stream-map (lambda (e) (vevent->sxml date e)) events))))))

(define (time-marker-div)
  `(div (@ (class "sideclock"))
        (div (@ (class "day"))
             (div (@ (class "meta")) #\space)
             (div (@ (class "clockbar"))
                  ,@(map (lambda (time)
                           `(div (@ (class "clock "
                                      ,(string-append "clock-" time)))
                                 (span (@ (class "clocktext"))
                                       ,(string-append time ":00"))))
                         (map number->string (iota 12 0 2)))))))

(define (include-css path)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path))))

(define opt-spec
  '((from (value #t) (single-char #\f))
    (to (value #t) (single-char #\t))))

(define-public (html-main calendars events args)

  (define opts (getopt-long args opt-spec))

  (define start (parse-freeform-date (option-ref opts 'from "2019-04-15")))
  (define end   (parse-freeform-date (option-ref opts 'to   "2019-05-10")))

  (define evs
    (filter-sorted-stream
     ;; TODO in-date-range? drops the first date
     (compose (in-date-range? start end)
              car)
     (group-stream events)))

  ((@ (sxml simple) sxml->xml)
   `(html (head
           (title "Calendar")
           (meta (@ (charset "utf-8")))
           ,(include-css "static/style.css")
           (style ,(format #f "~:{.CAL_~a { background-color: ~a; color: ~a }~%~}"
                           (map (lambda (c)
                                  (list (html-attr (if (pair? (attr c 'NAME))
                                                       (car (attr c 'NAME))
                                                       (attr c 'NAME)))
                                        (or (attr c 'COLOR) "white")
                                        (or (and=> (attr c 'COLOR) calculate-fg-color) "black")))
                                calendars))))
          (body (div (@ (class "calendar"))
                     ,(time-marker-div)
                     (div (@ (class "days"))
                          ,@(stream->list (stream-map lay-out-day evs))))))))
