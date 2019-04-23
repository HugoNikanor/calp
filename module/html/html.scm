(define-module (html html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcalendar)
  #:use-module (vcalendar datetime)
  #:use-module (util)
  #:use-module (util tree)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)

  )

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

  ;; @var{x} is how for left in the container we are.
  (define (inner x tree)
    (if (null? tree) #f
        (let ((w (/ (- 1 x)
                    (+ 1 (length-of-longst-branch (left-subtree tree))))))
          (set! (width (car tree)) w
                (x-pos (car tree)) x)
          (inner (+ x w) (left-subtree tree))
          (inner x (right-subtree tree)))))

  (inner 0 (make-tree (lambda (head e) (overlapping? head e))
                        ;; The tree construction is greedy. This means
                        ;; that if  a smaller  event preceeds a longer
                        ;; event it would capture  the longer event to
                        ;; only find  events which  also overlaps  the
                        ;; smaller event.
                      (sort* lst time>? (lambda (e) (event-length/day e start-of-day))))))

(define (time->decimal-hour time)
  "This should only be used on time intervals,
never on absolute times. For that see date->decimal-hour"
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
                (* (/ 24) 100
                   (time->decimal-hour
                    (time-difference (attr ev 'DTSTART)
                                     (start-of-day* (attr ev 'DTSTART)))))
                0)

            ;; height
            (* (/ 24) 100
               (time->decimal-hour
                (if (in-day? day (attr ev 'DTEND))
                    (if (in-day? day (attr ev 'DTSTART))
                        ;; regular
                        (time-difference (attr ev 'DTEND)
                                         (attr ev 'DTSTART))
                        ;; end today, start later
                        (time-difference (attr ev 'DTEND)
                                         time))
                    (if (in-day? day (attr ev 'DTSTART))
                        ;; end today, start earlier
                        (time-difference (add-day time)
                                         (attr ev 'DTSTART))
                        ;; start earlier, end earlier
                        (time-difference (add-day time)
                                         time)))))))

  `(div (@ (class "event CAL_" ,(html-attr (let ((l (attr (parent ev) 'NAME)))
                                             (if (pair? l) (car l) l)))
             ,(if (time<? (attr ev 'DTSTART) time)
                  " continued" "")
             ,(if (time<? (add-day time) (attr ev 'DTEND))
                  " continuing" ""))
           (style ,style))
        ,(attr ev 'SUMMARY)))

(define (lay-out-day day)
  (let* (((date . events) day))
    ;; (format (current-error-port) "Processing ~a~%" (date->string date))
    (fix-event-widths! (date->time-utc date) (stream->list events))
    `(div (@ (class "day"))
          (div (@ (class "meta"))
               (span (@ (class "dayname")) ,(date->string date "~a"))
               (span (@ (class "daydate")) ,(date->string date "~Y-~m-~d")))
          (div (@ (class "events"))
               " "
               ,@(stream->list (stream-map (lambda (e) (vevent->sxml date e)) events))))))


(define (time-marker-div)
  (map (lambda (time)
         `(div (@ (id ,(string-append "clock-" time))
                  (class "clock"))
               ,(string-append time ":00")))
       (map number->string (iota 12 0 2))))

(define (d str)
  (string->date str "~Y-~m-~d"))


(define (calculate-fg-color c)
  (define (str->num c n) (string->number (substring/shared c n (+ n 2)) 16))
  (let ((r (str->num c 1))
        (g (str->num c 3))
        (b (str->num c 5)))
    (if (< 1/2 (/ (+ (* 0.299 r)
                     (* 0.587 g)
                     (* 0.144 b))
                  #xFF))
        "black" "#e5e8e6")))

(define-public (html-main calendars events)
  `(html (head
          (title "Calendar")
          (meta (@ (charset "utf-8")))
          (link (@ (type "text/css")
                   (rel "stylesheet")
                   (href "static/style.css")))
          (style ,(format #f "~{.CAL_~a { background-color: ~a; color: ~a }~%~}"
                          (concat (map (lambda (c)
                                         (list
                                          (html-attr (if (pair? (attr c 'NAME))
                                                         (car (attr c 'NAME))
                                                         (attr c 'NAME)))
                                          (or (attr c 'COLOR) "white")
                                          (or (and=> (attr c 'COLOR) calculate-fg-color )
                                              "black")))
                                       calendars)))))
         (body (div (@ (class "calendar"))
                    ,@(time-marker-div)
                    (div (@ (class "days"))
                         ,@(stream->list
                            (stream-map
                             lay-out-day
                             (filter-sorted-stream
                              (compose (in-date-range?
                                        (d "2019-04-15")
                                        (d "2019-05-10"))
                                       car)
                              (group-stream events)))))))))

