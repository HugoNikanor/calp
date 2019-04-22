(define-module (html html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcalendar)
  #:use-module (vcalendar datetime)
  #:use-module (util)
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

;; Takes a list of vcomponents.
;;; And makes each sublist have better laid out elements.
;;; It's not perfect if there are many elements that overlap
;;; In different ways. But it works perfectly for a block
;;; schedule!
(define (fix-event-widths! ev-list)
  (if (null? ev-list)
      #f
      (let* ((pred? (lambda (next)
                      (time<? (attr next 'DTSTART)
                              (attr (car ev-list) 'DTEND))))
             (overlapping (take-while pred? ev-list))
             (rest (drop-while pred? ev-list)))
        (for-each (lambda (o x) (set! (x-pos o) x)) overlapping (iota (length overlapping)))
        (for-each (lambda (o) (set! (width o) (/ (length overlapping))))
                  overlapping)
        (fix-event-widths! rest))))

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
    (format #f "top: ~,3f%;  left: ~,3f%; height: ~,3f%; width: ~,3f%;"
            ;; top
            (if (in-day? day (attr ev 'DTSTART))
                (* (/ 24) 100
                   (time->decimal-hour
                    (time-difference (attr ev 'DTSTART)
                                     (start-of-day* (attr ev 'DTSTART)))))
                0)

            ;; left
            (* 100 (width ev) (x-pos ev))

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
                                         time)))))

            ;; width
            (* 100 (width ev))
            ))
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
    (fix-event-widths! (stream->list events))
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

(define-public (html-main calendars events)
  `(html (head
          (title "Calendar")
          (meta (@ (charset "utf-8")))
          (link (@ (type "text/css")
                   (rel "stylesheet")
                   (href "static/style.css")))
          (style ,(format #f "~{.CAL_~a { background-color: ~a }~%~}"
                          (concat (map (lambda (c)
                                         (list
                                          (html-attr (if (pair? (attr c 'NAME))
                                                         (car (attr c 'NAME))
                                                         (attr c 'NAME)))
                                          (or (attr c 'COLOR) "white")))
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
                                        (d "2019-04-22"))
                                       car)
                              (group-stream events)))))))))

