(define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  #:use-module (vcomponent group)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util tree)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (output general)

  #:use-module (ice-9 getopt-long)

  #:use-module (parameters)
  #:use-module (config))

(define (date-link date)
  (date->string date "~Y-~m-~d"))

(define (time-link time)
  (time->string time "~Y-~m-~d"))

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
    (unless (null? tree)
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

  `(a (@ (href "#" ,(time->string (attr ev 'DTSTART) "~s") ,(attr ev 'UID)))
      (div (@ (class "event CAL_" ,(html-attr (let ((l (attr (parent ev) 'NAME)))
                                                (if (pair? l) (car l) l)))
                ,(when (time<? (attr ev 'DTSTART) time)
                   " continued")
                ,(when (time<? (add-day time) (attr ev 'DTEND))
                   " continuing"))
              (style ,style))
           ,((summary-filter) ev (attr ev 'SUMMARY)))))

(define (lay-out-day day)
  (let* (((date . events) day))
    (fix-event-widths! (date->time-utc date) (stream->list events))
    `(div (@ (class "day"))
          (div (@ (class "meta"))
               ,(let ((str (date-link date)))
                  `(span (@ (id ,str) (class "daydate")) ,str))
               (span (@ (class "dayname")) ,(date->string date "~a")))
          (div (@ (class "events"))
               ,@(map (lambda (time)
                        `(div (@ (class "clock clock-" ,time)) ""))
                      (iota 12 0 2))
               ,@(stream->list (stream-map (lambda (e) (vevent->sxml date e)) events))))))

(define (time-marker-div)
  `(div (@ (class "sideclock"))
        (div (@ (class "day"))
             (div (@ (class "meta")) "")
             (div (@ (class "clockbar"))
                  ,@(map (lambda (time)
                           `(div (@ (class "clock clock-" ,time))
                                 (span (@ (class "clocktext"))
                                       ,time ":00")))
                          (iota 12 0 2))))))

(define (include-css path)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path))))

(define opt-spec
  '((from (value #t) (single-char #\f))
    (to (value #t) (single-char #\t))))

(define (fmt-time-span ev)
  (let* ((fmt (if (time<? (time-difference (attr ev 'DTEND) (attr ev 'DTSTART))
                          (make-duration (* 3600 24)))
                  "~H:~M" "~Y-~m-~d ~H:~M"))
         (start (time->string (attr ev 'DTSTART) fmt))
         (end (time->string (attr ev 'DTEND) fmt)))
    (values start end)))

(define (fmt-single-event ev)
  `(article (@ (id ,(time->string (attr ev 'DTSTART) "~s")
                   ,(html-attr (attr ev 'UID)))
               (class "eventtext CAL_bg_"
                 ,(html-attr (let ((l (attr (parent ev) 'NAME)))
                               (if (pair? l) (car l) l)))))
            (h1 (a (@ (href "#" ,(time-link (attr ev 'DTSTART))))
                   ,(attr ev 'SUMMARY)))
            (main
             ,(let* ((start end (fmt-time-span ev)))
                `(div ,start " — " ,end))
             ,(when (and=> (attr ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ") ,(attr ev 'LOCATION)))
             ,(attr ev 'DESCRIPTION))))

(define (fmt-day day)
  (let* (((date . events) day))
    `(div (@ (class "text-day"))
          ;; TODO this gives date +1
          (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                         `(a (@ (href "#" ,s)) ,s))))
          ,@(map fmt-single-event (stream->list events)))))

(define (days-in-month n)
  (cond ((memv n '(1 3 5 7 8 10 12)) 31)
        ((memv n '(4 6 9 11)) 30)
        ;; TODO leap years
        (else 28)))

(define (previous-month n)
  (1+ (modulo (- n 2) 12)))

(define (next-month n)
  (1+ (modulo n 12)))

(define (td param)
  (lambda (d) `(td (@ ,(map (lambda (p)
                         (cons `(quote ,(car p))
                               (cdr p)))
                       param)) ,d)))

;; 0 indexed, starting at monday.
(define (week-day date)
  (modulo (1- (date-week-day date)) 7))

;; date should be start of month
(define (cal-table date)
  (let ((td (lambda (p) (lambda (d) `(td (@ ,p) ,d)))))
    `(table (@ (class "small-calendar"))
            (thead (tr ,@(map (lambda (d) `(td ,d)) '(MÅ TI ON TO FR LÖ SÖ))))
            (tbody ,@(let recur
                         ((lst (let* ((month (date-month date))
                                      (month-len (days-in-month month))
                                      (prev-month-len (days-in-month (previous-month month)))
                                      (month-start (week-day date)))
                                 (append (map (td '(class "prev"))
                                              (iota month-start (- prev-month-len month-start)))
                                         (map (td '(class "cur"))
                                              (map (lambda (d) `(a (@ (href "#" ,(date->string date "~Y-~m-") ,d)) ,d))
                                                   (iota month-len 1)))
                                         (map (td '(class "next"))
                                              (iota (modulo (- (* 7 5) month-len month-start) 7) 1))))))
                       (unless (null? lst)
                         (let* ((w rest (split-at lst 7)))
                           (cons `(tr ,@w)
                                 (recur rest)))))))))

(define-public (html-main calendars events args)

  (define opts (getopt-long args opt-spec))

  (define start (parse-freeform-date (option-ref opts 'from "2019-04-15")))
  (define end   (parse-freeform-date (option-ref opts 'to   "2019-05-10")))

  (define evs (get-groups-between (group-stream events)
                                  start end))

  ((@ (sxml simple) sxml->xml)
   `(html (head
           (title "Calendar")
           (meta (@ (charset "utf-8")))
           ,(include-css "static/style.css")
           (style ,(format #f "~:{.CAL_~a { background-color: ~a; color: ~a }~%.CAL_bg_~a { border-color: ~a }~%~}"
                           (map (lambda (c)
                                  (let* ((name (html-attr (if (pair? (attr c 'NAME))
                                                              (car (attr c 'NAME))
                                                              (attr c 'NAME))))
                                         (bg-color (attr c 'COLOR))
                                         (fg-color (and=> (attr c 'COLOR)
                                                          calculate-fg-color)))
                                    (list name (or bg-color 'white) (or fg-color 'black)
                                          name (or bg-color 'black))))
                                calendars))))
          (body
           (div (@ (class "root"))
                (div (@ (class "calendar"))
                     ,(time-marker-div)
                     (div (@ (class "days"))
                          ,@(stream->list (stream-map lay-out-day evs))))
                (div (@ (class "sideinfo"))
                     (div (@ (class "about"))
                          (div ,(cal-table (parse-freeform-date "2019-04-01"))))
                     (div (@ (class "eventlist"))
                          ,@(stream->list (stream-map fmt-day evs)))))))))
