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

  #:use-module (git)
  #:use-module (parameters)
  #:use-module (config))

(define (date-link date)
  (date->string date "~Y-~m-~d"))

(define (time-link time)
  (time->string time "~Y-~m-~d"))

(define x-pos (make-object-property))
(define width (make-object-property))

(define (UID ev)
  (string-append
   (time->string (attr ev 'DTSTART) "~s")
   (html-attr (attr ev 'UID))))

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

  `(a (@ (href "#" ,(UID ev))
         (class "hidelink"))
      (div (@ (class "event CAL_" ,(html-attr (attr (parent ev) 'NAME))
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
               ,@(map (lambda (e) (vevent->sxml date e)) (stream->list events))))))

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


(define (fmt-time-span ev)
  (let* ((fmt (if (time<? (time-difference (attr ev 'DTEND) (attr ev 'DTSTART))
                          (make-duration (* 3600 24)))
                  "~H:~M" "~Y-~m-~d ~H:~M"))
         (start (time->string (attr ev 'DTSTART) fmt))
         (end (time->string (attr ev 'DTEND) fmt)))
    (values start end)))

(define (fmt-single-event ev)
  `(article (@ (id ,(UID ev))
               (class "eventtext CAL_bg_"
                 ,(html-attr (attr (parent ev) 'NAME))))
            (h3 (a (@ (href "#" ,(time-link (attr ev 'DTSTART)))
                      (class "hidelink"))
                   ,(attr ev 'SUMMARY)))
            (div
             ,(let* ((start end (fmt-time-span ev)))
                `(div ,start " — " ,end))
             ,(when (and=> (attr ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ") ,(attr ev 'LOCATION)))
             ,(attr ev 'DESCRIPTION))))

(define (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              ;; TODO this gives date +1
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
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

(define* (month+ date #:optional (change 1))
  (normalize-date* (set (date-month date) = (+ change))))

(define* (month- date #:optional (change -1))
  (month+ date change))

;; date should be start of month
;; @example
;; må ti on to fr lö sö
;;  1  2  3  4  5  6  7
;;  8  9 10 11 12 13 14
;; 15 16 17 18 19 20 21
;; 22 23 24 25 26 27 28
;; 29 30
;; @end example
(define (cal-table date today)
  #;(define (pad0 d) (when (< d 10) (format #f h0)))
  (define (pad0 d) (format #f "~2,'0d" d))
  (let ((td (lambda (attr other-date)
              (lambda (d)
                `(td (@ ,attr)
                     (a (@ (href ,(date->string other-date "~Y-~m-~d")
                                 ".html#" ,(date->string other-date "~Y-~m-")
                                 ,(pad0 d))
                           (class "hidelink")) ,d))))))

    `(table (@ (class "small-calendar"))
            (thead (tr ,@(map (lambda (d) `(td ,d)) '(MÅ TI ON TO FR LÖ SÖ))))
            (tbody ,@(let recur
                         ((lst (let* ((month (date-month date))
                                      (month-len (days-in-month month))
                                      (prev-month-len (days-in-month (previous-month month)))
                                      (month-start (week-day date)))
                                 (append (map (td '(class "prev") (month- date))
                                              (iota month-start (- prev-month-len month-start)))
                                         (map (lambda (p) `(td (@ ,@(assq-merge '((class " cur ")) (cdar p)))
                                                          ,@(cdr p)))
                                              ;; TODO only today in current month
                                              (map (lambda (d) `((@ (class ,(when (= d (date-day today)) "today")))
                                                            (a (@ (href "#" ,(date->string date "~Y-~m-")
                                                                        ,(pad0 d))
                                                                  (class "hidelink")) ,d)))
                                                   (iota month-len 1)))
                                         (map (td '(class "next") (month+ date))
                                              (iota (modulo (- (* 7 5) month-len month-start) 7) 1))))))
                       (unless (null? lst)
                         (let* ((w rest (split-at lst 7)))
                           (cons `(tr ,@w)
                                 (recur rest)))))))))

(define repo-url (make-parameter "https://git.hornquist.se"))

(define-public (html-generate calendars events start end)
  (define evs (get-groups-between (group-stream events)
                                  start end))

  ;; (display "<!doctype HTML>") (newline)
  (define (nav-link display date)
    `(a (@ (href ,(date->string date "~Y-~m-~d") ".html")
           (class "nav hidelink"))
        (div (@ (class "nav"))
             ,display)))

  ((@ (sxml simple) sxml->xml)
   `(html (@ (lang sv))
          (head
           (title "Calendar")
           (meta (@ (charset "utf-8")))
           (meta (@ (name viewport)
                    (content "width=device-width, initial-scale=0.5")))
           (meta (@ (name description)
                    (content "Calendar for the dates between " ,(date->string start)
                             " and " ,(date->string end))))
           ,(include-css "static/style.css")
           (script (@ (src "static/script.js")) "")
           (style ,(format #f "~:{.CAL_~a { background-color: ~a; color: ~a }~%.CAL_bg_~a { border-color: ~a }~%~}"
                           (map (lambda (c)
                                  (let* ((name (html-attr (attr c 'NAME)))
                                         (bg-color (attr c 'COLOR))
                                         (fg-color (and=> (attr c 'COLOR)
                                                          calculate-fg-color)))
                                    (list name (or bg-color 'white) (or fg-color 'black)
                                          name (or bg-color 'black))))
                                calendars))))

          (body
           (div (@ (class "root"))
                (main
                 ;; Actuall calendar
                 (div (@ (class "calendar"))
                      ,(time-marker-div)
                      (div (@ (class "days"))
                           ,@(stream->list (stream-map lay-out-day evs))))

                 ;; Page footer
                 (footer (span "Page generated " ,(date->string (current-date)))
                         (span (a (@ (href ,(repo-url) "/calparse"))
                                  "Source Code"))
                         ,(let* ((hash (get-git-version))
                                 (url (format #f "~a/calparse/commit/?id=~a"
                                              (repo-url) hash)))
                            `(span "Version " (a (@ (href ,url)) ,hash)))))

                ;; Whole sidebar
                (aside (@ (class "sideinfo"))
                       ;; Small calendar and navigation
                       (div (@ (class "about"))
                            ;; prev button
                            ,(nav-link "«" (month- start))

                            ;; calendar table
                            (div ,(cal-table (start-of-month start)
                                             (current-date)))

                            ;; next button
                            ,(nav-link "»" (month+ start)))

                       ;; List of events
                       (div (@ (class "eventlist"))
                            ,@(stream->list (stream-map fmt-day evs)))))))))

(define opt-spec
  '((from (value #t) (single-char #\f))
    (to (value #t) (single-char #\t))
    (chunked)
    )
  )

(define-public (html-main calendars events args)
  (define opts (getopt-long args opt-spec))

  (cond [(option-ref opts 'chunked #f)
         (let* ((start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                             [else (start-of-month (current-date))])))

           (stream-for-each (lambda (pair)
                              (format (current-error-port) "d = ~a~%u = ~a~%" (car pair) (cadr pair))
                              (let ((fname (format #f "./html/~a.html" (date->string (car pair) "~1"))))
                                (format (current-error-port) "Writing to [~a]~%" fname)
                                (with-output-to-file fname
                                  (lambda () (apply html-generate calendars events pair)))))
                            (let ((ms (month-stream start)))
                              (stream-take
                               12 (stream-zip
                                   ms (stream-map (lambda (d) (normalize-date
                                                          (set (date-day d) = (- 1))))
                                                  (stream-cdr ms))))
                              )))


         ]
        [else
         (let* ((start (cond [(option-ref opts 'from #f) => parse-freeform-date]
                            [else (start-of-month (current-date))]))
                (end (cond [(option-ref opts 'to  #f) => parse-freeform-date]
                           [else (normalize-date* (set (date-month start) = (+ 1)))])))
           (html-generate calendars events start end))]))
