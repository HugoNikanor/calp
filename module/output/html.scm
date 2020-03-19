(define-module (output html)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:use-module (srfi srfi-41 util)
  #:use-module (vcomponent)
  #:use-module (vcomponent group)
  #:use-module (vcomponent datetime)
  #:use-module (util)
  #:use-module (util tree)
  #:duplicates (last)
  #:use-module (datetime)
  #:use-module (datetime util)
  #:use-module (output general)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 match)


  #:use-module (git)
  #:use-module (parameters)
  )

(define (td param)
  (lambda (d) `(td (@ ,(map (lambda (p)
                         (cons `(quote ,(car p))
                               (cdr p)))
                       param)) ,d)))

(define (date-link date)
  (date->string date "~Y-~m-~d"))

(define x-pos (make-object-property))
(define width (make-object-property))

(define (UID ev)
  (string-append
   ;; (date/-time->string (attr ev 'DTSTART) "~s")
   (date->string (as-date (attr ev 'DTSTART)) "~Y~m~d")
   (time->string (as-time (attr ev 'DTSTART)) "~H~M~S")
   (html-attr (attr ev 'UID))))

;; Takes a list of vcomponents, sets their widths and x-positions to optimally
;; fill out the space, without any overlaps.
(define (fix-event-widths! date lst)
  ;; The tree construction is greedy. This means
  ;; that if  a smaller  event preceeds a longer
  ;; event it would capture  the longer event to
  ;; only find  events which  also overlaps  the
  ;; smaller event.

  ;; @var{x} is how for left in the container we are.
  (let inner ((x 0)
              (tree (make-tree overlapping?
                               (sort* lst time>?
                                      (lambda (e) (event-length/day date e))))))
    (unless (null? tree)
      (let ((w (/ (- 1 x)
                  (+ 1 (length-of-longst-branch (left-subtree tree))))))
        (set! (width (car tree)) w
              (x-pos (car tree)) x)
        (inner (+ x w) (left-subtree tree))
        (inner x (right-subtree tree))))))

;; This should only be used on time intervals, never on absolute times.
;; For that see @var{date->decimal-hour}.
;; NOTE Above comment probably deprecated
(define (time->decimal-hour time)
  (exact->inexact (+ (hour time)
                     (/ (minute time) 60)
                     (/ (second time) 3600))))

(define (html-attr str)
  (define cs (char-set-adjoin char-set:letter+digit #\- #\_))
  (string-filter (lambda (c) (char-set-contains? cs c)) str))

(define (create-block-general date ev fmt)
 ;; (define time (date->time-utc day))
  (define style
    (format #f fmt

            (* 100 (x-pos ev))          ; left
            (* 100 (width ev))          ; width

            ;; top
            (if (date= date (as-date (attr ev 'DTSTART)))
                (* 100/24
                   (time->decimal-hour
                    (as-time (attr ev 'DTSTART))))
                0)

            ;; height
            (* 100/24 (time->decimal-hour (event-length/day date ev)))))

  `(a (@ (href "#" ,(UID ev))
         (class "hidelink"))
      (div (@ (class "event CAL_" ,(html-attr (attr (parent ev) 'NAME))
                ,(when (and (attr ev 'PARTSTAT) (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                   " tentative")
                ,(when (date<? (as-date (attr ev 'DTSTART)) date)
                   " continued")
                ;; TODO all day events usually have the day after as DTEND.
                ;; So a whole day event the 6 june would have a DTEND of the
                ;; 7 june.
                ,(when (date<? date (as-date (attr ev 'DTEND)))
                   " continuing"))
              (style ,style))
           ,((summary-filter) ev (attr ev 'SUMMARY))))

  )

;; Format single event for graphical display
(define (create-block date ev)
  (create-block-general date ev "left:~,3f%;width:~,3f%;top:~,3f%;height:~,3f%;"))

(define (create-top-block date ev)
  (create-block-general date ev "top:~,3f%;height:~,3f%;left:~,3f%;width:~,3f%;"))

;; Lay out complete day (graphical)
;; (date . (events)) -> sxml
(define (lay-out-day day)
  (let* (((day-date . events) day)
         (time-obj (datetime date: day-date))
         (long-events short-events
                      (partition (lambda (ev)
                                   (or (date? (attr ev 'DTSTART))
                                       (datetime<=? (datetime date: (date day: 1))
                                                    (datetime-difference (attr ev 'DTEND)
                                                                         (attr ev 'DTSTART)))))
                                 (stream->list events))))

    (fix-event-widths! day-date short-events)
    (fix-event-widths! day-date long-events)
    `(div (@ (class "day"))
          (div (@ (class "meta"))
               ,(let ((str (date-link day-date)))
                  `(span (@ (id ,str) (class "daydate")) ,str))
               (span (@ (class "dayname")) ,(date->string day-date "~a")))
          (div (@ (class "wholeday"))
               "" ; To prevent self closing div tag
               ,@(map (lambda (e) (create-top-block day-date e))
                      long-events))
          (div (@ (class "events"))
               "" ; To prevent self closing div tag
               ,@(map (lambda (time)
                        `(div (@ (class "clock clock-" ,time)) ""))
                      (iota 12 0 2))
               ,@(map (lambda (e) (create-block day-date e)) short-events)))))

(define (time-marker-div)
  `(div (@ (class "sideclock"))
        (div (@ (class "day"))
             (div (@ (class "meta")) "")
             (div (@ (class "wholeday")) "")
             (div (@ (class "events clockbar"))
                  ,@(map (lambda (time)
                           `(div (@ (class "clock clock-" ,time))
                                 (span (@ (class "clocktext"))
                                       ,time ":00")))
                         (iota 12 0 2))))))

(define (include-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "stylesheet")
            (href ,path)
            ,@extra-attributes)))

(define (include-alt-css path . extra-attributes)
  `(link (@ (type "text/css")
            (rel "alternate stylesheet")
            (href ,path)
            ,@extra-attributes)))

(define (fmt-time-span ev)
  (cond [(attr ev 'DTSTART) date?
         => (lambda (s)
              (cond [(attr ev 'DTEND)
                     => (lambda (e)
                          (if (date= e (date+ s (date day: 1)))
                              (values (date->string s) "")
                              (values (date->string s)
                                      (date->string e))))]
                    [else (date->string s)]))]
        [else ; guaranteed datetime
         (let ((s (attr ev 'DTSTART))
               (e (attr ev 'DTEND)))
           (let ((s-str (time->string (get-time s) "~H:~M"))
                 (e-str (time->string (get-time e) "~H:~M")))
             (if (date= (get-date s) (get-date e))
                 (values s-str e-str)
                 (values (string-append (date->string (get-date s) "~Y-~m-~d ") s-str)
                         (string-append (date->string (get-date e) "~Y-~m-~d ") e-str)))))]))


;; For sidebar, just text
(define (fmt-single-event ev)
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (attr ev 'X-HNH-FILENAME))
  `(article (@ (id ,(UID ev))
               (class "eventtext CAL_bg_"
                 ,(html-attr (attr (parent ev) 'NAME))
                 ,(when (and (attr ev 'PARTSTAT) (string= "TENTATIVE" (attr ev 'PARTSTAT)))
                    " tentative")))
            (h3 (a (@ (href "#" ,(date-link (as-date (attr ev 'DTSTART))))
                      (class "hidelink"))
                   ,(attr ev 'SUMMARY)))
            (div
             ,(let* ((start end (fmt-time-span ev)))
                `(div ,start " — " ,end))
             ,(when (and=> (attr ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ") ,(attr ev 'LOCATION)))
             ,(and=> (attr ev 'DESCRIPTION) (lambda (str) ((description-filter) ev str))))))

;; Single event in side bar (text objects)
(define (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map fmt-single-event
                             (stream-filter
                              (lambda (ev)
                                ;; If start was an earlier day
                                ;; This removes all descriptions from
                                ;; events for previous days,
                                ;; solving duplicates.
                                (date/-time<=? date
                                               (attr ev 'DTSTART)))
                              events))))))


;; Given a list, partitions it up into sublists of width length,
;;; each starting with 'tr.
(define (tablify list width)
  (unless (null? list)
    (let* ((row rest (split-at list width)))
      (cons `(tr ,@row)
            (tablify rest width)))))


;; date should be start of month
;; @example
;; må ti on to fr lö sö
;;  1  2  3  4  5  6  7
;;  8  9 10 11 12 13 14
;; 15 16 17 18 19 20 21
;; 22 23 24 25 26 27 28
;; 29 30
;; @end example
;; date - a date in the month to display
;; today - used to highlight current date
;; week-start - which day the week begins on, see (datetime util)
(define (cal-table date today week-start)
  (define ((td attr) date)
    `(td (@ ,attr)
         (a (@ (href ,(date->string (set (day date) 1) "~Y-~m-~d")
                     ".html#" ,(date->string date "~Y-~m-~d"))
               (class "hidelink"))
            ,(day date))))

  `(table (@ (class "small-calendar"))
          (thead (tr ,@(map (lambda (d) `(td ,(week-day-name d 2)))
                            (weekday-list week-start))))

          ((tbody ,@(let* ((last current next
                                 (month-days date week-start)))
                      (define lst
                        (append
                         ;; ... 28 29 |
                         (map (td '(class "prev")) last)
                         ;; 1 2 ... 30 31
                         (map (td '(class "cur")) current)
                         ;; | 1 2 ...
                         (map (td '(class "next")) next)))

                      (tablify lst 7))))))


(define repo-url (make-parameter "https://git.hornquist.se"))


(define-public (render-calendar event-groups)
  `(div (@ (class "calendar"))
        ,(time-marker-div)
       (div (@ (class "days"))
            ,@(stream->list (stream-map lay-out-day event-groups)))))

;; TODO this currently only popuplates the table with the number of events for
;;      each day. It should show all the actual events.
;; (stream event-group) -> sxml
(define (render-calendar-table event-groups)
  `(div (@ (class "caltable"))
        ,@(map (lambda (d) `(div (@ (class "thead")) ,(week-day-name d)))
               (weekday-list (week-start)))
        ,@(stream->list
           (stream-map
            (match-lambda
              [(day-date . events)
               `(div (@ (class "tbody"))
                     ,(stream-length events))])
            event-groups))))


;;; NOTE
;;; The side bar filters all earlier events for each day to not create repeats,
;;; and the html-generate procedure also filters, but instead to find earlier eventns.
;;; All this filtering is probably slow, and should be looked into.



(define-public (html-generate calendars events start-date end-date render-calendar)
  ;; TODO maybe don't do this again for every month
  (define evs (get-groups-between (group-stream events)
                                  start-date end-date))


  (define (nav-link display date)
    `(a (@ (href ,(date->string date "~Y-~m-~d") ".html")
           (class "nav hidelink"))
        (div (@ (class "nav"))
             ,display)))

  (display "<!doctype HTML>\n")
  ((@ (sxml simple) sxml->xml)
   `(html (@ (lang sv))
          (head
           (title "Calendar")
           (meta (@ (charset "utf-8")))
           (meta (@ (http-equiv "Content-Type") (content "application/xhtml+xml")))
           (meta (@ (name viewport)
                    (content "width=device-width, initial-scale=0.5")))
           (meta (@ (name description)
                    (content "Calendar for the dates between " ,(date->string start-date)
                             " and " ,(date->string end-date))))
           ,(include-css "static/style.css")
           ,(include-alt-css "static/dark.css"  '(title "Dark"))
           ,(include-alt-css "static/light.css" '(title "Light"))
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
                 ,(render-calendar evs)

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
                            ,(nav-link "«" (month- start-date))

                            ;; calendar table
                            (div ,(cal-table (start-of-month start-date)
                                             (current-date)
                                             (week-start)))

                            ;; next button
                            ,(nav-link "»" (month+ start-date)))


                       ;; List of events
                       (div (@ (class "eventlist"))
                            ;; List of calendars
                            (div (@ (class "calendarlist"))
                                 (ul ,@(map (lambda (calendar)
                                              `(li (@ (class "CAL_bg_" ,(html-attr (attr calendar 'NAME))))
                                                   ,(attr calendar 'NAME)))
                                            calendars)))

                            ;; Events which started before our start point, but "spill" into our time span.
                            (section (@ (class "text-day"))
                                     (header (h2 "Tidigare"))
                                     ,@(stream->list
                                        (stream-map fmt-single-event
                                                    (stream-take-while (compose (cut date/-time<? <> start-date)
                                                                                (extract 'DTSTART))
                                                                       (cdr (stream-car evs))))))
                            ,@(stream->list (stream-map fmt-day evs)))))))))


(define-public (html-chunked-main count calendars events start-date)
  ;; TODO This still doesn't account for PWD, file existing but is of
  ;; wrong type, html directory existing but static symlink missing,
  ;; static being a different file type, and probably something else
  ;; i'm missing.
  (unless (file-exists? "./html")
    (mkdir "./html")
    (symlink "../static" "./html/static"))

  ;; NOTE Something here isn't thread safe.
  ;; TODO make it thread safe
  (stream-for-each
   (match-lambda
     [(start-date end-date)
      (let ((fname (format #f "./html/~a.html" (date->string start-date "~1"))))
        (format (current-error-port) "Writing to [~a]~%" fname)
        (with-output-to-file fname
          (lambda () (html-generate calendars events start-date end-date render-calendar))))])
   (let ((ms (month-stream start-date)))
     (with-streams
      (take count
            (zip ms
                 (map (cut date- <> (date day: 1)) ; last in month
                      (cdr ms))))))))


(define-public (html-table-main count calendars events start-date)
  ;; TODO same file creation as in html-chunked-main
  (stream-for-each
   (lambda (start-of-month)
     (let ((fname (format #f "./html/~a.html" (date->string start-of-month "~1"))))
       (format (current-error-port) "Writing to [~a]~%" fname)
       (let* ((before current after (month-days start-of-month (week-start))))
         (with-output-to-file fname
           ;; TODO this produces incorrect next and prev links
           ;; TODO It actually produces almost all date links wrong
           (lambda () (html-generate calendars events
                                ;; Appends for case where before or after is empty
                                (car (append before current))
                                (last (append current after))
                                render-calendar-table))))))
   (stream-take count (month-stream start-date))))
