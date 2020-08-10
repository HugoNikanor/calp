(define-module (html vcomponent)
  :use-module (util)
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (datetime)
  :use-module (html util)
  :use-module ((output general) :select (calculate-fg-color))
  :use-module ((vcomponent datetime output)
               :select (fmt-time-span
                        format-description
                        format-recurrence-rule
                                      ))
  )

(define-public (compact-event-list list)

  (define calendars
   (delete-duplicates!
    (filter (lambda (x) (eq? 'VCALENDAR (type x)))
            (map parent list))
    eq?))

  (define (summary event)
    `(summary (div (@ (class "summary-line "))
                   (span (@ (class "square CAL_"
                              ,(html-attr
                                (or (prop (parent event)
                                          'NAME)
                                    "unknown")))))
                   (time ,(let ((dt (prop event 'DTSTART)))
                            (if (datetime? dt)
                                (datetime->string dt "~Y-~m-~d ~H:~M")
                                (date->string dt "~Y-~m-~d" ))))
                   (span ,(prop event 'SUMMARY)))))
  (cons
   (calendar-styles calendars)
   (for event in list
        `(details
          ,(summary event)
          ;; TODO better format, add show in calendar button
          ,(fmt-single-event event)))))

;; For sidebar, just text
(define*-public (fmt-single-event ev
                                  optional: (attributes '())
                                  key: (fmt-header list))
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (prop ev 'X-HNH-FILENAME))
  `(article (@ ,@(assq-merge
                  attributes
                  `((class "eventtext CAL_bg_"
                      ,(html-attr (or (prop (parent ev) 'NAME) "unknown"))
                      ,(when (and (prop ev 'PARTSTAT)
                                  (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                         " tentative")))))
            (h3 ,(fmt-header
                  (when (prop ev 'RRULE)
                    `(span (@ (class "repeating")) "↺"))
                  `(span (@ (class "summary")) ,(prop ev 'SUMMARY))))
            (div
             ,(call-with-values (lambda () (fmt-time-span ev))
                (case-lambda [(start) `(div (span (@ (class "dtstart")
                                                     (data-fmt "%L%H:%M"))
                                                  ,start))]
                             [(start end) `(div (span (@ (class "dtstart")
                                                         ;; TODO same format string
                                                         ;; as fmt-time-span used
                                                         (data-fmt "%L%H:%M"))
                                                      ,start)
                                                " — "
                                                (span (@ (class "dtend")
                                                         (data-fmt "%L%H:%M"))
                                                      ,end))]))
             ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
                `(div (b "Plats: ")
                      (div (@ (class "location"))
                           ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                        (prop ev 'LOCATION)))))
             ,(and=> (prop ev 'DESCRIPTION)
                     (lambda (str) (format-description ev str)))
             ,(awhen (prop ev 'RRULE)
                     `(span (@ (class "rrule"))
                            ,@(format-recurrence-rule ev)))
             ,(when (prop ev 'LAST-MODIFIED)
                `(span (@ (class "last-modified")) "Senast ändrad "
                       ,(datetime->string (prop ev 'LAST-MODIFIED) "~1 ~H:~M")))

             )))


;; Single event in side bar (text objects)
(define-public (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map
                  (lambda (ev) (fmt-single-event
                           ev `((id ,(html-id ev)))
                           fmt-header:
                           (lambda body
                             `(a (@ (href "#" ,(date-link (as-date (prop ev 'DTSTART))))
                                    (class "hidelink"))
                                 ,@body))))
                  (stream-filter
                   (lambda (ev)
                     ;; If start was an earlier day
                     ;; This removes all descriptions from
                     ;; events for previous days,
                     ;; solving duplicates.
                     (date/-time<=? date (prop ev 'DTSTART)))
                   events))))))


(define-public (calendar-styles calendars)
  `(style
       ,(format
         #f "~:{.CAL_~a { background-color: ~a; color: ~a }~%.CAL_bg_~a { border-color: ~a }~%~}"
         (map (lambda (c)
                (let* ((name (html-attr (prop c 'NAME)))
                       (bg-color (prop c 'COLOR))
                       (fg-color (and=> (prop c 'COLOR)
                                        calculate-fg-color)))
                  (list name (or bg-color 'white) (or fg-color 'black)
                        name (or bg-color 'black))))
              calendars))))
