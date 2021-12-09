(define-module (calp html vcomponent)
  :use-module (calp util)
  :use-module ((calp util exceptions) :select (warning))
  :use-module (vcomponent)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module ((rnrs io ports) :select (put-bytevector))
  :use-module (datetime)
  :use-module ((text util) :select (add-enumeration-punctuation))
  :use-module ((web uri-query) :select (encode-query-parameters))
  :use-module (calp html util)
  :use-module ((calp html config) :select (edit-mode debug))
  :use-module ((calp html components) :select (btn tabset form with-label))
  :use-module ((calp util color) :select (calculate-fg-color))
  :use-module ((crypto) :select (sha256 checksum->string))
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module ((vcomponent recurrence) :select (repeating?))
  :use-module ((vcomponent recurrence internal) :prefix #{rrule:}#)
  :use-module ((vcomponent datetime output)
               :select (fmt-time-span
                        format-description
                        format-summary
                        format-recurrence-rule
                                      ))
  :use-module ((calp util config) :select (get-config))
  :use-module ((base64) :select (base64encode))
  )

;; used by search view
(define-public (compact-event-list list)

  (define calendars
   (delete-duplicates!
    (filter (lambda (x) (eq? 'VCALENDAR (type x)))
            (map parent list))
    eq?))

  (define (summary event)
    `(summary (div (@ (class "summary-line "))
                   (span (@ (class "square")
                            (data-calendar
                             ,(base64encode
                               (or (prop (parent event)
                                         'NAME)
                                   "unknown")))))
                   (time ,(let ((dt (prop event 'DTSTART)))
                            (if (datetime? dt)
                                (datetime->string dt "~Y-~m-~d ~H:~M")
                                (date->string dt "~Y-~m-~d" ))))
                   (a (@ (href ,(date->string (as-date (prop event 'DTSTART)) "/week/~Y-~m-~d.html")))
                      "View 📅")
                   (span ,(prop event 'SUMMARY)))))
  (cons
   (calendar-styles calendars)
   (for event in list
        `(details
          ,(summary event)
          ;; TODO better format
          ,(fmt-single-event event)))))

;; Format event as text.
;; Used in
;; - sidebar
;; - popup overwiew tab
;; - search result (event details)
(define*-public (fmt-single-event ev
                                  optional: (attributes '())
                                  key: (fmt-header list))
  ;; (format (current-error-port) "fmt-single-event: ~a~%" (prop ev 'X-HNH-FILENAME))
  `(vevent-description
    (@ ,@(assq-merge
          attributes
          `(
            (class ,(when (and (prop ev 'PARTSTAT)
                               (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                      " tentative "))
            (data-uid ,(prop ev 'UID)))))
    (h3 ,(fmt-header
          (when (prop ev 'RRULE)
            `(span (@ (class "repeating")) "↺"))
          `(span (@ (class "bind summary")
                    (data-property "summary"))
                 ,(prop ev 'SUMMARY))))
    (div
     ,(call-with-values (lambda () (fmt-time-span ev))
        (case-lambda [(start)
                      `(div (time (@ (class "bind dtstart")
                                     (data-property "dtstart")
                                     (data-fmt ,(string-append "~L" start))
                                     (datetime ,(datetime->string
                                                 (as-datetime (prop ev 'DTSTART))
                                                 "~1T~3")))
                                  ,(datetime->string
                                    (as-datetime (prop ev 'DTSTART))
                                    start)))]
                     [(start end)
                      `(div (time (@ (class "bind dtstart")
                                     (data-property "dtstart")
                                     (data-fmt ,(string-append "~L" start))
                                     (datetime ,(datetime->string
                                                 (as-datetime (prop ev 'DTSTART))
                                                 "~1T~3")))
                                  ,(datetime->string (as-datetime (prop ev 'DTSTART))
                                                     start))
                            " — "
                            (time (@ (class "bind dtend")
                                     (data-property "dtend")
                                     (data-fmt ,(string-append "~L" end))
                                     (datetime ,(datetime->string
                                                 (as-datetime (prop ev 'DTSTART))
                                                 "~1T~3")))
                                  ,(datetime->string (as-datetime (prop ev 'DTEND))
                                                     end)))]))

     ;; TODO add optional fields when added in frontend
     ;; Possibly by always having them here, just hidden.

     (div (@ (class "fields"))
          ,(when (and=> (prop ev 'LOCATION) (negate string-null?))
             `(div (b "Plats: ")
                   (div (@ (class "bind location") (data-property "location"))
                        ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                     (prop ev 'LOCATION)))))
          ,(awhen (prop ev 'DESCRIPTION)
                  `(div (@ (class "bind description")
                           (data-property "description"))
                        ,(format-description ev it)))

          ,@(awhen (prop* ev 'ATTACH)
                   ;; attach satisfies @code{vline?}
                   (for attach in it
                        (if (and=> (param attach 'VALUE)
                                   (lambda (p) (string=? "BINARY" (car p))))
                            ;; Binary data
                            ;; TODO guess datatype if FMTTYPE is missing
                            (awhen (and=> (param attach 'FMTTYPE)
                                          (lambda (it) (string-split
                                                   (car it) #\/)))
                                   ;; TODO other file formats
                                   (when (string=? "image" (car it))
                                     (let* ((chk (-> (value attach)
                                                     sha256
                                                     checksum->string))
                                            (dname
                                             (path-append (xdg-runtime-dir)
                                                          "calp-data" "images"))
                                            (filename (-> dname
                                                          (path-append chk)
                                                          ;; TODO second part of mimetypes
                                                          ;; doesn't always result in a valid
                                                          ;; file extension.
                                                          ;; Take a look in mime.types.
                                                          (string-append "." (cadr it)))))
                                       (unless (file-exists? filename)
                                         ;; TODO handle tmp directory globaly
                                         (mkdir (dirname dname))
                                         (mkdir dname)
                                         (call-with-output-file filename
                                           (lambda (port)
                                             (put-bytevector port (value attach)))))
                                       (let ((link (path-append
                                                    "/tmpfiles"
                                                    ;; TODO better mimetype to extension
                                                    (string-append chk "." (cadr it)))))
                                         `(a (@ (href ,link))
                                             (img (@ (class "attach")
                                                     (src ,link))))))))
                            ;; URI
                            (cond ((and=> (param attach 'FMTTYPE)
                                          (compose (cut string= <> "image" 0 5) car))
                                   `(img (@ (class "attach")
                                            (src ,(value attach)))))
                                  (else `(a (@ (class "attach")
                                               (href ,(value attach)))
                                            ,(value attach)))))))

          ;; TODO add bind once I figure out how to bind lists
          ,(awhen (prop ev 'CATEGORIES)
                  `(div (@ (class "categories"))
                        ,@(map (lambda (c)
                                 `(a (@ (class "category")
                                        ;; TODO centralize search terms
                                        (href
                                         "/search/?"
                                         ,(encode-query-parameters
                                           `((q . (member
                                                   ,(->string c)
                                                   (or (prop event 'CATEGORIES)
                                                       '())))))))
                                     ,c))
                               it)))

          ;; TODO bind
          ,(awhen (prop ev 'RRULE)
                  `(div (@ (class "rrule"))
                        ,@(format-recurrence-rule ev)))

          ,(when (prop ev 'LAST-MODIFIED)
             `(div (@ (class "last-modified")) "Senast ändrad "
                   ,(datetime->string (prop ev 'LAST-MODIFIED) "~1 ~H:~M"))))

     )))

(define*-public (fmt-for-edit ev
                              optional: (attributes '())
                              key: (fmt-header list))
  `(vevent-edit (@ (class "vevent")
                   (data-uid ,(prop ev 'UID)))))

(define-public (edit-template calendars)
 `(div (@ (class " eventtext edit-tab "))
       (form (@ (class "edit-form"))
             (select (@ (class "calendar-selection"))
               (option "- Choose a Calendar -")
               ,@(let ((dflt (get-config 'default-calendar)))
                   (map (lambda (calendar)
                          (define name (prop calendar 'NAME))
                          `(option (@ (value ,(base64encode name))
                                      ,@(when (string=? name dflt)
                                          '((selected))))
                                   ,name))
                        calendars)))
             (h3 (input (@ (type "text")
                           (placeholder "Sammanfattning")
                           (name "summary") (required)
                           (data-property "summary")
                           ; (value ,(prop ev 'SUMMARY))
                           )))

             (div (@ (class "timeinput"))

                  ,@(with-label
                     "Starttid"
                     '(date-time-input (@ (name "dtstart")
                                          (data-property "dtstart")
                                          )))

                  ,@(with-label
                     "Sluttid"
                     '(date-time-input (@ (name "dtend")
                                          (data-property "dtend"))))

                  (div (@ (class "checkboxes"))
                   ,@(with-label
                      "Heldag?"
                      `(input (@ (type "checkbox")
                                 (name "wholeday")
                                 )))
                   ,@(with-label
                      "Upprepande?"
                      `(input (@ (type "checkbox")
                                 (name "has_repeats")
                                 ))))

                  )

             ,@(with-label
                "Plats"
                `(input (@ (placeholder "Plats")
                           (name "location")
                           (type "text")
                           (data-property "location")
                           ; (value ,(or (prop ev 'LOCATION) ""))
                           )))

             ,@(with-label
                "Beskrivning"
                `(textarea (@ (placeholder "Beskrivning")
                              (data-property "description")
                              (name "description"))
                           ; ,(prop ev 'DESCRIPTION)
                           ))

             ,@(with-label
                "Kategorier"
                `(input-list
                  (@ (name "categories")
                     (data-property "categories"))
                  (input (@ (type "text")
                            (placeholder "Kattegori")))))

             ;; ,@(with-label
             ;;    "Kategorier"
             ;;    ;; It would be better if these input-list's worked on the same
             ;;    ;; class=bind system as the fields above. The problem with that
             ;;    ;; is however that each input-list requires different search
             ;;    ;; and join procedures. Currently this is bound in the JS, see
             ;;    ;; [CATEGORIES_BIND].
             ;;    ;; It matches on ".input-list[data-property='categories']".
             ;;    `(div (@ (class "input-list")
             ;;             (data-property "categories"))
             ;;          #;
             ;;          ,@(awhen (prop ev 'CATEGORIES)
             ;;                   (map (lambda (c)
             ;;                          `(input (@ (size 2)
             ;;                                     (class "unit")
             ;;                                     (value ,c))))
             ;;                        it))

             ;;          (input (@ (class "unit final")
             ;;                    (size 2)
             ;;                    (type "text")
             ;;                    ))))

             ;; (hr)

             ;; ;; For custom user fields
             ;; ;; TODO these are currently not bound to anything, so entering data
             ;; ;; here does nothing. Bigest hurdle to overcome is supporting arbitrary
             ;; ;; fields which will come and go in the JavaScript.
             ;; ;; TODO also, all (most? maybe not LAST-MODIFIED) remaining properties
             ;; ;; should be exposed here.
             ;; (div (@ (class "input-list"))
             ;;      (div (@ (class "unit final newfield"))
             ;;           (input (@ (type "text")
             ;;                     (list "known-fields")
             ;;                     (placeholder "Nytt fält")))
             ;;           (select (@ (name "TYPE"))
             ;;             (option (@ (value "TEXT")) "Text"))
             ;;           (span
             ;;            (input (@ (type "text")
             ;;                      (placeholder "Värde"))))))

             ;; (hr)


             (input (@ (type "submit")))
             )))

;; (define-public (property-input-template)
;;   (div (@ (class ""))
;;        (input (@ (type "text")
;;                  (name "name")
;;                  (list "known-fields")
;;                  (placeholder "Nytt fält")))
;;        (select (@ (name "type"))
;;          (option (@ (value "TEXT")) "Text"))
;;        (span
;;         (input (@ (type "text")
;;                   (name "value")
;;                   (placeholder "Värde"))))))

;; (define (list-input-template)
;;  ;; It would be better if these input-list's worked on the same
;;  ;; class=bind system as the fields above. The problem with that
;;  ;; is however that each input-list requires different search
;;  ;; and join procedures. Currently this is bound in the JS, see
;;  ;; [CATEGORIES_BIND].
;;  ;; It matches on ".input-list[data-property='categories']".
;;  `(div (@ (class "input-list")
;;           (data-property "categories"))
;;        #;
;;        ,@(awhen (prop ev 'CATEGORIES) ;
;;        (map (lambda (c)              ;
;;        `(input (@ (size 2)           ;
;;        (class "unit")                ;
;;        (value ,c))))                 ;
;;        it))

;;        (input (@ (class "unit final")
;;                  (size 2)
;;                  (type "text")
;;                  ))))

;; Single event in side bar (text objects)
(define-public (fmt-day day)
  (let* (((date . events) day))
    `(section (@ (class "text-day"))
              (header (h2 ,(let ((s (date->string date "~Y-~m-~d")))
                             `(a (@ (href "#" ,s)
                                    (class "hidelink")) ,s))))
              ,@(stream->list
                 (stream-map
                  (lambda (ev)
                    (fmt-single-event
                      ev `((id ,(html-id ev))
                           (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown"))))
                      fmt-header:
                      (lambda body
                        `(a (@ (href "#" ,(html-id ev) #; (date-link (as-date (prop ev 'DTSTART)))
                                     )
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


;; Specific styles for each calendar.
;; TODO only emit the CSS here, requiring the caller to handle the context,
;; since that would allow us to use this in other contexts.
(define-public (calendar-styles calendars)
  `(style
       ,(lambda () (format #t "~:{ [data-calendar=\"~a\"] { --color: ~a; --complement: ~a }~%~}"
                      (map (lambda (c)
                             (let* ((name (base64encode (prop c 'NAME)))
                                    (bg-color (prop c 'COLOR))
                                    (fg-color (and=> (prop c 'COLOR)
                                                     calculate-fg-color)))
                               (list name (or bg-color 'white) (or fg-color 'black))))
                           calendars)))))

;; "Physical" block in calendar view
(define*-public (make-block ev optional: (extra-attributes '()))

  `((a (@ (href "#" ,(html-id ev))
          (class "hidelink"))
       (vevent-block (@ ,@(assq-merge
                           extra-attributes
                           `((id ,(html-id ev))
                             (data-calendar ,(base64encode (or (prop (parent ev) 'NAME) "unknown")))
                             (data-uid ,(output-uid ev))

                             (class "vevent event"
                               ,(when (and (prop ev 'PARTSTAT)
                                           (eq? 'TENTATIVE (prop ev 'PARTSTAT)))
                                  " tentative")
                               ,(when (and (prop ev 'TRANSP)
                                           (eq? 'TRANSPARENT (prop ev 'TRANSP)))
                                  " transparent")
                               ))))
                     ;; Inner div to prevent overflow. Previously "overflow: none"
                     ;; was set on the surounding div, but the popup /needs/ to
                     ;; overflow (for the tabs?).
                     ;; TODO the above comment is no longer valid. Popups are now stored
                     ;; separately from the block.
                     (div (@ (class "event-body"))
                          ,(when (prop ev 'RRULE)
                             `(span (@ (class "repeating")) "↺"))
                          (span (@ (class "bind summary")
                                   (data-property "summary"))
                                ,(format-summary  ev (prop ev 'SUMMARY)))
                          ,(when (prop ev 'LOCATION)
                             `(span (@ (class "bind location")
                                       (data-property "location"))
                                    ,(string-map (lambda (c) (if (char=? c #\,) #\newline c))
                                                 (prop ev 'LOCATION))))
                          ;; Document symbol when we have text
                          ,(when (and=> (prop ev 'DESCRIPTION) (negate string-null?))
                             `(span (@ (class "description"))
                                    "🗎")))))))


(define (repeat-info event)
  `(div (@ (class "eventtext"))
        (h2 "Upprepningar")
        (table (@ (class "recur-components"))
               ,@((@@ (vcomponent recurrence internal) map-fields)
                  (lambda (key value)
                    `(tr (@ (class ,key)) (th ,key)
                         (td
                          ,(case key
                             ((wkst) (week-day-name value))
                             ((until) (if (date? value)
                                          (date->string value)
                                          (datetime->string value)))
                             ((byday) (add-enumeration-punctuation
                                       (map (lambda (pair)
                                              (string-append
                                               (if (car pair)
                                                   (format #f "~a " (car pair))
                                                   "")
                                               (week-day-name (cdr pair))))
                                            value)))
                             (else (->string value))))))
                  (prop event 'RRULE)))))


;; Return a unique identifier for a specific instance of an event.
;; Allows us to reference each instance of a repeating event separately
;; from any other
(define-public (output-uid event)
  (string-concatenate
   (cons
    (prop event 'UID)
    (when (repeating? event)
      ;; TODO this will break if a UID already looks like this...
      ;; Just using a pre-generated unique string would solve it,
      ;; until someone wants to break us. Therefore, we just give
      ;; up for now, until a proper solution can be devised.
      (list "---"
            ;; TODO Will this give us a unique identifier?
            ;; Or can two events share UID along with start time
            (datetime->string
             (as-datetime (or
                           ;; TODO What happens if the parameter RANGE=THISANDFUTURE is set?
                           (prop event 'RECURRENCE-ID)
                           (prop event 'DTSTART)))
             "~Y-~m-~dT~H:~M:~S"))))))


;; TODO bind this into the xcal
(define (editable-repeat-info event)
  (warning "editable-repeat-info is deprecated")
  `(div (@ (class "eventtext"))
        (h2 "Upprepningar")
        ,@(when (debug)
            '((button (@ (style "position:absolute;right:1ex;top:1ex")
                         (onclick "console.log(event_from_popup(this.closest('.popup-container')).properties.rrule.asJcal());"))
                      "js")))
        (table (@ (class "recur-components bind")
                  (name "rrule")
                  (data-bindby "bind_recur"))
               ,@(map ; (@@ (vcomponent recurrence internal) map-fields)
                  (lambda (key )
                    `(tr (@ (class ,key)) (th ,key)
                         (td
                          ,(case key
                             ((freq)
                              `(select (@ (class "bind-rr") (name "freq"))
                                 (option "-")
                                 ,@(map (lambda (x) `(option (@ (value ,x)
                                                           ,@(awhen (prop event 'RRULE)
                                                                    (awhen (rrule:freq it)
                                                                           (awhen (eq? it x)
                                                                                  '((selected))))))
                                                        ,(string-titlecase
                                                          (symbol->string x))))
                                        '(SECONDLY MINUTELY HOURLY
                                                   DAILY WEEKLY
                                                   MONTHLY YEARLY))))
                             ((until)
                              (if (date? (prop event 'DTSTART))
                                  `(input (@ (type "date")
                                             (name "until")
                                             (class "bind-rr")
                                             (value ,(awhen (prop event 'RRULE)
                                                            (awhen (rrule:until it)
                                                                   (date->string it))))))
                                  `(span (@ (class "bind-rr date-time")
                                            (name "until"))
                                    (input (@ (type "date")
                                              (value ,(awhen (prop event 'RRULE)
                                                             (awhen (rrule:until it)
                                                                    (date->string
                                                                     (as-date it)))))))
                                    (input (@ (type "time")
                                              (value ,(awhen (prop event 'RRULE)
                                                             (awhen (rrule:until it)
                                                                    (time->string
                                                                     (as-time it))))))))))
                             ((count)
                              `(input (@ (type number) (min 0) (size 4)
                                         (value ,(awhen (prop event 'RRULE)
                                                        (or (rrule:count it) "")))
                                         (name "count")
                                         (class "bind-rr")
                                         )))
                             ((interval)
                              `(input (@ (type number) (min 0) (size 4)
                                         (value ,(awhen (prop event 'RRULE)
                                                        (or (rrule:interval it) "")))
                                         (name "interval")
                                         (class "bind-rr"))))
                             ((wkst)
                              `(select (@ (name "wkst") (class "bind-rr"))
                                 (option "-")
                                 ,@(map (lambda (i)
                                          `(option (@ (value ,i)
                                                      ,@(awhen (prop event 'RRULE)
                                                               (awhen (rrule:wkst it)
                                                                      (awhen (eqv? it i)
                                                                             '((selected))))))
                                                   ,(week-day-name i)))
                                        (iota 7))))

                             ((byday)
                              (let ((input (lambda* (optional: (byday '(#f . #f)) key: final?)
                                             `(div (@ (class "unit" ,(if final? " final" "")))
                                                   ;; TODO make this thiner, and clearer that
                                                   ;; it belongs to the following dropdown
                                                   (input (@ (type number)
                                                             (value ,(awhen (car byday) it))))
                                                   (select (option "-")
                                                     ,@(map (lambda (i)
                                                              `(option (@ (value ,i)
                                                                          ,@(if (eqv? i (cdr byday))
                                                                                '((selected)) '()))
                                                                       ,(week-day-name i)))
                                                            (iota 7)))))))
                                ;; TODO how does this bind?
                                `(div (@ (class "bind-rr input-list"))
                                      ,@(cond ((and=> (prop event 'RRULE)
                                                      rrule:byday)
                                               => (lambda (it) (map input it)))
                                              (else '()))

                                      ,(input final?: #t))))

                             ((bysecond byminute byhour
                                        bymonthday byyearday
                                        byweekno bymonth bysetpos)
                              (let ((input
                                     (lambda* (value optional: (final ""))
                                       `(input (@ (class "unit " ,final)
                                                  (type "number")
                                                  (size 2)
                                                  (value ,value)
                                                  (min ,(case key
                                                          ((bysecond byminute byhour)  0)
                                                          ((bymonthday)              -31)
                                                          ((byyearday)              -366)
                                                          ((byweekno)                -53)
                                                          ((bymonth)                 -12)
                                                          ((bysetpos)               -366)
                                                          ))
                                                  (max ,(case key
                                                          ((bysecond)    60)
                                                          ((byminute)    59)
                                                          ((byhour)      23)
                                                          ((bymonthday)  31)
                                                          ((byyearday)  366)
                                                          ((byweekno)    53)
                                                          ((bymonth)     12)
                                                          ((bysetpos)   366))))))))
                                `(div (@ (name ,key)
                                         (class "bind-rr input-list"))
                                      ,@(map input
                                             (awhen (prop event 'RRULE)
                                                    (or ((case key
                                                           ((bysecond)   rrule:bysecond)
                                                           ((byminute)   rrule:byminute)
                                                           ((byhour)     rrule:byhour)
                                                           ((bymonthday) rrule:bymonthday)
                                                           ((byyearday)  rrule:byyearday)
                                                           ((byweekno)   rrule:byweekno)
                                                           ((bymonth)    rrule:bymonth)
                                                           ((bysetpos)   rrule:bysetpos))
                                                         it)
                                                        '())))
                                      ,(input '() "final"))))
                             (else (error "Unknown field, " key))))

                         ;; TODO enable this button
                         (td (button (@ (class "clear-input") (title "Rensa input")) "🗙"))
                         ))
                  '(freq until count interval bysecond byminute byhour
                         byday bymonthday byyearday byweekno bymonth bysetpos
                         wkst)
                  ; (prop event 'RRULE)
                  ))))




(define-public (popup ev id)
  (warning "popup is deprecated")
  `(div (@ (id ,id)
           (class "popup-container")
           (data-calendar
            ,(base64encode (or (prop (parent ev) 'NAME)
                                "unknown")))
           (onclick "event.stopPropagation()"))
        ;; TODO all (?) code uses .popup-container as the popup, while .popup sits and does nothing.
        ;; Do something about this?
        (div (@ (class "popup"))
             (nav (@ (class "popup-control"))
                  ,(btn "×"
                        title: "Stäng"
                        onclick: "close_popup(document.getElementById(this.closest('.popup-container').id))"
                        class: '("close-tooltip"))
                  ,(when (edit-mode)
                     (list
                      (btn "🖊️"
                           title: "Redigera"
                           onclick: "place_in_edit_mode(event_from_popup(this.closest('.popup-container')))")
                      (btn "🗑"
                           title: "Ta bort"
                           onclick: "remove_event(event_from_popup(this.closest('.popup-container')))"))))

             ,(tabset
                `(("📅" title: "Översikt"
                   ,(fmt-single-event ev))

                  ,@(when (edit-mode)
                     `(("📅" title: "Redigera"
                        ,(fmt-for-edit ev))))


                  ("⤓" title: "Nedladdning"
                   (div (@ (class "eventtext") (style "font-family:sans"))
                        (h2 "Ladda ner")
                        (div (@ (class "side-by-side"))
                             (ul (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".ics"))
                                        "som iCal"))
                                 (li (a (@ (href "/calendar/" ,(prop ev 'UID) ".xcs"))
                                        "som xCal")))
                             ,@(when (debug)
                                 `((ul
                                    ;; this.closest('.vevent').dataset['uid']
                                    (li (button (@ (onclick ,(format #f "console.log(vcal_objects['~a'].to_jcal())"
                                                                     (prop ev 'UID)))) "js"))
                                    (li (button (@ (onclick ,(format #f "console.log(jcal_to_xcal(vcal_objects['~a'].to_jcal()))"
                                                                     (prop ev 'UID)))) "xml"))
                                    (li (button (@ (onclick ,(format #f "console.log(vcal_objects['~a'])"
                                                                     (prop ev 'UID)))) "this"))
                                    ))))
                        ))

                  ,@(when (prop ev 'RRULE)
                      `(("↺" title: "Upprepningar" class: "repeating"
                         ,(editable-repeat-info ev)))))))))
