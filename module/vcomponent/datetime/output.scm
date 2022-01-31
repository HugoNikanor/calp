(define-module (vcomponent datetime output)
  :use-module (hnh util)
  :use-module (calp util config)
  :use-module (hnh util exceptions)
  :use-module (datetime)
  :use-module (vcomponent base)
  :use-module (text util)
  )

(define-config summary-filter (lambda (_ a) a)
  pre: (ensure procedure?))

(define-config description-filter (lambda (_ a) a)
  pre: (ensure procedure?))

;; ev → sxml
(define-public (format-recurrence-rule ev)
  `("Upprepas "
    ,((@ (vcomponent recurrence display) format-recurrence-rule)
      (prop ev 'RRULE))
    ,@(awhen (prop* ev 'EXDATE)
             (list
              ", undantaget "
              (add-enumeration-punctuation
               (map (lambda (d)
                      (if (date? d)
                          ;; NOTE possibly show year?
                          (date->string d "~e ~b")
                          ;; NOTE only show time when it's different than the start time?
                          ;; or possibly only when FREQ is hourly or lower.
                          (if (memv ((@ (vcomponent recurrence internal) freq)
                                     (prop ev 'RRULE))
                                    '(HOURLY MINUTELY SECONDLY))
                              (datetime->string d "~e ~b ~k:~M")
                              (datetime->string d "~e ~b"))))
                    (map value it)))))
    "."))

(define-public (format-summary ev str)
  ((get-config 'summary-filter) ev str))

;; NOTE this should have information about context (html/term/...)
(define-public (format-description ev str)
  (catch #t (lambda () ((get-config 'description-filter) ev str))
    (lambda (err . args)
      (warning "~a on formatting description, ~s" err args)
      str)))

;; Takes an event, and returns a pretty string for the time interval
;; the event occupies.
(define-public (fmt-time-span ev)
  (cond [(prop ev 'DTSTART) date?
         => (lambda (s)
              (cond [(prop ev 'DTEND)
                     => (lambda (e)
                          (if (date= e (date+ s (date day: 1)))
                              "~Y-~m-~d"  ; start = end, only return one value
                              (values "~Y-~m-~d"
                                      "~Y-~m-~d")))]
                    ;; no end value, just return start
                    [else (date->string s)]))]
        [else ; guaranteed datetime
         (let ((s (prop ev 'DTSTART))
               (e (prop ev 'DTEND)))
           (if e
               (let ((fmt-str (if (date= (get-date s) (get-date e))
                                  "~H:~M" "~Y-~m-~d ~H:~M")))
                 (values fmt-str fmt-str))
               "~Y-~m-~d ~H:~M"))]))
