;;; DEPRECATED
;;; This modules contains usefull things, but they are all in the wrong place!!!
(define-module (vcomponent datetime output)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (vcomponent)
  :use-module (text util)
  :use-module (calp translation)
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :export (format-recurrence-rule
           format-summary
           format-description
           fmt-time-span
           ))

;; ev → sxml
;; TODO move this to some form of general text output module
;; TODO translation
(define (format-recurrence-rule ev)
  ;; [FRR]
  ;; Part of the sentance "Repeated [every two weeks], except on ~a, ~a & ~a"
  ;; See everything tagged [FRR]
  `(,(G_ "Repeated ")
    ,((@ (vcomponent type recurrence display) format-recurrence-rule) (prop1 ev 'RRULE))
    ,@(awhen (unjust (get ev (prop* 'EXDATE)) #f)
             (list
              ;; See [FRR]
              (G_ ", except on ")
              (add-enumeration-punctuation
               (map (lambda (d)
                      ;; TODO show year if different from current year
                      (if (date? d)
                          ;; [FRR] Exception date without time
                          (date->string d (G_ "~e ~b"))
                          ;; NOTE only show time when it's different than the start time?
                          ;; or possibly only when FREQ is hourly or lower.
                          (if (memv ((@ (vcomponent type recurrence) freq)
                                  (prop1 ev 'RRULE))
                                 '(HOURLY MINUTELY SECONDLY))
                              ;; [FRR] Exception date with time
                              (datetime->string d (G_ "~e ~b ~k:~M"))
                              ;; [FRR] Exception date without time
                              (datetime->string d (G_ "~e ~b")))))
                    (map vline-value it)))))
    "."))

;;; TODO this is in the completely wrong place
(define (format-summary ev str)
  ((@ (calp html filter) summary-filter) ev str))

;; NOTE this should have information about context (html/term/...)
;;; TODO this is in the completely wrong place
(define (format-description ev str)
  (catch #t (lambda () ((@ (calp html filter) description-filter)
                   ev str))
    (lambda (err . args)
      ;; Warning message for failure to format description.
      ;; First argument is name of warning/error,
      ;; second is error arguments
      (warning (G_ "~a on formatting description, ~s") err args)
      str)))

;; Takes an event, and returns a pretty string for the time interval
;; the event occupies.
(define (fmt-time-span ev)
  (cond [(prop1 ev 'DTSTART) date?
         => (lambda (s)
              (cond [(prop1 ev 'DTEND)
                     => (lambda (e)
                          ;; start = end, only return one value
                          (if (date= e (date+ s (date day: 1)))
                              (G_ "~Y-~m-~d")
                              (values (G_ "~Y-~m-~d")
                                      (G_ "~Y-~m-~d"))))]
                    ;; no end value, just return start
                    [else (date->string s)]))]
        [else ; guaranteed datetime
         (let ((s (prop1 ev 'DTSTART))
               (e (prop1 ev 'DTEND)))
           (if e
               (let ((fmt-str (if (date= (datetime-date s) (datetime-date e))
                                  (G_ "~H:~M")
                                  ;; Note the non-breaking space
                                  (G_ "~Y-~m-~d ~H:~M"))))

                 (values fmt-str fmt-str))
               ;; Note the non-breaking space
               (G_ "~Y-~m-~d ~H:~M")))]))
