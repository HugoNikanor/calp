(define-module (vcomponent datetime output)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (vcomponent base)
  :use-module (text util)
  :use-module (calp translation)
  :use-module ((hnh util exceptions) :select (warning))
  )

;; ev → sxml
;; TODO translation
(define-public (format-recurrence-rule ev)
  ;; [FRR]
  ;; Part of the sentance "Repeated [every two weeks], except on ~a, ~a & ~a"
  ;; See everything tagged [FRR]
  `(,(_ "Repeated ")
    ,((@ (vcomponent recurrence display) format-recurrence-rule) (prop ev 'RRULE))
    ,@(awhen (prop* ev 'EXDATE)
             (list
              ;; See [FRR]
              (_ ", except on ")
              (add-enumeration-punctuation
               (map (lambda (d)
                      ;; TODO show year if different from current year
                      (if (date? d)
                          ;; [FRR] Exception date without time
                          (date->string d (_ "~e ~b"))
                          ;; NOTE only show time when it's different than the start time?
                          ;; or possibly only when FREQ is hourly or lower.
                          (if (memv ((@ (vcomponent recurrence internal) freq)
                                  (prop ev 'RRULE))
                                 '(HOURLY MINUTELY SECONDLY))
                              ;; [FRR] Exception date with time
                              (datetime->string d (_ "~e ~b ~k:~M"))
                              ;; [FRR] Exception date without time
                              (datetime->string d (_ "~e ~b")))))
                    (map value it)))))
    "."))

(define-public (format-summary ev str)
  ((@ (calp html filter) summary-filter) ev str))

;; NOTE this should have information about context (html/term/...)
(define-public (format-description ev str)
  (catch #t (lambda () ((@ (calp html filter) description-filter)
                   ev str))
    (lambda (err . args)
      ;; Warning message for failure to format description.
      ;; First argument is name of warning/error,
      ;; second is error arguments
      (warning (_ "~a on formatting description, ~s") err args)
      str)))

;; Takes an event, and returns a pretty string for the time interval
;; the event occupies.
(define-public (fmt-time-span ev)
  (cond [(prop ev 'DTSTART) date?
         => (lambda (s)
              (cond [(prop ev 'DTEND)
                     => (lambda (e)
                          (if (date= e (date+ s (date day: 1)))
                              (_ "~Y-~m-~d")  ; start = end, only return one value
                              (values (_ "~Y-~m-~d")
                                      (_ "~Y-~m-~d"))))]
                    ;; no end value, just return start
                    [else (date->string s)]))]
        [else ; guaranteed datetime
         (let ((s (prop ev 'DTSTART))
               (e (prop ev 'DTEND)))
           (if e
               (let ((fmt-str (if (date= (get-date s) (get-date e))
                                  (_ "~H:~M")
                                  ;; Note the non-breaking space
                                  (_ "~Y-~m-~d ~H:~M"))))
                 (values fmt-str fmt-str))
               ;; Note the non-breaking space
               (_ "~Y-~m-~d ~H:~M")))]))
