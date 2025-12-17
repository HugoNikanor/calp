(define-module (vcomponent datetime output)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (vcomponent)
  :use-module (text util)
  :use-module (calp translation)
  :use-module (hnh util type)
  :export (format-recurrence-rule))

;; ev → (list-of string)
;; TODO move this to some form of general text output module
;; TODO check how this shows up in the gettext output.
;;      will a translater be able to understand it without the source code?
(define (format-recurrence-rule ev)
  (typecheck ev vevent?)
  ;; [FRR]
  ;; Part of the sentance "Repeated [every two weeks], except on ~a, ~a & ~a"
  ;; See everything tagged [FRR]
  `(,(G_ "Repeated ")
    ,((@ (vcomponent type recurrence display) format-recurrence-rule)
      (prop1 ev 'RRULE))
    ,@(awhen (prop% ev 'EXDATE)
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

