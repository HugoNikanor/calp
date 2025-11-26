(define-module (test rrule-serialization)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent type recurrence internal)
               :select (byday))
  :use-module ((vcomponent media-type text calendar parse-semantics)
               :select (parse-recurrence-rule))
  :use-module ((vcomponent media-type text calendar output)
               :select (recur-rule->rrule-string))
  :use-module ((vcomponent media-type application calendar+xml output)
               :select (recur-rule->sxml))
  :use-module ((hnh util table) :select (table))
  :use-module ((vcomponent type recurrence parse) :select (parse-day-spec))
  :use-module ((calp namespaces) :select (xcal))
  :use-module (sxml namespaced)
  :use-module ((datetime) :select (mon tue wed fri)))

(test-equal
  "Parse of week day"
  '(#f . 3)
  (parse-day-spec "WE"))

(test-equal
  "Parse of week day with positive offset"
  '(1 . 3)
  (parse-day-spec "1WE"))

(test-equal
  "Parse of week day with positive offset (and plus)"
  '(2 . 3)
  (parse-day-spec "+2WE"))

(test-equal
  "Parse of week day with negative offset"
  '(-3 . 3)
  (parse-day-spec "-3WE"))


;; numeric prefixes in the BYDAY list is only valid when
;; FREQ={MONTHLY,YEARLY}, but that should be handled in a
;; later stage since we are just testing the parser here.
;; (p. 41)


(let ((rule (parse-recurrence-rule
             (table) "FREQ=WEEKLY;BYDAY=MO,TU,WE")))
  (test-equal "Direct return of parsed value"
    (list (cons #f mon)
          (cons #f tue)
          (cons #f wed))
    (byday rule))
  (test-equal "Direct return, but as SXML"
    ((xml xcal 'recur)
     ((xml xcal 'freq) "WEEKLY")
     ((xml xcal 'byday) "MO")
     ((xml xcal 'byday) "TU")
     ((xml xcal 'byday) "WE"))
    (recur-rule->sxml rule)))

(let ((rule (parse-recurrence-rule
             (table) "FREQ=WEEKLY;BYDAY=+1MO,1TU,-2FR")))
  (test-equal "Direct return of parsed value"
    (list (cons 1 mon)
          (cons 1 tue)
          (cons -2 fri))
    (byday rule))
  (test-equal "Direct return, but as SXML"
    ((xml xcal 'recur)
     ((xml xcal 'freq) "WEEKLY")
     ((xml xcal 'byday) "1MO")
     ((xml xcal 'byday) "1TU")
     ((xml xcal 'byday) "-2FR"))
    (recur-rule->sxml rule)))


'((vcomponent type recurrence internal)
  (vcomponent type recurrence parse))
