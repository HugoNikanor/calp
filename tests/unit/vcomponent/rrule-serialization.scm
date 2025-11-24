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
               :select (recur-rule->rrule-sxml))
  :use-module ((hnh util table) :select (table))
  :use-module ((vcomponent type recurrence parse) :select (parse-day-spec))
  :use-module ((ice-9 peg) :select (keyword-flatten)))

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


(define field->string
  (@@ (vcomponent media-type text calendar output)
      field->string))

(let ((rule (parse-recurrence-rule (table) "FREQ=WEEKLY;BYDAY=MO,TU,WE")))
  (test-equal
    "Direct return of parsed value"
    "MO,TU,WE"
    (field->string 'byday (byday rule)))
  (test-equal
    "Direct return, but as SXML"
    '((byday "MO") (byday "TU") (byday "WE"))
    (filter
      (lambda (pair) (eq? 'byday (car pair)))
      (keyword-flatten
        '(interval byday wkst freq)
        (recur-rule->rrule-sxml rule)))))

(let ((rule (parse-recurrence-rule (table) "FREQ=WEEKLY;BYDAY=+1MO,1TU,-2FR")))
  (test-equal
    "Direct return of parsed value"
    "1MO,1TU,-2FR"
    (field->string 'byday (byday rule)))
  (test-equal
    "Direct return, but as SXML"
    '((byday "1MO") (byday "1TU") (byday "-2FR"))
    (filter
      (lambda (pair) (eq? 'byday (car pair)))
      ;; TODO why is keyword-flatten used here?
      (keyword-flatten
        '(interval byday wkst freq)
        (recur-rule->rrule-sxml rule)))))


'((vcomponent type recurrence internal)
  (vcomponent type recurrence parse))
