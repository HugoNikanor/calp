(define-module (test rrule-serialization)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent recurrence internal)
               :select (recur-rule->rrule-string
                        recur-rule->rrule-sxml
                        byday))
  :use-module ((vcomponent recurrence parse)
               :select (parse-recurrence-rule))
  :use-module ((ice-9 peg) :select (keyword-flatten)))

(test-equal
  "Parse of week day"
  '(#f . 3)
  ((@@ (vcomponent recurrence parse) parse-day-spec)
   "WE"))

(test-equal
  "Parse of week day with positive offset"
  '(1 . 3)
  ((@@ (vcomponent recurrence parse) parse-day-spec)
   "1WE"))

(test-equal
  "Parse of week day with positive offset (and plus)"
  '(2 . 3)
  ((@@ (vcomponent recurrence parse) parse-day-spec)
   "+2WE"))

(test-equal
  "Parse of week day with negative offset"
  '(-3 . 3)
  ((@@ (vcomponent recurrence parse) parse-day-spec)
   "-3WE"))


;; numeric prefixes in the BYDAY list is only valid when
;; FREQ={MONTHLY,YEARLY}, but that should be handled in a
;; later stage since we are just testing the parser here.
;; (p. 41)


(define field->string
  (@@ (vcomponent recurrence internal)
      field->string))

(let ((rule (parse-recurrence-rule "BYDAY=MO,TU,WE")))
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
        '(interval byday wkst)
        (recur-rule->rrule-sxml rule)))))

(let ((rule (parse-recurrence-rule "BYDAY=+1MO,1TU,-2FR")))
  (test-equal
    "Direct return of parsed value"
    "1MO,1TU,-2FR"
    (field->string 'byday (byday rule)))
  (test-equal
    "Direct return, but as SXML"
    '((byday "1MO") (byday "1TU") (byday "-2FR"))
    (filter
      (lambda (pair) (eq? 'byday (car pair)))
      (keyword-flatten
        '(interval byday wkst)
        (recur-rule->rrule-sxml rule)))))


