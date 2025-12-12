(define-module (test vcomponent-text-calendar-types)
  :use-module (srfi srfi-64)
  :use-module (vcomponent media-type text calendar parse-semantics)
  :use-module (hnh util table)
  )

;;; TODO test date/-times with values outside of the valid intervals

(test-group "text/calendar invalid values"
  (test-group "BINARY"
    (test-error "Unknown encoding"
      'calendar-parse-error
      ((get-parser 'BINARY)
       (alist->table '((ENCODING . "BASE16")))
       "ABCD"))
    (test-error "Invalid base64"
      'calendar-parse-error
      ((get-parser 'BINARY)
       (table)
       "@@@@")))

  (test-group "BOOLEAN"
    (test-error "Invalid value"
      'calendar-parse-error
      ((get-parser 'BOOLEAN)
       (table)
       "yes")))

  (test-group "CAL-ADDRESS"
    (test-error "Invalid URI"
      'calendar-parse-error
      ((get-parser 'CAL-ADDRESS)
       (table)
       "Invalid uri")))

  (test-group "DATE"
    ;; malformed date
    (test-error "Malformed date"
      'calendar-parse-error
      ((get-parser 'DATE)
       (table)
       "2020-10-20 extra text")))

  (test-group "DATE-TIME"
    (test-error "Malformed datetime"
      'calendar-parse-error
      ((get-parser 'DATE-TIME)
       (table)
       "19980119T230000-0800")))

  (test-group "DURATION"
    (test-error "Malformed duration"
      'calendar-parse-error
      ((get-parser 'DURATION)
       (table)
       "7W"))
    (test-error "Garbage at end of duration"
      'calendar-parse-error
      ((get-parser 'DURATION)
       (table)
       "P7Wextra")))

  (test-group "FLOAT"
    (test-error "Invalid float string"
      'calendar-parse-error
      ((get-parser 'FLOAT)
       (table)
       "0.7f"))

    ;; (test-error "Exact fractionals shouldn't be accepted"
    ;;   'calendar-parse-error
    ;;   ((get-parser 'FLOAT)
    ;;    ...
    ;;    ))

    (test-error "Non-real number"
      'calendar-parse-error
      ((get-parser 'FLOAT)
       (table)
       "+i")))

  (test-group "INTEGER"
    (test-error "Non-numeric value"
      'calendar-parse-error
      ((get-parser 'INTEGER)
       (table)
       "not a number"))

    (test-error "Non-exact integer"
      'calendar-parse-error
      ((get-parser 'INTEGER)
       (table)
       "1.0")))

  (test-group "PERIOD"
    (test-error "Complete garbage"
      'calendar-parse-error
      ((get-parser 'PERIOD)
       (table)
       "uhteoansuhaeotn")))

  (test-group "RECUR"
    (test-error "Slightly invalid recur"
      'calendar-parse-error
      ((get-parser 'RECUR)
       (table)
       "FREQ=daily")))

  ;; (test-group "TEXT"
  ;;   ;; TODO
  ;;   )

  (test-group "TIME"
    (test-error "Trailing stuff at end of date"
      'calendar-parse-error
      ((get-parser 'TIME)
       (table)
       "102030trail")))

  (test-group "URI"
    (test-error "Invalid URI"
      'calendar-parse-error
      ((get-parser 'CAL-ADDRESS)
       (table)
       "Invalid uri")))

  (test-group "UTC-OFFSET"
    (test-error "Missing prefix"
      'calendar-parse-error
      ((get-parser 'UTC-OFFSET)
       (table)
       "1000")))

  ;; GEO
  ;; VERSION
  ;; REQUEST-STATUS
  )

;; (test-group "call/cc fallbacks")

'((vcomponent media-type text calendar parse-semantics))
