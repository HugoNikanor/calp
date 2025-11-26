(define-module (test vcomponent-formats-common-types)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-64 test-error)
  :use-module (srfi srfi-88)
  :use-module (hnh util table)
  :use-module (vcomponent media-type types)
  :use-module (web uri)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module ((datetime) :select (date time datetime))
  :use-module ((datetime timespec) :select (timespec)))


;;; TODO test with custom types appended to default-types

(test-equal "Type of known field"
  'TEXT (default-type 'SUMMARY))

(test-equal "Type of unknown field"
  #f (default-type 'X-UNKNOWN-FIELD))

;;; TODO test with custom types appended to apparent-types

(test-group "Apparent types"
  (test-equal 'BINARY      (apparent-type #vu8()))
  (test-equal 'BOOLEAN     (apparent-type #f))
  (test-equal 'CAL-ADDRESS (apparent-type (string->uri "mailto:hugo@example.com")))
  (test-equal 'DATE        (apparent-type (date)))
  (test-equal 'DATE-TIME   (apparent-type (datetime)))
  (test-equal 'DURATION    (apparent-type (duration)))
  (test-equal 'FLOAT       (apparent-type 1.0))
  (test-equal 'INTEGER     (apparent-type 1))
  (test-equal 'PERIOD      (apparent-type (period start: (datetime) end: (datetime))))
  (test-equal 'RECUR       (apparent-type (recur-rule freq: 'WEEKLY)))
  (test-equal 'TEXT        (apparent-type "Hello"))
  (test-equal 'TIME        (apparent-type (time)))
  (test-equal 'URI         (apparent-type (string->uri "https://example.com")))
  (test-equal 'UTC-OFFSET  (apparent-type (timespec (time) '+ 'utc)))

  (test-equal "Unknsown types return false"
    #f (apparent-type (sqrt -1)))
  ;; TODO maybe some more oddball types?
  )

'((vcomponent media-type types))
