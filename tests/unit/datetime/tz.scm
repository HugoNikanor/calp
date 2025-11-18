;;; Commentary:
;; Tests that datetime->unix-time correctly converts between Olssen
;; timezone definitions (e.g. Europe/Stockholm), into correct times
;; and offsets (in unix time).
;; Also indirectly tests the Zone Info Compiler (datetime zic), since
;; the zoneinfo comes from there.
;;; Code:

(define-module (test tz)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((datetime)
               :select (
                        datetime
                        date
                        time
                        datetime->unix-time
                        unix-time->datetime
                        ))
  :use-module ((hnh util env) :select (let-env)))

(test-equal
  (datetime
    date:
    (date year: 1970 month: 01 day: 01)
    time:
    (time hour: 00 minute: 00 second: 00)
    tz:
    "UTC")
  (unix-time->datetime 0))


'((datetime))
