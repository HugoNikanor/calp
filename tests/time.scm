(((srfi srfi-19 util)
  date day-stream normalize-date
  drop-time normalize-date/tz
  )
 ((util) set let-env)
 ((srfi srfi-19) date-day)
 )

(test-equal "Trivial normalize case"
  (date year: 2020 month: 1 day: 1 tz: 0)
  (normalize-date (date year: 2020 month: 1 day: 1 tz: 0)))

(test-equal "Trivial case, with timezone"
  (date year: 2020 month: 1 day: 1 tz: 3600)
  (normalize-date (date year: 2020 month: 1 day: 1 tz: 3600)))

;;; summer time begins 02:00 (becomes 03:00) during the night
;;; between the 28 and 29 of mars 2020, for Europe/Stockholm.
;;; (CET → CEST alt. UTC+1 → UTC+2)

(test-equal "Time zone spill over"
  (date year: 2020 month: 3 day: 29 tz: 3600)
  (normalize-date (set (date-day (date year: 2020 month: 3 day: 28 tz: 3600))
                       = (+ 1))))

;;; TODO normalize-date*



;;; !!! TODO !!!

(test-assert "normalize-date/tz"
  (not (unspecified? (normalize-date/tz (date)))))

(test-equal "Trivial normalize case"
  (date year: 2020 month: 1 day: 1 hour: 1 tz: 3600)
  (normalize-date/tz (date year: 2020 month: 1 day: 1 tz: 0)
                     "Europe/Stockholm"))

(test-equal "Trivial case, with timezone"
  (date year: 2020 month: 1 day: 1 tz: 3600)
  (normalize-date/tz (date year: 2020 month: 1 day: 1 tz: 3600)
                     "Europe/Stockholm"))

(test-equal "Time zone spill over"
  (date year: 2020 month: 3 day: 30 hour: 1 tz: 7200)
  (normalize-date/tz (set (date-day (date year: 2020 month: 3 day: 29 tz: 3600))
                          = (+ 1))
                     "Europe/Stockholm"))




(test-equal "drop time"
  (date)
  (drop-time (date hour: 10 minute: 70 second: 100)))


