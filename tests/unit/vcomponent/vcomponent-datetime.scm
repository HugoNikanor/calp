;;; Commentary:
;; Tests that event-clamping (checking how long part of an event
;; overlaps another time span) works.
;;; Code:

(define-module (test vcomponent-datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module ((hnh util) :select (->> sort*))
  :use-module (hnh util lens)
  :use-module ((datetime zoneinfo) :select (intermediary->zoneinfo read-zoneinfo))
  :use-module ((vcomponent) :select (vcomponent-diff extract1 prop1))
  :use-module (vcomponent datetime)
  :use-module (vcomponent datetime timezone)
  :use-module ((vcomponent type recurrence) :select (recur-rule))
  :use-module ((vcomponent type utc-offset) :select (utc-offset))
  :use-module ((vcomponent create) :select (vevent vtimezone daylight standard)))

;;; TODO RFC 5545 Specifies that an event lies in the range [start, end). Write explicit tests for this.


(test-group "instances-overlap?"
  (test-assert "date, datetime"
    (instances-overlap?
     "UTC"
     (vevent summary: "A"
             dtstart: (date year: 2020 month: jan day: 1)
             dtend:   (date year: 2022 month: dec day: 31))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))))

  (test-assert "date, date"
    (instances-overlap?
     "UTC"
     (vevent summary: "A"
             dtstart: (date year: 2020 month: jan day: 1)
             dtend:   (date year: 2020 month: jan day: 20))
     (vevent summary: "B"
             dtstart: (date year: 2020 month: jan day: 10)
             dtend:   (date year: 2020 month: feb day: 10))))

  (test-assert "datetime, date"
    (not
     (instances-overlap?
      "UTC"
      (vevent summary: "A"
              dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
              dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))
      (vevent summary: "B"
              dtstart: (date year: 2020 month: jan day: 10)
              dtend:   (date year: 2020 month: feb day: 10)))))

  (test-assert "datetime, datetime"
    (instances-overlap?
     "UTC"
     (vevent summary: "A"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 11)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 13))))

  (test-assert "Without dtend"
    (instances-overlap?
     "UTC"
     (vevent summary: "A"
             dtstart: (date year: 2020 month: apr day: 1))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)))))


(test-group "instance-length"
  (test-equal "Datetime, with DTEND"
    (duration day: 2 hour: 17)
    (instance-length
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17)
      dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10))))

  (test-equal "Datetime, without DTEND"
    (duration)
    (instance-length
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17))))

  (test-equal "Date, with DTEND"
    (duration day: 3)
    (instance-length
     (vevent
      dtstart: (date year: 2020 month: 3 day: 29)
      dtend:   (date year: 2020 month: 4 day:  1))))

  (test-equal "Date, without DTEND"
    (duration day: 1)
    (instance-length
     (vevent
      dtstart: (date year: 2020 month: 3 day: 29))))

  ;; TODO Events with durations instead of DTENDs
  )

(test-group "instance-length/clamped"
 (let ((ev
        (vevent
         dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17 tz: "UTC")
         dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10 tz: "UTC"))))

   ;; |-----------------| test interval
   ;;                 |----------| event interval

   (test-equal "Correct clamping"
     (duration hour: 7) ; 2020-03-29T17:00 - 2020-03-30T00:00
     (instance-length/clamped
      (datetime year: 2020 month: 3 day: 23 tz: "UTC") ; a time way before the start of the event
      (datetime year: 2020 month: 3 day: (1+ 29) tz: "UTC") ; a time slightly after the end of the event
      "UTC"
      ev))

   (test-equal "Correct clamping UTC"
     (duration hour: 7)
     (instance-length/clamped
      (datetime year: 2020 month: 3 day: 23 tz: "UTC")
      (datetime year: 2020 month: 3 day: (1+ 29) tz: "UTC")
      "UTC"
      ev)))

 (let ((ev (vevent dtstart: (datetime year: 2020 month: 3 day: 1))))
   (test-equal
       (duration)
     (instance-length/clamped
      (datetime year: 2020 month: 3 day: 1 tz: "UTC")
      (datetime year: 2020 month: 3 day: 2 tz: "UTC")
      "UTC"
      ev
      ))
   )

 ;; TODO test with no dtend (datetime)
 ;; TODO test with no dtend (date)

 ;; TODO test where both dtstart and dtend are date's

 )


(test-group "zoneinfo->vtimezone"
  (let* ((zoneinfo-sample
          "
# Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
Rule    Swiss 1941  1942  -  May  Mon>=1   1:00  1:00  S
Rule    Swiss 1941  1942  -  Oct  Mon>=1   2:00  0     -
Rule    EU    1977  1980  -  Apr  Sun>=1   1:00u 1:00  S
Rule    EU    1977  only  -  Sep  lastSun  1:00u 0     -
Rule    EU    1978  only  -  Oct   1       1:00u 0     -
Rule    EU    1979  1995  -  Sep  lastSun  1:00u 0     -
Rule    EU    1981  max   -  Mar  lastSun  1:00u 1:00  S
Rule    EU    1996  max   -  Oct  lastSun  1:00u 0     -

# Zone  NAME           STDOFF      RULES  FORMAT  [UNTIL]
Zone    Europe/Zurich  0:34:08     -      LMT     1853 Jul 16
                       0:29:45.50  -      BMT     1894 Jun
                       1:00        Swiss  CE%sT   1981
                       1:00        EU     CE%sT

Link    Europe/Zurich  Europe/Vaduz
")

         (zoneinfo
          (call-with-input-string
              zoneinfo-sample
            (compose intermediary->zoneinfo read-zoneinfo)))

         (timezone-component
          ;; Seed random to stable UID's.
          (parameterize (((@ (hnh util uuid) seed) (seed->random-state 0)))
            (zoneinfo->vtimezone
             zoneinfo "Europe/Zurich"
             (datetime year: 2020 month: jan day: 10 hour: 10)))))

    (test-equal
        '()
      (vcomponent-diff
       (vtimezone tzid: "Europe/Zurich"
                  (list
                   (daylight
                    dtstart: (datetime year: 1981 month: 3 day: 29 hour: 1 tz: "UTC")
                    rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun)) bymonth: '(3) wkst: monday)
                    tzname: "CEST"
                    ;; TODO why isn't this 'hour: 1'?
                    tzoffsetfrom: (utc-offset value: 0)
                    tzoffsetto: (utc-offset value: 7200))
                   (standard
                    dtstart: (datetime year: 1996 month: 10 day: 27 hour: 1 tz: "UTC")
                    rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun)) bymonth: '(10) wkst: monday)
                    tzname: "CET"
                    tzoffsetfrom: (utc-offset value: 7200)
                    tzoffsetto: (utc-offset value: 3600))))
       timezone-component))

    ;; TODO test where the requested timezone isn't available
    )

  ;; TODO Test where we have "FROM: only" rules
  )



'((vcomponent datetime)
  (vcomponent datetime timezone))
