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
  :use-module ((datetime zic) :select (read-zoneinfo))
  :use-module (datetime timespec)
  :use-module ((vcomponent) :select (vcomponent-equal? extract1 prop1))
  :use-module (vcomponent datetime)
  :use-module ((vcomponent type recurrence) :select (recur-rule))
  :use-module ((vcomponent create) :select (vevent vtimezone daylight standard)))


(test-group "overlapping?"
  (test-assert "date, datetime"
    (overlapping?
     (vevent summary: "A"
             dtstart: (date year: 2020 month: jan day: 1)
             dtend:   (date year: 2022 month: dec day: 31))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))))

  (test-assert "date, date"
    (overlapping?
     (vevent summary: "A"
             dtstart: (date year: 2020 month: jan day: 1)
             dtend:   (date year: 2020 month: jan day: 20))
     (vevent summary: "B"
             dtstart: (date year: 2020 month: jan day: 10)
             dtend:   (date year: 2020 month: feb day: 10))))

  (test-assert "datetime, date"
    (not
     (overlapping?
      (vevent summary: "A"
              dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
              dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))
      (vevent summary: "B"
              dtstart: (date year: 2020 month: jan day: 10)
              dtend:   (date year: 2020 month: feb day: 10)))))

  (test-assert "datetime, datetime"
    (overlapping?
     (vevent summary: "A"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 12))
     (vevent summary: "B"
             dtstart: (datetime year: 2020 month: apr day: 1 hour: 11)
             dtend:   (datetime year: 2020 month: apr day: 1 hour: 13))))

  (test-assert "Without dtend"
   (overlapping?
    (vevent summary: "A"
            dtstart: (date year: 2020 month: apr day: 1))
    (vevent summary: "B"
            dtstart: (datetime year: 2020 month: apr day: 1 hour: 10)))))

(test-group "event-contains?"
  (let* ((dt (datetime year: 2020 month: jan day: 1
                       hour: 10))
         (ev (vevent dtstart: dt
                     dtend: (datetime+ dt (datetime hour: 5)))))
    (test-assert (event-contains? ev dt))
    (test-assert (not (event-contains? ev (set dt (lens-compose date* day*) 10))))))

(test-group "event-zero-length?"
  (test-assert (not (event-zero-length? (vevent dtstart: (date)))))
  (test-assert (event-zero-length? (vevent dtstart: (datetime))))
  (test-assert (not (event-zero-length? (vevent dtstart: (datetime)
                                               dtend: (datetime))))))

(test-assert "ev-time<?"
  (ev-time<?
   (vevent summary: "A"
           dtstart: (datetime year: 2020 month: apr day: 1 hour: 10))
   (vevent summary: "B"
           dtstart: (datetime year: 2020 month: apr day: 1 hour: 11))))

(test-group "event-length"
  (test-equal "Datetime, with DTEND"
    (datetime day: 2 hour: 17)
    (event-length
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17)
      dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10))))

  (test-equal "Datetime, without DTEND"
    (datetime)
    (event-length
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17))))

  (test-equal "Date, with DTEND"
    (date day: 3)
    (event-length
     (vevent
      dtstart: (date year: 2020 month: 3 day: 29)
      dtend:   (date year: 2020 month: 4 day:  1))))

  (test-equal "Date, without DTEND"
    (date day: 1)
    (event-length
     (vevent
      dtstart: (date year: 2020 month: 3 day: 29)))))

(test-group "event-length/clamped"
 (let ((ev
        (vevent
         dtstart: (datetime year: 2020 month: 3 day: 29 hour: 17)
         dtend:   (datetime year: 2020 month: 4 day:  1 hour: 10))))

   ;; |-----------------| test interval
   ;;                 |----------| event interval

   (test-equal "Correct clamping"
     (datetime hour: 7) ; 2020-03-29T17:00 - 2020-03-30T00:00
     (event-length/clamped
      (date year: 2020 month: 3 day: 23) ; a time way before the start of the event
      (date year: 2020 month: 3 day: 29) ; a time slightly after the end of the event
      ev))

   ;; TODO why is this object created?
   (define utc-ev
     (vevent
      dtstart: (datetime year: 2020 month: 3 day: 29 hour: 15 tz: "UTC")
      dtend:   (datetime year: 2020 month: 4 day:  1 hour:  8 tz: "UTC")))

   (test-equal "Correct clamping UTC"
     (datetime hour: 7)
     (event-length/clamped
      (date year: 2020 month: 3 day: 23)
      (date year: 2020 month: 3 day: 29)
      ev)))

 (let ((ev (vevent dtstart: (datetime year: 2020 month: 3 day: 1))))
   (test-equal
       (datetime)
     (event-length/clamped
      (date year: 2020 month: 3 day: 1)
      (date year: 2020 month: 3 day: 2)
      ev
      ))
   )

 ;; TODO test with no dtend (datetime)
 ;; TODO test with no dtend (date)

 ;; TODO test where both dtstart and dtend are date's

 )

(let ((d (date year: 2020 month: jan day: 10)))
  (test-group "event-length/day"

    ;; TODO shouldn't a check for the correct date be done?
    (test-equal
        (time hour: 24)
      (event-length/day
       d
       (vevent dtstart: (date))))

    (test-equal
        (time)
      (event-length/day
       d
       (vevent dtstart: (datetime))))

    (test-equal "Within day"
      (time hour: 10)
      (event-length/day
       d
       (vevent dtstart: (datetime date: d hour: 10)
               dtend: (datetime date: d hour: 20))))

    (test-equal "Ends tommorrow"
      (time hour: 14)
      (event-length/day
       d
       (vevent dtstart: (datetime date: d hour: 10)
               dtend: (datetime date: (date+ d (date day: 1)) hour: 20))))

    (test-equal "Started yesterday"
      (time hour: 10)
      (event-length/day
       d
       (vevent dtstart: (datetime date: (date- d (date day: 1)) hour: 10)
               dtend: (datetime date: d hour: 10))))

    (test-equal "Starts before date, ends after date"
      (time hour: 24)
      (event-length/day
       d
       (vevent dtstart: (datetime date: (date- d (date day: 1)) hour: 10)
               dtend:   (datetime date: (date+ d (date day: 1)) hour: 10))))

    ;; TODO Test invalid cases
    ))

(test-group "long-event?"
  (test-assert "DTSTART being date is always a long event"
    (long-event? (vevent dtstart: (date))))
  (test-assert "datetime DTSTART without DTEND is always short"
    (not (long-event? (vevent dtstart: (datetime)))))
  (test-assert "Event longer than 24h"
    (not
     (long-event? (vevent dtstart: (datetime year: 2020 month: 1 day: 1 hour: 10)
                          dtend:   (datetime year: 2020 month: 1 day: 1 hour: 20)))))
  (test-assert "Event shorter than 24h"
    (long-event? (vevent dtstart: (datetime year: 2020 month: 1 day: 1
                                            hour: 1)
                         dtend:   (datetime year: 2020 month: 1 day: 2
                                            hour: 1 minute: 1)))))

(test-group "really-long-event?"
  (test-assert (not (really-long-event?
                     (vevent dtstart: (date year: 2020 month: jan day: 1)
                             dtend:   (date year: 2020 month: jan day: 2)))))
  (test-assert (really-long-event?
                (vevent dtstart: (date year: 2020 month: jan day: 1)
                        dtend:   (date year: 2020 month: jan day: 3))))
  (test-assert (not (really-long-event?
                     (vevent dtstart: (datetime year: 2020 month: jan day: 1)
                             dtend:   (datetime year: 2020 month: jan day: 2)))))
  (test-assert (really-long-event?
                (vevent dtstart: (datetime year: 2020 month: jan day: 1)
                        dtend:   (datetime year: 2020 month: jan day: 2 second: 1))))
  )

(test-group "events-between"
  (let ((start (date year: 2020 month: jan day: 1))
        (end   (date year: 2022 month: jan day: 1)))
    (let ((expected
           (list (vevent dtstart: (date year: 2020 month: jan day: 1))
                 (vevent dtstart: (date year: 2021 month: dec day: 31))
                 (vevent dtstart: (date year: 2022 month: jan day: 1))))
          (actual
           (->> (sort*
                 (list (vevent dtstart: (date year: 2019 month: jan))
                       (vevent dtstart: (date year: 2019 month: dec day: 31))
                       (vevent dtstart: (date year: 2020 month: jan day: 1))
                       (vevent dtstart: (date year: 2021 month: dec day: 31))
                       (vevent dtstart: (date year: 2022 month: jan day: 1)))
                 date< (extract1 'DTSTART))
                list->stream
                (events-between start end)
                stream->list
                )))
      (test-equal (length expected) (length actual))
      (for-each
       (lambda (name a b)
         (test-equal name
           (prop1 a 'DTSTART)
           (prop1 b 'DTSTART)
           ))
       (map number->string (iota 10))
       expected
       actual)))
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
            (compose read-zoneinfo list)))

         (timezone-component
          ;; Seed random to stable UID's.
          (parameterize (((@ (hnh util uuid) seed) (seed->random-state 0)))
            (zoneinfo->vtimezone
             zoneinfo
             "Europe/Zurich"
             (vevent summary: "Zoneinfo test"
                     dtstart: (datetime year: 2020 month: jan day: 10 hour: 10))))))

    (test-assert
        (vcomponent-equal?
         (vtimezone tzid: "Europe/Zurich"
                    (list
                     (daylight
                      dtstart: (datetime year: 1981 month: 3 day: 29 hour: 1 tz: "UTC")
                      rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun)) bymonth: '(3) wkst: monday)
                      tzname: "CEST"
                      ;; TODO why isn't this 'hour: 1'?
                      tzoffsetfrom: (timespec (time hour: 0) '+ 'wall)
                      tzoffsetto: (timespec (time hour: 2) '+ 'wall)
                      uid: "d19c9347-9a85-4432-a876-5fb9c0d24d2b")
                     (standard
                      dtstart: (datetime year: 1996 month: 10 day: 27 hour: 1 tz: "UTC")
                      rrule: (recur-rule freq: 'YEARLY interval: 1 byday: `((-1 . ,sun)) bymonth: '(10) wkst: monday)
                      tzname: "CET"
                      tzoffsetfrom: (timespec (time hour: 2) '+ 'wall)
                      tzoffsetto: (timespec (time hour: 1) '+ 'wall)
                      uid: "7dce30d4-6aaa-4cfb-85dc-813f74d7f4a9")))
         timezone-component)))

  ;; TODO these tests
  ;; (let* () "min max")
  ;; (let () "min - time")
  ;; (let "only")
  )



'((vcomponent datetime))
