;;; Commentary:
;; The human readable tests are expected to fail with any change to the
;; text creator. Proof-read them manually, and update the test cases
;; to match. `x-summary' used for target string. Target strings should
;; be in swedish.
;;; Code:

(((vcomponent recurrence parse) parse-recurrence-rule)
 ((vcomponent recurrence display) format-recurrence-rule)
 ((vcomponent base) make-vcomponent attr attr*)
 ((datetime) parse-ics-datetime)
 ((util) -> mod!)
 ((srfi srfi-88) keyword->string)
 ((guile) set!)
 )

;; Examples copied from RFC5545

(define (run-test comp)
  (test-equal (attr comp 'SUMMARY)
    (attr comp 'X-SUMMARY)
    (format-recurrence-rule (attr comp 'RRULE))))


(define (vevent . rest)
  (define v (make-vcomponent 'VEVENT))

  (let loop ((rem rest))
    (unless (null? rem)
      (let ((symb (-> (car rem)
                      keyword->string
                      string-upcase
                      string->symbol)))
        (set! (attr v symb)
          (case symb
            [(DTSTART EXDATE) (parse-ics-datetime (cadr rem))]
            [(RRULE) (parse-recurrence-rule (cadr rem))]
            [else (cadr rem)]))
        ;; hack for multi valued fields
        (when (eq? symb 'EXDATE)
          (mod! (attr* v symb) list)))
      (loop (cddr rem))))

  v)

(map run-test
 (list
  (vevent
   summary: "Daily for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;COUNT=10"
   x-summary: "dagligen, totalt 10 gånger")

  (vevent
   summary: "Daily until December 24, 1997"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;UNTIL=19971224T000000Z"
   x-summary: "dagligen, till och med den 24 december, 1997 kl.  0:00")


  (vevent
   summary: "Every other day - forever"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;INTERVAL=2"
   x-summary: "varannan dag")

  (vevent
   summary: "Every 10 days, 5 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;INTERVAL=10;COUNT=5"
   x-summary: "var tionde dag, totalt 5 gånger")

  (vevent
   summary: "Every day in January, for 3 years (alt 1)"
   dtstart: "19980101T090000"
   rrule: "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"
   x-summary: "varje lördag, fredag, torsdag, onsdag, tisdag, måndag & söndag i januari, årligen, till och med den 31 januari, 2000 kl. 14:00")

  (vevent
   summary: "Every day in January, for 3 years (alt 2)"
   dtstart: "19980101T090000"
   rrule: "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1" 
   x-summary: "dagligen, till och med den 31 januari, 2000 kl. 14:00")

  (vevent
   summary: "Weekly for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;COUNT=10"
   x-summary: "varje vecka, totalt 10 gånger")

  (vevent
   summary: "Weekly until December 24, 1997"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;UNTIL=19971224T000000Z"
   x-summary: "varje vecka, till och med den 24 december, 1997 kl.  0:00")

  (vevent
   summary: "Every other week - forever"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;WKST=SU"
   x-summary: "varannan vecka")

  (vevent
   summary: "Weekly on Tuesday and Thursday for five weeks (alt 1)"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH"
   x-summary: "varje tisdag & torsdag, till och med den 07 oktober, 1997 kl.  0:00")

  (vevent
   summary: "Weekly on Tuesday and Thursday for five weeks (alt 2)"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"
   x-summary: "varje tisdag & torsdag, totalt 10 gånger")

  (vevent
   summary: "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:"
   dtstart: "19970901T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR"
   x-summary: "varannan måndag, onsdag & fredag, till och med den 24 december, 1997 kl.  0:00")

  ;; TOOD tittta närmare vad de egentligen vill ha här
  (vevent
   summary: "Every other week on Tuesday and Thursday, for 8 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"
   x-summary: "varannan tisdag & torsdag, totalt 8 gånger")

  (vevent                               ; 10
   summary: "Monthly on the first Friday for 10 occurrences"
   dtstart: "19970905T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYDAY=1FR"
   x-summary: "första fredagen varje månad, totalt 10 gånger")

  (vevent
   summary: "Monthly on the first Friday until December 24, 1997"
   dtstart: "19970905T090000"
   rrule: "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"
   x-summary: "första fredagen varje månad, till och med den 24 december, 1997 kl.  0:00")

  (vevent
   summary: "Every other month on the first and last Sunday of the month for 10 occurrences"
   dtstart: "19970907T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"
   x-summary: "första söndagen samt sista söndagen varannan månad, totalt 10 gånger")

  (vevent
   summary: "Monthly on the second-to-last Monday of the month for 6 months"
   dtstart: "19970922T090000"
   rrule: "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"
   x-summary: "näst sista måndagen varje månad, totalt 6 gånger")

  (vevent
   summary: "Monthly on the third-to-the-last day of the month, forever"
   dtstart: "19970928T090000"
   rrule: "FREQ=MONTHLY;BYMONTHDAY=-3"
   x-summary: "den tredje sista varje månad")

  (vevent
   summary: "Monthly on the 2nd and 15th of the month for 10 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"
   x-summary: "den andre & femtonde varje månad, totalt 10 gånger")

  (vevent
   summary: "Monthly on the first and last day of the month for 10 occurrences"
   dtstart: "19970930T090000"
   rrule: "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"
   x-summary: "den förste & sista varje månad, totalt 10 gånger")

  (vevent
   summary: "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
   dtstart: "19970910T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"
   x-summary: "den tionde, elfte, tolfte, trettonde, fjortonde & femtonde var artonde månad, totalt 10 gånger")

  (vevent
   summary: "Every Tuesday, every other month"
   dtstart: "19970902T090000"
   rrule: "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU"
   x-summary: "varje tisdag varannan månad")

  (vevent
   summary: "Yearly in June and July for 10 occurrences:
Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
components are specified, the day is gotten from \"DTSTART\""
   dtstart: "19970610T090000"
   rrule: "FREQ=YEARLY;COUNT=10;BYMONTH=6,7"
   x-summary: "juni & juli, årligen, totalt 10 gånger")

  (vevent
   summary: "Every other year on January, February, and March for 10 occurrences"
   dtstart: "19970310T090000"
   rrule: "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"
   x-summary: "januari, februari & mars vartannat år, totalt 10 gånger")

  (vevent
   summary: "Every third year on the 1st, 100th, and 200th day for 10 occurrences"
   dtstart: "19970101T090000"
   rrule: "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"
   x-summary: "dag 1, 100 & 200 vart tredje år, totalt 10 gånger")

  (vevent
   summary: "Every 20th Monday of the year, forever"
   dtstart: "19970519T090000"
   rrule: "FREQ=YEARLY;BYDAY=20MO"
   x-summary: "tjugonde måndagen, årligen")

  (vevent
   summary: "Monday of week number 20 (where the default start of the week is Monday), forever"
   dtstart: "19970512T090000"
   rrule: "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO"
   x-summary: "varje måndag v.20, årligen")

  (vevent
   summary: "Every Thursday in March, forever"
   dtstart: "19970313T090000"
   rrule: "FREQ=YEARLY;BYMONTH=3;BYDAY=TH"
   x-summary: "varje torsdag i mars, årligen")

  (vevent
   summary: "Every Thursday, but only during June, July, and August, forever"
   dtstart: "19970605T090000"
   rrule: "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8"
   x-summary: "varje torsdag i juni, juli & augusti, årligen")

  ;; NOTE This has some weird grammar in swedish
  (vevent
   summary: "Every Friday the 13th, forever"
   dtstart: "19970902T090000"
   exdate: "19970902T090000"
   rrule: "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13"
   x-summary: "varje fredag den trettonde varje månad")

  (vevent
   summary: "The first Saturday that follows the first Sunday of the month,forever"
   dtstart: "19970913T090000"
   rrule: "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13"
   x-summary: "varje lördag den sjunde, åttonde, nionde, tionde, elfte, tolfte & trettonde varje månad")

  (vevent
   summary:
   "Every 4 years, the first Tuesday after a Monday in November,
forever (U.S. Presidential Election day)"
   dtstart: "19961105T090000"
   rrule: "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"
   x-summary: "varje tisdag den andre, tredje, fjärde, femte, sjätte, sjunde eller åttonde i november vart fjärde år")

  (vevent
   summary: "The third instance into the month of one of Tuesday, Wednesday, or
Thursday, for the next 3 months"
   dtstart: "19970904T090000"
   rrule: "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3"
   x-summary: "NOT YET IMPLEMENTED")

  (vevent
   summary: "The second-to-last weekday of the month"
   dtstart: "19970929T090000"
   rrule: "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"
   x-summary: "NOT YET IMPLEMENTED")

  (vevent
   summary: "Every 3 hours from 9:00 AM to 5:00 PM on a specific day"
   dtstart: "19970902T090000"
   rrule: "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z"
   x-summary: "var tredje timme, till och med den 02 september, 1997 kl. 17:00")

  (vevent
   summary: "Every 15 minutes for 6 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=15;COUNT=6"
   x-summary: "varje kvart, totalt 6 gånger")

  (vevent
   summary: "Every hour and a half for 4 occurrences"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=90;COUNT=4"
   x-summary: "var sjätte kvart, totalt 4 gånger")

  (vevent
   summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 1)"
   dtstart: "19970902T090000"
   rrule: "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40"
   x-summary: "dagligen kl. 09:00, 09:20, 09:40, 10:00, 10:20, 10:40, 11:00, 11:20, 11:40, 12:00, 12:20, 12:40, 13:00, 13:20, 13:40, 14:00, 14:20, 14:40, 15:00, 15:20, 15:40, 16:00, 16:20 & 16:40")

  (vevent
   summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 2)"
   dtstart: "19970902T090000"
   rrule: "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16"
   x-summary: "var tjugonde minut kl. 9, 10, 11, 12, 13, 14, 15 & 16")

  (vevent
   summary: "An example where the days generated makes a difference because of WKST"
   dtstart: "19970805T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO"
   x-summary: "varannan tisdag & söndag, totalt 4 gånger")

  (vevent
   summary: "changing only WKST from MO to SU, yields different results.."
   dtstart: "19970805T090000"
   rrule: "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU"
   x-summary: "varannan tisdag & söndag, totalt 4 gånger")

  (vevent
   summary: "An example where an invalid date (i.e., February 30) is ignored"
   dtstart: "20070115T090000"
   rrule: "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5"
   x-summary: "den femtonde & tretionde varje månad, totalt 5 gånger")



;;; End of examples from RFC, start of own examples


  (vevent
   summary: "Every Friday & Wednesday the 13th, forever"
   dtstart: "19970902T090000"
   exdate: "19970902T090000"
   rrule: "FREQ=MONTHLY;BYDAY=FR,WE;BYMONTHDAY=13"
   x-summary: "varje onsdag & fredag den trettonde varje månad")

(vevent
   summary: "Monday & Wednesday of week number 20 (where the default start of the week is Monday), forever"
   dtstart: "19970512T090000"
   rrule: "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO,WE"
   x-summary: "varje onsdag & måndag v.20, årligen")
  ))
