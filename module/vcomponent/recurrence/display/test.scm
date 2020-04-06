;;; Commentary:
;; This is a pure test file, but it's not placed together with the other
;; tests since it requires a human to read the output.
;;; Code:

(use-modules (vcomponent recurrence display)
             (vcomponent recurrence parse)
             (util))

;; Examples copied from RFC5545

(define (run-test desc sources)
  (format #t "~%> ~a~%" desc)
  (for el in (map (compose format-recurrence-rule
                           parse-recurrence-rule)
                  sources)
       (format #t "=> upprepas ~a.~%" el)))

(define-syntax test
  (syntax-rules (description source)
    [(_ (description desc)
        (source sources ...))
     (run-test desc (list sources ...))]))

(test
 (description "Daily for 10 occurrences")
 (source "FREQ=DAILY;COUNT=10"))
;; => "dagligen, 10 gånger."

(test
 (description "Daily until December 24, 1997")
 (source "FREQ=DAILY;UNTIL=19971224T000000Z"))
;; => "dagligen, till och med den 24 december, 1997 kl.  0:00"


(test
 (description "Every other day - forever")
 (source "FREQ=DAILY;INTERVAL=2"))
;; => "varannan dag"

(test
 (description "Every 10 days, 5 occurrences")
 (source "FREQ=DAILY;INTERVAL=10;COUNT=5"))
;; => "var tionde dag, 5 gånger."

;; TODO sortera ordningen på dagar
(test
 (description "Every day in January, for 3 years")
 (source "FREQ=YEARLY;UNTIL=20000131T140000Z;BYMONTH=1;BYDAY=SU,MO,TU,WE,TH,FR,SA"
         "FREQ=DAILY;UNTIL=20000131T140000Z;BYMONTH=1" ))
;; => "varje lördag, fredag, torsdag, onsdag, tisdag, måndag & söndag januari varje år, till och med den 31 januari, 2000 kl. 14:00"
;; => "dagligen, till och med den 31 januari, 2000 kl. 14:00"

(test
 (description "Weekly for 10 occurrences")
 (source "FREQ=WEEKLY;COUNT=10"))
;; => "varje vecka, 10 gånger."

(test
 (description "Weekly until December 24, 1997")
 (source "FREQ=WEEKLY;UNTIL=19971224T000000Z"))
;; => "varje vecka, till och med den 24 december, 1997 kl.  0:00"

(test
 (description "Every other week - forever")
 (source "FREQ=WEEKLY;INTERVAL=2;WKST=SU"))
;; => "varannan vecka"

(test
 (description "Weekly on Tuesday and Thursday for five weeks")
 (source "FREQ=WEEKLY;UNTIL=19971007T000000Z;WKST=SU;BYDAY=TU,TH"
         "FREQ=WEEKLY;COUNT=10;WKST=SU;BYDAY=TU,TH"))
;; => "varje tisdag & torsdag, till och med den 07 oktober, 1997 kl. 00:00"
;; => "varje tisdag & torsdag, 10 gånger."

(test
 (description "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:")
 (source "FREQ=WEEKLY;INTERVAL=2;UNTIL=19971224T000000Z;WKST=SU;BYDAY=MO,WE,FR"))
;; => "varannan måndag, onsdag & fredag, till och med den 24 december, 1997 kl.  0:00"

;; TOOD tittta närmare vad de egentligen vill ha här
(test
 (description "Every other week on Tuesday and Thursday, for 8 occurrences")
 (source "FREQ=WEEKLY;INTERVAL=2;COUNT=8;WKST=SU;BYDAY=TU,TH"))
;; => "varannan tisdag & torsdag, 8 gånger."

(test
 (description "Monthly on the first Friday for 10 occurrences")
 (source "FREQ=MONTHLY;COUNT=10;BYDAY=1FR"))
;; => "första fredagen varje månad, 10 gånger."

(test
 (description "Monthly on the first Friday until December 24, 1997")
 (source "FREQ=MONTHLY;UNTIL=19971224T000000Z;BYDAY=1FR"))
;; => "förste fredagen varje månad, till och med den 24 december, 1997 kl.  0:00"

(test
 (description "Every other month on the first and last Sunday of the month for 10 occurrences")
 (source "FREQ=MONTHLY;INTERVAL=2;COUNT=10;BYDAY=1SU,-1SU"))
;; => "första söndagen samt siste söndagen varannan månad, 10 gånger."

(test
 (description "Monthly on the second-to-last Monday of the month for 6 months")
 (source "FREQ=MONTHLY;COUNT=6;BYDAY=-2MO"))
;; => "näst sista måndagen varje månad, 6 gånger."

(test
 (description "Monthly on the third-to-the-last day of the month, forever")
 (source "FREQ=MONTHLY;BYMONTHDAY=-3"))
;; => "den tredje sista varje månad"

(test
 (description "Monthly on the 2nd and 15th of the month for 10 occurrences")
 (source "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=2,15"))
;; => "den andre & femtonde varje månad, 10 gånger."

(test
 (description "Monthly on the first and last day of the month for 10 occurrences")
 (source "FREQ=MONTHLY;COUNT=10;BYMONTHDAY=1,-1"))
;; => "den förste & sista varje månad, 10 gånger."

(test
 (description "Every 18 months on the 10th thru 15th of the month for 10 occurrences")
 (source "FREQ=MONTHLY;INTERVAL=18;COUNT=10;BYMONTHDAY=10,11,12,13,14,15"))
;; => "den tionde, elfte, tolfte, trettonde, fjortonde & femtonde var artonde månad, 10 gånger."

(test
 (description "Every Tuesday, every other month")
 (source "FREQ=MONTHLY;INTERVAL=2;BYDAY=TU"))
;; => "varje tisdag varannan månad"

(test
 (description "Yearly in June and July for 10 occurrences:
Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
components are specified, the day is gotten from \"DTSTART\".")
 (source "FREQ=YEARLY;COUNT=10;BYMONTH=6,7"))
;; => "juni & juli årligen, 10 gånger."

(test
 (description "Every other year on January, February, and March for 10 occurrences")
 (source "FREQ=YEARLY;INTERVAL=2;COUNT=10;BYMONTH=1,2,3"))
;; => "januari, februari & mars vartannat år, 10 gånger."

(test
 (description "Every third year on the 1st, 100th, and 200th day for 10 occurrences")
 (source "FREQ=YEARLY;INTERVAL=3;COUNT=10;BYYEARDAY=1,100,200"))
;; => "dag 1, 100 & 200 vart tredje år, 10 gånger."

(test
 (description "Every 20th Monday of the year, forever")
 (source "FREQ=YEARLY;BYDAY=20MO"))
;; => "tjugonde måndagen årligen"

(test
 (description "Monday of week number 20 (where the default start of the week is Monday), forever")
 (source "FREQ=YEARLY;BYWEEKNO=20;BYDAY=MO"))
;; => "varje måndag v.20 årligen"

(test
 (description "Every Thursday in March, forever")
 (source "FREQ=YEARLY;BYMONTH=3;BYDAY=TH"))
;; => "varje torsdag i mars, årligen"

(test
 (description "Every Thursday, but only during June, July, and August, forever")
 (source "FREQ=YEARLY;BYDAY=TH;BYMONTH=6,7,8"))
;; => "varje torsdag i juni, juli & augusti, årligen"

;; NOTE This has some weird grammar in swedish
(test
 (description "Every Friday the 13th, forever")
 (source "FREQ=MONTHLY;BYDAY=FR;BYMONTHDAY=13"))
;; => "varje fredag den trettonde varje månad"

(test
 (description "The first Saturday that follows the first Sunday of the month,forever")
 (source "FREQ=MONTHLY;BYDAY=SA;BYMONTHDAY=7,8,9,10,11,12,13"))
;; => "varje lördag den sjunde, åttonde, nionde, tionde, elfte, tolfte & trettonde varje månad"

;; TODO
(test
 (description
  "Every 4 years, the first Tuesday after a Monday in November,
forever (U.S. Presidential Election day)")
 (source "FREQ=YEARLY;INTERVAL=4;BYMONTH=11;BYDAY=TU;BYMONTHDAY=2,3,4,5,6,7,8"))
;; => "varje tisdag  i novembervart fjärde år"
;; => "varje tisdag den andre, tredje, fjärde, femte, sjätte, sjunde & åttonde i november vart fjärde år"

;; TODO bysetpos

(test
 (description "The third instance into the month of one of Tuesday, Wednesday, or
Thursday, for the next 3 months")
 (source "FREQ=MONTHLY;COUNT=3;BYDAY=TU,WE,TH;BYSETPOS=3"))

(test
 (description "The second-to-last weekday of the month")
 (source "FREQ=MONTHLY;BYDAY=MO,TU,WE,TH,FR;BYSETPOS=-2"))

(test
 (description "Every 3 hours from 9:00 AM to 5:00 PM on a specific day")
 (source "FREQ=HOURLY;INTERVAL=3;UNTIL=19970902T170000Z"))
;; => "var tredje timme, till och med den 02 september, 1997 kl. 17:00"

(test
 (description "Every 15 minutes for 6 occurrences")
 (source "FREQ=MINUTELY;INTERVAL=15;COUNT=6"))
;; => "var femtonde minut, 6 gånger."

(test
 (description "Every hour and a half for 4 occurrences")
 (source "FREQ=MINUTELY;INTERVAL=90;COUNT=4"))
;; => "var nitionde minut, 4 gånger."

(test
 (description "Every 20 minutes from 9:00 AM to 4:40 PM every day")
 (source "FREQ=DAILY;BYHOUR=9,10,11,12,13,14,15,16;BYMINUTE=0,20,40"
         "FREQ=MINUTELY;INTERVAL=20;BYHOUR=9,10,11,12,13,14,15,16"))
;; => "dagligen kl. 09:00, 09:20, 09:40, 10:00, 10:20, 10:40, 11:00, 11:20, 11:40, 12:00, 12:20, 12:40, 13:00, 13:20, 13:40, 14:00, 14:20, 14:40, 15:00, 15:20, 15:40, 16:00, 16:20 & 16:40"
;; or
;; => "var 20e minut kl. 9, 10, 11, 12, 13, 14, 15 & 16"

(test
 (description "An example where the days generated makes a difference because of WKST")
 (source "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=MO"))
;; => "varannan tisdag & söndag, 4 gånger."

(test
 (description "changing only WKST from MO to SU, yields different results...")
 (source "FREQ=WEEKLY;INTERVAL=2;COUNT=4;BYDAY=TU,SU;WKST=SU"))
;; => "varannan tisdag & söndag, 4 gånger."

(test
 (description "An example where an invalid date (i.e., February 30) is ignored.")
 (source "FREQ=MONTHLY;BYMONTHDAY=15,30;COUNT=5"))
;; => "den 15 & 30 varje månad, 5 gånger."
