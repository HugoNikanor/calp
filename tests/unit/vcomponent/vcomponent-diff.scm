(define-module (test vcomponent-diff)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module ((datetime) :select ())
  :use-module (datetime timezone)
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo))
  :use-module (vcomponent)
  :use-module (vcomponent media-type))

(define media-format (resolve-media-type "text/calendar"))

(define fake-zoneinfo (call-with-input-string "
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone America/New_York	-4:56:02 -	LMT	1883 Nov 18 17:00u
			-5:00	US	E%sT	1920
			-5:00	NYC	E%sT	1942
			-5:00	US	E%sT	1946
			-5:00	NYC	E%sT	1967
			-5:00	US	E%sT

Link America/New_York US/Eastern

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER
Rule	NYC	1920	only	-	Mar	lastSun	2:00	1:00	D
Rule	NYC	1920	only	-	Oct	lastSun	2:00	0	S
Rule	NYC	1921	1966	-	Apr	lastSun	2:00	1:00	D
Rule	NYC	1921	1954	-	Sep	lastSun	2:00	0	S
Rule	NYC	1955	1966	-	Oct	lastSun	2:00	0	S

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	US	1918	1919	-	Mar	lastSun	2:00	1:00	D
Rule	US	1918	1919	-	Oct	lastSun	2:00	0	S
Rule	US	1942	only	-	Feb	9	2:00	1:00	W # War
Rule	US	1945	only	-	Aug	14	23:00u	1:00	P # Peace
Rule	US	1945	only	-	Sep	30	2:00	0	S
Rule	US	1967	2006	-	Oct	lastSun	2:00	0	S
Rule	US	1967	1973	-	Apr	lastSun	2:00	1:00	D
Rule	US	1974	only	-	Jan	6	2:00	1:00	D
Rule	US	1975	only	-	Feb	lastSun	2:00	1:00	D
Rule	US	1976	1986	-	Apr	lastSun	2:00	1:00	D
Rule	US	1987	2006	-	Apr	Sun>=1	2:00	1:00	D
Rule	US	2007	max	-	Mar	Sun>=8	2:00	1:00	D
Rule	US	2007	max	-	Nov	Sun>=1	2:00	0	S
" (compose intermediary->zoneinfo read-zoneinfo)))

(parameterize ((zoneinfo fake-zoneinfo))
  (test-equal
      '(((VEVENT 0) *properties* DTSTART
         (list (vline value: #2006-01-03T17:00))
         (list (vline value: (datetime year: 2006
                                       month: 1
                                       day: 3
                                       hour: 12
                                       tz: "US/Eastern"))))

        ((VEVENT 0) *properties* RECURRENCE-ID
         (list (vline value: #2006-01-03T17:00))
         _)

        ((VEVENT 0) *properties* RRULE
         _ (list (vline value: (recur-rule freq: 'DAILY
                                           recur-count: 5
                                           interval: 1
                                           wkst: mon))))


        ((VEVENT 1) *properties* DTSTART
         (list (vline value: #2006-01-04T19:00))
         (list (vline value: (datetime year: 2006 month: 1 day: 4
                                       hour: 14
                                       tz: "US/Eastern"))))


        ((VEVENT 1) *properties* RECURRENCE-ID
         (list (vline value: #2006-01-04T17:00))
         (list (vline value: (datetime year: 2006 month: 1 day: 4
                                       hour: 12 tz: "US/Eastern")))))

    (vcomponent-diff
     (call-with-input-string
         "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART:20060103T170000
DURATION:PT1H
RECURRENCE-ID:20060103T170000
SUMMARY:Event #2
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART:20060104T190000
DURATION:PT1H
RECURRENCE-ID:20060104T170000
SUMMARY:Event #2 bis
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
END:VCALENDAR"
       (parser media-format))
     (call-with-input-string
         "BEGIN:VCALENDAR
PRODID:-//Example Corp.//CalDAV Client//EN
VERSION:2.0
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060103T120000
DURATION:PT1H
RRULE:FREQ=DAILY;COUNT=5
SUMMARY:Event #2
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
BEGIN:VEVENT
DTSTAMP:20060206T001121Z
DTSTART;TZID=US/Eastern:20060104T140000
DURATION:PT1H
RECURRENCE-ID;TZID=US/Eastern:20060104T120000
SUMMARY:Event #2 bis
UID:00959BC664CA650E933C892C@example.com
END:VEVENT
END:VCALENDAR"
       (parser media-format))))


  (test-equal
      '((VEVENT *left-only* 1))
    (vcomponent-diff
     (call-with-input-string
         "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
DTSTAMP:20060206T001220Z
DTSTART:20060104T150000
DURATION:PT1H
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
UID:DC6C50A017428C5216A2F1CD@example.com
X-ABC-GUID:E1CX5Dr-0007ym-Hz@example.com
END:VEVENT
END:VCALENDAR"
       (parser media-format))
     (call-with-input-string
         "BEGIN:VCALENDAR
PRODID:-//Example Corp.//CalDAV Client//EN
VERSION:2.0
END:VCALENDAR"
       (parser media-format))))

  (test-equal '()
    (vcomponent-diff
     (call-with-input-string
         "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
DTSTAMP:20060206T001220Z
DTSTART:20060104T150000
DURATION:PT1H
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
UID:DC6C50A017428C5216A2F1CD@example.com
X-ABC-GUID:E1CX5Dr-0007ym-Hz@example.com
END:VEVENT
END:VCALENDAR"
       (parser media-format))
     (call-with-input-string
         "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//Example Corp.//CalDAV Client//EN
BEGIN:VEVENT
ATTENDEE;PARTSTAT=ACCEPTED;ROLE=CHAIR:mailto:cyrus@example.com
ATTENDEE;PARTSTAT=NEEDS-ACTION:mailto:lisa@example.com
DTSTAMP:20060206T001220Z
DTSTART:20060104T150000
DURATION:PT1H
LAST-MODIFIED:20060206T001330Z
ORGANIZER:mailto:cyrus@example.com
SEQUENCE:1
STATUS:TENTATIVE
SUMMARY:Event #3
UID:DC6C50A017428C5216A2F1CD@example.com
X-ABC-GUID:E1CX5Dr-0007ym-Hz@example.com
END:VEVENT
END:VCALENDAR"
       (parser media-format)))))

'((vcomponent))
