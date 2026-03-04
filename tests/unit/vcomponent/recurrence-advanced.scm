;;; Commentary:
;; Tests of recurrence rule generation with focus on correct instances
;; being generated. For tests of basic recurrence functionallity, see
;; recurrence-simple.scm.
;;
;; This file also tests format-recurrence-rule, which checks that human
;; readable representations of the RRULES work.
;;
;; Also contains the tests for EXDATE.
;;
;; Most examples copied from RFC5545, some home written.
;;; Code:

(define-module (test recurrence-advanced)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent type recurrence)
               :select (recur-rule))
  :use-module ((vcomponent type recurrence generate)
               :select (generate-recurrence-set))
  :use-module ((vcomponent type recurrence display)
               :select (format-recurrence-rule))
  :use-module ((vcomponent type recurrence)
               :select (recur-count until))
  :use-module ((vcomponent)
               :select (prop% prop1 extract1 vline-value))
  :use-module (vcomponent create)
  :use-module ((datetime)
               :select (
                        datetime
                        datetime-date
                        time
                        date
                        jan feb mar apr may jun jul aug sep oct nov dec
                        mon tue wed thu fri sat sun
                        datetime->string

                        zoneinfo
                        ))
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo))
  :use-module ((hnh util) :select (-> set!))
  :use-module ((hnh util env) :select (with-locale1))
  :use-module ((srfi srfi-41) :select (stream->list))
  :use-module ((srfi srfi-88) :select (keyword->string)))

;;; Not yet implemented
(test-expect-fail "STR: The second-to-last weekday of the month")
(test-expect-fail "STR: The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months")

;;; We change weekstart to sunday, meaning that sunday should come before monday
(test-expect-fail "STR: changing only WKST from MO to SU, yields different results.")



;;; TODO write speed tests for some deranged cases
;;; For example, FREQ=YEARLY;BYSECOND=60,..,1



;;; Set up a local zoneinfo here. These rules are an extract from the
;;; "common" timezone database, but limited to Europe/Stockholm,
;;; America/New_York, Australia/Sydney, and UTC
(zoneinfo
 (call-with-input-string "
# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone	Europe/Berlin	0:53:28 -	LMT	1893 Apr
			1:00	C-Eur	CE%sT	1945 May 24  2:00
			1:00 SovietZone	CE%sT	1946
			1:00	Germany	CE%sT	1980
			1:00	EU	CE%sT

# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone America/New_York	-4:56:02 -	LMT	1883 Nov 18 17:00u
			-5:00	US	E%sT	1920
			-5:00	NYC	E%sT	1942
			-5:00	US	E%sT	1946
			-5:00	NYC	E%sT	1967
			-5:00	US	E%sT

# Zone	NAME		STDOFF	RULES	FORMAT	[UNTIL]
Zone Australia/Sydney	10:04:52 -	LMT	1895 Feb
			10:00	Aus	AE%sT	1971
			10:00	AN	AE%sT


Zone	Etc/UTC			0	-	UTC

Link	Etc/UTC			UTC
Link	Europe/Berlin		Europe/Stockholm

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	Germany	1946	only	-	Apr	14	2:00s	1:00	S
Rule	Germany	1946	only	-	Oct	 7	2:00s	0	-
Rule	Germany	1947	1949	-	Oct	Sun>=1	2:00s	0	-
# https://www.ptb.de/cms/en/ptb/fachabteilungen/abt4/fb-44/ag-441/realisation-of-legal-time-in-germany/dst-and-midsummer-dst-in-germany-until-1979.html
# says the following transition occurred at 3:00 MEZ, not the 2:00 MEZ
# given in Shanks & Pottenger. Go with the PTB.
Rule	Germany	1947	only	-	Apr	 6	3:00s	1:00	S
Rule	Germany	1947	only	-	May	11	2:00s	2:00	M
Rule	Germany	1947	only	-	Jun	29	3:00	1:00	S
Rule	Germany	1948	only	-	Apr	18	2:00s	1:00	S
Rule	Germany	1949	only	-	Apr	10	2:00s	1:00	S

Rule SovietZone	1945	only	-	May	24	2:00	2:00	M # Midsummer
Rule SovietZone	1945	only	-	Sep	24	3:00	1:00	S
Rule SovietZone	1945	only	-	Nov	18	2:00s	0	-

# Older C-Eur rules are for convenience in the tables.
# From 1977 on, C-Eur differs from EU only in that C-Eur uses standard time.
Rule	C-Eur	1916	only	-	Apr	30	23:00	1:00	S
Rule	C-Eur	1916	only	-	Oct	 1	 1:00	0	-
Rule	C-Eur	1917	1918	-	Apr	Mon>=15	 2:00s	1:00	S
Rule	C-Eur	1917	1918	-	Sep	Mon>=15	 2:00s	0	-
Rule	C-Eur	1940	only	-	Apr	 1	 2:00s	1:00	S
Rule	C-Eur	1942	only	-	Nov	 2	 2:00s	0	-
Rule	C-Eur	1943	only	-	Mar	29	 2:00s	1:00	S
Rule	C-Eur	1943	only	-	Oct	 4	 2:00s	0	-
Rule	C-Eur	1944	1945	-	Apr	Mon>=1	 2:00s	1:00	S
# Whitman gives 1944 Oct 7; go with Shanks & Pottenger.
Rule	C-Eur	1944	only	-	Oct	 2	 2:00s	0	-

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	EU	1977	1980	-	Apr	Sun>=1	 1:00u	1:00	S
Rule	EU	1977	only	-	Sep	lastSun	 1:00u	0	-
Rule	EU	1978	only	-	Oct	 1	 1:00u	0	-
Rule	EU	1979	1995	-	Sep	lastSun	 1:00u	0	-
Rule	EU	1981	max	-	Mar	lastSun	 1:00u	1:00	S
Rule	EU	1996	max	-	Oct	lastSun	 1:00u	0	-

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

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	Aus	1917	only	-	Jan	 1	2:00s	1:00	D
Rule	Aus	1917	only	-	Mar	lastSun	2:00s	0	S
Rule	Aus	1942	only	-	Jan	 1	2:00s	1:00	D
Rule	Aus	1942	only	-	Mar	lastSun	2:00s	0	S
Rule	Aus	1942	only	-	Sep	27	2:00s	1:00	D
Rule	Aus	1943	1944	-	Mar	lastSun	2:00s	0	S
Rule	Aus	1943	only	-	Oct	 3	2:00s	1:00	D

# Rule	NAME	FROM	TO	-	IN	ON	AT	SAVE	LETTER/S
Rule	AN	1971	1985	-	Oct	lastSun	2:00s	1:00	D
Rule	AN	1972	only	-	Feb	27	2:00s	0	S
Rule	AN	1973	1981	-	Mar	Sun>=1	2:00s	0	S
Rule	AN	1982	only	-	Apr	Sun>=1	2:00s	0	S
Rule	AN	1983	1985	-	Mar	Sun>=1	2:00s	0	S
Rule	AN	1986	1989	-	Mar	Sun>=15	2:00s	0	S
Rule	AN	1986	only	-	Oct	19	2:00s	1:00	D
Rule	AN	1987	1999	-	Oct	lastSun	2:00s	1:00	D
Rule	AN	1990	1995	-	Mar	Sun>=1	2:00s	0	S
Rule	AN	1996	2005	-	Mar	lastSun	2:00s	0	S
Rule	AN	2000	only	-	Aug	lastSun	2:00s	1:00	D
Rule	AN	2001	2007	-	Oct	lastSun	2:00s	1:00	D
Rule	AN	2006	only	-	Apr	Sun>=1	2:00s	0	S
Rule	AN	2007	only	-	Mar	lastSun	2:00s	0	S
Rule	AN	2008	max	-	Apr	Sun>=1	2:00s	0	S
Rule	AN	2008	max	-	Oct	Sun>=1	2:00s	1:00	D
" (compose intermediary->zoneinfo read-zoneinfo)))




(define (run-test comp)
  (test-equal
      (string-append "REC: " (prop1 comp 'SUMMARY))
    (map vline-value (prop% comp 'X-SET))
    (let ((r (generate-recurrence-set (vcalendar (list comp)))))
      (map (extract1 'DTSTART)
           (if (or (until       (prop1 comp 'RRULE))
                   (recur-count (prop1 comp 'RRULE)))
               (stream->list r)
               (stream->list 20 r)))))
  (test-equal
      (string-append "STR: " (prop1 comp 'SUMMARY))
    (prop1 comp 'X-SUMMARY)
    ;; NOTE care must be taken so LC_TIME is set to match the parameter to the recurrence rule.
    ;; TODO possibly test with other languages
    (with-locale1
     LC_TIME "sv_SE.UTF-8"
     (lambda ()
       (format-recurrence-rule (prop1 comp 'RRULE) 'sv)))))

(map run-test
     (list (vevent
            summary:
            "Daily for 10 occurrences"
            dtstart:
            (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule:
            (recur-rule
             freq: 'DAILY
             count: 10)
            x-summary:
            "dagligen, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 11 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Daily until December 24, 1997"
            dtstart:
            (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule:
            (recur-rule
             freq: 'DAILY
             until: (datetime year: 1997 month: 12 day: 24 tz: "UTC"))
            x-summary:
            "dagligen, till och med den 24 december, 1997 kl.  0:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 23 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every other day - forever"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule freq: 'DAILY interval: 2)
            x-summary: "varannan dag"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 10 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every 10 days, 5 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'DAILY
                    interval: 10
                    count: 5)
            x-summary: "var tionde dag, totalt 5 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 12 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every day in January, for 3 years (alt 1)"
            dtstart: (datetime year: 1998 month: 01 day: 01 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    until: (datetime year: 2000 month: 01 day: 31 hour: 14 tz: "UTC")
                    bymonth: (list jan)
                    byday: (list sun mon tue wed thu fri sat))
            ;; TODO replace with something like:
            ;; "varje dag i januari, årligen, t.o.m den 31 januari 2000, kl 14:00"
            x-summary:
            "varje lördag, fredag, torsdag, onsdag, tisdag, måndag & söndag i januari, årligen, till och med den 31 januari, 2000 kl. 14:00"
            x-set:
            (list (datetime year: 1998 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 31 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every day in January, for 3 years (alt 2)"
            dtstart: (datetime year: 1998 month: 01 day: 01 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'DAILY
                    until: (datetime year: 2000 month: 01 day: 31 hour: 14 tz: "UTC")
                    bymonth: (list jan))
            ;; TODO something like "dagligen i januari, t.o.m. den 31 januari 2000, kl 14:00"
            x-summary:
            "dagligen, till och med den 31 januari, 2000 kl. 14:00"
            x-set:
            (list (datetime year: 1998 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 31 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Weekly for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    count: 10)
            x-summary: "varje vecka, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 04 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Weekly until December 24, 1997"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    until: (datetime year: 1997 month: 12 day: 24 tz: "UTC"))
            x-summary: "varje vecka, till och med den 24 december, 1997 kl.  0:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 23 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every other week - forever"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    interval: 2
                    wkst: sun)
            x-summary: "varannan vecka"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 04 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 04 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 26 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Weekly on Tuesday and Thursday for five weeks (alt 1)"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    until: (datetime year: 1997 month: 10 day: 07 tz: "UTC")
                    wkst: sun
                    byday: (list tue thu))
            x-summary:
            "varje tisdag & torsdag, till och med den 07 oktober, 1997 kl.  0:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Weekly on Tuesday and Thursday for five weeks (alt 2)"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    count: 10
                    wkst: sun
                    byday: (list tue thu))
            x-summary: "varje tisdag & torsdag, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Every other week on Monday, Wednesday, and Friday until December 24, 1997, starting on Monday, September 1, 1997:"
            dtstart: (datetime year: 1997 month: 09 day: 01 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    interval: 2
                    until: (datetime year: 1997 month: 12 day: 24 tz: "UTC")
                    wkst: sun
                    byday: (list mon wed fri))
            x-summary:
            "varannan måndag, onsdag & fredag, till och med den 24 december, 1997 kl.  0:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 22 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every other week on Tuesday and Thursday, for 8 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    interval: 2
                    count: 8
                    wkst: sun
                    byday: (list tue thu))
            x-summary: "varannan tisdag & torsdag, totalt 8 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 16 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Monthly on the first Friday for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    count: 10
                    byday: (list (cons 1 fri)))
            x-summary: "första fredagen varje månad, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 04 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 05 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Monthly on the first Friday until December 24, 1997"
            dtstart:
            (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    until: (datetime year: 1997 month: 12 day: 24 tz: "UTC")
                    byday: (list (cons 1 fri)))
            x-summary:
            "första fredagen varje månad, till och med den 24 december, 1997 kl.  0:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 05 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Every other month on the first and last Sunday of the month for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 07 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    interval: 2
                    count: 10
                    byday: (list (cons 1 sun)
                                 (cons -1 sun)))
            ;; TODO replace with something like
            ;; "första och sista söndagen varannan månad, totalt 10 gånger"
            x-summary: "första söndagen samt sista söndagen varannan månad, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 31 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Monthly on the second-to-last Monday of the month for 6 months"
            dtstart: (datetime year: 1997 month: 09 day: 22 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    count: 6
                    byday: (list (cons -2 mon)))
            x-summary: "näst sista måndagen varje månad, totalt 6 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 22 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 16 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Monthly on the third-to-the-last day of the month, forever"
            dtstart: (datetime year: 1997 month: 09 day: 28 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    bymonthday: (list -3))
            x-summary: "den tredje sista varje månad"
            x-set:
            (list (datetime year: 1997 month: 09 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 04 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 08 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 09 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 10 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 11 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 12 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 02 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 04 day: 28 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Monthly on the 2nd and 15th of the month for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    count: 10
                    bymonthday: (list 2 15))
            x-summary: "den andre & femtonde varje månad, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 15 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Monthly on the first and last day of the month for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    count: 10
                    bymonthday: (list 1 -1))
            x-summary: "den förste & sista varje månad, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 01 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every 18 months on the 10th thru 15th of the month for 10 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 10 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    interval: 18
                    count: 10
                    bymonthday: (list 10 11 12 13 14 15))
            ;; TODO replace with something like
            ;; "den tionde till femtonde var artonde månad, totalt 10 gånger"
            x-summary:
            "den tionde, elfte, tolfte, trettonde, fjortonde & femtonde var artonde månad, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 13 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every Tuesday, every other month"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    interval: 2
                    byday: (list tue))
            x-summary: "varje tisdag varannan månad"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 12 hour: 09 tz: "America/New_York")))
           (vevent
            ;; Note: Since none of the BYDAY, BYMONTHDAY, or BYYEARDAY
            ;; components are specified, the day is gotten from "DTSTART".
            summary: "Yearly in June and July for 10 occurrences:"
            dtstart: (datetime year: 1997 month: 06 day: 10 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    count: 10
                    bymonth: (list 6 7))
            x-summary: "juni & juli, årligen, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 06 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 06 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 07 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 06 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 07 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 06 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 07 day: 10 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every other year on January, February, and March for 10 occurrences"
            dtstart: (datetime year: 1997 month: 03 day: 10 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    interval: 2
                    count: 10
                    bymonth: (list jan feb mar))
            x-summary: "januari, februari & mars vartannat år, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 03 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 02 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 02 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 03 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 02 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 03 day: 10 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every third year on the 1st, 100th, and 200th day for 10 occurrences"
            dtstart: (datetime year: 1997 month: 01 day: 01 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    interval: 3
                    count: 10
                    byyearday: (list 1 100 200))
            x-summary: "dag 1, 100 & 200 vart tredje år, totalt 10 gånger"
            x-set:
            (list (datetime year: 1997 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 04 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 04 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 07 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 01 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 04 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 07 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2006 month: 01 day: 01 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every 20th Monday of the year, forever"
            dtstart: (datetime year: 1997 month: 05 day: 19 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    byday: (list (cons 20 mon)))
            ;; TODO clarify to "årets tjugonde måndag, varje år"
            x-summary: "tjugonde måndagen, årligen"
            x-set:
            (list (datetime year: 1997 month: 05 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 05 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 05 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2002 month: 05 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 05 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2004 month: 05 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2005 month: 05 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2006 month: 05 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2008 month: 05 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2009 month: 05 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 2010 month: 05 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2011 month: 05 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2012 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2013 month: 05 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 2014 month: 05 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 2015 month: 05 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 2016 month: 05 day: 16 hour: 09 tz: "America/New_York")))
           (vevent
            summary:
            "Monday of week number 20 (where the default start of the week is Monday), forever"
            dtstart: (datetime year: 1997 month: 05 day: 12 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    byweekno: (list 20)
                    byday: (list mon))
            x-summary: "varje måndag v.20, årligen"
            x-set:
            (list (datetime year: 1997 month: 05 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 05 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 05 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2002 month: 05 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 05 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 2004 month: 05 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 2005 month: 05 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2006 month: 05 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2008 month: 05 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 2009 month: 05 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 2010 month: 05 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 2011 month: 05 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2012 month: 05 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 2013 month: 05 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2014 month: 05 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 2015 month: 05 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 2016 month: 05 day: 16 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every Thursday in March, forever"
            dtstart: (datetime year: 1997 month: 03 day: 13 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    bymonth: (list mar)
                    byday: (list thu))
            ;; TODO The ", årligen" part is redundant, try to remove
            x-summary: "varje torsdag i mars, årligen"
            x-set:
            (list (datetime year: 1997 month: 03 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 03 day: 20 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 03 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 03 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 03 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 03 day: 16 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 03 day: 23 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 03 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 03 day: 01 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 03 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 03 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 03 day: 22 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every Thursday, but only during June, July, and August, forever"
            dtstart: (datetime year: 1997 month: 06 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    byday: (list thu)
                    bymonth: (list 6 7 8))
            ;; TODO ", årligen" is redundant
            x-summary: "varje torsdag i juni, juli & augusti, årligen"
            x-set:
            (list (datetime year: 1997 month: 06 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 06 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 06 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 06 day: 26 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 24 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 07 day: 31 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 14 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 21 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 28 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 18 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 25 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 16 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every Friday the 13th, forever"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            exdate: (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York"))
            rrule: (recur-rule
                    freq: 'MONTHLY
                    byday: (list fri)
                    bymonthday: (list 13))
            ;; TODO " varje månad" is redundant
            x-summary: "varje fredag den trettonde varje månad"
            x-set:
            (list (datetime year: 1998 month: 02 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 11 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 08 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 10 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 04 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2001 month: 07 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2002 month: 09 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2002 month: 12 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2003 month: 06 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2004 month: 02 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2004 month: 08 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2005 month: 05 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2006 month: 01 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2006 month: 10 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 04 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 07 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2008 month: 06 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2009 month: 02 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 2009 month: 03 day: 13 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "The first Saturday that follows the first Sunday of the month, forever"
            dtstart: (datetime year: 1997 month: 09 day: 13 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    byday: (list sat)
                    bymonthday: (list 7 8 9 10 11 12 13))
            ;; TODO something like
            ;; "varje lördag vilken inträffar mellan den sjunde och trettonde varje månad"
            ;; Currently the bymonthday sounds like an expander, while it in fact is a limiter
            ;; See also rule below (U.S. Presidential Election day)
            x-summary: "varje lördag den sjunde, åttonde, nionde, tionde, elfte, tolfte & trettonde varje månad"
            x-set:
            (list (datetime year: 1997 month: 09 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 02 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 03 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 04 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 05 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 06 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 07 day: 11 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 08 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 09 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 10 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 12 day: 12 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 01 day: 09 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 02 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 03 day: 13 hour: 09 tz: "America/New_York")
                  (datetime year: 1999 month: 04 day: 10 hour: 09 tz: "America/New_York")))
           (vevent
            ;; (U.S. Presidential Election day)
            summary: "Every 4 years, the first Tuesday after a Monday in November, forever"
            dtstart: (datetime year: 1996 month: 11 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'YEARLY
                    interval: 4
                    bymonth: (list nov)
                    byday: (list tue)
                    bymonthday: (list 2 3 4 5 6 7 8))
            ;; TODO something like
            ;; "varje tisdag mellan den andre och åttonde november vart fjärde år"
            ;; see also rule above
            x-summary: "varje tisdag den andre, tredje, fjärde, femte, sjätte, sjunde eller åttonde i november vart fjärde år"
            x-set:
            (list (datetime year: 1996 month: 11 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 2000 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 2004 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2008 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 2012 month: 11 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 2016 month: 11 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 2020 month: 11 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 2024 month: 11 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 2028 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 2032 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2036 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 2040 month: 11 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 2044 month: 11 day: 08 hour: 09 tz: "America/New_York")
                  (datetime year: 2048 month: 11 day: 03 hour: 09 tz: "America/New_York")
                  (datetime year: 2052 month: 11 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 2056 month: 11 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 2060 month: 11 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 2064 month: 11 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 2068 month: 11 day: 06 hour: 09 tz: "America/New_York")
                  (datetime year: 2072 month: 11 day: 08 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "The third instance into the month of one of Tuesday, Wednesday, or Thursday, for the next 3 months"
            dtstart: (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    count: 3
                    byday: (list tue wed thu)
                    bysetpos: (list 3))
            ;; TODO
            ;; "Den tredje instansen av en tis-, ons-, eller torsdag i månaden, i totalt 3 månader"
            x-summary: "NOT YET IMPLEMENTED"
            x-set:
            (list (datetime year: 1997 month: 09 day: 04 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 07 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 06 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "The second-to-last weekday of the month"
            dtstart: (datetime year: 1997 month: 09 day: 29 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    byday: (list mon tue wed thu fri)
                    bysetpos: (list -2)
                    count: 5            ; added by me
                    )
            ;; TODO
            ;; "Den näst sista måndagen till fredagen i månaden, i totalt 3 månader"
            ;; TODO like mo-su could be shortened to "all days", maybe shorten mo-fr to "all weekdays"?
            x-summary: "NOT YET IMPLEMENTED"
            x-set:
            (list (datetime year: 1997 month: 09 day: 29 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 10 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 11 day: 27 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 12 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 1998 month: 01 day: 29 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "Every 3 hours from 9:00 AM to 5:00 PM on a specific day"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'HOURLY
                    interval: 3
                    until: (datetime year: 1997 month: 09 day: 02 hour: 17 tz: "UTC"))
            x-summary: "var tredje timme, till och med den 02 september, 1997 kl. 17:00"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 15 tz: "America/New_York")))
           (vevent
            summary: "Every 15 minutes for 6 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MINUTELY
                    interval: 15
                    count: 6)
            x-summary: "varje kvart, totalt 6 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 15 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 30 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 45 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 15 tz: "America/New_York")))
           (vevent
            summary: "Every hour and a half for 4 occurrences"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MINUTELY
                    interval: 90
                    count: 4)
            ;; TODO halvannan timma
            x-summary: "var sjätte kvart, totalt 4 gånger"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 30 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 30 tz: "America/New_York")))
           (vevent
            summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 1)"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'DAILY
                    byhour: (list 9 10 11 12 13 14 15 16)
                    byminute: (list 0 20 40))
            ;; TODO
            x-summary:
            "dagligen kl. 09:00, 09:20, 09:40, 10:00, 10:20, 10:40, 11:00, 11:20, 11:40, 12:00, 12:20, 12:40, 13:00, 13:20, 13:40, 14:00, 14:20, 14:40, 15:00, 15:20, 15:40, 16:00, 16:20 & 16:40"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 20 tz: "America/New_York")))
           (vevent
            summary: "Every 20 minutes from 9:00 AM to 4:40 PM every day (alt 2)"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MINUTELY
                    interval: 20
                    byhour: (list 9 10 11 12 13 14 15 16))
            ;; TODO "var tjugonde minut mellan 9 & 16"
            x-summary: "var tjugonde minut kl. 9, 10, 11, 12, 13, 14, 15 & 16"
            x-set:
            (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 10 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 11 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 12 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 13 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 20 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 14 minute: 40 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 00 tz: "America/New_York")
                  (datetime year: 1997 month: 09 day: 02 hour: 15 minute: 20 tz: "America/New_York")))
           (vevent
            summary: "An example where the days generated makes a difference because of WKST"
            dtstart: (datetime year: 1997 month: 08 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    interval: 2
                    count: 4
                    byday: (list tue sun)
                    wkst: mon)
            x-summary: "varannan tisdag & söndag, totalt 4 gånger"
            x-set:
            (list (datetime year: 1997 month: 08 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 10 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 24 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "changing only WKST from MO to SU, yields different results."
            dtstart: (datetime year: 1997 month: 08 day: 05 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'WEEKLY
                    interval: 2
                    count: 4
                    byday: (list tue sun)
                    wkst: sun)
            x-summary: "varannan söndag & tisdag, totalt 4 gånger"
            x-set:
            (list (datetime year: 1997 month: 08 day: 05 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 17 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 19 hour: 09 tz: "America/New_York")
                  (datetime year: 1997 month: 08 day: 31 hour: 09 tz: "America/New_York")))
           (vevent
            summary: "An example where an invalid date (i.e., February 30) is ignored"
            dtstart: (datetime year: 2007 month: 01 day: 15 hour: 09 tz: "America/New_York")
            rrule: (recur-rule
                    freq: 'MONTHLY
                    bymonthday: (list 15 30)
                    count: 5)
            ;; TODO " varje månad" is redundant
            x-summary: "den femtonde & trettionde varje månad, totalt 5 gånger"
            x-set:
            (list (datetime year: 2007 month: 01 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 01 day: 30 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 02 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 03 day: 15 hour: 09 tz: "America/New_York")
                  (datetime year: 2007 month: 03 day: 30 hour: 09 tz: "America/New_York")))




           (vevent
            summary: "Every Friday & Wednesday the 13th, forever"
            dtstart: (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00)
            exdate: (list (datetime year: 1997 month: 09 day: 02 hour: 09 minute: 00 second: 00))
            rrule:
            (recur-rule
             freq: 'MONTHLY
             byday: (list fri wed)
             bymonthday: (list 13))
            ;; TODO " varje månad" is redundant
            x-summary: "varje onsdag & fredag den trettonde varje månad"
            x-set:
            (list (datetime year: 1998 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1998 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1998 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1998 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1999 month: 01 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1999 month: 08 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1999 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2000 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2000 month: 10 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2000 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2001 month: 04 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2001 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2001 month: 07 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 02 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 03 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 09 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 11 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 12 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2003 month: 06 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2003 month: 08 day: 13 hour: 09 minute: 00 second: 00)))
           (vevent
            summary:
            "Monday & Wednesday of week number 20 (where the default start of the week is Monday), forever"
            dtstart: (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
            rrule: (recur-rule
                    freq: 'YEARLY
                    byweekno: (list 20)
                    byday: (list mon wed))
            ;; TODO ", årligen" is redundant
            x-summary: "varje onsdag & måndag v.20, årligen"
            x-set:
            (list (datetime year: 1997 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                  (datetime year: 1997 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                  (datetime year: 1998 month: 05 day: 11 hour: 09 minute: 00 second: 00)
                  (datetime year: 1998 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 1999 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                  (datetime year: 1999 month: 05 day: 19 hour: 09 minute: 00 second: 00)
                  (datetime year: 2000 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                  (datetime year: 2000 month: 05 day: 17 hour: 09 minute: 00 second: 00)
                  (datetime year: 2001 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                  (datetime year: 2001 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 05 day: 13 hour: 09 minute: 00 second: 00)
                  (datetime year: 2002 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                  (datetime year: 2003 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                  (datetime year: 2003 month: 05 day: 14 hour: 09 minute: 00 second: 00)
                  (datetime year: 2004 month: 05 day: 10 hour: 09 minute: 00 second: 00)
                  (datetime year: 2004 month: 05 day: 12 hour: 09 minute: 00 second: 00)
                  (datetime year: 2005 month: 05 day: 16 hour: 09 minute: 00 second: 00)
                  (datetime year: 2005 month: 05 day: 18 hour: 09 minute: 00 second: 00)
                  (datetime year: 2006 month: 05 day: 15 hour: 09 minute: 00 second: 00)
                  (datetime year: 2006 month: 05 day: 17 hour: 09 minute: 00 second: 00)))
           (vevent
            summary: "Each second, for ever"
            dtstart: (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'SECONDLY)
            x-summary: "varje sekund"
            x-set: (list (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 01)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 02)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 03)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 04)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 05)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 06)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 07)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 08)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 09)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 10)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 11)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 12)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 13)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 14)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 15)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 16)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 17)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 18)
                         (datetime year: 2020 month: 10 day: 10 hour: 10 minute: 00 second: 19)))

           ;; Exdates are applied after rrule's, meaning that less than count
           ;; instances may be present.
           (vevent
            summary: "Exdates are applied AFTER rrule's"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            exdate: (list (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         ;; (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00) ; skipped by exdate
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         ))
           (vevent
            summary: "RDATE:s add to the recurrence rule"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            rdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00) ; added by rdate
                         ))

           ;; TODO test where rdata exactly matches entry added by rrule

           (vevent
            summary: "RDATE:s add to the recurrence rule"
            dtstart: (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
            rrule: (recur-rule freq: 'DAILY count: 5)
            exdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            rdate: (list (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00))
            x-summary: "dagligen, totalt 5 gånger"
            x-set: (list (datetime year: 2022 month: 06 day: 10 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 11 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 12 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 13 hour: 10 minute: 00 second: 00)
                         (datetime year: 2022 month: 06 day: 14 hour: 10 minute: 00 second: 00)
                         ;; (datetime year: 2022 month: 06 day: 20 hour: 10 minute: 00 second: 00) ; added by rdate, removed by exdate
                         ))
           ;; TODO rdate with different timezone than dtstart
           ;; TODO rdate with period
           ))



'((vcomponent type recurrence)
  (vcomponent type recurrence generate)
  (vcomponent type recurrence display)
  (vcomponent type recurrence internal))
