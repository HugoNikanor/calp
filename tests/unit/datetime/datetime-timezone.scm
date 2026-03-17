(define-module (test datetime-timezone)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (datetime timezone)
  :use-module (datetime core)
  :use-module ((datetime) :select (datetime-difference duration))
  :use-module ((hnh test util) :select (test-match-path))
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo))
  )


;; RFC 5545 §3.3.5. Date-Time
;; > If, based on the definition of the referenced time zone, the local
;; > time described occurs more than once (when changing from daylight
;; > to standard time), the DATE-TIME value refers to the first
;; > occurrence of the referenced time.  Thus, TZID=America/
;; > New_York:20071104T013000 indicates November 4, 2007 at 1:30 A.M.
;; > EDT (UTC-04:00).
;;
;; > If the local time described does not occur (when
;; > changing from standard to daylight time), the DATE-TIME value is
;; > interpreted using the UTC offset before the gap in local times.
;; > Thus, TZID=America/New_York:20070311T023000 indicates March 11,
;; > 2007 at 3:30 A.M. EDT (UTC-04:00), one hour after 1:30 A.M. EST
;; > (UTC-05:00).

;; The above is only fully tested in the America/New_York zone tests below,
;; since we know the result (and that we don't implement them properly).
;; The other zones should have the "same" test written once we figure out
;; how to properly implement it.
(test-expect-fail (test-match-path
                   ... "America/New_York"
                   ... "Non-existant times are interpreted using previous UTC offset"
                   "RFC-provided test"))


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

;;; Europe/Stockholm rules are given in UTC time
;;; America/New_York rules are given in wall time
;;; Australia/Sydney is given in standard time

;;; All the (after) rules only exists to ensure we don't have off-by-one errors.
;;; It's the (before) and (on) rules which are actually interesting

(test-group "Europe/Stockholm"
  (test-group "utc->zone"
    (test-group "standard -> summer"
      (test-group "(before)"
        (let ((dt info (utc->zone #2026-03-29T00:59:59Z "Europe/Stockholm")))
          (test-equal (tz #2026-03-29T01:59:59 "Europe/Stockholm") dt)
          (test-equal 3600 (expanded-utc-offset info))
          (test-equal "CE%sT" (expanded-base-name info))
          (test-equal "" (expanded-zone-letters info))))

      (test-group "(on)"
        (let ((dt info (utc->zone #2026-03-29T01:00:00Z "Europe/Stockholm")))
          (test-equal (tz #2026-03-29T03:00:00 "Europe/Stockholm") dt)
          (test-equal 7200 (expanded-utc-offset info))
          (test-equal "CE%sT" (expanded-base-name info))
          (test-equal "S" (expanded-zone-letters info))))

      (test-group "(after)"
        (let ((dt info (utc->zone #2026-03-29T01:00:01Z "Europe/Stockholm")))
          (test-equal (tz #2026-03-29T03:00:01 "Europe/Stockholm") dt)
          (test-equal 7200 (expanded-utc-offset info))))
      )

    (test-group "summer -> standard"

      (test-group "(before)"
        (let ((dt info (utc->zone #2026-10-25T00:59:59Z "Europe/Stockholm")))
          (test-equal (tz #2026-10-25T02:59:59 "Europe/Stockholm") dt)
          (test-equal 7200 (expanded-utc-offset info))))

      (test-group "(on)"
        (let ((dt info (utc->zone #2026-10-25T01:00:00Z "Europe/Stockholm")))
          (test-equal (tz #2026-10-25T02:00:00 "Europe/Stockholm") dt)
          (test-equal 3600 (expanded-utc-offset info))))

      (test-group "(after)"
        (let ((dt info (utc->zone #2026-10-25T01:00:01Z "Europe/Stockholm")))
          (test-equal (tz #2026-10-25T02:00:01 "Europe/Stockholm") dt)
          (test-equal 3600 (expanded-utc-offset info))))
      ))

  (test-group "zone->utc"
    (test-group "standard -> summer"

      (test-group "(before)"
        (let ((dt info (zone->utc (tz #2026-03-29T01:59:59 "Europe/Stockholm"))))
          (test-equal #2026-03-29T00:59:59Z dt)
          (test-equal 3600 (expanded-utc-offset info))))

      ;; TODO test with the 02:xx times (which don't exist)

      (test-group "(on)"
        (let ((dt info (zone->utc (tz #2026-03-29T03:00:00 "Europe/Stockholm"))))
          (test-equal #2026-03-29T01:00:00Z dt)
          (test-equal 7200 (expanded-utc-offset info))))

      (test-group "(after)"
        (let ((dt info (zone->utc (tz #2026-03-29T03:00:01 "Europe/Stockholm"))))
          (test-equal #2026-03-29T01:00:01Z dt)
          (test-equal 7200 (expanded-utc-offset info)))))

    (test-group "summer -> standard"
      ;; The date 2026-10-25T02:30 Europe/Stockholm appears twice,
      ;; first in CEST, then in CET. RFC 5545 §3.3.5 says that we
      ;; should prefer the first instance.

      (test-group "still summer"
        (let ((dt info (zone->utc (tz #2026-10-25T01:59:59 "Europe/Stockholm"))))
          (test-equal #2026-10-24T23:59:59Z dt)
          (test-equal 7200 (expanded-utc-offset info))))

      (test-group "Times occurring twice gets their \"first\" instance."
       (let ((dt info (zone->utc (tz #2026-10-25T02:30 "Europe/Stockholm"))))
         (test-equal #2026-10-25T00:30Z dt)
         (test-equal 7200 (expanded-utc-offset info)))))))


(test-group "America/New_York"
  (test-group "utc->zone"
    (test-group "standard -> summer"
      (test-group "(before)"
        (let ((dt info (utc->zone #2026-03-08T06:59:59Z "America/New_York")))
          (test-equal (tz #2026-03-08T01:59:59 "America/New_York") dt)
          (test-equal (* -5 3600) (expanded-utc-offset info))))

      (test-group "(on)"
        (let ((dt info (utc->zone #2026-03-08T07:00:00Z "America/New_York")))
          (test-equal (tz #2026-03-08T03:00:00 "America/New_York") dt)
          (test-equal (* -4 3600) (expanded-utc-offset info))))

      (test-group "(after)"
        (let ((dt info (utc->zone #2026-03-08T07:00:01Z "America/New_York")))
          (test-equal (tz #2026-03-08T03:00:01 "America/New_York") dt)
          (test-equal (* -4 3600) (expanded-utc-offset info)))))

    (test-group "summer -> standard"
      (test-group "(before)"
        (let ((dt info (utc->zone #2026-11-01T05:59:59Z "America/New_York")))
          (test-equal (tz #2026-11-01T01:59:59 "America/New_York") dt)
          (test-equal (* -4 3600) (expanded-utc-offset info)))
        )
      (test-group "(on)"
        (let ((dt info (utc->zone #2026-11-01T06:00:00Z "America/New_York")))
          (test-equal (tz #2026-11-01T01:00 "America/New_York") dt)
          (test-equal (* -5 3600) (expanded-utc-offset info)))
        )
      (test-group "(after)"
        (let ((dt info (utc->zone #2026-11-01T06:00:01Z "America/New_York")))
          (test-equal (tz #2026-11-01T01:00:01 "America/New_York") dt)
          (test-equal (* -5 3600) (expanded-utc-offset info))))))

  (test-group "zone->utc"
    (test-group "stardard -> summer"
      (test-group "(before)"
        (let ((dt info (zone->utc (tz #2026-03-08T01:59:59 "America/New_York"))))
          (test-equal #2026-03-08T06:59:59Z dt)
          (test-equal (* -5 3600) (expanded-utc-offset info))))

      (test-group "(on)"
        (let ((dt info (zone->utc (tz #2026-03-08T03:00:00 "America/New_York"))))
          (test-equal #2026-03-08T07:00:00Z dt)
          (test-equal (* -4 3600) (expanded-utc-offset info))))

      (test-group "(after)"
        (let ((dt info (zone->utc (tz #2026-03-08T03:00:01 "America/New_York"))))
          (test-equal #2026-03-08T07:00:01Z dt)
          (test-equal (* -4 3600) (expanded-utc-offset info))))

      (test-group "Non-existant times are interpreted using previous UTC offset"
        (test-equal "Reference, when both are valid dates"
            (duration hour: 1)
            (datetime-difference (tz #2007-03-11T03:30 "America/New_York")
                                 (tz #2007-03-11T01:30 "America/New_York")))
        (test-equal "RFC-provided test"
          (duration hour: 1)
          (datetime-difference (tz #2007-03-11T02:30 "America/New_York")
                               (tz #2007-03-11T01:30 "America/New_York")))))

    (test-group "summer -> standard"
      (test-group "still summer"
        (let ((dt info (zone->utc (tz #2026-11-01T01:59:59 "America/New_York"))))
          (test-equal #2026-11-01T05:59:59Z dt)
          (test-equal (* -4 3600) (expanded-utc-offset info))))

      (test-group "Times occurring twice gets their \"first\" instance."
        (let ((dt info (zone->utc (tz #2007-11-04T01:30 "America/New_York"))))
          (test-equal (* -4 3600) (expanded-utc-offset info))
          ;; Resulting time irrelevant to check, since we know the offset
          ;; (and other checks exists for this case).
          ))

      )))

(test-group "Australia/Sydney"
  (test-group "utc->zone"
    (test-group "standard -> summer"
      (test-group "(before)"
        (let ((dt info (utc->zone #2026-10-03T15:59:59Z "Australia/Sydney")))
          (test-equal (tz #2026-10-04T01:59:59 "Australia/Sydney") dt)
          (test-equal (* 10 3600) (expanded-utc-offset info))))
      (test-group "(on)"
        (let ((dt info (utc->zone #2026-10-03T16:00Z "Australia/Sydney")))
          (test-equal (tz #2026-10-04T03:00 "Australia/Sydney") dt)
          (test-equal (* 11 3600) (expanded-utc-offset info))))
      (test-group "(after)"
        (let ((dt info (utc->zone #2026-10-03T16:00:01Z "Australia/Sydney")))
          (test-equal (tz #2026-10-04T03:00:01 "Australia/Sydney") dt)
          (test-equal (* 11 3600) (expanded-utc-offset info)))))

    (test-group "summer -> standard"
      (test-group "(before)"
        (let ((dt info (utc->zone #2026-04-04T15:59:59Z "Australia/Sydney")))
          (test-equal (tz #2026-04-05T02:59:59 "Australia/Sydney") dt)
          (test-equal (* 11 3600) (expanded-utc-offset info))))
      (test-group "(on)"
        (let ((dt info (utc->zone #2026-04-04T16:00Z "Australia/Sydney")))
          (test-equal (tz #2026-04-05T02:00 "Australia/Sydney") dt)
          (test-equal (* 10 3600) (expanded-utc-offset info))))
      (test-group "(after)"
        (let ((dt info (utc->zone #2026-04-04T16:01Z "Australia/Sydney")))
          (test-equal (tz #2026-04-05T02:01 "Australia/Sydney") dt)
          (test-equal (* 10 3600) (expanded-utc-offset info))))))



  (test-group "zone->utc"
    (test-group "standard -> summer"
      (test-group "(before)"
        (let ((dt info (zone->utc (tz #2026-10-04T01:59:59 "Australia/Sydney"))))
          (test-equal #2026-10-03T15:59:59Z dt)
          (test-equal (* 10 3600) (expanded-utc-offset info))))

      ;; TODO test with the 02:xx times (which don't exist)

      ;; AAA
      (test-group "(on)"
        (let ((dt info (zone->utc (tz #2026-10-04T03:00 "Australia/Sydney"))))
          (test-equal #2026-10-03T16:00Z dt)
          (test-equal (* 11 3600) (expanded-utc-offset info))))
      ;; BBB
      (test-group "(after)"
        (let ((dt info (zone->utc (tz #2026-10-04T03:00:01 "Australia/Sydney"))))
          (test-equal #2026-10-03T16:00:01Z dt)
          (test-equal (* 11 3600) (expanded-utc-offset info)))))

    (test-group "summer -> standard"
      (test-group "still summer"
        (let ((dt info (zone->utc (tz #2026-04-05T01:59:59 "Australia/Sydney"))))
          (test-equal #2026-04-04T14:59:59Z dt)
          (test-equal (* 11 3600) (expanded-utc-offset info))))

      ;; TODO times appearing twice
     )
    )



  )

;; zone->zone

;; datetime+/zoneinfo
;; datetime-/zoneinfo
;; datetime-difference/zoneinfo


;;; TODO test where we go from one rule to another rule



;; Below are examples copied from zic (8). Convert all these into proper tests



;; == rule-on ==
;; 5        the fifth of the month
;; lastSun  the last Sunday in the month
;; lastMon  the last Monday in the month
;; Sun>=8   first Sunday on or after the eighth
;; Sun<=25  last Sunday on or before the 25th

;; == rule-at ==
;; 2            time in hours
;; 2:00         time in hours and minutes
;; 01:28:14     time in hours, minutes, and seconds
;; 00:19:32.13  time with fractional seconds
;; 12:00        midday, 12 hours after 00:00
;; 15:00        3 PM, 15 hours after 00:00
;; 24:00        end of day, 24 hours after 00:00
;; 260:00       260 hours after 00:00
;; -2:30        2.5 hours before 00:00
;; -            equivalent to 0



;; If a continuation line subtracts N seconds from the UT offset  after  a
;; transition that would be interpreted to be later if using the continua‐
;; tion  line's UT offset and rules, the “until” time of the previous zone
;; or continuation line  is  interpreted  according  to  the  continuation
;; line's  UT offset and rules, and any rule that would otherwise take ef‐
;; fect in the next N seconds is instead assumed to take effect simultane‐
;; ously.  For example:

;;   # Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
;;   Rule    US    1967  2006  -  Oct  lastSun  2:00  0     S
;;   Rule    US    1967  1973  -  Apr  lastSun  2:00  1:00  D
;;   # Zone  NAME               STDOFF  RULES  FORMAT  [UNTIL]
;;   Zone    America/Menominee  -5:00   -      EST     1973 Apr 29 2:00
;;                              -6:00   US     C%sT

;; Here, an incorrect reading would be there were  two  clock  changes  on
;; 1973-04-29, the first from 02:00 EST (-05) to 01:00 CST (-06) according
;; to  the  “until”  value  in the zone line, and the second an hour later
;; from 02:00 CST (-06) to 03:00 CDT (-05) according to the values in  the
;; April  rule line.  However, zic interprets this more sensibly as a sin‐
;; gle transition from 02:00 EST (-05) to 02:00 CDT (-05).




;; EXTENDED EXAMPLE
;; Here  is  an extended example of zic input, intended to illustrate many
;; of its features.

;;   # Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
;;   Rule    Swiss 1941  1942  -  May  Mon>=1   1:00  1:00  S
;;   Rule    Swiss 1941  1942  -  Oct  Mon>=1   2:00  0     -
;;   Rule    EU    1977  1980  -  Apr  Sun>=1   1:00u 1:00  S
;;   Rule    EU    1977  only  -  Sep  lastSun  1:00u 0     -
;;   Rule    EU    1978  only  -  Oct   1       1:00u 0     -
;;   Rule    EU    1979  1995  -  Sep  lastSun  1:00u 0     -
;;   Rule    EU    1981  max   -  Mar  lastSun  1:00u 1:00  S
;;   Rule    EU    1996  max   -  Oct  lastSun  1:00u 0     -

;;   # Zone  NAME           STDOFF      RULES  FORMAT  [UNTIL]
;;   Zone    Europe/Zurich  0:34:08     -      LMT     1853 Jul 16
;;                          0:29:45.50  -      BMT     1894 Jun
;;                          1:00        Swiss  CE%sT   1981
;;                          1:00        EU     CE%sT

;;   Link    Europe/Zurich  Europe/Vaduz

;; In this example, the EU rules are for the European Union  and  for  its
;; predecessor  organization,  the  European Communities.  The timezone is
;; named Europe/Zurich and it has the alias  Europe/Vaduz.   This  example
;; says  that  Zurich  was  34  minutes  and  8  seconds  east of UT until
;; 1853-07-16 at 00:00, when the legal offset was changed to 7 degrees  26
;; minutes  22.50  seconds, which works out to 0:29:45.50; zic treats this
;; by rounding it to 0:29:46.  After 1894-06-01 at 00:00 the UT offset be‐
;; came one hour and Swiss daylight saving rules (defined with  lines  be‐
;; ginning  with  “Rule  Swiss”) apply.  From 1981 to the present, EU day‐
;; light saving rules have applied, and the UTC offset has remained at one
;; hour.

;; In 1941 and 1942, daylight saving time applied from the first Monday in
;; May at 01:00 to the first Monday in October at 02:00.  The pre-1981  EU
;; daylight-saving  rules  have  no effect here, but are included for com‐
;; pleteness.  Since 1981, daylight saving has begun on the last Sunday in
;; March at 01:00 UTC.  Until 1995 it ended the last Sunday  in  September
;; at  01:00  UTC, but this changed to the last Sunday in October starting
;; in 1996.

;; For purposes of display, “LMT” and “BMT” were initially  used,  respec‐
;; tively.   Since  Swiss  rules and later EU rules were applied, the time
;; zone abbreviation has been CET for standard time and CEST for  daylight
;; saving time.



;;; TODO write proper tests with manually written timezone database, containing at least:

;;; 1. Direct rule forever:
;;; - no preconditions
;;; - will give a single stream value

;;; 2. Indirect rule forever:
;;; 2.1. If rules go on forever
;;; 2.1.1. And we start before the first rule
;;; 2.1.2. And we start on the first rule
;;; 2.1.3. And we start after the first rule
;;; 2.2. If the rule eventually stops
;;; 2.2.1. And we start before the first rule
;;; 2.2.2. And we start on the first rule
;;; 2.2.3. And we start after the first rule

;;; 3. Direct rule until:
;;; - no preconditions
;;; - will give a single stream value

;;; 4. Indirct rule until:


(test-equal
    (expanded-rule
     wall: #2026-03-29T02:00 utc: #2026-03-29T01:00Z
     type: 'daylight offset: 7200 name: "CE%sT" letters: "S" from: 'final
     ;; NOTE `rule` is only for debug. Update this test to match whatever
     ;; the procedure happens to return
     rule: "EU 1981-maximum")
  (find-rule "Europe/Berlin" #2026-06-10T00:00 expanded-start-utc))



(define ->utc (@@ (datetime timezone) ->utc))
(define ->wall (@@ (datetime timezone) ->wall))

(test-group "Time type conversions"
  (let ((dt #2026-06-10T10:00))
    (test-group "Group 1"
      (test-equal #2026-06-10T10:00 (->utc  (cons 'utc dt) 3600 7200))
      (test-equal #2026-06-10T12:00 (->wall (cons 'utc dt) 3600 7200))
      (test-equal #2026-06-10T09:00 (->utc  (cons 'standard dt) 3600 7200))
      (test-equal #2026-06-10T11:00 (->wall (cons 'standard dt) 3600 7200))
      (test-equal #2026-06-10T08:00 (->utc  (cons 'wall dt) 3600 7200))
      (test-equal #2026-06-10T10:00 (->wall (cons 'wall dt) 3600 7200)))

    (test-group "Group 2"
      (test-equal #2026-03-08T07:00 (->utc (cons 'wall #2026-03-08T02:00) (* -5 3600) (* -5 3600)))
      (test-equal #2026-11-01T06:00 (->utc (cons 'wall #2026-11-01T02:00) (* -5 3600) (* -4 3600))))

    ))



(test-group "expanded-rule-printf"
  (parameterize ((zoneinfo (call-with-input-string "
# Zone	NAME		STDOFF	RULES	FORMAT			[UNTIL]
Zone	Europe/Berlin	1:00	-	Local			2001
			1:00	EU	Standard-%s/Summer-%s

Rule	EU	1981	max	-	Mar	lastSun	 1:00u	1:00	1
Rule	EU	1996	max	-	Oct	lastSun	 1:00u	0	2
" (compose intermediary->zoneinfo read-zoneinfo))))
    (test-equal
        "Summer-1"
      (expanded-rule-printf
       (find-rule "Europe/Berlin" #2025-06-01T00:00 expanded-start-utc)))

    (test-equal
        "Standard-2"
      (expanded-rule-printf
       (find-rule "Europe/Berlin" #2025-12-01T00:00 expanded-start-utc)))
    ))

'((datetime timezone))
