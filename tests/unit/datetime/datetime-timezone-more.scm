(define-module (test datetime-timezone-more)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module ((datetime zoneinfo)
               :select (read-zoneinfo intermediary->zoneinfo)))

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


(test-equal "+1 day keeps time"
 (tz #1997-10-26T09:00 "America/New_York")
 (datetime+ (tz #1997-10-25T09:00 "America/New_York")
            (duration day: 1)))


(test-equal "+24 hours does NOT keeps time"
  (tz #1997-10-26T08:00 "America/New_York")
  (datetime+ (tz #1997-10-25T09:00 "America/New_York")
             (duration hour: 24)))


;;; TODO tests with other transitions
;;; TODO test datetime= for differently zoned datetimes

'()
