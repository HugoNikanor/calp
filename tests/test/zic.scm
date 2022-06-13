(define-module (test zic)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zic))


(test-expect-fail "Simple Leap")
(test-expect-fail "Simple Expire")

(define big-sample
 "# Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
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

(define parse-zic-file (@@ (datetime zic) parse-zic-file))

;; Some of the tests are slightly altered to score better on the coverage
(test-group "From zic(8)"
            (test-equal "Basic Rule"
              (list ((@@ (datetime zic) make-rule)
                     'US 1967 1973 4 '(last 0)
                     ((@ (datetime zic) make-timespec) #02:00:00 '+ #\w)
                     ((@ (datetime zic) make-timespec) #01:00:00 '+ #\d)
                     "D"))
              (call-with-input-string "Rule  US    1967  1973  -  Apr  lastSun  2:00w  1:00d  D"
                parse-zic-file))

            ;; Technically not from zic(8), since that example has an until field
            (test-equal "Basic Zone"
              (list ((@@ (datetime zic) make-zone) "Asia/Amman"
                     (list ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #02:00:00 '+ #\w)
                            'Jordan "EE%sT" #f))))

              (call-with-input-string
                  "Zone  Asia/Amman  2:00    Jordan  EE%sT"
                parse-zic-file))

            ;; Modified from the following example
            (test-equal "Basic Zone with continuation"
              (list ((@@ (datetime zic) make-zone) "America/Menominee"
                     (list ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #05:00:00 '- #\w)
                            #f "EST" #1973-04-29T02:00:00)
                           ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #06:00:00 '- #\w)
                            'US "C%sT" #f))))
              ;; Why can't I single read a zone with an until field?
              (call-with-input-string
                  "Zone  America/Menominee  -5:00   -      EST     1973 Apr 29 2:00
                         -6:00   US     C%sT"
                parse-zic-file))


            (test-equal "Rules and Zone"
              (list ((@@ (datetime zic) make-zone) "America/Menominee"
                     (list ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #05:00:00 '- #\w)
                            #f "EST" #1973-04-29T02:00:00)
                           ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #06:00:00 '- #\w)
                            'US "C%sT" #f)))
                    ((@@ (datetime zic) make-rule)
                     'US 1967 1973 dec '(last 0)
                     (make-timespec #02:00:00 '+ #\w)
                     (make-timespec #01:00:00 '+ #\w)
                     "D")
                    ((@@ (datetime zic) make-rule)
                     'US 1967 2006 nov '(last 0)
                     (make-timespec #02:00:00 '+ #\w)
                     (make-timespec #00:00:00 '+ #\w)
                     "S"))
              (call-with-input-string
                  "# Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
Rule    US    1967  2006  -  Nov  lastSun  2:00  0     S
Rule    US    1967  1973  -  Dec  lastSun  2:00  1:00  D
# Zone  NAME             STDOFF  RULES  FORMAT  [UNTIL]
Zone  America/Menominee  -5:00   -      EST     1973 Apr 29 2:00
                         -6:00   US     C%sT
" parse-zic-file))


            (test-equal "Simple Link"
              (list ((@@ (datetime zic) make-link) "Asia/Istanbul" "Europe/Istanbul"))
              (call-with-input-string "Link Europe/Istanbul Asia/Istanbul"
                parse-zic-file))

            (test-equal "Simple Leap"
              'not-yet-implemented
              (call-with-input-string "Leap 2016 Dec 31 23:59:60 + S"
                parse-zic-file))

            (test-equal "Simple Expire"
              'not-yet-implemented
              (call-with-input-string "Expires 2020 Dec 28 00:00:00"
                parse-zic-file))


            (test-equal "Extended example"
              ;; Items are in reverse order of discovery
              (list ((@@ (datetime zic) make-link) "Europe/Vaduz" "Europe/Zurich")
                    ((@@ (datetime zic) make-zone) "Europe/Zurich"
                     (list ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #00:34:08 '+ #\w)
                            #f "LMT" #1853-07-16T00:00:00)
                           ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #00:29:45 '+ #\w) ; NOTE that the .50 is discarded
                            #f "BMT" #1894-06-01T00:00:00)
                           ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #01:00:00 '+ #\w)
                            'Swiss "CE%sT" #1981-01-01T00:00:00)
                           ((@@ (datetime zic) make-zone-entry)
                            (make-timespec #01:00:00 '+ #\w)
                            'EU "CE%sT" #f)))
                    ((@@ (datetime zic) make-rule) 'EU 1996 'maximum 10 '(last 0)
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #00:00:00 '+ #\w)
                     "")
                    ((@@ (datetime zic) make-rule) 'EU 1981 'maximum 3 '(last 0)
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #01:00:00 '+ #\w)
                     "S")
                    ((@@ (datetime zic) make-rule) 'EU 1979 1995 9 `(last ,sun)
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #00:00:00 '+ #\w)
                     "")
                    ((@@ (datetime zic) make-rule) 'EU 1978 'only 10 1
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #00:00:00 '+ #\w)
                     "")
                    ((@@ (datetime zic) make-rule) 'EU 1977 'only 9 `(last ,sun)
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #00:00:00 '+ #\w)
                     "")
                    ((@@ (datetime zic) make-rule) 'EU 1977 1980 4 `(> ,sun 1)
                     (make-timespec #01:00:00 '+ #\u)
                     (make-timespec #01:00:00 '+ #\w)
                     "S")
                    ((@@ (datetime zic) make-rule) 'Swiss 1941 1942 10 `(> ,mon 1)
                     (make-timespec #02:00:00 '+ #\w)
                     (make-timespec #00:00:00 '+ #\w)
                     "")
                    ((@@ (datetime zic) make-rule) 'Swiss 1941 1942 5 `(> ,mon 1)
                     (make-timespec #01:00:00 '+ #\w)
                     (make-timespec #01:00:00 '+ #\w)
                     "S"))
              (call-with-input-string big-sample
                parse-zic-file)))

(test-group "rule->dtstart"
            (test-equal "last sunday"
              #1967-04-30T02:00:00
              (rule->dtstart
               ((@@ (datetime zic) make-rule)
                'US 1967 1973 4 '(last 0)
                ((@ (datetime zic) make-timespec) #02:00:00 '+ #\w)
                ((@ (datetime zic) make-timespec) #01:00:00 '+ #\d)
                "D")))

            (test-equal "sunday >= 1"
              #1977-04-03T01:00:00Z
              (rule->dtstart
               ((@@ (datetime zic) make-rule) 'EU 1977 1980 4 `(> ,sun 1)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #01:00:00 '+ #\w)
                "S")))

            ;; Max and min uses dummy dates, which is slightly wrong
            ;; but shouldn't cause any real problems

            (test-equal "Minimum time"
              #0000-10-30T01:00:00Z
              (rule->dtstart
               ((@@ (datetime zic) make-rule) 'EU 'minimum 2000 10 '(last 0)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")))

            (test-equal "Maximum time"
              (datetime year: 9999 month: oct day: 27
                        hour: 1 tz: "UTC")
              (rule->dtstart
               ((@@ (datetime zic) make-rule) 'EU 'maximum 2000 10 '(last 0)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                ""))))

(test-group "zone-format"

            (test-equal "Zone format with argument" "CEST" (zone-format "CE%sT" "S"))
            (test-equal "Zone format with empty"    "CET"  (zone-format "CE%sT" ""))

            ;; TODO zone-format %z is not yet implemented, and therefore untested

            ;; TODO this error message is currently translatable...
            (test-equal "Invalid format specifier"
              '(misc-error "zone-format" "Invalid format char ~s in ~s at position ~a" (#\S "%S" 1) #f)
              (catch 'misc-error (lambda () (zone-format "%S" "A"))
                list)))

(test-group "Actual object"
            ;; NOTE this doesn't test read-zoneinfos ability to
            ;; - take filenames
            ;; - take multiple items
            (let ((zoneinfo (call-with-input-string big-sample (compose read-zoneinfo list))))
              (test-assert "get-zone returns a zone-entry object"
                (every zone-entry? (get-zone zoneinfo "Europe/Zurich")))
              (test-equal "A link resolves to the same object as its target"
                (get-zone zoneinfo "Europe/Zurich") (get-zone zoneinfo "Europe/Vaduz"))
              (test-equal "Get rules returns correctly, and in order"
                  ;; Rules are sorted
                  (list ((@@ (datetime zic) make-rule) 'Swiss 1941 1942 5 `(> ,mon 1)
                         (make-timespec #01:00:00 '+ #\w)
                         (make-timespec #01:00:00 '+ #\w)
                         "S")
                        ((@@ (datetime zic) make-rule) 'Swiss 1941 1942 10 `(> ,mon 1)
                         (make-timespec #02:00:00 '+ #\w)
                         (make-timespec #00:00:00 '+ #\w)
                         ""))
               (get-rule zoneinfo 'Swiss))))


(test-group "rule->rrule"
            (test-equal "Basic example, and to = maximum"
              ((@ (vcomponent recurrence internal) make-recur-rule)
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons -1 sun))
               bymonth: (list oct))
              (rule->rrule
               ((@@ (datetime zic) make-rule) 'EU 1996 'maximum 10 '(last 0)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")
               ))

            (test-equal "with to = only"
              #f
              (rule->rrule
               ((@@ (datetime zic) make-rule) 'EU 1996 'only 10 '(last 2)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")))

            (test-equal "with definitive to year"
              ((@ (vcomponent recurrence internal) make-recur-rule)
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons -1 tue))
               bymonth: (list oct)
               until: #2000-01-01T00:00:00)
              (rule->rrule
               ((@@ (datetime zic) make-rule) 'EU 1996 2000 10 '(last 2)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")))

            (test-equal "on being a month day"
              ((@ (vcomponent recurrence internal) make-recur-rule)
               freq: 'YEARLY interval: 1 wkst: mon
               bymonthday: (list 2)
               bymonth: (list oct))
              (rule->rrule
               ((@@ (datetime zic) make-rule) 'EU 1996 'maximum 10 2
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")))

            (test-equal "on being first day after date"
              ((@ (vcomponent recurrence internal) make-recur-rule)
               freq: 'YEARLY interval: 1 wkst: mon
               byday: (list (cons 1 mon))
               bymonth: (list oct))
              (rule->rrule
               ((@@ (datetime zic) make-rule) 'EU 1996 'maximum 10 `(> ,mon 2)
                (make-timespec #01:00:00 '+ #\u)
                (make-timespec #00:00:00 '+ #\w)
                "")))

            (test-equal "Crash on counting backwards from date"
              '(warning "Counting backward for RRULES unsupported" ())
              (catch 'warning
                (lambda ()
                 (rule->rrule
                  ((@@ (datetime zic) make-rule) 'EU 1996 'maximum 10 `(< ,mon 2)
                   (make-timespec #01:00:00 '+ #\u)
                   (make-timespec #00:00:00 '+ #\w)
                   "")))
                list))

            (test-equal "Crash on to = minimum"
              '(misc-error "rule->rrule" "Check your input" #f #f)
              (catch 'misc-error
                (lambda ()
                  (rule->rrule
                   ((@@ (datetime zic) make-rule) 'EU 1996 'minimum 10 `(< ,mon 2)
                    (make-timespec #01:00:00 '+ #\u)
                    (make-timespec #00:00:00 '+ #\w)
                    "")))
                list))
            )
