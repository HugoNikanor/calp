(define-module (test zic)
  :use-module ((srfi srfi-1) :select (every))
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (datetime zoneinfo)
  :use-module (datetime zoneinfo types)
  :use-module (datetime zoneinfo intermediary)
  :use-module (datetime zoneinfo zic)
  :use-module ((vcomponent type recurrence)
               :select (recur-rule)))

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

(define parse-zic-file
  (@@ (datetime zoneinfo zic) parse-zic-file))

;; Some of the tests are slightly altered to score better on the coverage
(test-group "From zic(8)"
            (test-equal "Basic Rule"
              (list (zi-rule
                     rule-name: 'US
                     rule-from: 1967
                     rule-to: 1973
                     rule-in: 4
                     rule-on: '(last 0)
                     rule-at: (timespec (time hour: 02 minute: 00 second: 00) '+ 'wall)
                     rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                     rule-letters: "D"))
              (call-with-input-string "Rule  US    1967  1973  -  Apr  lastSun  2:00w  1:00d  D"
                parse-zic-file))

            ;; Technically not from zic(8), since that example has an until field
            (test-equal "Basic Zone"
              (list ((@@ (datetime zoneinfo zic) zone)
                     zone-name: "Asia/Amman"
                     zone-entries: (list (zone-entry
                                          stdoff: (timespec (time hour: 02 minute: 00 second: 00) '+ #f)
                                          rule: 'Jordan
                                          format: "EE%sT"
                                          until: #f))))

              (call-with-input-string
                  "Zone  Asia/Amman  2:00    Jordan  EE%sT"
                parse-zic-file))

            ;; Modified from the following example
            (test-equal "Basic Zone with continuation"
              (list ((@@ (datetime zoneinfo zic) zone)
                     zone-name: "America/Menominee"
                     zone-entries: (list (zone-entry
                                        stdoff: (timespec (time hour: 05 minute: 00 second: 00) '- #f)
                                        rule: (timespec (time) '+ 'standard)
                                        format: "EST"
                                        until: (datetime year: 1973 month: 04 day: 29 hour: 02 minute: 00 second: 00))
                                       (zone-entry
                                        stdoff: (timespec (time hour: 06 minute: 00 second: 00) '- #f)
                                        rule: 'US
                                        format: "C%sT"
                                        until: #f))))
              ;; Why can't I single read a zone with an until field?
              (call-with-input-string
                  "Zone  America/Menominee  -5:00   -      EST     1973 Apr 29 2:00
                         -6:00   US     C%sT"
                parse-zic-file))


            (test-equal "Rules and Zone"
              (list ((@@ (datetime zoneinfo zic) zone)
                     zone-name: "America/Menominee"
                     zone-entries: (list (zone-entry
                                          stdoff: (timespec (time hour: 05 minute: 00 second: 00) '- #f)
                                          rule: (timespec (time) '+ 'standard)
                                          format: "EST"
                                          until: (datetime year: 1973 month: 04 day: 29 hour: 02 minute: 00 second: 00))
                                         (zone-entry
                                          stdoff: (timespec (time hour: 06 minute: 00 second: 00) '- #f)
                                          rule: 'US
                                          format: "C%sT"
                                          until: #f)))
                    (zi-rule
                     rule-name: 'US
                     rule-from: 1967
                     rule-to: 1973
                     rule-in: dec
                     rule-on: '(last 0)
                     rule-at: (timespec (time hour: 02 minute: 00 second: 00) '+ 'wall)
                     rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                     rule-letters: "D")
                    (zi-rule
                     rule-name: 'US
                     rule-from: 1967
                     rule-to: 2006
                     rule-in: nov
                     rule-on: '(last 0)
                     rule-at: (timespec (time hour: 02 minute: 00 second: 00) '+ 'wall)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "S"))
              (call-with-input-string
                  "# Rule  NAME  FROM  TO    -  IN   ON       AT    SAVE  LETTER/S
Rule    US    1967  2006  -  Nov  lastSun  2:00  0     S
Rule    US    1967  1973  -  Dec  lastSun  2:00  1:00  D
# Zone  NAME             STDOFF  RULES  FORMAT  [UNTIL]
Zone  America/Menominee  -5:00   -      EST     1973 Apr 29 2:00
                         -6:00   US     C%sT
" parse-zic-file))


            (test-equal "Simple Link"
              (list (zone-link
                     name: "Asia/Istanbul"
                     target: "Europe/Istanbul"))
              (call-with-input-string "Link Europe/Istanbul Asia/Istanbul"
                parse-zic-file))

            (test-error "Simple Leap"
              'not-implemented
              (call-with-input-string "Leap 2016 Dec 31 23:59:60 + S"
                parse-zic-file))

            (test-error "Simple Expire"
              'not-implemented
              (call-with-input-string "Expires 2020 Dec 28 00:00:00"
                parse-zic-file))


            (test-equal "Extended example"
              ;; Items are in reverse order of discovery
              (list (zone-link
                     name: "Europe/Vaduz"
                     target: "Europe/Zurich")
                    ((@@ (datetime zoneinfo zic) zone)
                     zone-name: "Europe/Zurich"
                     zone-entries: (list (zone-entry
                                        stdoff: (timespec (time hour: 00 minute: 34 second: 08) '+ #f)
                                        rule: (timespec (time) '+ 'standard)
                                        format: "LMT"
                                        until: (datetime year: 1853 month: 07 day: 16 hour: 00 minute: 00 second: 00))
                                       (zone-entry
                                        stdoff: (timespec (time hour: 00 minute: 29 second: 45) '+ #f) ; NOTE that the .50 is discarded
                                        rule: (timespec (time) '+ 'standard)
                                        format: "BMT"
                                        until: (datetime year: 1894 month: 06 day: 01 hour: 00 minute: 00 second: 00))
                                       (zone-entry
                                        stdoff: (timespec (time hour: 01 minute: 00 second: 00) '+ #f)
                                        rule: 'Swiss
                                        format: "CE%sT"
                                        until: (datetime year: 1981 month: 01 day: 01 hour: 00 minute: 00 second: 00))
                                       (zone-entry
                                        stdoff: (timespec (time hour: 01 minute: 00 second: 00) '+ #f)
                                        rule: 'EU
                                        format: "CE%sT"
                                        until: #f)))
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1996
                     rule-to: 'maximum
                     rule-in: 10
                     rule-on: '(last 0)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "")
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1981
                     rule-to: 'maximum
                     rule-in: 3
                     rule-on: '(last 0)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                     rule-letters: "S")
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1979
                     rule-to: 1995
                     rule-in: 9
                     rule-on: `(last ,sun)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "")
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1978
                     rule-to: 'only
                     rule-in: 10
                     rule-on: 1
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "")
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1977
                     rule-to: 'only
                     rule-in: 9
                     rule-on: `(last ,sun)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "")
                    (zi-rule
                     rule-name: 'EU
                     rule-from: 1977
                     rule-to: 1980
                     rule-in: 4
                     rule-on: `(> ,sun 1)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'utc)
                     rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                     rule-letters: "S")
                    (zi-rule
                     rule-name: 'Swiss
                     rule-from: 1941
                     rule-to: 1942
                     rule-in: 10
                     rule-on: `(> ,mon 1)
                     rule-at: (timespec (time hour: 02 minute: 00 second: 00) '+ 'wall)
                     rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                     rule-letters: "")
                    (zi-rule
                     rule-name: 'Swiss
                     rule-from: 1941
                     rule-to: 1942
                     rule-in: 5
                     rule-on: `(> ,mon 1)
                     rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'wall)
                     rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                     rule-letters: "S"))
              (call-with-input-string big-sample
                parse-zic-file)))


(test-group "zone-format"

            (test-equal "Zone format with argument"
              "CEST" (zone-format "CE%sT" "S" (timespec (time))))
            (test-equal "Zone format with empty"
              "CET"  (zone-format "CE%sT" ""  (timespec (time))))

            ;; TODO zone-format %z is not yet implemented, and therefore untested

            ;; TODO this error message is currently translatable...
            (test-equal "Invalid format specifier"
              '(misc-error "zone-format" ; "Invalid format char ~s in ~s at position ~a" (#\S "%S" 1) #f
                           )
              (catch 'misc-error (lambda () (zone-format "%S" "A" (timespec (time))))
                (lambda (err fmt . rest)
                  (list err fmt)))))

(test-group "Actual object"
            ;; NOTE this doesn't test read-zoneinfos ability to
            ;; - take filenames
            ;; - take multiple items
            (let ((zoneinfo (call-with-input-string big-sample
                              (compose intermediary->zoneinfo read-zoneinfo))))
              (test-assert "get-zone returns a zone-entry object"
                (every zone-entry? (get-zone zoneinfo "Europe/Zurich")))
              (test-equal "A link resolves to the same object as its target"
                (get-zone zoneinfo "Europe/Zurich") (get-zone zoneinfo "Europe/Vaduz"))
              (test-equal "Get rules returns correctly, and in order"
                  ;; Rules are sorted
                (list (zi-rule
                       rule-name: 'Swiss
                       rule-from: 1941
                       rule-to: 1942
                       rule-in: 5
                       rule-on: `(> ,mon 1)
                       rule-at: (timespec (time hour: 01 minute: 00 second: 00) '+ 'wall)
                       rule-save: (timespec (time hour: 01 minute: 00 second: 00) '+ 'daylight)
                       rule-letters: "S")
                      (zi-rule
                       rule-name: 'Swiss
                       rule-from: 1941
                       rule-to: 1942
                       rule-in: 10
                       rule-on: `(> ,mon 1)
                       rule-at: (timespec (time hour: 02 minute: 00 second: 00) '+ 'wall)
                       rule-save: (timespec (time hour: 00 minute: 00 second: 00) '+ 'standard)
                       rule-letters: ""))
               (get-rule zoneinfo 'Swiss))))



(lambda ()
  (define intermediary
   (call-with-input-string "
Link Greenwich G_M_T
Link Etc/GMT Greenwich
Zone Etc/GMT 0 - GMT
" read-zoneinfo))

  (let ((root-link (zone-link name: (symbol->string (gensym))
                              target: "G_M_T")))
    (test-equal "Link trace"
      (list root-link
            (zone-link name: "G_M_T" target: "Greenwich")
            (zone-link name: "Greenwich" target: "Etc/GMT")
            (cons "Etc/GMT"
                  (zone-entry stdoff: (timespec (time))
                              rule: (timespec (time) '+ 'standard)
                              format: "GMT"
                              until: #f)))
      ((@@ (vcomponent zic) resolve-link) intermediary root-link))))

'((datetime zoneinfo)
  (datetime zoneinfo types)
  (datetime zoneinfo intermediary)
  (datetime zoneinfo zic)
  )
