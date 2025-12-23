(define-module (test datetime-io)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime core)
  :use-module (datetime io)
  :use-module ((ice-9 i18n) :select (make-locale))
  :use-module ((guile) :select (LC_CTYPE LC_TIME)))

;;; Skipped since the code generating the (expected) error is disabled, due to
;;; optional fields at the end of string. See the (null? str) case is
;;; datetime->string
(test-expect-fail "Premature end of string to parse")


;;; Global locale objects, to save all tests from creating them
(define en_US (make-locale (list LC_CTYPE LC_TIME) "en_US.UTF-8"))
(define sv_SE (make-locale (list LC_CTYPE LC_TIME) "sv_SE.UTF-8"))

;; Before the general parser, since it's a dependency string->datetime.
(test-group "Parse Month"

  (test-equal "Parse full month name" jan (parse-month "January" en_US))
  (test-equal "Parse full weird case" jan (parse-month "jaNuaRy" en_US))
  (test-equal "Parse partial month name" jan (parse-month "Jan" en_US))
  (test-equal "Failing parse of month name" -1 (parse-month "Unknown" en_US))
  (test-equal "Overlap gives earliest month" mar (parse-month "m" en_US))

  (test-equal "Parse month with different locale" may (parse-month "maj" sv_SE)))


(test-group "Parser"
  (test-group "Simple individual rules"
    (test-group "Year"
      (test-equal "~Y year"  (datetime year: 2020)  (string->datetime "2020" "~Y"))
      (test-equal "~Y year single digit"  (datetime year: 2)  (string->datetime "2" "~Y"))
      (test-equal "~Y year leading zero"  (datetime year: 2)  (string->datetime "02" "~Y"))
      (test-error "~Y parses at max four digits" 'misc-error (string->datetime "14411" "~Y")))

    (test-group "Month"
      (test-equal "~m month"  (datetime month: 10)  (string->datetime "10" "~m"))
      (test-equal "~m month single digit"  (datetime month: 1)  (string->datetime "1" "~m"))
      (test-equal "~m month leading zero"  (datetime month: 1)  (string->datetime "01" "~m"))
      (test-error "~m parses at max two digits" 'misc-error (string->datetime "111" "~m")))

    ;; Extra tests are skipped for these, since they are shared with Month
    (test-equal "~d day"    (datetime day: 20)    (string->datetime "20" "~d"))
    (test-equal "~H hour"   (datetime hour: 15)   (string->datetime "15" "~H"))
    (test-equal "~M minute" (datetime minute: 30) (string->datetime "30" "~M"))
    (test-equal "~S second" (datetime second: 59) (string->datetime "59" "~S")))


  (test-equal "Literal character" (datetime) (string->datetime "T" "T"))
  (test-equal "~~ '~'" (datetime) (string->datetime "~" "~~"))
  (test-error "Mismatched literal ~" 'misc-error (string->datetime "A" "~~"))

  (test-error "Stray ~ at end of fmt" 'misc-error (string->datetime "~" "~"))
  (test-error "Stray ~ in middle of fmt" 'misc-error (string->datetime "~ 1" "~ ~d"))
  (test-error "Unknown escape" 'misc-error (string->datetime "10" "~x"))
  (test-error "Premature end of string to parse" 'misc-error (string->datetime "" "~Y"))
  (test-error "Wrong Literal character" 'misc-error (string->datetime "T" "Z"))


  ;; Does the parser continue correctly
  (test-group "Tokens following each other"
    (test-equal "Year indirectly followed by month"
      (datetime year: 2020 month: 1)
      (string->datetime "2020-01" "~Y-~m"))
    ;; Does the parser handle tokens without delimiters, instead going by their max size
    (test-equal "Year directly follewed by month"
      (datetime year: 2020 month: 1)
      (string->datetime "202001" "~Y~m")))


  (test-group "Timezone"
    (test-equal "~Z 'Z'"
      (datetime tz: "UTC") (string->datetime "Z" "~Z"))
    (test-equal "~Z Is optional"
      (datetime) (string->datetime "" "~Z"))
    (test-equal "~Z Is optional with stuff after"
      (datetime hour: 20) (string->datetime "20" "~Z~H"))
    ;; This was earlier a bug
    (test-equal "Zoneinfo is kept while not at end"
      (datetime year: 2020 tz: "UTC")
      (string->datetime "Z2020" "~Z~Y")))


  (test-group "Month by name"
    ;; ~b, ~B, and ~h all does the same thing, and exists for symmetry with
    ;; datetime->string (where they don't do the exact same thing). Each is used
    ;; at least once below to ensure that they all work.
    (test-equal "Standalone month, and at end"
      (datetime month: 1)
      (string->datetime "Jan" "~b" en_US))

    ;; Separate test from above, since month does the check itself
    (test-error "Stray ~ after month"
      'misc-error (string->datetime "Jan" "~b~" en_US))

    (test-equal "Month with explicit ~ after"
      (datetime month: mar)
      (string->datetime "M~" "~B~~" en_US))

    (test-error "Month with other specifier directly after"
      'misc-error (string->datetime "January" "~b~b"))

    (test-equal "Month with other explict char after"
      (datetime month: mar)
      (string->datetime "Mar|" "~h|" en_US))

    (test-equal "Locale information is used"
      (datetime month: may)
      (string->datetime "Maj" "~h" sv_SE)))

  (test-group "AM/PM"
    (test-equal "AM (and no periods)"
      (time hour: 10)
      (string->time "10 AM" "~H ~p"))
    (test-equal "PM (and periods)"
      (time hour: 22)
      (string->time "10 p.m." "~H ~p"))
    (test-group "Period after AM/PM"
      (call-with-values
          (lambda ()
            (string->time "Meeting at 12 pm." "Meeting at ~H ~p."
                          return-trailing: #t))
        (lambda (dt trailing)
          (test-equal "Trailing period"
            (time hour: 12) dt)
          (test-equal "No remaining items"
            '() trailing))))
    (test-equal "12 am is midnight"
      (time hour: 0)
      (string->time "12 AM" "~H ~p"))
    )

  (test-group "Complete parses"
    (test-equal "Parse complete ISO date"
      (datetime year: 2020 month: 3 day: 10)
      (string->datetime "2020-03-10" "~Y-~m-~d"))

    (test-equal "Parse complete ISO time"
      (datetime hour: 10 minute: 20 second: 30)
      (string->datetime "10:20:30" "~H:~M:~S"))

    (test-equal "Parse complete ISO date-time"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30)
      (string->datetime "2020-03-10T10:20:30"
                        "~Y-~m-~dT~H:~M:~S")))

  (test-group "string->datetime default format-specifier"
    (test-equal "Default date-time format-specifier takes ISO date-times"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30)
      (string->datetime "2020-03-10T10:20:30"))

    (test-equal "Default date-time format-specifier takes ISO date-times (with zone)"
      (datetime year: 2020 month: 3 day: 10
                hour: 10 minute: 20 second: 30
                tz: "UTC")
      (string->datetime "2020-03-10T10:20:30Z")))


  (test-group "string->time"
    (test-assert "String->time returns time objects"
      (time? (string->time "10" "~H")))

    (test-equal "String->time complete parse"
      (time hour: 10 minute: 20 second: 30)
      (string->time "10:20:30" "~H:~M:~S"))

    (test-equal "String->time complete parse, default format-specifier"
      (time hour: 10 minute: 20 second: 30)
      (string->time "10:20:30")))

  (test-group "string->date"
    (test-assert "String->date returns time objects"
      (date? (string->date "10" "~Y")))

    (test-equal "String->date complete parse"
      (date year: 2020 month: 3 day: 10)
      (string->date "2020-03-10" "~Y-~m-~d"))

    (test-equal "String->date complete parse, default format-specifier"
      (date year: 2020 month: 3 day: 10)
      (string->date "2020-03-10")))

  (test-group "Pre-specified parsers"
    (test-group "ICS (RFC 5545)"
      (test-equal "date"
        (date year: 2020 month: 10 day: 20)
        (parse-ics-date "20201020"))
      (test-equal "time"
        (time hour: 10 minute: 20 second: 30)
        (parse-ics-time "102030")))

    (test-group "ISO"
      (test-equal "date"
        (date year: 2020 month: 10 day: 20)
        (parse-iso-date "2020-10-20"))
      (test-equal "time"
        (time hour: 10 minute: 20 second: 30)
        (parse-iso-time "10:20:30"))
      (test-equal "datetime"
        (datetime year: 2020 month: 10 day: 20
                  hour: 10 minute: 20 second: 30)
        (parse-iso-datetime "2020-10-20T10:20:30")))

    ;; Parse freeform date
    )
)



(test-group "To string"

  (test-group "Week day name"
    (test-equal "Simple" "Saturday" (week-day-name sat locale: en_US))
    (test-equal "Truncated" "Sa" (week-day-name sat 2 locale: en_US))
    (test-equal "Other locale" "lördag" (week-day-name sat locale: sv_SE))
    (test-equal "Other locale, truncated" "lö" (week-day-name sat 2 locale: sv_SE)))

  (test-group "Datetime->string"
    (test-equal "A letter becomes itself"
      "H" (datetime->string (datetime) "H"))
    (test-group "Single rules"
      (test-equal "~" (datetime->string (datetime) "~~"))
      (test-equal "01" (datetime->string (datetime hour: 1)   "~H"))
      (test-equal " 1" (datetime->string (datetime hour: 1)   "~k"))
      (test-equal "02" (datetime->string (datetime minute: 2) "~M"))
      (test-equal "03" (datetime->string (datetime second: 3) "~S"))
      (test-equal "0002" (datetime->string (datetime year: 2) "~Y"))
      (test-equal "02" (datetime->string (datetime month: 2)  "~m"))
      (test-equal "04" (datetime->string (datetime day: 4)    "~d"))
      (test-equal " 4" (datetime->string (datetime day: 4)    "~e"))
      (test-equal "1600000000" (datetime->string (datetime year: 2020 month: 09 day: 13 hour: 12 minute: 26 second: 40 tz: "UTC") "~s"))

      (test-equal "2022-10-20" (datetime->string (datetime date: (date year: 2022 month: 10 day: 20)) "~1"))
      (test-equal "10:20:30"   (datetime->string (datetime time: (time hour: 10 minute: 20 second: 30))   "~3"))

      (test-group "Locale dependant (en_US)"
        (test-equal "January"  (datetime->string (datetime date: (date month: 1)) "~B" en_US))
        (test-equal "Jan"      (datetime->string (datetime date: (date month: 1)) "~b" en_US)))

      (test-group "Locale dependant (sv_SE)"
        (test-equal "januari" (datetime->string (datetime date: (date month: 1)) "~B" sv_SE))
        (test-equal "jan"     (datetime->string (datetime date: (date month: 1)) "~b" sv_SE)))

      (test-group "Timezone"
        (test-equal "Z" (datetime->string (datetime tz: "UTC")           "~Z"))
        (test-equal ""  (datetime->string (datetime tz: #f)              "~Z"))
        (test-equal ""  (datetime->string (datetime tz: "Anything else") "~Z"))))


    (test-equal "Default fomat specifier gives ISO-formatted date"
      "2006-01-02T15:04:05" (datetime->string (datetime year: 2006 month: 01 day: 02 hour: 15 minute: 04 second: 05)))

    (test-group "Invalid specifiers"
      (test-equal "" (datetime->string (datetime) "~x" allow-unknown?: #t))
      (test-error 'misc-error (datetime->string (datetime) "~x")))

    (test-group "Print syntax for datatypes"
      (test-equal "Date writer" "#2020-01-02" (with-output-to-string (lambda () (write (date year: 2020 month: 01 day: 02)))))
      (test-equal "Time writer" "#20:30:40"   (with-output-to-string (lambda () (write (time hour: 20 minute: 30 second: 40)))))
      (test-equal "Datetime writer"           "#2020-01-02T20:30:40"  (with-output-to-string (lambda () (write (datetime year: 2020 month: 01 day: 02 hour: 20 minute: 30 second: 40)))))
      (test-equal "Datetime writer (with tz)" "#2020-01-02T20:30:40Z" (with-output-to-string (lambda () (write (datetime year: 2020 month: 01 day: 02 hour: 20 minute: 30 second: 40 tz: "UTC")))))))

  ;; Really basic tests, since these are rather thin wrappers
  (test-equal "date->string" "0000-00-00" (date->string (date)))
  (test-equal "time->string" "00:00:00"   (time->string (time))))


'((datetime io))
