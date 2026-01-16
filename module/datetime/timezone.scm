(define-module (datetime timezone)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (ice-9 regex)
  :use-module (datetime core)
  :use-module (datetime timespec)
  :use-module ((datetime zoneinfo)
               :select (
                        zi-rule?
                        rule-from
                        rule-to
                        rule-in
                        rule-on
                        rule-at
                        rule-save
                        rule-letters

                        zone-entry
                        zone-entry-stdoff
                        zone-entry-rule
                        zone-entry-format
                        zone-entry-until

                        get-rule
                        get-zone

                        execute-day-spec
                        zone-format
                                ))
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :export (zoneinfo
           utc->zone
           zone->utc
           zone->zone

           query-timezone
           datetime+/zoneinfo
           datetime-/zoneinfo
           datetime-difference/zoneinfo
           ))



;;; TODO instances where we move from one advanced zone rule to
;;; another advanced rule break (within a timezone, has nothing to do with
;;; multiple timezones).
;;; For example, the following crashes
;;;     $ ./calp tz convert -f America/New_York 1946-01-01T00:00
;;; Solution is to extend generate-backwards to work with multiple
;;; zone entries at once, picking appropriate rules as needed.

;;; TODO zic files go hard on having standard or wall time. Part of the reason is to differentiate between multiple instances of the same timestamp (which happens when we go from daylights saving time to standard time)

(define zoneinfo
  (make-parameter
   (@ (datetime timezone vendored-tzdb)
      zoneinfo-database)))



;;; Returns the datetime a given rule would take effect the given year.
;;; No check is done that the rule actually applies for the given year,
;;; (e.g. a rule only relevant between 1990 and 1999 can be applied to
;;; the year 2015).


(define (rule->datetime year rule)
  (typecheck year exact-integer?)
  (typecheck rule zi-rule?)

  ;; TODO type of time
  ;; Timespec allows 'utc, 'daylight, 'wall, and 'utc
  ;; it semes that rule-on doesn't allow 'daylight for those
  (datetime-timespec-add
   (datetime date: (execute-day-spec (date year: year month: (rule-in rule))
                                     (rule-on rule)))
   (rule-at rule)))





;;; Find relevent timezone rule instances for the date dt
(define (find-relevant-rule-instances dt rules)
  (typecheck dt datetime?)
  (typecheck rules (list-of zi-rule?))
  ;; TODO if changeover happens at midnight between two years,
  ;; this may be incorrect.
  (define y (year (datetime-date dt)))
  (filter (lambda (rule)
            (case (rule-to rule)
              ((only) (= y (rule-from rule)))
              ((maximum) (<= (rule-from rule) y))
              (else (<= (rule-from rule) y (rule-to rule)))))
          rules))

;; For all rules which MAY be of interest to the given datetime,
;; expand those, and return a STREAM of all changeover dates together
;; with the rule, from youngest to oldest.
;; For example, Given 2026-01-03, and the US rules, then the following
;; two rules would be found as "relevant":
;;      Rule	US	2007	max	-	Mar	Sun>=8	2:00	1:00	D
;;      Rule	US	2007	max	-	Nov	Sun>=1	2:00	0	S
;; The returned STREAM would then start with (2026-11-01T02:00, S),
;; (2026-03-08T02:00, D), (2025-11-02T02:00, S), (2025-03-09T02:00, D), ...
;; (Where `S' denotes the "standard" rule, and `D' the "daylight savings" rule).
;; 
;; Note that the datetimes returned are zoneless, and MUST be
;; interpreted through the rule, making them UTC, WALL, or STANDARD
;; times.
(define (find-changeovers dt rules)

  ;; For a given year and zoneinfo rule,
  ;; return a list of all instances of that rule between the given date
  ;; and the first instance of the rule, in reverse chronological order
  ;; (e.g. newest first).
  ;; If no such instances exists, then the empty list is returned.
  ;; Each element in the stream consists of a pair consisting of:
  ;; - a datetime without zoneinfo, which MUST be interpreted
  ;;   according to `(timespec-type (rule-at rule)), which denotes when
  ;;   this rule takes effect that year.
  ;; - the rule which takes effect.
  (define (generate-backwards year rule)
    (typecheck year exact-integer?)
    (typecheck rule zi-rule?)

    (if (<= (rule-from rule) year (case (rule-to rule)
                                   ((only) (rule-from rule))
                                   ;; float inf works for integers to
                                   ((maximum) (inf))
                                   (else (rule-to rule))))
        (map (lambda (y) (cons (rule->datetime y rule) rule))
             (iota (1+ (- year (rule-from rule))) year -1))
        '()))

  ;; TODO Implement an `interleave-lists`, and use that instead of streams
  ;; Streams where initially used here since a rule can technically
  ;; expand to a ridicolous amount of items, if given a date far enough in
  ;; the future.
  (interleave-streams
   (lambda (a b)
     ;; TODO This assumes that no two changes happened very
     ;; close to each other, using different time tracking
     ;; systems (wall, utc,standard). That is PROBABLY a safe
     ;; assumption.
     (datetime>= (car a) (car b)))
   (map list->stream
        (map (lambda (rule) (generate-backwards (year (datetime-date dt)) rule))
             (find-relevant-rule-instances
              dt rules)))))


(define (get-other x)
  (if (= 1 (hour (timespec-time x)))
      (set x (lens-compose timespec-time* hour*) 0)
      (set x (lens-compose timespec-time* hour*) 1)))

;; Find the revelent zoneinfo rule for the given datetime.
;; The datetime can be in either UTC or a known timezone
;;
;; Find first rule instance which is is the PAST.
;; This is guaranteed to work, since no two rules will ever overlap,
;; and they have already been expanded. See example in
;; documentation for find-changeovers.
(define (find-exact-changeover dt zone changeovers)
  (let loop ((rules (stream->list changeovers)))

    (define rule-matches?
      (cond ((null? rules)
             (scm-error 'misc-error #f
                        "No rule was relevant. Try previous zone entry"
                        '() #f))
            (else
             (let ((changeover-dt rule (car+cdr (car rules))))
               (case (timespec-type (rule-at rule))

                 ((utc)
                  (datetime<=
                   (tz changeover-dt "UTC")
                   (if (utc-datetime? dt)
                       dt
                       ;; This is zone->utc/simple
                       (datetime-timespec-add
                        (tz dt "UTC")
                        (timespec-negate
                         (timespec+ (zone-entry-stdoff zone)
                                    ;; NOTE removing this line seems to make
                                    ;; NO difference. Find the cases where it does
                                    (rule-save rule)))))))

                 ((wall)
                  (if (utc-datetime? dt)
                      (datetime<=
                       (datetime-timespec-add
                        (tz changeover-dt "UTC")
                        (timespec-negate
                         ;; TODO (cdr rules) may fail. In that case we
                         ;; need to check the previous zone entry.
                         (timespec+ (zone-entry-stdoff zone)
                                    (rule-save (cdadr rules)))))
                       dt)

                      ;; Both are in wall time, strip
                      ;; the information to appease datetime<=
                      (datetime<= changeover-dt (tz dt #f))))

                 ((standard)
                  ;; Same reasoning as for wall, except we ignore the
                  ;; offset added by the savings rule.
                  (if (utc-datetime? dt)
                      (datetime<=
                       (datetime-timespec-add
                        (tz changeover-dt "UTC")
                        (timespec-negate (zone-entry-stdoff zone)))
                       dt)
                      (datetime<= changeover-dt (tz dt #f))))

                 (else (scm-error 'misc-error #f
                                  "Unexpected timespec type in rule-at: ~s"
                                  (list (timespec-type (rule-at rule)))
                                  #f)))))))

    (if rule-matches?
        (cdar rules)
        (loop (cdr rules)))))


;;; Get the abreviation of a zone, with regards to a specific rule.
;;; - entry-format is the format field of a zone rule, meaning that it
;;;   should either contain a single fixed string, a string containing
;;;   optianal `%s' and `%z' placeholders, or a pair of the
;;;   afformentioned strings split by a solidus character.
;;; - rule is the relevent rule to format for
;;; - offset is the timezone offset used if the %z format specifier is used.
;;;   (TODO doesn't this depend both on the base offset and the rule?)
;;;   TODO write tests for it
;;; TODO write tests for this
(define (run-zone-format entry-format rule offset)
  (zone-format
   (cond ((string-contains entry-format "/")
          => (lambda (idx)
               (case (timespec-type (rule-save rule))
                 ((standard) (substring entry-format 0 idx))
                 ((daylight) (substring entry-format (1+ idx)))
                 (else (scm-error 'misc-error "run-zone-format"
                                  "Unknown timespec type for rule save: ~s"
                                  (list rule) #f)))))
         (else entry-format))
   (rule-letters rule)
   offset))

;; Find relevant rule for converting given UTC time to desired timezone
;; What can be relevant to return?
;; Input:
;; dt :: UTC datetime, it's tz component will be ignored
;; zone-name :: Name of the database, for example "Europe/Stockholm"
;;              Note that most "short" names (such as "CEST") aren't
;;              available in most zoneinfo databases
;; Return:
;; - datetime moved to specified zone
;; - name of the zone
;; - UTC offset
(define (utc->zone/name dt zone-name)
  (typecheck dt utc-datetime?)
  (typecheck zone-name string?)

  (define zone-entry
    (find (lambda (zone)
            (let ((until (zone-entry-until zone)))
             (cond ((not until) zone)
                   ((datetime<
                     dt
                     (case (car until)
                       ((utc) (tz (cdr until) "UTC"))
                       ((wall)
                        ;; TODO
                        ;; (get-rule (zoneinfo) (zone-entry-rule zone))
                        (tz (cdr until) "UTC"))
                       ((standard)
                        (-> (cdr until)
                            (datetime-timespec-add
                             (timespec-negate (zone-entry-stdoff zone)))
                            (tz "UTC")))
                       (else (scm-error 'misc-error "utc->zone/name"
                                        "Bad value for zone-entry-until: ~s"
                                        (list (car (zone-entry-until zone)))
                                        (list zone)))))
                    zone)
                   (else #f))))
          (get-zone (zoneinfo) zone-name)))

  (cond ((not zone-entry)
         (scm-error 'misc-error "utc->zone"
                    "Failed finding any relevant offset"
                    '() #f))

        ((timespec? (zone-entry-rule zone-entry))
         (let ((offset (timespec+ (zone-entry-rule zone-entry)
                                  (zone-entry-stdoff zone-entry))))
           (values (-> dt
                       (datetime-timespec-add offset)
                       (tz zone-name))
                   offset
                   (zone-entry-format zone-entry))))

        (else ; symbolic rule name
         (define changeovers
           (find-changeovers
            dt (get-rule (zoneinfo) (zone-entry-rule zone-entry))))

         (define rule (find-exact-changeover dt zone-entry changeovers))

         (let ((offset (timespec+ (zone-entry-stdoff zone-entry)
                                  (rule-save rule))))
           (values (-> dt
                       (datetime-timespec-add offset)
                       (tz zone-name))
                   offset
                   (run-zone-format (zone-entry-format zone-entry)
                                    rule offset))))))


;; See utc->zone
;; Difference here is that `dt` is wall time in the specified zone
;; The returned offset is still in the "regular" direction, meaning that
;; (returned dt) + (returned offset) == input dt
(define (zone->utc/name dt)
  (typecheck dt datetime?)
  (typecheck (tz dt) (not false?))

  (define zone-entry
    (find (lambda (zone)
            (let ((until (zone-entry-until zone)))
             (cond ((not until) zone)
                   ((datetime<=
                     (tz dt #f)
                     (case (car until)
                       ((utc)
                        ;; TODO
                        (cdr until))
                       ((wall) (cdr until))
                       ((standard)
                        ;; TODO
                        (cdr until))
                       (else (scm-error 'misc-error "zone->utc/name"
                                        "Bad value for zone-entry-until: ~s"
                                        (list (car (zone-entry-until zone)))
                                        (list zone)))))
                    zone)
                   (else #f))))
          (get-zone (zoneinfo) (tz dt))))

  (cond ((not zone-entry)
         (scm-error 'misc-error "zone->utc"
                    "Failed finding any relevant offset"
                    '() #f))

        ((timespec? (zone-entry-rule zone-entry))
         (let ((offset (timespec+ (zone-entry-rule zone-entry)
                                  (zone-entry-stdoff zone-entry))))
           (values (-> dt
                       (datetime-timespec-add (timespec-negate offset))
                       (tz "UTC"))
                   offset
                   (zone-entry-format zone-entry))))

        (else ; symbolic rule name
         (define changeovers
           (find-changeovers
            dt (get-rule (zoneinfo) (zone-entry-rule zone-entry))))

         (define rule (find-exact-changeover dt zone-entry changeovers))

         (let ((offset (timespec+ (zone-entry-stdoff zone-entry)
                                  (rule-save rule))))
           (values (-> dt
                       (datetime-timespec-add (timespec-negate offset))
                       (tz "UTC"))
                   offset
                   (run-zone-format (zone-entry-format zone-entry)
                                    rule offset))))))


;; Parses a UTC offest specifier string inte a timespec value.
;; For exampleo, "UTC-2" or "UTC+01:30". Values after the ± are
;; treated as hour offsets up to (and including) the value of 99, after
;; which they become hours and minutes (meaning that UTC+0100 == UTC+1).
;; The "UTC" part is optional
(define utc-offset-rx
  (make-regexp "^(UTC)?([+-])(([0-9]{1,2}):([0-9]{2})|[0-9]+)$"))

;;; TODO rename to utc-indicator->timespec (or similar)
(define (parse-utc-offset s)
  (and=> (regexp-exec utc-offset-rx  s)
         (lambda (m)
           (timespec
            (if (match:substring m 4)
                (time hour: (string->number (match:substring m 4))
                      minute: (string->number (match:substring m 5)))
                (let ((s (match:substring m 3)))
                  (cond ((string->number s)
                         (lambda (x) (< x 100))
                         => (lambda (x) (time hour: x)))
                        ((= 4 (string-length s))
                         (time hour: (string->number (substring s 0 2))
                               minute: (string->number (substring s 2 4))))
                        (else (scm-error 'misc-error "parse-utc-offset"
                                         "Invalid UTC offset: ~s"
                                         (list s)
                                         #f)))))
            (string->symbol (match:substring m 2))
            #f))))


(define (utc->zone dt identifier)
  (typecheck (tz dt) (and string? (string= "UTC")))
  (cond ((parse-utc-offset identifier)
         => (lambda (offset)
              (define name (string-append "UTC" (timespec->string offset)))
              (values (-> (datetime-timespec-add dt offset)
                          (tz name))
                      offset
                      name)))
        (else (utc->zone/name dt identifier))))

(define (zone->utc dt)
  (typecheck (tz dt) string?)
  (cond ((parse-utc-offset (tz dt))
         => (lambda (offset)
              (values (-> (datetime-timespec-add dt (timespec-negate offset))
                          (tz "UTC"))
                      offset
                      (string-append "UTC" (timespec->string offset)))))
        (else (zone->utc/name dt))))

(define (zone->zone dt identifier)
  (typecheck (tz dt) string?)
  (let ((utc-dt ((unval zone->utc) dt)))
    ((unval utc->zone) utc-dt identifier)))

;;; Retrieve UTC offset, and pretty name from a given timezone
(define (query-timezone dt)
  (cond ((parse-utc-offset (tz dt))
         => (lambda (offset) (values offset (string-append "UTC" (timespec->string offset)))))
        ;; NOTE this is a ridiculous way to query the data.
        ;; Write an actually query procedure
        (else (let ((_ offset name (utc->zone ((unval zone->utc) dt) (tz dt))))
                (values offset name)))))


;;; Start re-implementation of basic operations, now timezone aware

;;; we have the two types *datetime* and *datetime-difference*, which
;;; we both encode as the same actual type.
;;; datetime-difference objects are always assumed to have their tz field.
;;; datetime± keeps the zone of the input

(define (datetime±/zoneinfo datetime± dt dt-difference)
  (cond ((tz dt)
         => (lambda (zone)
              (let ((utc-dt ((unval zone->utc) dt)))
                ((unval utc->zone)
                 (datetime± utc-dt dt-difference)
                 zone))))
        (else (datetime± dt dt-difference))))

(define (datetime+/zoneinfo dt dt-difference)
  (datetime±/zoneinfo datetime+ dt dt-difference))

(define (datetime-/zoneinfo dt dt-difference)
  (datetime±/zoneinfo datetime- dt dt-difference))

(define (datetime-difference/zoneinfo end start)
  (cond ((and (tz start) (tz end))
         (datetime-difference
          ((unval zone->utc) end)
          ((unval zone->utc) start)))
        ((not (or (tz start) (tz end)))
         (datetime-difference end start))
        (else
         (scm-error 'misc-error "datetime-difference/zoneinfo"
                    "Can't compare datetimes where only one has a timezone"
                    '() #f))))

;;; TODO comperators (datetime<, ...)
