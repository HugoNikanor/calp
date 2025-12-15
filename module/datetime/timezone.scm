(define-module (datetime timezone)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (datetime)
  :use-module ((datetime zic)
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
  :use-module (datetime timespec)
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


;;; TODO document me
(define (rule->datetime year rule)
  (typecheck year exact-integer?)
  (typecheck rule zi-rule?)

  (datetime-timespec-add
   (datetime date: (execute-day-spec (date year: year month: (rule-in rule))
                                     (rule-on rule)))
   (rule-at rule)))

;;; TODO document me
(define (generate-backwards year rule)
  (typecheck year exact-integer?)
  (typecheck rule zi-rule?)

  (case (rule-to rule)
    ((only)
     (if (= year (rule-from rule))
         (stream (cons (rule->datetime year rule)
                       rule))
         stream-null))
    ((maximum)
     (cond ((< (rule-from rule) year) (stream-cons
                                       (cons (rule->datetime year rule)
                                             rule)
                                       (generate-backwards (1- year) rule)))
           ((= (rule-from rule) year) (stream (cons (rule->datetime year rule)
                                                    rule)))
           (else stream-null)))
    (else
     (cond ((= (rule-from rule) year) (stream (cons (rule->datetime year rule)
                                                    rule)))
           ((<= (rule-from rule) year (rule-to rule))
            (stream-cons (cons (rule->datetime year rule)
                               rule)
                         (generate-backwards (1- year) rule)))
           (else stream-null)))))





(define (find-relevant-rule-instances dt rules)
  ;; TODO if changeover happens at midnight between two years,
  ;; this may be incorrect.
  (define y (year (datetime-date dt)))
  (filter (lambda (rule)
            (case (rule-to rule)
              ((only) (= y (rule-from rule)))
              ((maximum) (<= (rule-from rule) y))
              (else (<= (rule-from rule) y (rule-to rule)))))
          rules))

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
  (typecheck dt datetime?)
  (typecheck zone-name string?)

  (define zone-entry
    (find (lambda (zone)
            (cond ((not (zone-entry-until zone)) zone)
                  ;; TODO UNTIL is only *usually* in wall time
                  ((datetime< dt (datetime-timespec-add
                                  (zone-entry-until zone)
                                  (timespec-negate
                                   ;; timespec+
                                   (zone-entry-stdoff zone)
                                   ;; TODO this is USUALLY a symbol
                                   ;; referencing a zone, not a literal offset!
                                   ; (zone-entry-rule zone)
                                   )))
                   zone)
                  (else #f)))
          (get-zone (zoneinfo) zone-name)))


  (cond ((not zone-entry)
         (scm-error 'misc-error "utc->zone"
                    "Failed finding any relevant offset"
                    '() #f))

        ((timespec? (zone-entry-rule zone-entry))
         (let ((offset (timespec+ (zone-entry-rule zone-entry)
                                  (zone-entry-stdoff zone-entry))))
           (values (-> (datetime-timespec-add dt offset)
                       (tz zone-name))
                   offset
                   (zone-entry-format zone-entry))))

        (else ; symbolic rule name
         (define y (year (datetime-date dt)))
         (define changeovers
           (interleave-streams
            (lambda (a b) (datetime>= (car a) (car b)))
            (map (lambda (rule) (generate-backwards y rule))
                 (find-relevant-rule-instances
                  dt (get-rule (zoneinfo) (zone-entry-rule zone-entry))))))

         ;; Past changeovers is a stream of pairs, where each value is:
         ;; a datetime of "unspecified" format, meaning that it may be in UTC, or may be in local time. How it should be interpreted depends on the rule in the cdr.
         ;; This could in theory cause an issue if way to many changes happens at once, but that *shouldn't* happen


         ;; Find the first changeover in the past relative dt
         (define rule
           (cdar
            (stream-find
             (lambda (p)
               (match p
                 (((tm . rule) . (_ . rule-prev))
                  (case (timespec-type (rule-at rule))
                    ((utc) ; tm is in utc format, we can compare directly
                     (datetime>= dt tm))
                    ((wall)
                     ;; tm is in wall time, convert to utc and compare
                     ;; This requires looking at the previous rule to figure out local time
                     ;; TODO is this correct, or might we overflow back to being invalid?
                     ;; UTC time + offset ⇒ local time
                     (datetime>=
                      (datetime-timespec-add
                       dt
                       (timespec+ (zone-entry-stdoff zone-entry)
                                  (rule-save rule-prev)))
                      tm))
                    ((standard)
                     ;; time is in standard offset with regards to stdoff of the zone
                     ;; Convert time to utc, and compare.
                     (datetime>=
                      (datetime-timespec-add dt (zone-entry-stdoff zone-entry))
                      tm))
                    (else (scm-error 'misc-error "utc->zone"
                                     "Unexpected timespec type in rule-at: ~s"
                                     (list (timespec-type (rule-at rule)))
                                     #f))))))
             (stream-map
              cons
              changeovers
              (stream-append (stream-cdr changeovers)
                             ;; TODO here we need to fall back to previous zone entries
                             ;; (which might contain rules themselves)
                             (stream (cons #f #f)))))))

         (let ((offset (timespec+ (zone-entry-stdoff zone-entry)
                                  (rule-save rule))))
           (values (-> (datetime-timespec-add dt offset)
                       (tz zone-name))
                   offset
                   (run-zone-format (zone-entry-format zone-entry)
                                    rule offset))))))


;; See utc->zone
;; Difference here is that `dt` is wall time in the specified zone
;; The returned offset is still in the "regular" direction, meaning that
;; (returned dt) + (returned offset) == input dt
(define (zone->utc/name dt)
  (define zone
    (find (lambda (zone)
            (cond ((not (zone-entry-until zone)) zone)
                  ;; TODO UNTIL is only *usually* in wall time
                  ((datetime<= dt (zone-entry-until zone)) zone)
                  (else #f)))
          (get-zone (zoneinfo) (tz dt))))

  (cond ((not zone)
         (scm-error 'misc-error "utc->zone"
                    "Failed finding any relevant offset"
                    '() #f))

        ((timespec? (zone-entry-rule zone))
         (let ((offset (timespec+ (zone-entry-rule zone)
                                  (zone-entry-stdoff zone))))
           (values (-> dt
                       (datetime-timespec-add (timespec-negate offset))
                       (tz "UTC"))
                   offset
                   (zone-entry-format zone))))

        (else
         ;; Identical with flipflop
         (define y (year (datetime-date dt)))
         (define changeovers
           (interleave-streams
            (lambda (a b) (datetime>= (car a) (car b)))
            (map (lambda (rule) (generate-backwards y rule))
                 (find-relevant-rule-instances
                  dt (get-rule (zoneinfo) (zone-entry-rule zone))))))


         ;; Find the first changeover in the past relative dt
         (define rule
           (cdar
            (stream-find
             (lambda (p)
               (match p
                 (((tm . rule) . (_ . rule-prev))
                  (case (timespec-type (rule-at rule))
                    ((utc)
                     (datetime>= (datetime-timespec-add
                                 dt (timespec-negate
                                     (timespec+ (zone-entry-stdoff zone)
                                                (rule-save rule-prev))))
                                tm))
                    ((wall)
                     ;; Changeover is in wall time, we cane compare directly
                     (datetime>= dt tm))
                    ((standard)
                     ;; Changeover is in local time, without any daylight savings.
                     (datetime>= (datetime-timespec-add
                                 dt (timespec-negate
                                     (timespec+ (zone-entry-stdoff zone)
                                                (rule-save rule-prev))))
                                tm))
                    (else (scm-error 'misc-error "utc->zone"
                                     "Unexpected timespec type in rule-at: ~s"
                                     (list (timespec-type (rule-at rule)))
                                     #f))))))
             (stream-map
              cons
              changeovers
              (stream-append (stream-cdr changeovers)
                             ;; TODO here we need to fall back to previous zone entries
                             ;; (which might contain rules themselves)
                             (stream (cons #f #f)))))))

         (let ((offset (timespec+ (zone-entry-stdoff zone)
                                  (rule-save rule))))
           (values (-> dt
                       (datetime-timespec-add (timespec-negate offset))
                       (tz "UTC"))
                   offset
                   (run-zone-format (zone-entry-format zone)
                                    rule offset))))))


;; Parses a UTC offest specifier string inte a timespec value.
;; For exampleo, "UTC-2" or "UTC+01:30". Values after the ± are
;; treated as hour offsets up to (and including) the value of 99, after
;; which they become hours and minutes (meaning that UTC+0100 == UTC+1).
;; The "UTC" part is optional
(define utc-offset-rx
  (make-regexp "^(UTC)?([+-])(([0-9]{1,2}):([0-9]{2})|[0-9]+)$"))

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
        (else (datetime- dt dt-difference))))

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

;;; TODO extend output format to include zoneinfo
;; (define (showoff)
;;   (define start-time (-> #2025-03-30T01:59:55
;;                          (tz "Europe/Stockholm")))
;;   (let loop ((dt start-time)
;;              (i 0))
;;     (if (> i 10)
;;         'done
;;         (begin
;;           (format #t "~a~%" (datetime->string dt))
;;           (loop (datetime+ dt (datetime second: 1))
;;                 (1+ i)))))
;;   (newline)
;;   (let loop ((dt start-time)
;;              (i 0))
;;     (if (> i 10)
;;         'done
;;         (begin
;;           (format #t "~a~%" (datetime->string dt))
;;           (loop (datetime+/zoneinfo dt (datetime second: 1))
;;                 (1+ i))))) )
