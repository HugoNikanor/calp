(define-module (datetime timezone)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 regex)
  :use-module (datetime core)
  :use-module (datetime arithmetic)
  :use-module (datetime duration)
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

                        cached-zone-expansions
                        ))
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (hnh util destructure)
  :export (zoneinfo
           utc->zone utc->zone1
           zone->utc zone->utc1
           zone->zone

           find-rule
           datetime+/zoneinfo
           datetime-/zoneinfo
           datetime-difference/zoneinfo

           datetime=/zoneinfo
           datetime</zoneinfo
           datetime>/zoneinfo
           datetime<=/zoneinfo
           datetime>=/zoneinfo

           ensure-zoned-datetime

           expand-zone

           expanded-rule expanded-rule?
           expanded-start-wall   expanded-start-wall*
           expanded-start-utc    expanded-start-utc*
           expanded-save-type    expanded-save-type*
           expanded-utc-offset   expanded-utc-offset*
           expanded-base-name    expanded-base-name*
           expanded-zone-letters expanded-zone-letters*
           expanded-from         expanded-from*

           expanded-rule-printf
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




;; (define (datetime-max/naive a b)
;;   (if (datetime</naive a b) a b))

;; (define (datetime-min/naive a b)
;;   (if (datetime</naive a b) b a))


(define (->utc dt stdoff walloff)
  (typecheck dt (pair-of (memv '(standard utc wall))
                         unzoned-datetime?))
  (typecheck stdoff rational?)
  (typecheck walloff rational?)
  (case (car dt)
    ((utc) (cdr dt))
    ((wall) (datetime-/naive (cdr dt) (seconds->duration walloff)))
    ((standard) (datetime-/naive (cdr dt) (seconds->duration stdoff)))))

(define (->wall dt stdoff walloff)
  (typecheck dt (pair-of (memv '(standard utc wall))
                         unzoned-datetime?))
  (typecheck stdoff rational?)
  (typecheck walloff rational?)
  (case (car dt)
    ((utc) (datetime+/naive (cdr dt) (seconds->duration walloff)))
    ((wall) (cdr dt))
    ((standard) (datetime+/naive (cdr dt) (seconds->duration (- walloff stdoff))))))


(define-type (partially-expanded)
  (partial-at keyword: at type: (pair-of (memv '(utc wall standard))
                                         unzoned-datetime?))
  (partial-save keyword: save type: (pair-of (memv '(standard daylight))
                                             rational?))
  (partial-letters keyword: letters type: string?)
  (partial-identifier keyword: identifier
                      ;; (pair-of (typeof rule-from) (typeof rule-to))
                      type: (pair-of integer? (or integer? (memv '(only maximum)))))
  )

;;; End time is gotten from start time of next entry in stream.
;;; If no more entries exists, then this is assumed to be the final
;;; entry in the stream.
(define-type (expanded-rule)
  (expanded-start-wall keyword: wall type: unzoned-datetime?)
  (expanded-start-utc  keyword: utc  type: utc-datetime?)

  (expanded-save-type keyword: type type: (memv '(standard daylight)))

  (expanded-utc-offset keyword: offset type: rational?)
  (expanded-base-name  keyword: name type: string?)
  (expanded-zone-letters keyword: letters type: string? default: "")

  ;; "Opaque" indicator of what this rule was expanded from.
  ;; Will usually be some variant of the UNTIL field of the initial
  ;; zone-entry.
  ;; TODO rename to `expanded-source-zone`
  (expanded-from keyword: from)

  ;; TODO populate this with the concatenation of:
  ;; - the rule name
  ;; - the rule from
  ;; - the rule to
  ;; - (rule at if the above doesn't uniquely identify the rule)
  ;; OR
  ;; - the direct rule
  ;; TODO update the above TODO to a NOTE once implemented
  (expanded-source-rule keyword: rule)
  )


;;; For indirect rules, two extra cases exists:
;;; - We enter a rule before it starts.
;;;   Then we generate a "virtual" rule from when we entered the rule, until the first instance of the rule.
;;; - we stay in a rule after the last entry ended.
;;;   we extend it until we leave the rule.


;;; Given the name of a zoneinfo rule, generate the complete stream of all its instances as partially expanded rule objects. These can't be fully expanded without the context of the zone entry they are used within.
;;; These will be sorted by date, assuming that the stream is
;;; trivially sortable. See internal comment.
;;; From-year gives a start year to generate from.
(define* (rule-expansion zoneinfo name optional: from-year)
  ;; TODO the resulting stream is a candidate for caching, since the
  ;; same rule may be used by many zones (for example, most European
  ;; countries follow the EU rule).
  (typecheck name symbol?)

  ;; Given a single zoneinfo rule instance, generates a stream of all its instances
  (define (rule-instance->stream rule)
    (typecheck rule zi-rule?)

    (define year-stream
      (case (rule-to rule)
        ((only)    (stream (rule-from rule)))
        ((maximum) (stream-from (rule-from rule)))
        (else (stream-range (rule-from rule) (1+ (rule-to rule))))))

    (stream-map (lambda (y)
                  (partially-expanded
                   at: (cons (car (rule-at rule))
                             (datetime+/naive
                              (datetime
                               date: (execute-day-spec
                                      (date year: y month: (rule-in rule))
                                      (rule-on rule)))
                              (seconds->duration (cdr (rule-at rule)))))
                   save: (rule-save rule)
                   letters: (rule-letters rule)
                   identifier: (cons (rule-from rule) (rule-to rule))))
                year-stream))

  ;; NOTE this assumes that the `at` times for each changeover are
  ;; directly comparable. This isn't guaranteed to work, since if two
  ;; changeovers happens very close to each other, and use different time
  ;; types (utc, wall, standard), then this might be wrong.
  (interleave-streams
   (lambda (a b) (datetime</naive (cdr (partial-at a))
                             (cdr (partial-at b))))
   (map rule-instance->stream
        (if from-year
            ;; Filter out all rules which *definitely* ended in the past
            (filter (lambda (rule)
                      (case (rule-to rule)
                        ((only) (= from-year (rule-from rule)))
                        ((maximum) #t)
                        (else (<= (rule-from rule) from-year (rule-to rule)))))
                    (get-rule zoneinfo name))
            (get-rule zoneinfo name)))))



;;; INTERNAL
(define* (direct-rule-forever key: zone-entry zone-entry-start)
  ;; (typecheck zone-entry zone-entry?)
  ;; (typecheck zone-entry-start (pair-of (memv '(standard utc wall))
  ;;                                      unzoned-datetime?))
  (destructure zone-entry
    ((zone-entry rule: (@ rule (cons save-type rule-stdoff))
                 stdoff: stdoff format: base-name)
     (let ((utc-offset (+ stdoff rule-stdoff)))
       (stream (expanded-rule wall: (->wall zone-entry-start stdoff utc-offset)
                              utc:  (tz (->utc zone-entry-start stdoff utc-offset) "UTC")
                              type: save-type
                              offset: utc-offset
                              name: base-name
                              letters: ""
                              from: 'final
                              rule: rule))))))


;;; INTERNAL
(define* (direct-rule-until
          key: zone-entry zone-entry-start loop)
  ;; (typecheck zone-entry zone-entry?)
  ;; (typecheck zone-entry-start (pair-of (memv '(standard utc wall))
  ;;                                      unzoned-datetime?))
  (destructure zone-entry
    ((zone-entry until: (@ until (cons _ until-dt))
                 rule: (@ rule (cons save-type rule-stdoff))
                 stdoff: stdoff
                 format: base-name)
     (let ((utc-offset (+ stdoff rule-stdoff)))
       (stream-cons
        (expanded-rule
         wall: (->wall zone-entry-start utc-offset utc-offset)
         utc:  (tz (->utc zone-entry-start utc-offset utc-offset) "UTC")
         type: save-type offset: utc-offset
         name: base-name letters: ""
         from: until-dt rule: rule)
        (loop until utc-offset))))))


;;; INTERNAL
(define* (indirect-rule-forever
          key: zoneinfo zone-entry zone-entry-start prev-offset)
  ;; (typecheck zone-entry zone-entry?)
  ;; (typecheck zone-entry-start (pair-of (memv '(standard utc wall))
  ;;                                      unzoned-datetime?))
  ;; (typecheck prev-offset rational?)

  (destructure zone-entry
    ((zone-entry rule: rule-name
                 format: base-name
                 stdoff: stdoff)
     (let ((partials (rule-expansion zoneinfo rule-name (year (datetime-date (cdr zone-entry-start))))))
       (cond
        ;; No more partial rules, assume time ended
        ((stream-null? partials)
         (stream))
        ;; first partial rule starts the future. Create a virtual entry to align us
        ((datetime</naive (->utc zone-entry-start stdoff prev-offset)
                          (->utc (partial-at (stream-car partials))
                                 stdoff
                                 (cdr (partial-save (stream-car partials)))))
         (stream-cons
          ;; virtual-rule
          ;; TODO shouldn't this use prev-offset?
          (expanded-rule wall: (->wall zone-entry-start stdoff stdoff)
                         utc: (tz (->utc zone-entry-start stdoff stdoff) "UTC")
                         type: 'standard offset: stdoff
                         name: base-name letters: ""
                         from: 'final-virtual
                         rule: (destructure (stream-car partials)
                                 ((partially-expanded identifier: (cons a b))
                                  (format #f "~a ~a-~a" rule-name a b))))
          (indirect-rule-forever
           zoneinfo: zoneinfo
           zone-entry: zone-entry
           zone-entry-start: (partial-at (stream-car partials))
           prev-offset: stdoff)))

        ;; TODO case where rule ended in the past?

        (else
         (let inner ((partials partials)
                     (current-offset prev-offset))
           (if (stream-null? partials)
               (stream)
               (let* ((partial (stream-car partials))
                      (expanded
                       (expanded-rule
                        wall: (->wall (partial-at partial)
                                      stdoff current-offset)
                        utc: (tz (->utc (partial-at partial)
                                        stdoff current-offset)
                                 "UTC")
                        type: (car (partial-save partial))
                        offset: (+ stdoff (cdr (partial-save partial)))
                        name: base-name letters: (partial-letters partial)
                        from: 'final
                        rule: (destructure partial
                                ((partially-expanded identifier: (cons a b))
                                 (format #f "~a ~a-~a" rule-name a b))))))
                 ;; TODO truncate?
                 (stream-cons expanded
                              (inner (stream-cdr partials)
                                     (expanded-utc-offset expanded))))))))))))



;;; INTERNAL
(define* (indirect-rule-until
          key: zoneinfo zone-entry zone-entry-start prev-offset loop)
  ;; (typecheck zone-entry zone-entry?)
  ;; (typecheck zone-entry-start (pair-of (memv '(standard utc wall))
  ;;                                      unzoned-datetime?))
  ;; (typecheck prev-offset rational?)

  (destructure zone-entry
    ((zone-entry until: (@ zone-entry-until (cons _ until-dt))
                 rule: rule-name
                 format: base-name
                 stdoff: stdoff)
     (let ((partials (rule-expansion zoneinfo rule-name (year (datetime-date (cdr zone-entry-start))))))
       (cond
        ;; No more partial rules, continue on to next zone entry
        ((stream-null? partials)
         (loop zone-entry-until prev-offset))

        ;; first partial rule starts in the future. Create a virtual entry to align us
        ((datetime</naive (->utc zone-entry-start stdoff prev-offset)
                          (->utc (partial-at (stream-car partials))
                                 stdoff
                                 (cdr (partial-save (stream-car partials)))))
         (stream-cons
          ;; Virtual rule
          ;; TODO shouldn't this use prev-offset?
          (expanded-rule wall: (->wall zone-entry-start stdoff stdoff)
                         utc: (tz (->utc zone-entry-start stdoff stdoff) "UTC")
                         type: 'standard offset: stdoff name: base-name
                         letters: "" from: (format #f "~a (virtual)" until-dt)
                         rule: (destructure (stream-car partials)
                                 ((partially-expanded identifier: (cons a b))
                                  (format #f "~a ~a-~a" rule-name a b))))
          (indirect-rule-until
           zoneinfo: zoneinfo
           zone-entry: zone-entry
           zone-entry-start: (partial-at (stream-car partials))
           prev-offset: stdoff
           loop: loop)))

        ;; TODO case where rule ended in the past?

        (else
         ;; (define zone-start-utc (->utc zone-entry-start stdoff prev-offset))
         (let inner ((partials partials)
                     (current-offset prev-offset))

           ;; Needs recalculating each iteration, since if the until date is in wall time,
           ;; then the end will be in different UTC times.
           (define until-utc (->utc zone-entry-until stdoff current-offset))
           (if (stream-null? partials)
               (loop zone-entry-until prev-offset)
               (let* ((partial (stream-car partials))
                      (expanded
                       (expanded-rule
                        wall: (->wall (partial-at partial)
                                      stdoff current-offset)
                        utc: (tz (->utc (partial-at partial)
                                        stdoff current-offset)
                                 "UTC")
                        type: (car (partial-save partial))
                        offset: (+ stdoff (cdr (partial-save partial)))
                        name: base-name
                        letters: (partial-letters partial)
                        from: until-dt
                        rule: (destructure partial
                                ((partially-expanded identifier: (cons a b))
                                 (format #f "~a ~a-~a" rule-name a b))))))
                 ;; Rule starts after our zone ends:
                 ;; - leave rule, go to the next zone entry
                 (if (datetime</naive until-utc (expanded-start-utc expanded))
                     (loop (cons 'utc until-utc)
                           (expanded-utc-offset expanded))
                     ;; let ((truncated
                     ;;       (-> expanded
                     ;;           ;; This truncates the rule start to our zone start, if it happened to be earlier.
                     ;;           (modify expanded-start-utc*  (lambda (dt) (tz (datetime-max/naive dt zone-start-utc) "UTC")))
                     ;;           (modify expanded-start-wall*
                     ;;                   ;; TODO utc->wall
                     ;;                   ;; TODO invalid comparison
                     ;;                   (lambda (dt) (datetime-max/naive dt zone-start-utc))))))
                     (stream-cons expanded
                                  (inner (stream-cdr partials)
                                         (expanded-utc-offset expanded)))
                     ))))))))))


;;; INTERNAL
(define* (expand-zone-entry key: zoneinfo zone-entry zone-entry-start prev-offset loop)
  ;; (typecheck zone-entry zone-entry?)
  ;; (typecheck zone-entry-start (pair-of (memv '(standard utc wall))
  ;;                                      unzoned-datetime?))
  ;; (typecheck prev-offset rational?)
  (destructure zone-entry
    ((zone-entry until: #f rule: (cons _ _))
     (direct-rule-forever
      zone-entry-start: zone-entry-start
      zone-entry: zone-entry))

    ((zone-entry until: #f)
     (indirect-rule-forever
      zoneinfo: zoneinfo
      zone-entry: zone-entry
      zone-entry-start: zone-entry-start
      prev-offset: prev-offset))

    ((zone-entry rule: (cons _ _))
     (direct-rule-until
      zone-entry: zone-entry
      zone-entry-start: zone-entry-start
      loop: loop ))

    ((zone-entry)
     (indirect-rule-until
      zoneinfo: zoneinfo
      zone-entry: zone-entry
      zone-entry-start: zone-entry-start
      prev-offset: prev-offset
      loop: loop))))


;;; Given the name of a timezone (e.g. Europe/Stockholm), produce a
;;; list of exact changeover times
(define (expand-zone/uncached zoneinfo zone-name)
  (let loop ((zone-entries (get-zone zoneinfo zone-name))
             (zone-entry-start (cons 'utc (datetime month: 1 day: 1)))
             (prev-offset 0))
    (expand-zone-entry
     zoneinfo: zoneinfo
     zone-entry: (car zone-entries)
     zone-entry-start: zone-entry-start
     prev-offset: prev-offset
     loop: (lambda args (apply loop (cdr zone-entries) args)))))


(define (expand-zone zoneinfo zone-name)
  (or (hash-ref (cached-zone-expansions zoneinfo) zone-name)
      (let ((strm (expand-zone/uncached zoneinfo zone-name)))
        (hash-set! (cached-zone-expansions zoneinfo)
                   zone-name
                   strm)
        strm)))

;;; Quick and dirty comparizon of timezone lookup before and after cache was populated.
;; ,time (utc->zone (current-datetime) "America/New_York")
;; $14 = #.(tz #2026-03-13T14:41:20 "America/New_York")
;; ;; 0.179028s real time, 0.303324s run time.  0.146252s spent in GC.

;; ,time (utc->zone (current-datetime) "America/New_York")
;; $15 = #.(tz #2026-03-13T14:41:24 "America/New_York")
;; ;; 0.002760s real time, 0.002743s run time.  0.000000s spent in GC.


(define (find-rule zone-name dt field)
 (define strm (expand-zone (zoneinfo) zone-name))
 (let loop ((last-rule (stream-car strm))
            (rules (stream-cdr strm)))
   (if (or (stream-null? rules)
           (datetime</naive dt (field (stream-car rules))))
       last-rule
       (loop (stream-car rules) (stream-cdr rules)))))

(define (utc->zone/name dt zone)
  (define rule (find-rule zone dt expanded-start-utc))
  (values (-> dt
              (datetime+/naive (seconds->duration (expanded-utc-offset rule)))
              (tz zone))
          rule))

(define (zone->utc/name dt)
  (define rule (find-rule (tz dt) dt expanded-start-wall))
  (values (-> dt
              (datetime-/naive (seconds->duration (expanded-utc-offset rule)))
              (tz "UTC"))
          rule))




;; Parses a UTC offest specifier string inte a numeric offset
;; For example, "UTC-2" or "UTC+01:30". Values after the ± are
;; treated as hour offsets up to (and including) the value of 99, after
;; which they become hours and minutes (meaning that UTC+0100 == UTC+1).
;; The "UTC" part is optional
(define utc-offset-rx
  (make-regexp "^(UTC)?([+-])(([0-9]{1,2}):([0-9]{2})|[0-9]+)$"))

(define (parse-utc-offset s)
  (and=> (regexp-exec utc-offset-rx  s)
         (lambda (m)
           (* (if (string=? "-" (match:substring m 2))
                  -1 1)
              (time->seconds
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
                                            #f))))))))))

(define (utc->zone dt identifier)
  (typecheck dt utc-datetime?)
  (typecheck identifier string?)
  (cond ((equal? "UTC" identifier)
         (values dt (expanded-rule wall: (datetime) utc: (datetime tz: "UTC")
                                   type: 'standard offset: 0 name: "UTC")))
        ((parse-utc-offset identifier)
         => (lambda (offset)
              (values (-> (datetime+/naive dt (seconds->duration offset))
                          (tz (zone-format "UTC%z" "" offset)))
                      (expanded-rule wall: (datetime) utc: (datetime tz: "UTC")
                                     type: 'standard name: "UTC%z" offset: offset))))
        (else (utc->zone/name dt identifier))))


(define (zone->utc dt)
  (typecheck dt zoned-datetime?)
  (cond ((equal? "UTC" (tz dt))
         (values dt (expanded-rule wall: (datetime) utc: (datetime tz: "UTC")
                                   type: 'standard offset: 0 name: "UTC")))
        ((parse-utc-offset (tz dt))
         => (lambda (offset)
              (values (-> (datetime-/naive dt (seconds->duration offset))
                          (tz (zone-format "UTC%z" "" offset)))
                      (expanded-rule wall: (datetime) utc: (datetime tz: "UTC")
                                     type: 'standard offset: offset name: "UTC%z"))))
        (else (zone->utc/name dt))))

(define zone->utc1 (unval zone->utc))
(define utc->zone1 (unval utc->zone))

(define (zone->zone dt identifier)
  (typecheck (tz dt) string?)
  (-> dt
      zone->utc1
      (utc->zone identifier)))



;;; Start re-implementation of basic operations, now timezone aware

;;; we have the two types *datetime* and *datetime-difference*, which
;;; we both encode as the same actual type.
;;; datetime-difference objects are always assumed to have their tz field.
;;; datetime± keeps the zone of the input

;; (define (datetime±/zoneinfo datetime± dt dt-difference)
;;   (cond ((tz dt)
;;          => (lambda (zone)
;;               (let ((utc-dt ((unval zone->utc) dt)))
;;                 ((unval utc->zone)
;;                  (datetime± utc-dt dt-difference)
;;                  zone))))
;;         (else (datetime± dt dt-difference))))

;; (define (datetime+/zoneinfo dt dt-difference)
;;   (datetime±/zoneinfo datetime+/naive dt dt-difference))

;; (define (datetime-/zoneinfo dt dt-difference)
;;   (datetime±/zoneinfo datetime-/naive dt dt-difference))

(define (datetime+/zoneinfo dt dt-difference)
  (-> (modify dt date* (lambda (d) (date+ d dt-difference)))
      zone->utc1
      (add-time-duration dt-difference)
      (utc->zone1 (tz dt))))

(define (date-/zoneinfo dt dt-difference)
  (-> (modify dt date* (lambda (d) (date- d dt-difference)))
      zone->utc1
      (remove-time-duration dt-difference)
      (utc->zone1 (tz dt))))

(define (datetime-difference/zoneinfo end start)
  (cond ((and (tz start) (tz end))
         (datetime-difference/naive
          ((unval zone->utc) end)
          ((unval zone->utc) start)))
        ((not (or (tz start) (tz end)))
         (datetime-difference/naive end start))
        (else
         (scm-error 'misc-error "datetime-difference/zoneinfo"
                    "Can't compare datetimes where only one has a timezone, got start: ~s, end: ~s"
                    (list start end) #f))))


(define (datetime=/zoneinfo  . args) (apply datetime=/naive  (map (unval zone->utc) args)))
(define (datetime</zoneinfo  . args) (apply datetime</naive  (map (unval zone->utc) args)))
(define (datetime>/zoneinfo  . args) (apply datetime>/naive  (map (unval zone->utc) args)))
(define (datetime<=/zoneinfo . args) (apply datetime<=/naive (map (unval zone->utc) args)))
(define (datetime>=/zoneinfo . args) (apply datetime>=/naive (map (unval zone->utc) args)))

(define (ensure-zoned-datetime reference-zone s)
  (cond ((date? s) (datetime date: s tz: reference-zone))
        ((unzoned-datetime? s) (tz s reference-zone))
        ;; guaranteed zoned datetime
        ((datetime? s) s)
        (else (scm-error 'type-error "ensure-zoned-datetime"
                         "Expected date or datetime, got: ~s"
                         (list s) #f))))

(define (expanded-rule-printf expanded-rule)
  ((@ (datetime zoneinfo) zone-format)
   (expanded-base-name    expanded-rule)
   (expanded-zone-letters expanded-rule)
   (expanded-utc-offset   expanded-rule)
   (expanded-save-type    expanded-rule)))
