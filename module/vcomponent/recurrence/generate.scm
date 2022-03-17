(define-module (vcomponent recurrence generate)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (vcomponent base)
  :use-module (vcomponent recurrence internal)
  :use-module (vcomponent recurrence parse)

  :use-module (datetime)
  :use-module (ice-9 curried-definitions) )






;; a → b → (a . b)
(define ((con x) y)
  (cons x y))

(eval-when (expand load compile eval)
 (define (stx->bystx symbol str-transformer)
   (->> symbol
        symbol->string
        str-transformer
        (string-append (str-transformer "by"))
        string->symbol))

 (define (by-proc symbol)
   (stx->bystx symbol string-downcase))

 (define (by-symb symbol)
   (stx->bystx symbol string-upcase))

 (use-modules (ice-9 match))

 (define (make-extender-or-limiter extender? rr cases)
   `(case (freq ,rr)
      ,@(map (match-lambda
               [(key '|| cc ...)
                `((,key)
                  (filter
                   identity
                   (list
                    ,@(map (label self
                            (match-lambda
                              [('unless pred field)
                               `(let ((yearday (,(by-proc 'yearday) ,rr))
                                      (monthday (,(by-proc 'monthday) ,rr))
                                      (weekno (,(by-proc 'weekno) ,rr))
                                      (month (,(by-proc 'month) ,rr))
                                      )
                                  (if ,pred #f
                                      ,(self field)))]
                              [field
                               `(and=> (,(by-proc field) ,rr)
                                       ,(if extender?
                                            `(cut map (con (quote ,(by-symb field)))
                                                  <>)
                                            `(con (quote ,(by-symb field)))))]))
                           cc))))])
             cases)))

 (define-macro (make-extenders rr . cases)
   `(apply cross-product ,(make-extender-or-limiter #t rr cases)))

 (define-macro (make-limiters rr . cases)
   (make-extender-or-limiter #f rr cases)))



;; rrule → (list extension-rule)
(define (all-extenders rrule)
  (make-extenders
   rrule
   [YEARLY  || month weekno yearday monthday

            ;; see Note 2, p. 44
            (unless (or yearday monthday
                        ;; weekno and month are still expanders. They however
                        ;; cause day to be omited here to prevent datetimes
                        ;; from being generated from both directions.
                        ;; They are instead handled under BYWEEKNO & BYMONTH
                        ;; respectively.
                        weekno month) day)
               hour minute second]
   [MONTHLY || monthday (unless monthday day) hour minute second]
   [WEEKLY  || day hour minute second]
   [DAILY   || hour minute second]
   [HOURLY  || minute second]
   [MINUTELY || second]
   [SECONDLY || #| null |#]))

(define (all-limiters rrule)
  (make-limiters
   rrule
   [YEARLY   || day #| setpos |#]
   [MONTHLY  || month day #|setpos|#]
   [WEEKLY   || month #|setpos|#]
   [DAILY    || month monthday day #|setpos|#]
   [HOURLY   || month yearday monthday day hour #|setpos|#]
   [MINUTELY || month yearday monthday day hour minute #|setpos|#]
   [SECONDLY || month yearday monthday day hour minute second #|setpos|#]
   ))

;; next, done
;; (a, a → values a), a, (list a) → values a
(define (branching-fold proc init collection)
  (if (null? collection)
      init
      (call-with-values
          (lambda () (proc (car collection) init))
        (lambda vv
          (apply values
                 (concatenate
                  (map (lambda (v)
                         (call-with-values
                             (lambda () (branching-fold proc v (cdr collection)))
                           list))
                       vv)))))))

;; (a := (date|datetime)), rrule, extension-rule → a
(define (update date-object rrule extension-rule)
  ;; Branching fold instead of regular fold since BYDAY
  ;; can extend the recurrence set in weird ways.
  (branching-fold
   (lambda (rule dt)
     (let* (((key . value) rule)
            (d (if (date? dt) dt (get-date dt)))
            ;; NOTE It's proably an error to give BYHOUR, BYMINUTE, and BYSECOND
            ;; rules for a date object. This doesn't warn if those are given, but
            ;; instead silently discards them.
            (t (as-time dt))
            (to-dt (lambda (o)
                     (if (date? dt)
                         (if (date? o) o d)
                         (if (date? o)
                             (datetime date: o time: t tz: (get-timezone dt))
                             (datetime date: d time: o tz: (get-timezone dt)))))))
       (case key
         [(BYMONTH)
          (if (and (eq? 'YEARLY (freq rrule))
                   (byday rrule)
                   (not (or (byyearday rrule)
                            (bymonthday rrule))))
              (valued-map
               to-dt
               (concatenate
                (map (lambda (wday)
                       (all-wday-in-month
                        wday (start-of-month (set (month d) value))))
                     (map cdr (byday rrule)))))

              ;; else
              (to-dt (set (month d) value)))]

         [(BYDAY)
          (let* (((offset . value) value))
            (case (freq rrule)
              [(WEEKLY)
               ;; set day to that day in the week which d lies within
               (to-dt (date+ (start-of-week d (wkst rrule))
                             (date day: (modulo (- value (wkst rrule))
                                                7))))]

              [(MONTHLY)
               (let* ((instances (all-wday-in-month value (start-of-month d))))
                 (catch 'out-of-range
                   (lambda ()
                     (cond [(eqv? #f offset)
                            ;; every of that day in this month
                            (valued-map to-dt instances)]

                           [(positive? offset)
                            (to-dt (list-ref instances (1- offset)))]

                           [(negative? offset)
                            (to-dt (list-ref (reverse instances)
                                             (1- (- offset))))]))

                   (lambda (err proc fmt args  . rest)
                     (warning "BYDAY out of range for MONTHLY.
 Possibly stuck in infinite loop")
                     dt)))]

              [(YEARLY)
               (let ((instances (all-wday-in-year
                                 value (start-of-year d))))
                 (to-dt
                  (if (positive? offset)
                      (list-ref instances (1- offset))
                      (list-ref (reverse instances) (1- (- offset))))))
               ]))]

         [(BYWEEKNO)
          (let ((start-of-week (date-starting-week value d (wkst rrule))))
            (if (and (eq? 'YEARLY (freq rrule))
                     (byday rrule))
                (stream->values
                 (stream-map to-dt
                  (stream-filter
                   (lambda (d) (memv (week-day d) (map cdr (byday rrule))))
                   (stream-take 7 (day-stream start-of-week)))))

                ;; else
                (to-dt start-of-week)))]

         [(BYYEARDAY) (to-dt (date+ (start-of-year d)
                                    (date day: (1- value))))]
         [(BYMONTHDAY)
          (to-dt (set (day d)
                      (if (positive? value)
                          value (+ 1 value (days-in-month d)))))]
         [(BYHOUR) (to-dt (set (hour t) value))]
         [(BYMINUTE) (to-dt (set (minute t) value))]
         [(BYSECOND) (to-dt (set (second t) value))]
         [else (scm-error 'wrong-type-arg "update"
                          "Unrecognized by-extender ~s"
                          key #f)])))
   date-object
   extension-rule))


;; (or 'YEARLY 'MONTHLY 'WEEKLY 'HOURLY 'MINUTELY 'SECONDLY)
;; → <datetime>
(define (make-date-increment rr)
  (case (freq rr)
    [(YEARLY)   (datetime date: (date year: (interval rr)))]
    [(MONTHLY)  (datetime date: (date month: (interval rr)))]
    [(WEEKLY)   (datetime date: (date day: (* 7 (interval rr))))]
    [(DAILY)    (datetime date: (date day: (interval rr)))]
    [(HOURLY)   (datetime time: (time hour: (interval rr)))]
    [(MINUTELY) (datetime time: (time minute: (interval rr)))]
    [(SECONDLY) (datetime time: (time second: (interval rr)))]
    [else (error "Bad freq")]))

;; NOTE
;; [3.8.5.3. description]
;; The initial DTSTART SHOULD be synchronized with the RRULE.
;; An unsynchronized DTSTART/RRULE results in an undefined recurrence set.

;; rrule, (a := date|datetime) → (stream a)
(define-stream (extend-recurrence-set rrule base-date)
  (stream-append
   ;; If no BY-rules are present just add the base-date to the set.
   ;; NOTE a possible alternative version (which would probably be better)
   ;; would be to always add the base-date to the set, and make sure that the
   ;; updated date ≠ base-date
   ;; A third alternative would be to add a by rule for the current type. for example:
   ;; FREQ=MONTHLY => BYMONTHDAY=(day base-date)
   (if (null? (all-extenders rrule))
       (stream base-date)
       (list->stream
        (sort*
         (concatenate
          (map (lambda (ext)
                 (call-with-values (lambda () (update base-date rrule ext))
                   list))
               (all-extenders rrule)))
         (if (date? base-date) date< datetime<))))
   (extend-recurrence-set
    rrule
    (if (date? base-date)
        (date+ base-date (get-date (make-date-increment rrule)))
        (datetime+ base-date (make-date-increment rrule))))))

(define ((month-mod d) value)
  (if (positive? value)
      value (+ value 1 (days-in-month d))))

;; returns a function which takes a datetime and is true
;; if the datetime is part of the reccurrence set, and
;; false otherwise.
;; 
;; limiters → (a → bool)
(define (limiters->predicate limiters)
  (lambda (dt)
   (let loop ((remaining limiters))
     (if (null? remaining)
         #t
         (let* (((key . values) (car remaining))
                (t (as-time dt))
                (d (if (date? dt) dt (get-date dt))))
           (and (case key
                  [(BYMONTH) (memv (month d) values)]
                  [(BYMONTHDAY) (memv (day d) (map (month-mod d) values))]
                  [(BYYEARDAY) (memv (year-day d) values)]
                  [(BYDAY) (memv (week-day d) (map cdr values))]
                  [(BYHOUR) (memv (hour t) values)]
                  [(BYMINUTE) (memv (minute t) values)]
                  [(BYSECOND) (memv (second t) values)]
                  ;; [(BYSETPOS)]
                  [else
                   (error "Unknown by-limiter")])
                (loop (cdr remaining))))))))


(define-stream (limit-recurrence-set rrule date-stream)
  (stream-filter
   ;; filter inlavid datetimes (feb 30, times droped due to zone-shift, ...)
   (lambda (dt)
     (let ((d (as-date dt))
           (t (as-time dt)))
       (and (<= 0 (hour t) 23)
            (<= 0 (minute t) 59)
            (<= 0 (second t) 60)
            (<= 1 (month d) 12)
            (<= 1 (day d) (days-in-month d)))))
   (stream-filter
    (limiters->predicate (all-limiters rrule))
    date-stream)))

;; (a := <date|datetime>) => <rrule>, a → (stream a)
(define-stream (generate-posibilities rrule start-date)
  (limit-recurrence-set
   rrule
   (extend-recurrence-set
    rrule start-date)))


;; Recurring <vcomponent> → (stream <date|datetime>)
(define-stream (rrule-instances event)
  (define rrule (prop event 'RRULE))

  ;; 3.8.5.1 exdate are evaluated AFTER rrule (and rdate)
  (let ((date-stream (stream-remove
                      (aif (prop* event 'EXDATE)
                           (cut member <> (map value it))
                           (const #f))
                      ;; Some expanders can produce dates before our start time.
                      ;; For example FREQ=WEEKLY;BYDAY=MO where DTSTART is
                      ;; anything after monday. This filters these out.
                      (stream-drop-while
                       (lambda (d) (date/-time< d (prop event 'DTSTART)))
                       (if rrule
                           (generate-posibilities rrule (prop event 'DTSTART))
                           stream-null))
                      ;; TODO ideally I should merge the limited recurrence set
                      ;; with the list of rdates here. However, I have never
                      ;; sen an event with an RDATE property, so I wont worry
                      ;; about it for now.
                      ;; (stream-merge (list->stream (#|rdate's|#))
                      )))
    ;; TODO count and until shoud be applied to the RRULE events,
    ;; but not the RDATE events ???
    ;; (TODO test against some other calendar program)
    (cond [(count rrule) => (lambda (c) (stream-take c date-stream))]
          [(until rrule) => (lambda (end) (stream-take-while
                                      (cut (if (date? (prop event 'DTSTART))
                                               date<= datetime<=) <> end)
                                      date-stream))]
          [else date-stream])))

(export rrule-instances)


(define-public (final-event-occurence event)
  (define rrule (prop event 'RRULE))

  (if (or (count rrule) (until rrule))
      (let ((instances (rrule-instances event)))
        (stream-ref instances (1- (stream-length instances))))
      #f))


;; <vevent> -> (stream <vevent>)
(define-public (generate-recurrence-set base-event)

  (define duration
    ;; NOTE DTEND is an optional field.
    (let ((end (prop base-event 'DTEND)))
      (if end
          (if (date? end)
              (date-difference end (prop base-event 'DTSTART))
              (datetime-difference end (prop base-event 'DTSTART)))
          #f)))



  (define rrule-stream-regular
    (if (prop base-event 'RRULE)
        (rrule-instances base-event)
        stream-null))

  (define alternative-times
    (awhen (prop base-event '-X-HNH-ALTERNATIVES)
           (list (list->stream
                  (sort*
                   (hash-map->list (lambda (_ v) (prop v 'DTSTART)) it)
                   date/-time<?)))))

  (define rrule-stream
    ;; TODO remove duplicates
    (interleave-streams
     date/-time<?
     (cons rrule-stream-regular
           alternative-times)))

  (stream-map
   (aif (prop base-event '-X-HNH-ALTERNATIVES)
        (lambda (dt)
          (aif (hash-ref it dt)
               it ; RECURRENCE-ID objects come with their own DTEND
               (let ((ev (copy-vcomponent base-event)))
                 (set! (prop ev 'DTSTART) dt)
                 (when duration ; (and (not (prop ev 'DTEND)) duration)
                   ;; p. 123 (3.8.5.3 Recurrence Rule)
                   ;; specifies that the DTEND should be updated to match how the
                   ;; initial dtend related to the initial DTSTART. It also notes
                   ;; that an event of 1 day in length might be longer or shorter
                   ;; than 24h depending on timezone shifts.
                   (set! (prop ev 'DTEND)
                     (cond [(date? dt)
                            (unless (date? duration)
                              (warning "Expected date, got ~a" duration))
                            (date+ dt (as-date duration))]
                           [(datetime? dt)
                            (unless (datetime? duration)
                              (warning "Expected datetime, got ~a" duration))
                            (datetime+ dt (as-datetime duration)) ]
                           [else (error "Bad type")])))
                 ev)))
        (lambda (dt)
          (let ((ev (copy-vcomponent base-event)))
            (set! (prop ev 'DTSTART) dt)
            (when duration
              (set! (prop ev 'DTEND) ((cond [(date? dt) date+]
                                            [(datetime? dt) datetime+]
                                            [else (error "Bad type")])
                                      dt duration)))
            ev)))
   rrule-stream))

