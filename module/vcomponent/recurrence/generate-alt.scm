(define-module (vcomponent recurrence generate-alt)
  :export (generate-recurrence-set)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (vcomponent base)
  :use-module (vcomponent recurrence internal)
  :use-module (vcomponent recurrence parse)

  :use-module (datetime)
  :use-module (datetime util)
  :use-module (ice-9 curried-definitions) )






(eval-when (expand)
 (define (stx->bystx syntax-fragment syntax-ctx str-transformer)
   (->> syntax-fragment
        syntax->datum
        symbol->string
        str-transformer
        (string-append (str-transformer "by"))
        string->symbol
        (datum->syntax syntax-ctx))))

;; a → b → (a . b)
(define ((con x) y)
  (cons x y))

(define-syntax (by stx)
  (syntax-case stx ()
    [(_ type rr)
     #`(and=> (#,(stx->bystx #'type stx string-downcase)
               rr)
              (cut map
                (con (quote #,(stx->bystx #'type stx string-upcase)))
                <>))]))


;; (define-syntax (single-rule stx)
;;   (syntax-case stx (when)
;;     [(_ (when condition expr)
;;         #`(when
;;               #,(let-syntax ((and (syntax-rules ()
;;                                     [(_ expr ...)
;;                                      ]
;;                                     ))
;;                              (or (syntax-rules ()
;;                                    )))
;;                   #'condition)
;;             expr))]
;;     [(_ expr)
;;      expr
;;      ]))

(define-syntax make-single-extender
  (syntax-rules (||)
    [(_ rr field ...)
     (apply cross-product
            (filter identity
                    (list
                     (by field rr) ...)))]))

(define-syntax make-extenders
  (syntax-rules (||)
    [(_ rr (key || cc ...) ...)
     (case (freq rr)
       [(key)
        (make-single-extender rr cc ...)] ...)]))

;; TODO compliacted fields
;; rrule → (list extension-rule)
(define (all-extenders rrule)
  (make-extenders
   rrule
   [YEARLY  || month weekno yearday monthday #| day |# hour minute second]
   [MONTHLY || monthday day hour minute second]
   [WEEKLY  || day hour minute second]
   [DAILY   || hour minute second]
   [HOURLY  || minute second]
   [MINUTELY || second]
   [SECONDLY || #| null |#]))

;; TODO this isn't nuted for limiting, only for extension
;; TODO I think the fix would be to not do a cross-product,
;; but rather have a list of fileds, each with a list of values
(define (all-limiters rrule)
  (make-extenders
   rrule
   [YEARLY   ||  day #| setpos |#]
   [MONTHLY  || month #| day |# #|setpos|#]
   [WEEKLY   || month #|setpos|#]
   [DAILY    || month monthday day #|setpos|#]
   [HOURLY   || month yearday monthday day hour #|setpos|#]
   [MINUTELY || month yearday monthday day hour minute #|setpos|#]
   [SECONDLY || month yearday monthday day hour minute second #|setpos|#]
   ;; [else]
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

;; TODO more special expands (p. 44)
;; TODO which of THESE can be negative
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
                         (cond [(date? o) o]
                               [(time? o) d]
                               [else (error "faoeuhtnsaoeu htnaoeu" )])
                         (cond [(date? o) (datetime date: o time: t tz: (get-timezone dt))]
                               [(time? o) (datetime date: d time: o tz: (get-timezone dt))]
                               [else (error "faoeuhtnsaoeu htnaoeu" )])))))
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
                        wday (set (month d) value)))
                     (map cdr (byday rrule)))))

              ;; else
              (to-dt (set (month d) value)))]

         [(BYDAY)
          (let* (((offset . value) value))
            (case (freq rrule)
              [(WEEKLY)
               ;; set day to that day in the week which d lies within
               (to-dt (date+ (start-of-week d (wkst rrule))
                             ;; TODO check that this actually is the correct calculation
                             (date day: (modulo (- value (wkst rrule))
                                                7))))]

              [(MONTHLY)
               ;; if bymonthday is present byday turns into a limiter
               (if (bymonthday rrule)
                   dt
                   (let* ((instances (all-wday-in-month value d)))
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
                         dt))))]

              ;; see Note 2, p. 44
              [(YEARLY)
               (cond
                ;; turns it into a limiter
                [(or (byyearday rrule) (bymonthday rrule))
                      dt]
                ;; [(byweekno rrule) =>
                ;;  ;; offset MUST be #f here (according to the spec)
                ;;  ]
                ;; [(bymonth rrule) =>]

                [else
                 (let ((instances (all-wday-in-year
                                   value (start-of-year d))))
                   (if (positive? offset)
                       (list-ref instances (1- offset))
                       (list-ref (reverse instances) (1- (- offset)))))])
               ]))
          ]
         [(BYWEEKNO)
          (to-dt (date-starting-week value d (wkst rrule)))]
         [(BYYEARDAY) (to-dt (date+ (start-of-year d)
                                    (date day: (1- value))))]
         [(BYMONTHDAY)
          (to-dt (set (day d)
                      (if (positive? value)
                          value (+ 1 value (days-in-month d)))))]
         [(BYHOUR) (to-dt (set (hour t) value))]
         [(BYMINUTE) (to-dt (set (minute t) value))]
         [(BYSECOND) (to-dt (set (second t) value))]
         [else (error "Unrecognized by-extender" key)])))
   date-object
   extension-rule))


(define (make-date-increment rr)
  (case (freq rr)
    [(YEARLY)   (datetime date: (date year: (interval rr)))]
    [(MONTHLY)  (datetime date: (date month: (interval rr)))]
    ;; please say that a week has always seven days...
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

;; TODO ought to be [rrule, a → (stream a)] where a := (date | datetime)
;; rrule, date → (stream datetime)
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


;; returns a function which takes a datetime and is true
;; if the datetime is part of the reccurrence set, and
;; false otherwise.
;; 
;; TODO how many of these can take negative numbers?
;; 
;; limiters → (a → bool)
(define (limiters->predicate limiters)
  (lambda (dt)
   (let loop ((remaining limiters))
     (if (null? remaining)
         #t
         (let* (((key . value) (car remaining))
                (t (as-time dt))
                (d (if (date? dt) dt (get-date dt))))
           (and (case key
                  [(BYMONTH) (eqv? value (month d))]
                  [(BYMONTHDAY)
                   (eqv? (day d)
                         (if (positive? value)
                             value (+ value 1 (days-in-month d))))]
                  [(BYYEARDAY) (eqv? value (year-day d))]
                  ;; TODO special cases?
                  [(BYDAY) (eqv? (cdr value) (week-day d))]
                  [(BYHOUR) (eqv? value (hour t))]
                  [(BYMINUTE) (eqv? value (minute t))]
                  [(BYSECOND) (eqv? value (second t))]
                  ;; TODO
                  ;; [(BYSETPOS)]
                  [else
                   (error "Unknown by-limiter")])
                (loop (cdr remaining))))))))


(define-stream (limit-recurrence-set rrule date-stream)
  ;; TODO BYSETPOS
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
    ;; TODO fix limiter generation
    ;; ===========================
    (limiters->predicate (car (append (all-limiters rrule) '(()))))
    date-stream)))

(define-stream (generate-posibilities rrule base-date)
  (limit-recurrence-set
   rrule
   (extend-recurrence-set
    rrule base-date)))

(define-stream (rrule-instances event)
  ;; (define rrule  (parse-recurrence-rule
  ;;                 (attr event 'RRULE)
  ;;                 (if (date? (attr event 'DTSTART))
  ;;                     parse-ics-date parse-ics-datetime)))

  (define rrule (attr event 'RRULE))

  ;; 3.8.5.1 exdate are evaluated AFTER rrule (and rdate)
  (let ((date-stream (stream-remove
                      (aif (attr* event 'EXDATE)
                           (cut member <> (map value it))
                           (const #f))
                      (generate-posibilities rrule (attr event 'DTSTART))
                      ;; TODO ideally I should merge the limited recurrence set
                      ;; with the list of rdates here. However, I have never
                      ;; sen an event with an RDATE attribute, so I wont worry
                      ;; about it for now.
                      ;; (stream-merge (list->stream (#|rdate's|#))
                      )))
    (cond [(count rrule) => (lambda (c) (stream-take c date-stream))]
          [(until rrule) => (lambda (end) (stream-take-while
                                      (cut (if (date? (attr event 'DTSTART)) date<= datetime<=) <> end)
                                      date-stream))]
          [else date-stream])))


(define-public (final-event-occurence event)
  (define rrule  (parse-recurrence-rule
                  (attr event 'RRULE)
                  (if (date? (attr event 'DTSTART))
                      parse-ics-date parse-ics-datetime)))

  (if (or (count rrule) (until rrule))
      (let ((instances (rrule-instances event)))
        (stream-ref instances (1- (stream-length instances))))
      #f))


(define (generate-recurrence-set base-event)


  (define duration
    ;; NOTE DTEND is an optional field.
    (let ((end (attr base-event 'DTEND)))
      (if end
          (if (date? end)
              (date-difference end (attr base-event 'DTSTART))
              (datetime-difference end (attr base-event 'DTSTART)))
          #f)))

  (define rrule-stream (rrule-instances base-event))

  (stream-map
   (aif (attr base-event 'X-HNH-ALTERNATIVES)
        (lambda (dt)
          (aif (hash-ref it dt)
               it ; RECURRENCE-ID objects come with their own DTEND
               (let ((ev (copy-vcomponent base-event)))
                 (set! (attr ev 'DTSTART) dt)
                 (when duration
                   ;; p. 123 (3.8.5.3 Recurrence Rule)
                   ;; specifies that the DTEND should be updated to match how the
                   ;; initial dtend related to the initial DTSTART. It also notes
                   ;; that an event of 1 day in length might be longer or shorter
                   ;; than 24h depending on timezone shifts.
                   (set! (attr ev 'DTEND) ((cond [(date? dt) date+]
                                                 [(datetime? dt) datetime+]
                                                 [else (error "Bad type")])
                                           dt duration)))
                 ev)))
        (lambda (dt)
          (let ((ev (copy-vcomponent base-event)))
            (set! (attr ev 'DTSTART) dt)
            (when duration
             (set! (attr ev 'DTEND) ((cond [(date? dt) date+]
                                           [(datetime? dt) datetime+]
                                           [else (error "Bad type")])
                                     dt duration)))
            ev)))
   rrule-stream))

