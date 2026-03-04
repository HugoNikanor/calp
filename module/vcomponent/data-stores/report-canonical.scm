;;; Commentary:
;;; "Cannonical" implementation of calendar-query and calendar-multiget REPORTs.
;;; code:
(define-module (vcomponent data-stores report-canonical)
  :use-module (ice-9 match)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((rnrs base) :select (assert) :version (6))
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util destructure)
  :use-module ((calp namespaces) :select (caldav webdav))
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (web uri)
  :use-module (vcomponent)
  :use-module (vcomponent alarm)
  :use-module (vcomponent datetime)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type period)
  :use-module (vcomponent type duration)
  ;; TDOO shouldn't this be imported from the "base" recurrence module?
  :use-module ((vcomponent type recurrence generate) :select (find-base-instance generate-recurrence-set))

  :export (extract-time-range

           execute-comp-filter
           execute-calendar-data
           )
  )

;; Extracts the start and end attributes from a <time-range/> element
;; Returns 2 values, the start and end time as UTC datetimes.
;; If either value is absent in the tag, or is [+-]infinity, then the
;; symbol 'infinity is returned in the datetimes place.
(define (extract-time-range time-range)
  (assert (tag-matches? time-range 'time-range caldav))

  (let ((attrs (xml-element-attributes time-range)))
    (match (list (table-get attrs 'start "-infinity")
                 (table-get attrs 'end   "+infinity"))
      (("-infinity" "+infinity")
       (values 'infinity 'infinity))
      (("-infinity" end)
       (values 'infinity (parse-ics-datetime end)))
      ((start "+infinity")
       (values (parse-ics-datetime start) 'infinity))
      ((start end)
       (values (parse-ics-datetime start)
               (parse-ics-datetime end))))))

(define (extract-required-times xml-element)
  (values
   (string->datetime (attribute xml-element 'start) "~Y~m~dT~H~M~S~Z")
   (string->datetime (attribute xml-element 'end)   "~Y~m~dT~H~M~S~Z")))

;; TODO see (vcomponent data-stores common) for the true implementation.
;;; TODO figure out how to define and import that one without creating a loop
(define (supported-collations _)
  `(("i;ascii-casemap" . ,(@ (hnh util ascii) string-ascii-contains-ci))
    ("i;octet" . ,string-contains)
    ("i;unicode-casemap" . ,string-contains-ci)
    )
  )

;;; text-match: a <C:text-match/> element
;;; store: calendar data store, needed for proper collation support
;;; text: the text we want to see if matched by the tag
(define (execute-text-match text-match store text)
  (assert (tag-matches? text-match 'text-match caldav))
  (typecheck text string?)
  (let ((collation (or (attribute text-match 'collation) "i;ascii-casemap"))
        (negate? (string=? "yes" (or (attribute text-match 'negate-condition) "no"))))
    (cond ((assoc-ref (supported-collations store) collation)
           => (lambda (string-contains)
                ((if negate? not identity)
                 (string-contains text (xml-text-content text-match)))))
          (else
           ;; TODO correct error
           (throw 'unsupported-collation-type)))))


(define (execute-param-filter param-filter store parameters)
  (assert (tag-matches? param-filter 'param-filter caldav))
  (typecheck parameters table?)

  (define key (string->symbol (attribute param-filter 'name)))
  (cond ((find (lambda (ch) (tag-matches? ch 'is-not-defined caldav))
               (xml-element-children param-filter))
         (not (table-get parameters key)))

        ((find (lambda (ch) (tag-matches? ch 'text-match caldav))
               (xml-element-children param-filter))
         => (lambda (text-match)
              (and=> (table-get parameters key)
                     (lambda (value) (execute-text-match text-match store value)))))

        (else (table-get parameters key))))


;;; Run <time-range/> filters found inside <prop-filter/>
(define (execute-prop-filter-time-range time-range target-dt)
  (assert (tag-matches? time-range 'time-range caldav))
  (typecheck target-dt zoned-datetime?)

  (let ((start end (extract-time-range time-range)))
    (match (list start end)
      ('(infinity infinity)
       'TODO-error)

      ((start 'infinity)
       (datetime<= start target-dt))

      (('infinity end)
       (datetime</zoneinfo target-dt end))

      ((start end)
       (unless (datetime<= start end)
         'TODO-throw-error)
       (and (datetime<=/zoneinfo start target-dt)
            (datetime</zoneinfo target-dt end))))))


(define (execute-prop-filter reference-zone prop-filter store component)
  (and=> (prop% component (string->symbol (attribute prop-filter 'name)))
         (lambda (vlines)
           (any
            (lambda (vline)
              ;; Note: Pre-finding the different child elements
              ;; (instead of locating them for every vline) would be
              ;; faster. However, the speedup is basically negliable,
              ;; since the child set will always be relatively small,
              ;; more time will be spent searching for recurrence
              ;; instances, and any efficient data store will provide
              ;; their own (caching) implementation anyways.
              (and
               ;; (<time-range /> | <text-match />)?
               (cond ((find (lambda (ch) (tag-matches? ch 'time-range caldav))
                            (xml-element-children prop-filter))
                      => (lambda (time-range)
                           (execute-prop-filter-time-range
                            time-range
                            (let ((v (vline-value vline)))
                              (catch #t
                                (lambda () (ensure-zoned-datetime reference-zone v))
                                (lambda _ (throw 'report-pre-condition
                                            ((xml caldav 'supported-filter)
                                             ((xml caldav 'prop-filter
                                                   `((name . ,(attribute prop-filter 'name)))))))))))))

                     ((find (lambda (ch) (tag-matches? ch 'text-match caldav))
                            (xml-element-children prop-filter))
                      => (lambda (text-match)
                           (execute-text-match
                            text-match
                            store
                            (let ((v (vline-value vline)))
                              (cond ((boolean? v) (if v "TRUE" "FALSE"))
                                    ((uri? v) (uri->string v))
                                    ((number? v) (number->string v))
                                    ((string? v) v)
                                    ((unknown? v) (from-unknown v))
                                    ;; The following types are currently not usable in text searches:
                                    ;; BINARY: Not convertable to text, could be allowed if collation is "i;octet"
                                    ;; DATE, DATE-TIME, DURATION, PERIOD, TIME: better handled by <time-range/> searches
                                    ;; RECUR, UTC-OFFSET: Other reasons
                                    ;; geo?, version?, request-status?: just annoying
                                    (else (throw 'report-pre-condition
                                                 ((xml caldav 'supported-filter)
                                                  ((xml caldav 'prop-filter
                                                        `((name . ,(attribute prop-filter 'name)))))))))))))

                     (else #t))

               ;; <param-filter />*
               (every (lambda (filter) (execute-param-filter filter store (vline-parameters vline)))
                      (filter (lambda (ch) (tag-matches? ch 'param-filter caldav))
                              (xml-element-children prop-filter)))))
            vlines))))


(define (vtodo-instance-overlaps? reference-zone component start end)
  (typecheck reference-zone string?)
  (typecheck component vtodo?)
  (typecheck start utc-datetime?)
  (typecheck end utc-datetime?)

  (destructure (vector (get component (prop* 'DTSTART))
                       (get component (prop* 'DURATION))
                       (get component (prop* 'DUE)))
    ((vector (just dtstart) (just duration) (nothing))
     (let* ((start* (ensure-zoned-datetime reference-zone dtstart))
            (end* (datetime+ start* duration)))
       (and (datetime<=/zoneinfo start end*)
            (or (datetime>/zoneinfo end start*)
                (datetime>=/zoneinfo end end*)))))

    ((vector (just dtstart) (nothing) (just due))
     (let ((start* (ensure-zoned-datetime reference-zone dtstart))
           (due*   (ensure-zoned-datetime reference-zone due)))
       (and (or (datetime</zoneinfo start due*)
                (datetime<=/zoneinfo start start*))
            (or (datetime>/zoneinfo end start*)
                (datetime>=/zoneinfo end due*)))))

    ((vector (just dtstart) (nothing) (nothing))
     (let ((start* (ensure-zoned-datetime reference-zone dtstart)))
       (and (datetime<=/zoneinfo start start*)
            (datetime>/zoneinfo end start*))))

    ((vector (nothing) (nothing) (just due))
     (let ((due* (ensure-zoned-datetime reference-zone due)))
       (and (datetime</zoneinfo start due*)
            (datetime>=/zoneinfo end due*))))

    ;; The else clause technically requires
    ;; (vector (nothing) (nothing) (nothing)).
    ;; However, making it catch all makes this procedure
    ;; not crash due to match errors
    ;; The CMPLETED and CREATED properties must be given in UTC
    ;; meaning that we can skip zoneinfo.
    (_ (destructure (vector (get component (prop* 'COMPLETED))
                            (get component (prop* 'CREATED)))
         ((vector (just completed) (just created))
          (and (or (datetime<= start created)
                   (datetime<= start completed))
               (or (datetime>= end created)
                   (datetime>= end completed))))
         ((vector (just completed) (nothing))
          (and (datetime<= start completed)
               (datetime>= end completed)))
         ((vector (nothing) (just created))
          (datetime> end created))
         ((vector (nothing) (nothing))
          #t)))))


(define (freebusy-overlaps? reference-zone component start end)
  (typecheck component vfreebusy?)
  (typecheck start utc-datetime?)
  (typecheck end utc-datetime?)
  ;; - The FREEBUSY property must be given in UTC
  ;;   meaning that we can skip zoneinfo.
  ;; - VFREEBUSY components have no recurrence mechanisms

  (or (and (prop% component 'DTSTART)
           (prop% component 'DTEND)
           (datetime<=/zoneinfo start (prop1 component 'DTEND))
           (datetime>/zoneinfo end (prop1 component 'DTSTART)))
      (and=> (prop% component 'FREEBUSY)
             (lambda (freebusy)
               (any
                (lambda (v)
                  (let ((start* end* (period->utc-datetimes reference-zone v)))
                    (and (datetime< start end*)
                         (datetime> end start*))))
                (map vline-value freebusy))))))

;;; TODO we assume that the name parameter exists at all places where it's required.
;;; For any missing name, we MUST fail with a CALDAV:valid-filter error
(define* (execute-comp-filter timezone filter store component trace)
  (assert (tag-matches? filter 'comp-filter caldav))
  (typecheck component vcomponent?)
  ;; TODO a true timezone object should be taken, but currently all datetime operations only work on timezone names, we take that instead. The #f clause means that the default timezone for the store should be used, or an implementation defined default otherwise (e.g. UTC or we let the user configure it)
  ;; (typecheck timezone (or false? vtimezone?))
  (typecheck timezone string?)

  (define (execute-comp-filter-part filter-part)
    (cond
     ;; <prop-filter name="&name;"><is-not-defined /></prop-filter>
     ((and (tag-matches? filter-part 'prop-filter caldav)
           (find (lambda (ch) (tag-matches? ch 'is-not-defined caldav))
                 (xml-element-children filter-part)))
      (not (prop% component (string->symbol (attribute filter-part 'name)))))

     ((tag-matches? filter-part 'prop-filter caldav)
      (execute-prop-filter timezone filter-part store component))

     ;; <comp-filter name="&name;"><is-not-defined /></prop-filter>
     ((and (tag-matches? filter-part 'comp-filter caldav)
           (find (lambda (ch) (tag-matches? ch 'is-not-defined caldav))
                 (xml-element-children filter-part)))
      (not
       (find (lambda (t) (eq? t (string->symbol (attribute filter-part 'name))))
             (map type (vcomponent-children component)))))

     ;; <comp-filter name="&name;">...</comp-filter>
     ((tag-matches? filter-part 'comp-filter caldav)
      (any (lambda (sub-component)
             (execute-comp-filter timezone filter-part store sub-component (cons component trace)))
           (vcomponent-children component)))

     ;; <time-range />
     ;; We put this one last, since it's the most expensive one.
     ;; This is an non-profiled micro-optimization, but it's free
     ;; to implement.
     ((tag-matches? filter-part 'time-range caldav)
      (define-values (start end) (extract-time-range filter-part))
      ;; See RFC 4791 §9.9
      (case (type component)
        ((VEVENT)
         (event-overlaps? timezone (find vcalendar? trace) start end))
        ((VTODO)

         ;; TODO this only checks if THIS instance of the VTODO
         ;; overlaps the timespan. VTODO's however can repeat, and a
         ;; propper method must be created.
         (vtodo-instance-overlaps? timezone component start end))

        ((VFREEBUSY)
         (freebusy-overlaps? timezone component start end))

        ((VALARM)
         ;; TODO TODO this only checks if THIS instance overlaps.
         ;; It should check that if any instance of the event overlaps.
         ;; This is however MUCH more work, due to recurring events.
         (any (lambda (trigger)
                (and (datetime<=/zoneinfo start trigger)
                     (datetime>/zoneinfo end trigger)))
              (alarm-triggers timezone
                              (find (lambda (c) (or (vevent? c)
                                               (vtodo? c)))
                                    trace) component)))

        ((VJOURNAL)
         (throw 'not-implemented "execute-comp-filter" "Recurrence for ~s"
                (list (type component)) #f))
        (else
         ;; Unknown calendar component, meaning we can't check if it overlaps.
         #f)))

     ;; Unknown filter, ignore
     (else #t)))

  (and
   (eq? (type component)
        (string->symbol (attribute filter 'name)))
   (every execute-comp-filter-part
          (xml-element-children filter))))



(define (execute-comp-tag entry comp)
  (typecheck entry vcomponent?)
  (assert (tag-matches? comp 'comp caldav))

  ;; Compare resource type with expected type
  ;; (attribute comp 'name)

  (define (prop-limiter-helper props)
    ;; `comp` is taken from the environment
    (cond ((find (lambda (el) (tag-matches? el 'allprop caldav))
                 (xml-element-children comp))
           ;; allprop doesn't have any more data,
           ;; just keep the properties list as is
           props)
          ((filter (lambda (el) (tag-matches? el 'prop caldav))
                   (xml-element-children comp))
           => (lambda (prop-limiters)
                ;; limit to only the found properties
                ;; Each <prop/> element MUST have a `name` attribute,
                ;; and MAY have a `novalue` attribute.
                (fold (lambda (prop-limiter resulting-table)
                        (define key (string->symbol (attribute prop-limiter 'name)))
                        (destructure (table-preview props key)
                          ((just x)
                           (table-put
                            resulting-table key
                            (if (string=? "yes" (or (attribute prop-limiter 'novalue) "no"))
                                ;; NOTE: we need a no-value sentinel type.
                                ;; - the empty string prevents self-closing xml tags
                                ;; - it's unclear how type information interacts with no values,
                                ;;   especially for xCal and other formats with obligatory types.
                                "" x)))
                          ((nothing) resulting-table)))
                      (table (table-type props))
                      prop-limiters)))))

  (define (comp-limiter-helper children)
    ;; `comp` is taken from the environment
    (cond ((find (lambda (el) (tag-matches? el 'allcomp caldav))
                 (xml-element-children comp))
           children)
          ((filter (lambda (el) (tag-matches? el 'comp caldav))
                   (xml-element-children comp))
           => (lambda (comp-limiters)
                ;; <C:comp name="&name;">...</C:comp>
                (concatenate
                 ;; This code treats multiple comp limiters sharing a name as undefined behaviour,
                 ;; and will insert duplicate copies of content if so be.
                 (for comp-limiter in comp-limiters
                      (let ((sub-name (string->symbol (attribute comp-limiter 'name))))
                        (map (lambda (entry) (execute-comp-tag entry comp-limiter))
                         (filter (lambda (ch) (eq? sub-name (type ch)))
                                 children)))))))))

  (if (null? (xml-element-children comp))
      entry
      (-> entry
          (modify vcomponent-properties* prop-limiter-helper)
          (modify vcomponent-children* comp-limiter-helper))))


(define (execute-limit-freebusy-set-tag component limit-freebusy-set)
  (typecheck component vcalendar?)
  (assert (tag-matches? limit-freebusy-set 'limit-freebusy-set caldav))

  (let ((start (parse-ics-datetime (attribute limit-freebusy-set 'start)))
        (end   (parse-ics-datetime (attribute limit-freebusy-set 'end))))
    (modify
     component vcomponent-children*
     (lambda (children)
       (for child in children
            (modify
             child (prop* 'FREEBUSY)
             (destructure-lambda
              ((nothing) (nothing))
              ((just vlines)
               (just
                (filter (lambda (vline)
                          (let ((v (vline-value vline)))
                            (cond ((period? v)
                                   ;; Reference zone ignored, since all dates involved
                                   ;; already MUST be in UTC.
                                   (let ((p-start p-end (period->utc-datetimes "UTC" v)))
                                     (and (datetime<= start p-start)
                                          (datetime< p-end end))))
                                  (else (scm-error 'type-error "execute-limit-freebusy-set"
                                                   "Non-period found in FREEBUSY property: ~s"
                                                   (list v) #f)))))
                        vlines))))))))))

;; Executes the instructions from a <calendar-data /> tag as part of a REPORT request.
;; This includes (optionally) expanding and limiting the component set,
;; and finally limiting the fields of the returned object.
;; ALWAYS returns a vcalendar object
(define (execute-calendar-data component calendar-data)
  (typecheck component vcalendar?)
  (assert (tag-matches? calendar-data 'calendar-data caldav))

  ;; - (optionall) run expand or limit-recurrence-set, giving
  ;;   us a new vcalendar root object with a (possibly changed)
  ;;   list of children.
  ;; - on this object, execute the <C:comp>...</> filter

  (define expanded-component
    (cond
     ;; <C:expand start="&start;" end="&end;" />
     ((find (lambda (el) (tag-matches? el 'expand caldav))
            (xml-element-children calendar-data))
      => (lambda (expand)
           ;; Both start and end required
           (let ((start end (extract-required-times expand)))
             (set component
                  vcomponent-children*
                  (->> (generate-recurrence-set component)
                       (stream-take-while
                        (lambda (instance) (datetime</zoneinfo
                                       (instance-start-datetime "TODO reference zone" instance)
                                       end)))
                       (stream-filter
                        (lambda (instance)
                          (instance-overlaps? "TODO reference zone" instance start end)))
                       stream->list

                       (map (lambda (instance)
                              (-> instance
                                  ;; See RFC 4791 §9.6.5.
                                  (set (prop* 'EXDATE) (nothing))
                                  (set (prop* 'EXRULE) (nothing))
                                  (set (prop* 'RDATE) (nothing))
                                  (set (prop* 'RRULE) (nothing))
                                  (modify (lens-compose (prop* 'DTSTART) just* car* vline-value*)
                                          (unval zone->utc))
                                  (modify (lens-compose (prop* 'DTEND)   just* car* vline-value*)
                                          (unval zone->utc))

                                  ;; TODO RFC 4791 §9.6.5 states
                                  ;; > The returned calendar components [...] MUST NOT
                                  ;; > have reference to or include VTIMEZONE components.  Date and local
                                  ;; > time with reference to time zone information MUST be converted
                                  ;; > into date with UTC time.
                                  ;; This means that we actually need to scan all fields
                                  ;; and remove any timezone references.
                                  (modify (lens-compose (prop* 'RECURRENCE-ID) just* car* vline-value*)
                                          (unval zone->utc))
                                  ))))))))

     ;; <C:limit-recurrence-set start="&start;" end="&end;" />
     ((find (lambda (el) (tag-matches? el 'limit-recurrence-set caldav))
            (xml-element-children calendar-data))
      => (lambda (limit-recurrence-set)
           ;; both start and end required
           ;; this simply includes the "master component", and all overridden
           ;; instances which overlap the specified timespan. No expansion is done
           ;; TODO special handling when RANGE parameter is present on RECURRENCE-ID
           (let ((base-instance other-instances (find-base-instance component))
                 (start end (extract-required-times limit-recurrence-set)))
             (set component
                  vcomponent-children*
                  (cons base-instance
                        (filter (lambda (instance)
                                  (case (type instance)
                                    ((VTIMEZONE) #t)
                                    ((VEVENT) (instance-overlaps? "TODO REFERENCE ZONE" instance start end))
                                    (else (throw 'not-implemented "Instance overlaps for" instance))))
                                other-instances))))))

     (else component)))

  (define expanded-component*
   (cond ((find (lambda (el) (tag-matches? el 'limit-freebusy-set caldav))
                (xml-element-children calendar-data))
          => (lambda (limit-freebusy-set)
               (execute-limit-freebusy-set-tag expanded-component limit-freebusy-set)))
         (else expanded-component)))

  (cond ((find (lambda (el) (tag-matches? el 'comp caldav))
               (xml-element-children calendar-data))
         => (lambda (comp)
              (execute-comp-tag expanded-component* comp)))
        (else expanded-component*)))
