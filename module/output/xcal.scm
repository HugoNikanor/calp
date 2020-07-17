(define-module (output xcal)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (vcomponent)
  :use-module (vcomponent geo)
  :use-module (output sxml-types)
  :use-module (ice-9 match)
  :use-module (output common)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  )


(define (vline->value-tag vline)
  (define key (vline-key vline))

  (define writer
   (cond
    [(and=> (param vline 'VALUE) (compose string->symbol car))
     => get-writer]
    [(memv key '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID
                        CREATED DTSTAMP LAST-MODIFIED
                        ACKNOWLEDGED EXDATE))
     (get-writer 'DATE-TIME)]

    [(memv key '(TRIGGER DURATION))
     (get-writer 'DURATION)]

    [(memv key '(FREEBUSY))
     (get-writer 'PERIOD)]

    [(memv key '(CALSCALE METHOD PRODID COMMENT DESCRIPTION
                       LOCATION SUMMARY TZID TZNAME
                       CONTACT RELATED-TO UID

                       CATEGORIES RESOURCES

                       VERSION))
     (get-writer 'TEXT)]

    [(memv key '(TRANSP
              CLASS
              PARTSTAT
              STATUS
              ACTION))
     (lambda (p v) ((get-writer 'TEXT) p (symbol->string v)))]

    [(memv key '(TZOFFSETFROM TZOFFSETTO))
     (get-writer 'UTC-OFFSET)]

    [(memv key '(ATTACH TZURL URL))
     (get-writer 'URI)]

    [(memv key '(PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE))
     (get-writer 'INTEGER)]

    [(memv key '(GEO))
     (lambda (_ v)
       `(geo
         (latitude ,(geo-latitude v))
         (longitude ,(geo-longitude v))))]

    [(memv key '(RRULE))
     (get-writer 'RECUR)]

    [(memv key '(ORGANIZER ATTENDEE))
     (get-writer 'CAL-ADDRESS)]

    [(x-property? key)
     (get-writer 'TEXT)]

    [else
     (warning "Unknown key ~a" key)
     (get-writer 'TEXT)]))

  (writer ((@@ (vcomponent base) get-vline-parameters) vline) (value vline)))

(define (property->value-tag tag . values)
  (if (or (eq? tag 'VALUE)
          (internal-field? tag))
      #f
      `(,(downcase-symbol tag)
        ,@(map (lambda (v)
                 ;; TODO parameter types!!!! (rfc6321 3.5.)
                 `(text ,(->string v)))
               values))))

;; ((key value ...) ...) -> `(parameters , ... )
(define (parameters-tag parameters)
  (define outparams (filter-map
                     (lambda (x) (apply property->value-tag x))
                     parameters))

  (unless (null? outparams)
    `(parameters ,@outparams)))

(define-public (vcomponent->sxcal component)

  (define tagsymb (downcase-symbol (type component)))


  (remove null?
   `(,tagsymb
     ;; only have <properties> when it's non-empty.
     ,(let ((props
             (filter-map
              (match-lambda
                [(? (compose internal-field? car)) #f]

                [(key vlines ...)
                 (remove null?
                         `(,(downcase-symbol key)
                           ,(parameters-tag (reduce assq-merge
                                                    '() (map parameters vlines)))
                           ,@(for vline in vlines
                                  (vline->value-tag vline))))]

                [(key . vline)
                 (remove null?
                         `(,(downcase-symbol key)
                           ,(parameters-tag (parameters vline))
                           ,(vline->value-tag vline)))])
              (properties component))))
        (unless (null? props)
          `(properties ,@props)))
     ,(unless (null? (children component))
        `(components ,@(map vcomponent->sxcal (children component)))))))

(define-public (ns-wrap sxml)
  `(icalendar (@ (xmlns "urn:ietf:params:xml:ns:icalendar-2.0"))
              ,sxml))
