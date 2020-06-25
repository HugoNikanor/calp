(define-module (output xcal)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (vcomponent)
  :use-module (vcomponent geo)
  :use-module (output sxml-types)
  :use-module (ice-9 match)
  :use-module (output common)
  :use-module (datetime)
  :use-module (datetime util)
  )


(define (vline->value-tag vline)
  (define key (vline-key vline))

  (define writer
   (cond
    [(and=> (prop vline 'VALUE) (compose string->symbol car))
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

(define-public (vcomponent->sxml component)
  `(,(downcase-symbol (type component))
    (properties
     ,@(hash-map->list
        (match-lambda*
          [(? (compose internal-field? car)) '()]

          ;; TODO parameters

          [(key (vlines ...))
           `(,(downcase-symbol key)
             #;
             ,(unless (null? (properties vline))
                `(parameters
                  ,@(map vline->value-tag (properties vline))))
             ,@(for vline in vlines
                    (vline->value-tag vline)))]

          [(key vline)
           `(,(downcase-symbol key)
             #;
             ,(unless (null? (properties vline))
                `(parameters
                  ,@(map vline->value-tag (properties vline))))
             ,(vline->value-tag vline))])
        (attributes component)))
    (components ,@(map vcomponent->sxml (children component)))))

(define-public (main calendar)
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"utf-8\"")
          (icalendar (@ (xmlns "urn:ietf:params:xml:ns:icalendar-2.0"))
                     ,(vcomponent->sxml calendar))))
