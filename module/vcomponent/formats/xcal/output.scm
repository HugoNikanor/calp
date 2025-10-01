(define-module (vcomponent formats xcal output)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (vcomponent)
  :use-module (vcomponent geo)
  :use-module (vcomponent formats xcal types)
  :use-module (ice-9 match)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (calp translation)
  :use-module (calp namespaces)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :export (vcomponent->sxcal))


(define (vline->value-tag vline)
  (define k (key vline))

  (define writer
   (cond
    [(and=> (param vline 'VALUE) (compose string->symbol car))
     => get-writer]
    [(memv k '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID
                        CREATED DTSTAMP LAST-MODIFIED
                        ACKNOWLEDGED EXDATE))
     (get-writer 'DATE-TIME)]

    [(memv k '(TRIGGER DURATION))
     (get-writer 'DURATION)]

    [(memv k '(FREEBUSY))
     (get-writer 'PERIOD)]

    [(memv k '(CALSCALE METHOD PRODID COMMENT DESCRIPTION
                       LOCATION SUMMARY TZID TZNAME
                       CONTACT RELATED-TO UID

                       CATEGORIES RESOURCES

                       VERSION))
     (get-writer 'TEXT)]

    [(memv k '(TRANSP
              CLASS
              PARTSTAT
              STATUS
              ACTION))
     (lambda (p v) ((get-writer 'TEXT) p (symbol->string v)))]

    [(memv k '(TZOFFSETFROM TZOFFSETTO))
     (get-writer 'UTC-OFFSET)]

    [(memv k '(ATTACH TZURL URL))
     (get-writer 'URI)]

    [(memv k '(PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE))
     (get-writer 'INTEGER)]

    [(memv k '(GEO))
     (lambda (_ v)
       ((xml xcal 'geo)
        (list
         ((xml xcal 'latitude)  (geo-latitude v))
         ((xml xcal 'longitude) (geo-longitude v)))))]

    [(memv k '(RRULE))
     (get-writer 'RECUR)]

    [(memv k '(ORGANIZER ATTENDEE))
     (get-writer 'CAL-ADDRESS)]

    [(x-property? k)
     (get-writer 'TEXT)]

    [else
     (warning (G_ "Unknown key ~a") k)
     (get-writer 'TEXT)]))

  (writer (vline-parameters vline)
          (vline-value vline)))

(define (property->value-tag pair)
  (define-values (tag value) (car+cdr pair))
  (if (or (eq? tag 'VALUE)
          (internal-field? tag))
      #f
      (apply (xml xcal (downcase-symbol tag))
             (map (lambda (v)
                    ;; TODO parameter types!!!! (rfc6321 3.5.)
                    ((xml xcal 'text) (->string v)))
                  value))))

;; ((key value ...) ...) -> #<xml parameters>
(define (parameters-tag parameters)
  (define outparams (filter-map
                     (lambda (x) (property->value-tag x))
                     parameters))

  (apply (xml xcal 'parameters) outparams))

(define (vcomponent->sxcal component)

  (define tagsymb (downcase-symbol (type component)))

  (apply (xml xcal tagsymb)
         (remove (compose null? xml-element-children)
                 ;; only have <properties> when it's non-empty.
                 (list
                  (let ((props
                         (filter-map
                          (match-lambda
                            [(? (compose internal-field? car)) #f]

                            [(key (vlines ...))
                             (apply (xml xcal (downcase-symbol key))
                                    (remove (compose null? xml-element-children)
                                            (cons
                                             (parameters-tag (reduce assq-merge
                                                                     '()
                                                                     (map parameters vlines)))
                                             (map vline->value-tag vlines))))]

                            [(key vline)
                             (apply (xml xcal (downcase-symbol key))
                                    (remove (compose null? xml-element-children)
                                            (list
                                             (parameters-tag (parameters vline))
                                             (vline->value-tag vline))))])
                          ;; NOTE this sort is unnecesasary, but here so tests can work
                          ;; Possibly add it as a flag instead
                          (sort* (properties component)
                                 string< (compose symbol->string car)))))
                    (apply (xml xcal 'properties)
                           ;; NOTE
                           ;; (x-hnh-calendar-name (text ,(prop (parent component) 'NAME)))
                           props))
                  (apply (xml xcal 'components)
                         (map vcomponent->sxcal (children component)))))))
