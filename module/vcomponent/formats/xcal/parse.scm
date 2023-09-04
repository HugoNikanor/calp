(define-module (vcomponent formats xcal parse)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (base64)
  :use-module (ice-9 match)
  :use-module (calp namespaces)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (sxml match)
  :use-module (vcomponent)
  :use-module (vcomponent geo)
  :use-module (vcomponent formats common types)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (calp translation)
  :use-module (hnh util table)
  :export (sxcal->vcomponent)
  )

;; symbol, ht, (list a) -> non-list
(define (handle-value type parameters value)
  (case type

    [(binary)
     ;; rfc6321 allows whitespace in binary
     (base64-string->bytevector
      (string-delete char-set:whitespace (car value)))]

    [(boolean) (string=? "true" (car value))]

    ;; TODO possibly trim whitespace on text fields
    [(cal-address uri text unknown) (string-concatenate value)]

    [(date)
     ;; TODO this is correct, but ensure remaining types
     (hashq-set! parameters 'VALUE "DATE")
     (parse-iso-date (car value))]

    [(date-time) (parse-iso-datetime (car value))]

    [(duration)
     ((get-parser 'DURATION) parameters value)]

    [(float integer) ; (3.0)
     (string->number (car value))]

    [(period)
     (sxml-match
      (cons 'period value)
      [(period (start ,start-dt) (end ,end-dt))
       (cons (parse-iso-datetime start-dt)
             (parse-iso-datetime end-dt))]
      [(period (start ,start-dt) (duration ,duration))
       (cons (parse-iso-datetime start-dt)
             ((@ (vcomponent duration) parse-duration) duration))])]

    [(recur)
     ;; RFC6221 (xcal) Appendix A 3.3.10 specifies that all components should
     ;; come in a specified order, and by extension that all components of the
     ;; same type should follow each other. Actually checking that is harder
     ;; than to just accept anything in any order. It would also make us less
     ;; robust for other implementations with other ideas.
     (let ((parse-value-of-that-type
            (lambda (type value)
              (case type
                ((wkst)
                 ((@ (vcomponent recurrence parse)
                     rfc->datetime-weekday)
                  (string->symbol value)))
                ((freq) (string->symbol value))
                ((until)
                 ;; RFC 6321 (xcal), p. 30 specifies type-until as
                 ;;     type-until = element until {
                 ;;         type-date |
                 ;;         type-date-time
                 ;;     }
                 ;; but doesn't bother defining type-date[-time]...
                 ;; This is acknowledged in errata 3315 [1], but
                 ;; it lacks a solution...
                 ;; Seeing as RFC 7265 (jcal) in Example 2 (p. 16)
                 ;; show the date as a direct string we will roll
                 ;; with that here to.
                 ;; [1]: https://www.rfc-editor.org/errata/eid3315
                 (string->date/-time value))
                ((byday) ((@@ (vcomponent recurrence parse) parse-day-spec) value))
                ((count interval bysecond bymunite byhour
                        bymonthday byyearday byweekno
                        bymonth bysetpos)
                 (string->number value))
                (else (scm-error 'key-error "handle-value"
                       (G_ "Invalid type ~a, with value ~a")
                       (list type value)
                       #f))))))

       ;; freq until count interval wkst

       (apply (@ (vcomponent recurrence internal) make-recur-rule)
              (concatenate
               (filter identity
                       (for key in '(bysecond byminute byhour byday bymonthday
                                              byyearday byweekno bymonth bysetpos
                                              freq until count interval wkst)
                            (cond ((find-element (xml xcal key) value)
                                   => (lambda (v)
                                        (case key
                                          ;; These fields all have zero or one value
                                          ((freq until count interval wkst)
                                           (list (symbol->keyword key)
                                                 (parse-value-of-that-type
                                                  key (cadr v))))
                                          ;; these fields take lists
                                          ((bysecond byminute byhour byday bymonthday
                                                     byyearday byweekno bymonth bysetpos)
                                           (list (symbol->keyword key)
                                                 (map (lambda (v) (parse-value-of-that-type key v))
                                                      (cadr v))))
                                          (else (scm-error 'misc-error "handle-value"
                                                           "Invalid key ~s"
                                                           (list key)
                                                           #f)))))
                                  (else #f)))))))]

    [(time) (parse-iso-time (car value))]

    [(utc-offset) ((get-parser 'UTC-OFFSET) parameters (car value))]

    [(geo) ; ((long 1) (lat 2))
     (sxml-match
      (cons 'geo value)
      [(geo (latitude ,x) (longitude ,y))
       ((@ (vcomponent geo) make-geo) x y)])]

    [else (scm-error 'misc-error "handle-value"
                     "Unknown value type: ~s"
                     (list type) #f)]))

(define (symbol-upcase symb)
  (-> symb
      symbol->string
      string-upcase
      string->symbol))

(define (handle-parameters parameters)

  ;; (assert (element-matches? (xml xcal 'parameters)
  ;;                           parameters))

  (fold (lambda (param table)
          (define ptag (xml-element-tagname (car param)))
          ;; (define-values (ptype pvalue) (car+cdr cdr))
          ;; TODO multi-valued parameters!!!
          (define-values (pytpe pvalue) (car+cdr (cadr param)))
          ;; TODO parameter type (rfc6321 3.5.)
          ;; TODO namespaces
          (table-put table (symbol-upcase ptag)
                    (concatenate pvalue)))
        (table)
        (cdr parameters)))

(define* (parse-enum str enum optional: (allow-other #t))
  (let ((symb (string->symbol str)))
    (unless (memv symb enum)
      (warning "~a ∉ { ~{~a~^, ~} }" symb enum))
    symb))


;; symbol non-list -> non-list
(define (handle-tag xml-tag data)
  (define tag-name (xml-element-tagname xml-tag))
  (case tag-name
    [(request-status)
     ;; TODO
     (warning (G_ "Request status not yet implemented"))
     #f]

    ((transp) (parse-enum
               data '(OPAQUE TRANSPARENT) #f))
    ((class) (parse-enum
              data '(PUBLIC PRIVATE CONFIDENTIAL)))
    ((partstat) (parse-enum
                 data '(NEEDS-ACTION ACCEPTED DECLINED TENTATIVE
                                     DELEGATED IN-PROCESS)))
    ((status) (parse-enum
               data '(TENTATIVE CONFIRMED CANCELLED NEEDS-ACTION COMPLETED
                                IN-PROCESS DRAFT FINAL CANCELED)))
    ((action) (parse-enum
               data '(AUDIO DISPLAY EMAIL NONE)))
    [else data]))

(define (handle-single-property component tree)
  (define xml-tag (car tree))
  (define tag (xml-element-tagname xml-tag))
  (define tag* (symbol-upcase tag))

  (define body (cdr tree))

  ;; TODO request-status
  (define-values (parameters data)
    (if (element-matches? (xml xcal 'parameters)
                          (car body))
        (values (handle-parameters (car body))
                (cdr body))
        (values (make-hash-table)
                body)))

  (fold (lambda (typetag component)
          (define type (xml-element-tagname (car typetag)))
          ;; TODO multi valued data
          (define raw-value (cdr typetag))
          (define vline*
            (vline type: tag*
                   value: (handle-tag
                           xml-tag
                           (let ((v (handle-value type parameters raw-value)))
                             ;; TODO possibly more list fields
                             ;; (if (eq? tag 'categories)
                             ;;     (string-split v #\,)
                             ;;     v)

                             v))
                   parameters: parameters))
          (if (memv tag* '(ATTACH ATTENDEE CATEGORIES
                               COMMENT CONTACT EXDATE
                               REQUEST-STATUS RELATED-TO
                               RESOURCES RDATE
                               ;; x-prop
                               ;; iana-prop
                               ))
              (aif (prop* component tag*)
                   (prop* component tag* (cons vline* it))
                   (prop* component tag* (list vline*)))
              (prop* component tag* vline*)))
        component data))

;; Note
;; This doesn't verify the inter-field validity of the object,
;; meaning that value(DTSTART) == DATE and value(DTEND) == DATE-TIME
;; are possibilities, which other parts of the code will crash on.
;; TODO
;; since we are feeding user input into this it really should be fixed.
(define (sxcal->vcomponent sxcal)

  ;; TODO the surrounding icalendar element needs to be removed BEFORE this procedue is called

  (define xml-tag (car sxcal))
  (define type (symbol-upcase (xml-element-tagname xml-tag)))

  (let ((component
         (aif (find-element (xml xcal 'properties) (cdr sxcal))
              ;; Loop over multi valued fields, creating one vline
              ;; for every value. So
              ;;     KEY;p=1:a,b
              ;; would be expanded into
              ;;     KEY;p=1:a
              ;;     KEY;p=1:b
              (fold swap handle-single-property
                    (vcomponent type: type) (cdr it))
              (vcomponent type: type))))

    ;; children
    (aif (find-element (xml xcal 'components) (cdr sxcal))
           ;; NOTE Order of children is insignificant, but this allows
           ;;      diffs to be stable (which is used by the format tests).
         (fold (swap add-child)
               component
               (map sxcal->vcomponent
                    (reverse (cdr it))))
         component)))
