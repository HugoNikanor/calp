(define-module (vcomponent xcal parse)
  :use-module (calp util)
  :use-module (calp util exceptions)
  :use-module (base64)
  :use-module (ice-9 match)
  :use-module (sxml match)
  :use-module (vcomponent)
  :use-module (vcomponent geo)
  :use-module (vcomponent parse types)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  )

;; symbol, ht, (list a) -> non-list
(define (handle-value type props value)
  (case type

    [(binary)
     ;; rfc6321 allows whitespace in binary
     (base64-string->bytevector
      (string-delete char-set:whitespace (car value)))]

    [(boolean) (string=? "true" (car value))]

    ;; TODO possibly trim whitespace on text fields
    [(cal-address uri text unknown) (car value)]

    [(date)
     ;; TODO this is correct, but ensure remaining types
     (hashq-set! props 'VALUE "DATE")
     (parse-iso-date (car value))]

    [(date-time) (parse-iso-datetime (car value))]

    [(duration)
     ((get-parser 'DURATION) props value)]

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
     (apply (@ (vcomponent recurrence internal) make-recur-rule)
            (concatenate
             (for (k v) in value
                  (list (symbol->keyword k)
                        (case k
                          ((wkst)
                           ((@ (vcomponent recurrence parse)
                               rfc->datetime-weekday)
                            (string->symbol v)))
                          ((freq) (string->symbol v))
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
                           (string->date/-time v))
                        ((byday) #|TODO|#
                         (throw 'not-yet-implemented))
                        ((count interval bysecond bymunite byhour
                                bymonthday byyearday byweekno
                                bymonth bysetpos)
                         (string->number v))
                        (else (throw
                               'key-error
                               "Invalid key ~a, with value ~a"
                               k v)))))))]

    [(time) (parse-iso-time (car value))]

    [(utc-offset) ((get-parser 'UTC-OFFSET) props (car value))]

    [(geo) ; ((long 1) (lat 2))
     (sxml-match
      (cons 'geo value)
      [(geo (latitude ,x) (longitude ,y))
       ((@ (vcomponent geo) make-geo) x y)])]))

(define (symbol-upcase symb)
  (-> symb
      symbol->string
      string-upcase
      string->symbol))

(define (handle-parameters parameters)

  (define ht (make-hash-table))

  (for param in parameters
       (match param
         [(ptag (ptype pvalue ...) ...)
          ;; TODO parameter type (rfc6321 3.5.)
          ;; TODO multi-valued parameters!!!
          (hashq-set! ht (symbol-upcase ptag)
                      (car (concatenate pvalue)))]))
  ht)

(define* (parse-enum str enum optional: (allow-other #t))
  (let ((symb (string->symbol str)))
    (unless (memv symb enum)
      (warning "~a ∉ { ~{~a~^, ~} }" symb enum))
    symb))


;; symbol non-list -> non-list
(define (handle-tag tag-name data)
  (case tag-name
    [(request-status)
     ;; TODO
     (warning "Request status not yet implemented")
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

;; Note
;; This doesn't verify the inter-field validity of the object,
;; meaning that value(DTSTART) == DATE and value(DTEND) == DATE-TIME
;; are possibilities, which other parts of the code will crash on.
;; TODO
;; since we are feeding user input into this it really should be fixed.
(define-public (sxcal->vcomponent sxcal)
  (define type (symbol-upcase (car sxcal)))
  (define component (make-vcomponent type))

  (awhen (assoc-ref sxcal 'properties)
         ;; Loop over multi valued fields, creating one vline
         ;; for every value. So
         ;;     KEY;p=1:a,b
         ;; would be expanded into
         ;;     KEY;p=1:a
         ;;     KEY;p=1:b
         (for property in it
              (match property
                ;; TODO request-status

                [(tag ('parameters parameters ...)
                      (type value ...) ...)
                 (let ((params (handle-parameters parameters))
                       (tag* (symbol-upcase tag)))
                   (for (type value) in (zip type value)
                        ;; ignore empty fields
                        ;; mostly for <text/>
                        (unless (null? value)
                          (set! (prop* component tag*)
                            (make-vline tag*
                                        (handle-tag
                                          tag (handle-value type params value))
                                        params)))))]

                [(tag (type value ...) ...)
                 (for (type value) in (zip type value)
                      ;; ignore empty fields
                      ;; mostly for <text/>
                      (unless (null? value)
                        (let ((params (make-hash-table))
                              (tag* (symbol-upcase tag)))
                          (set! (prop* component tag*)
                            (make-vline tag*
                                        (handle-tag
                                         tag (let ((v (handle-value type params value)))
                                               ;; TODO possibly more list fields
                                               (if (eq? tag 'categories)
                                                   (string-split v #\,)
                                                   v)))
                                        params)))))])))

  ;; children
  (awhen (assoc-ref sxcal 'components)
         (for child in (map sxcal->vcomponent it)
              (add-child! component child)))

  component)
