(define-module (vcomponent media-type application calendar+xml output)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (vcomponent)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type utc-offset)
  :use-module (ice-9 match)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (calp translation)
  :use-module (calp namespaces)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (web uri)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (vcomponent type unknown)
  :use-module (base64)
  :export (vcomponent->sxcal
           serializers

           recur-rule->sxml
           ))

;;; TODO why isn't `apparent-type` used?


(define (recur-rule->sxml rrule)
  (apply
   (xml xcal 'recur)
   (concatenate
    (record->list
     (lambda (field value)
       (cond [(or (not value)
                  (and (eq? field 'interval) (= 1 value))
                  (and (eq? field 'wkst) (= mon value)))
              '()]
             [(eq? 'until field)
              (list
               ((xml xcal 'until)
                (if (date? value)
                    (date->string value "~Y-~m-~d")
                    (datetime->string
                     value "~Y-~m-~dT~H:~M:~S~Z"))))]

             [(eq? 'byday field)
              (map (xml xcal field)
                   (map byday->string value))]

             [(string=? "by" (substring (symbol->string field)
                                           0 2))
              (map (xml xcal field)
                   (map number->string value))]

             [(memv field '(wkst))
              (list ((xml xcal field)
                     (symbol->string (weekday->symbol value))))]

             [(memv field '(freq))
              (list ((xml xcal field)
                     (symbol->string value)))]

             [(memv field '(recur-count))
              (list ((xml xcal 'count)
                     (number->string value)))]

             [(memv field '(interval))
              (list ((xml xcal field)
                     (number->string value)))]

             [else
              (scm-error 'misc-error "recur-rule->sxml"
                         "Unknown key: ~s"
                         (list field) #f)]))

     rrule))))

;;; TODO this is basically (@ (vcompoanent media-type common) serialize-datetime),
;;; But with an xml wrapper. Use that one, and only handle xml wrapping here
(define (datetime->sxml parameters dt)
  (define (->xml dt)
    (list ((xml xcal 'date-time) (datetime->string dt))))
  (cond ((not (tz dt)) (->xml dt))
        ((string=? "UTC" (tz dt)) (->xml dt))
        (else
         (values (->xml (tz dt #f))
                 (table-put parameters 'TZID (tz dt))))))

;;; TODO simplify this by using serialize-period instead
(define (period->sxml params v)
  (call-with-values (lambda () (datetime->sxml params (period-start v)))
    (lambda* (serialized optional: (params params))
      (values
       (list
        ((xml xcal 'period)
         ;; NOTE make datetime->sxml return a more bare value
         (apply (xml xcal 'start) (map xml-text-content serialized))
         (if (datetime? (period-end v))
             ((xml xcal 'end)
              (datetime->string
               (period-end v)
               ;; NOTE this assumes that ~Z only outputs "Z" or "".
               "~Y-~m-~dT~H:~M:~S~Z"))
             ((xml xcal 'duration)
              (duration->string (period-end v))))))
       params))))


(define-once serializers
  (make-parameter
   (list (cons (@ (scheme base) bytevector?)
               (lambda (params v)
                 (values (list ((xml xcal 'binary) (bytevector->base64-string v)))
                         (table-put params 'ENCODING "BASE64"))))
         (cons boolean? (lambda (_ v) (list ((xml xcal 'boolean) (if v "true" "false")))))
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? (lambda (_ v) (list ((xml xcal 'uri) (uri->string v)))))
         (cons date? (lambda (_ v) (list ((xml xcal 'date) (date->string v)))))
         (cons datetime? datetime->sxml)

         (cons duration? (lambda (_ v) (list ((xml xcal 'duration) (duration->string v)))))
         (cons exact-integer? (lambda (_ v) (list ((xml xcal 'integer) (number->string v)))))
         (cons number? (lambda (_ v) (list ((xml xcal 'float) (number->string v)))))
         (cons period? period->sxml)
         (cons recur-rule? (lambda (_ v) (list (recur-rule->sxml v))))
         (cons string? (lambda (_ v) (list ((xml xcal 'text) v))))
         (cons time? (lambda (_ v) (list ((xml xcal 'time) (time->string v)))))
         (cons utc-offset? (lambda (_ v) (list ((xml xcal 'utc-offset)
                                           (utc-offset->string v colon: ":")))))


         ;;
         (cons geo?
               (lambda (_ o)
                 (list
                  ((xml xcal 'latitude)  (number->string (geo-latitude o)))
                  ((xml xcal 'longitude) (number->string (geo-longitude o))))))
         ;; RFC 6321 specifies this as the only valid value
         ;; TODO actually serialize what we have instead
         (cons vcalendar-version?
               (lambda (_ v) (list ((xml xcal 'text) (vcalendar-version->string v)))))
         (cons request-status?
               (lambda (_ o)
                 (cons*
                  ((xml xcal 'code) (string-join (map number->string (statcode o)) "."))
                  ((xml xcal 'description) (statdesc o))
                  (cond ((extdata o) => (lambda (data) (list ((xml xcal 'data) data))))
                        (else '())))))

         (cons unknown?
               (lambda (_ o)
                 (list
                  ((xml xcal (cond ((unknown-type o)
                                    => (compose string->symbol string-downcase))
                                   (else 'unknown)))
                   (from-unknown o))))))))


;; Generate a complete xml representation of a given vline
(define (vline->value-tag key vline)
  (typecheck key symbol?)
  (typecheck vline vline?)

  (call-with-values
      (lambda ()
        (cond ((predicate-list-get (serializers) (vline-value vline))
               => (lambda (serializer)
                    (serializer (vline-parameters vline)
                                (vline-value vline))))
              (else (scm-error 'misc-error "vline->value-tag"
                               "Unknown type stored in vline: ~s, failed to serialize"
                               (list vline) #f))))

    (lambda* (contents optional: (parameters (vline-parameters vline)))
      (if (table-empty? parameters)
          (apply (xml xcal (downcase-symbol key))
                 contents)
          (apply (xml xcal (downcase-symbol key))
                 (parameters-tag parameters)
                 contents)))))



;; Generate an XML xcal:parameters tag from a table
;; (table-of string?) -> #<xml parameters>
(define (parameters-tag parameters)
  (apply (xml xcal 'parameters)
         (table->list parameters
                      (lambda (tag value)
                        ((xml xcal (downcase-symbol tag))
                         ;; TODO parameter types!!!! (rfc6321 3.5.)
                         ((xml xcal 'text) value))))))

(define (vcomponent->sxcal component)
  (typecheck component vcomponent?)

  (apply (xml xcal (downcase-symbol (type component)))
         (apply (xml xcal 'properties)
                (concatenate
                 (for (key . value) in (table->list (vcomponent-properties component))
                      (map (lambda (v) (vline->value-tag key v))
                           value))))

         (if (null? (vcomponent-children component))
             '()
             (list
              (apply (xml xcal 'components)
                     (map vcomponent->sxcal (vcomponent-children component)))))))
