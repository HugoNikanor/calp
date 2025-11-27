(define-module (vcomponent media-type application calendar+json output)
  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module (srfi srfi-71)
  :use-module (vcomponent)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent media-type common)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (hnh util object)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (web uri)
  :export (serialize/object
           serializers)
  )

(define (list->vector/1 lst)
  (if (null? (cdr lst))
      (car lst)
      (list->vector lst)))

(define (recur-rule->scm-json _ record)
  (record->list/filtered
   (lambda (key value)
     (if (or (not value)
             (and (eq? key 'interval) (= value 1))
             (and (eq? key 'wkst) (= value mon))
             )
         #f
         (cons (case key
                 ((recur-count) 'count)
                 (else key))
               (case key
                 ((freq recur-count interval)
                  value)
                 ((bysecond byminute byhour bymonthday byyearday byweekno bymonth bymonthpos bysetpos)
                  (list->vector/1 value))
                 ((wkst)
                  (weekday->symbol value))
                 ((until)
                  (cond (value date?     => date->string)
                        (value datetime? => datetime->string)
                        (else (scm-error
                               'misc-error "recur-rule->scm-json"
                               "Unexpected value in until field of recurrence rule: ~s"
                               (list value) #f))))
                 ((byday)
                  (list->vector/1 (map byday->string (byday record))))
                 (else (scm-error 'misc-error "recur-rule->scm-json"
                                  "Unknown key: ~s"
                                  (list key) #f))))))
   record))

(define (request-status->scm-json _ status)
  (define code (string-join (map number->string (statcode status)) "."))

  (if (extdata status)
      (vector code (statdesc status) (extdata status))
      (vector code (statdesc status))))


(define serializers
  (make-parameter
   (list (cons (@ (scheme base) bytevector?)
               (lambda (params v)
                 (values
                  ((@ (base64) bytevector->base64-string) v)
                  (table-put params 'ENCODING "BASE64"))
                 ))
         (cons boolean? (lambda (_ v) v))
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? (lambda (_ v) (uri->string v)))
         (cons date? (lambda (_ v) (date->string v)))
         (cons datetime? (serialize-datetime "~Y-~m-~dT~H:~M:~S~Z"))
         (cons duration? (lambda (_ v) (duration->string v)))
         (cons number? (lambda (_ v) v))
         (cons period?
               (lambda (p v)
                 (let ((start end params (serialize-period p v "~Y-~m-~dT~H:~M:~S~Z")))
                   (values (vector start end)
                           params))))
         (cons recur-rule? recur-rule->scm-json)
         (cons string? (lambda (_ v) v))
         ;; TODO timezone
         (cons time? (lambda (_ v) (time->string v)))
         (cons timespec?
               ;; NOTE this is identical to the one for text/calendar
               (lambda (_ v)
                 (string-append
                  (symbol->string (timespec-sign v))
                  (let ((t (timespec-time v)))
                    (time->string t (if (zero? (second t))
                                        "~H:~M" "~H:~M:~S"))))))

         (cons geo? (lambda (_ v) (vector (geo-latitude v) (geo-longitude v))))

         (cons vcalendar-version? (lambda (_ v) (vcalendar-version->string v)))

         (cons request-status? request-status->scm-json)
         (cons unknown? (lambda (_ v) (from-unknown v)))

         )))

(define (value->scm-json parameters value)
  (cond ((predicate-list-get (serializers) value)
         => (lambda (serializer) (serializer parameters value)))
        (else
         (scm-error 'misc-error "value->scm-json"
                    "No json serializer for ~s"
                    (list value) #f))))

(define (serialize/object component)
  (typecheck component vcomponent?)

  (parameterize ((apparent-types (cons* (cons geo? 'FLOAT)
                                        (cons request-status? 'TEXT)
                                        (cons vcalendar-version? 'TEXT)
                                        (apparent-types))))

    (vector (string-downcase (symbol->string (type component)))
            (list->vector
             (concatenate
              (for (key . vlines) in (table->list (vcomponent-properties component))
                   ;; TODO group vlines by identical parameters
                   (for vline in vlines
                        (define value (vline-value vline))
                        (define in-params (vline-parameters vline))
                        (call-with-values
                            (lambda () (value->scm-json in-params value))
                          (lambda* (serialized optional: (out-params in-params))
                            (vector (-> key symbol->string string-downcase)
                                    (map (lambda (p) (modify p car* (compose string-downcase
                                                                        symbol->string)))
                                         (table->list out-params))
                                    (cond ((apparent-type value)
                                           => (compose string-downcase symbol->string))
                                          ((and (unknown? value)
                                                (unknown-type value))
                                           => string-downcase)
                                          (else "unknown"))
                                    serialized
                                    )))))))
            (list->vector (map serialize/object (vcomponent-children component))))))
