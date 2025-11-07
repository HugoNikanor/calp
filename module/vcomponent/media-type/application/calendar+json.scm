(define-module (vcomponent media-type application calendar+json)
  :use-module ((srfi srfi-1) :select (concatenate))
  :use-module (vcomponent)
  :use-module (vcomponent media-type)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type utc-offset)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (datetime)
  :use-module (web uri)
  :export ((jcal-format . format)))

(catch 'misc-error
  (lambda ()
    (use-modules (json))
    (provide 'formats-jcal))
  (lambda args 'no-op))

(define (json->vcomponent port)
  ;; TODO implement this
  (throw 'not-implemented)
  )

(define (recur-rule->scm-json record)
  ;; TODO take a look at the map-fields defined in (vcomponent type recurrence internal)
  `(,@(when (freq record) `((freq . ,(freq record))))
    ,@(when (until record)
        (list
         (cons 'until
               (cond ((until record) date?     => date->string)
                     ((until record) datetime? => datetime->string)
                     (else (scm-error
                            'misc-error "recur-rule->scm-json"
                            "Unexpected value in until field of recurrence rule: ~s"
                            (list (until record)) #f))))))
    ,@(when (count record) `((count . ,(count record))))
    ,@(when (interval record) `((interval . ,(interval record))))

    ,@(when (bysecond record) (list (cons 'bysecond (list->vector (bysecond record)))))
    ,@(when (byminute record) (list (cons 'byminute (list->vector (byminute record)))))
    ,@(when (byhour   record) (list (cons 'byhour   (list->vector (byhour record)))))

    ,@(when (byday record)
        (list
         (cons 'byday
               (list->vector
                (map (lambda (x) (format #f "~a~a"
                                    (or (car x) "")
                                    (weekday->symbol (cdr x))))
                     (byday record))))))

    ,@(when (bymonthday record) (list (cons 'bymonthday (list->vector (bymonthday record)))))
    ,@(when (byyearday  record) (list (cons 'byyearday  (list->vector (byyearday  record)))))
    ,@(when (byweekno   record) (list (cons 'byweekno   (list->vector (byweekno   record)))))
    ,@(when (bymonth    record) (list (cons 'bymonth    (list->vector (bymonth    record)))))
    ,@(when (bysetpos   record) (list (cons 'bysetpos   (list->vector (bysetpos   record)))))

    ,@(when (wkst record) `((wkst . ,(weekday->symbol (wkst record)))))))

(define (request-status->scm-json status)
  (define code (string-join (map number->string (statcode status)) "."))

  (if (extdata status)
      (vector code (statdesc status) (extdata status))
      (vector code (statdesc status))))


(define serializers
  (make-parameter
   (list (cons (@ (scheme base) bytevector?)
               (@ (base64) bytevector->base64-string))
         (cons boolean? identity)
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? uri->string)
         (cons date? date->string)
         ;; TODO timezone
         (cons datetime? datetime->string)
         (cons duration? duration->string)
         (cons number? identity)
         (cons period?
               (lambda (v)
                 (vector (datetime->string (period-start v))
                         (cond ((period-end v) datetime? => datetime->string)
                               ((period-end v) duration? => duration->string)
                               (else (scm-error 'misc-error "json period serializer"
                                                "Bad data in period-end: ~s"
                                                (list (period-end v))
                                                #f))))))
         (cons recur-rule? recur-rule->scm-json)
         (cons string? identity)
         ;; TODO timezone
         (cons time? time->string)
         (cons utc-offset? (lambda (v) (utc-offset->string v "~H:~M:~S")))

         (cons geo? (lambda (v) (vector (geo-latitude v) (geo-longitude v))))

         (cons vcalendar-version?
               (lambda (v)
                 (string-append
                  (cond ((version-min v) => (lambda (v) (format #f "~a;" v)))
                        (else ""))
                  (version-max v))))

         (cons request-status? request-status->scm-json)
         (cons unknown? from-unknown)

         )))

(define (value->scm-json value)
  (let loop ((pairs (serializers)))
   (cond ((null? pairs) #f
          ;; TODO do something here
          )
         (((caar pairs) value)
          ((cdar pairs) value))
         (else (loop (cdr pairs))))))

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
                        (vector (-> key symbol->string string-downcase)
                                (table->list (vline-parameters vline))
                                (-> (or (apparent-type (vline-value vline)) 'UNKNOWN)
                                    symbol->string string-downcase)
                                (value->scm-json (vline-value vline)))))))
            (list->vector (map serialize/object (vcomponent-children component))))))

(define (vcomponent->json component port)
  (scm->json
   (serialize/object component)
   port))

(define jcal-format
  (calendar-data-format
   serializer: vcomponent->json
   parser: json->vcomponent))
