(define-module (vcomponent media-type application calendar+json parse)
  :use-module ((srfi srfi-1) :select (fold))
  :use-module ((srfi srfi-43) :select (vector-fold))
  :use-module (srfi srfi-71)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (vcomponent)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (web uri)
  :use-module ((vcomponent type recurrence parse) :select (rfc->datetime-weekday parse-day-spec))
  :use-module (base64)
  :export (parse/component
           parsers)
  )

;;; TODO rename this function
(define (to-vector x)
  (if (vector? x)
      (vector->list x) (list x)))

(define (parse-recurrence-rule _ rrule)
  (fold (lambda (pair rule)
          (case (string->symbol (car pair))
            ((freq) (freq rule (string->symbol (cdr pair))))
            ((wkst) (wkst rule (rfc->datetime-weekday (string->symbol (cdr pair)))))
            ((until)
             ;; TODO date values
             (until rule (string->datetime (cdr pair) "~Y-~m-~dT~H:~M:~S~Z")))
            ((count)      (recur-count rule (cdr pair)))
            ((interval)   (interval   rule (cdr pair)))
            ((bysecond)   (bysecond   rule (to-vector (cdr pair))))
            ((byminute)   (byminute   rule (to-vector (cdr pair))))
            ((byhour)     (byhour     rule (to-vector (cdr pair))))
            ((bymonthday) (bymonthday rule (to-vector (cdr pair))))
            ((byyearday)  (byyearday  rule (to-vector (cdr pair))))
            ((byweekno)   (byweekno   rule (to-vector (cdr pair))))
            ((bymonth)    (bymonth    rule (to-vector (cdr pair))))
            ((bysetpos)   (bysetpos   rule (to-vector (cdr pair))))
            ((byday) (byday rule (map parse-day-spec (to-vector (cdr pair))))
             )))
        (recur-rule)
        rrule))

(define (parse-utc-offset _ s)
  (cond ((string-match "^([+-])([0-9]{2}):([0-9]{2})(:([0-9]{2}))?$" s)
         => (lambda (m)
              (timespec (time hour: (string->number (match:substring m 2))
                              minute: (string->number (match:substring m 3))
                              second: (cond ((match:substring m 5) => string->number)
                                            (else 0)))
                        (string->symbol (match:substring m 1))
                        ;; TODO is this correct?
                        'utc)))))

(define-once parsers
  (make-parameter
   (alist->table
    (list
     (cons 'BINARY (lambda (props v)
                     (values
                      (case (string->symbol (or (table-get props 'ENCODING) "BASE64"))
                        ((BASE64) (base64-string->bytevector v))
                        (else => (lambda (enc) (scm-error 'misc-error "json-parser"
                                                     "Unknown encoding of binary data: ~s"
                                                     (list enc) #f))))
                      (table-remove props 'ENCODING))))
     (cons 'BOOLEAN (lambda (_ v) v))
     (cons 'CAL-ADDRESS (lambda (_ v) (string->uri v)))
     (cons 'DATE (lambda (_ d) (string->date d "~Y-~m-~d")))
     (cons 'DATE-TIME
           ;; TODO this is identical to (@ (vcomponent media-type text calendar parse) parse-datetime)
           (lambda (props value)
             (values (modify (string->datetime value "~Y-~m-~dT~H:~M:~S~Z")
                             tz* (lambda (tz) (or tz (table-get props 'TZID))))
                     (table-remove props 'TZID))))
     (cons 'DURATION (lambda (_ v) (string->duration v)))
     (cons 'FLOAT (lambda (_ v) v))
     (cons 'INTEGER (lambda (_ v) v))
     (cons 'PERIOD
           (match-lambda*
             ((props #(start end))
              (values (period start: (modify (string->datetime start "~Y-~m-~dT~H:~M:~S~Z")
                                             tz* (lambda (tz) (or tz (table-get props 'TZID))))
                              end: (if (string-match "^[+-]?P" end)
                                       (string->duration end)
                                       (modify (string->datetime end  "~Y-~m-~dT~H:~M:~S~Z")
                                               tz* (lambda (tz) (or tz (table-get props 'TZID))))))
                      (table-remove props 'TZID)))))
     (cons 'RECUR parse-recurrence-rule)
     (cons 'TEXT (lambda (_ v) v))
     ;; TODO timezone
     (cons 'TIME (lambda (_ t) (string->time t)))
     (cons 'URI (lambda (_ v) (string->uri v)))
     (cons 'UTC-OFFSET parse-utc-offset)))))

;;; jCal serialization of value, to our internal representation
(define (parse-value key type params value)
  (cond
   ((eq? key 'GEO)
    (vline params: params value: (match value (#(lat lon) (geo x: lon y: lat)))))

   ((eq? key 'REQUEST-STATUS)
    (vline
     params: params
     value: (apply (lambda* (code desc optional: data)
                     (request-status
                      statcode: (map string->number (string-split code #\.))
                      statdesc: desc
                      extdata: data))
                   (vector->list value))))

   ((eq? key 'VERSION)
    (vline
     params: params
     value: (apply
             (case-lambda ((min max)
                           (vcalendar-version min: min max: max))
                          ((max)
                           (vcalendar-version max: max)))
             (string-split value #\;))))

   ((table-get (parsers) type)
    => (lambda (p) (call-with-values (lambda () (p params value))
                (lambda* (value optional: (params params))
                  (vline params: params value: value)))))

   (else
    (vline params: params
           value: (unknown value
                           (and (not (eq? 'UNKNOWN type))
                                (-> type symbol->string string-upcase)))))))

(define (parse-params params)
  (alist->table (map (lambda (p) (modify p car* (compose string->symbol string-upcase)))
                     params)))

(define (parse/component data)
  (match data
    (#(type properties children)
     (modify
      (vcomponent type: (-> type string-upcase string->symbol)
                  children: (map parse/component (vector->list children)))
      vcomponent-properties*
      (lambda (prop-table)
        (vector-fold
         (match-lambda*
           ((_ tbl #(field params type values ...))
            (let ((key (-> field string-upcase string->symbol)))
              (modify tbl (table-focus key)
                      (lambda (m)
                        (just
                         (append
                          (map (lambda (v) (parse-value
                                       key
                                       (-> type string-upcase string->symbol)
                                       (parse-params params)
                                       v))
                               values)
                          (unjust m '()))))))))
         prop-table properties))))))
