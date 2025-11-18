(define-module (vcomponent media-type application calendar+xml parse)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (vcomponent)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type recurrence parse)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type duration)
  :use-module (hnh util)
  :use-module (hnh util table)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :use-module (hnh util type)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (web uri)
  :use-module (base64)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module ((calp namespaces) :select (xcal))
  :use-module (ice-9 regex)
  :export (sxml->vcomponent
           parsers))


(define (sxml->parameters el)
  (fold (lambda (el params)
          ;; TODO namespaces
          (define parameter-name (xml-element-tagname el))
          ;; TODO parameter types! (rfc6321 3.5.)
          (table-put params (upcase-symbol parameter-name)
                     (xml-text-content el)))
        (table)
        (xml-element-children el)))

(define (sxml->recur els)
  (fold (lambda (el rule)
          (case (xml-element-tagname el)
            ((freq) (freq rule (string->symbol (xml-text-content el))))
            ((wkst) (wkst rule (-> el
                                   xml-text-content
                                   string->symbol
                                   rfc->datetime-weekday)))
            ((until)
             ;; TODO date values
             (until rule (string->datetime (xml-text-content el)
                                           "~Y-~m-~dT~H:~M:~S~Z")))
            ((count)
             (count rule (string->number (xml-text-content el))))
            ((interval)
             (interval rule (string->number (xml-text-content el))))


            ((bysecond byminute byhour bymonthday byyearday byweekno bymonth bysetpos)
             (define accessor
               (case (xml-element-tagname el)
                 ((bysecond) bysecond) ((byminute) byminute) ((byhour) byhour)
                 ((bymonthday) bymonthday) ((byyearday) byyearday)
                 ((byweekno) byweekno) ((bymonth) bymonth) ((bysetpos) bysetpos)))
             (accessor rule
                       (snoc (string->number (xml-text-content el))
                             (or (accessor rule) '()))))

            ((byday) (byday rule
                            (snoc (parse-day-spec (xml-text-content el))
                                  (or (byday rule) '()))))
            (else (scm-error 'misc-error "sxml->recur"
                             "" '() #f))))
        (recur-rule)
        els))

;;; TODO this is identical to the one in jcal
(define (parse-utc-offset s)
  (cond ((string-match "^([+-])([0-9]{2}):([0-9]{2})(:([0-9]{2}))?$" s)
         => (lambda (m)
              (timespec (time hour: (string->number (match:substring m 2))
                              minute: (string->number (match:substring m 3))
                              second: (cond ((match:substring m 5) => string->number)
                                            (else 0)))
                        (string->symbol (match:substring m 1))
                        ;; TODO is this correct?
                        'utc)))))

;;; Like `find`, but returns 2 values:
;;; - the found value
;;; - the rest of the list, in the same order as the source, with the found value removed.
(define (find/pop pred lst)
  (let loop ((lst lst)
             (visited '()))
    (cond ((null? lst) (values (nothing) (reverse visited)))
          ((pred (car lst))
           (values (just (car lst)) (append (reverse visited) (cdr lst))))
          (else (loop (cdr lst)
                      (cons (car lst) visited))))))

(define (sxml->period props v)
  (define start
    (xml-text-content
     (find-child ((xml xcal 'start))
                 (xml-element-children v))))

  (values
   (period
    start: (modify
            (string->datetime start "~Y-~m-~dT~H:~M:~S~Z")
            tz* (lambda (tz) (or tz (table-get props 'TZID))))
    end:
    (cond ((find-child ((xml xcal 'end))
                       (xml-element-children v))
           => (lambda (end)
                (modify
                 (string->datetime end "~Y-~m-~dT~H:~M:~S~Z")
                 tz* (lambda (tz) (or tz (table-get props 'TZID))))))
          ((find-child ((xml xcal 'duration))
                       (xml-element-children v))
           => (compose string->duration xml-text-content))
          (else (scm-error 'misc-error "sxml->period"
                           "No xcal:end or xcal:period element found"
                           '() #f))))
   (table-remove props 'TZID)))

(define (snoc x xs)
  (append xs (list x)))

(define-once parsers
  (make-parameter
   (alist->table
    (list
     (cons 'binary
           (lambda (params v)
             (values
              (case (string->symbol (or (table-get params 'ENCODING) "BASE64"))
                ((BASE64) (base64-string->bytevector
                           (string-delete char-set:whitespace (xml-text-content v))))
                (else => (lambda (enc) (scm-error 'misc-error "xcal-parser"
                                             "Unknown encoding of binary data: ~s"
                                             (list enc) #f))))
              (table-remove params 'ENCODING))))
     (cons 'boolean (lambda (_ v) (not (not (member (string-downcase (xml-text-content v))
                                               '("true" "1"))))))
     (cons 'cal-address (lambda (_ v) (string->uri (xml-text-content v))))
     (cons 'date (lambda (_ v) (string->date (xml-text-content v) "~Y-~m-~d")))
     (cons 'date-time
           (lambda (props value)
             ;; NOTE this is identical to the application/calendar+json one
             (values (modify (string->datetime (xml-text-content value)
                                               "~Y-~m-~dT~H:~M:~S~Z")
                             tz* (lambda (tz) (or tz (table-get props 'TZID))))
                     (table-remove props 'TZID))))

     (cons 'duration (lambda (_ v) (string->duration (xml-text-content v))))

     (cons 'float   (lambda (_ v) (string->number (xml-text-content v))))
     (cons 'integer (lambda (_ v) (string->number (xml-text-content v))))
     (cons 'period sxml->period)
     (cons 'recur (lambda (_ v) (sxml->recur (xml-element-children v))))
     (cons 'text (lambda (_ v) (xml-text-content v)))
     (cons 'time (lambda (_ v) (string->time (xml-text-content v)
                                        "~H:~M:~S")))
     (cons 'uri (lambda (_ v) (string->uri (xml-text-content v))))
     (cons 'utc-offset (lambda (_ v) (parse-utc-offset (xml-text-content v))))

     ))))


;;; Input:
;;;    <xcal:dtstart>
;;;      <xcal:parameters>
;;;        <xcal:tzid><xcal:text>Europe/Stockholm</xcal:text></xcal:tzid>
;;;      </xcal:parameters>
;;;      <xcal:date-time>2025-11-16T22:32:41</xcal:date-time>
;;;    </xcal:dtstart>
;;; output:
;;;    (vline value: (datetime date: #2025-11-16 time: #22:32:41 tz: "Europe/Stockholm"))
;;; namespace of all component is asumed to be xcal
(define (sxml->vlines el)

  (let ((m-params values
                  (find/pop (lambda (e) (tag-matches? e 'parameters xcal))
                            (xml-element-children el))))

    ;; - parse parameters
    (let ((params (sxml->parameters (unjust m-params ((xml xcal 'parameters))))))
      (map (lambda (type-el)
             (cond ((table-get (parsers) (xml-element-tagname type-el))
                    => (lambda (parser)
                         ;; - pass parameters and unparsed value tag to procedure
                         (call-with-values (lambda () (parser params type-el))
                           ;; - retrieve value (and optionall parameters) from procedure
                           (lambda* (result optional: (params params))
                             ;; - create vline object
                             (vline params: params value: result)))))
                   (else (scm-error 'misc-error "sxml->vlines"
                                    "No parser for ~s"
                                    (list type-el) #f))))
           values))))

(define (sxml->vcomponent/object data)
  (typecheck data xml-element?)

  (unless (eq? xcal (xml-element-namespace data))
    (scm-error 'misc-error "sxml->vcomponent/object"
               "Non xcal component given as object root: ~s"
               (list (xml-element-hash-key data)) #f))

  (vcomponent
   type: (upcase-symbol (xml-element-tagname data))
   properties:
   (cond ((find-child ((xml xcal 'properties))
                      (xml-element-children data))
          => (lambda (el)
               (fold
                (lambda (el props)
                  (if (eq? xcal (xml-element-namespace el))
                      (let ((values
                             (case (xml-element-tagname el)
                               ;; TODO vline parameters for the special types
                               ((geo)
                                (define (f x)
                                  (string->number
                                   (xml-text-content
                                    (find-child ((xml xcal x))
                                                (xml-element-children el)))))
                                (list
                                 (vline value:
                                        (geo y: (f 'latitude)
                                             x: (f 'longitude)))))
                               ((request-status)
                                (list
                                 (vline value:
                                        (request-status
                                         statcode: (map string->number
                                                        (string-split
                                                         (xml-text-content
                                                          (find-child ((xml xcal 'code))
                                                                      (xml-element-children el)))
                                                         #\.))
                                         statdesc: (xml-text-content
                                                    (find-child ((xml xcal 'description))
                                                                (xml-element-children el)))
                                         extdata: (and=> (find-child ((xml xcal 'data))
                                                                     (xml-element-children el))
                                                         xml-text-content)))))
                               ((version)
                                (list
                                 (vline
                                  value:
                                  (apply
                                   (case-lambda ((min max)
                                                 (vcalendar-version min: min max: max))
                                                ((max)
                                                 (vcalendar-version max: max)))
                                   (string-split (xml-text-content el) #\;)))))

                               (else
                                (sxml->vlines el)))))


                        (modify props (table-focus (upcase-symbol (xml-element-tagname el)))
                                (lambda (m) (just (append values (unjust m '()))))))

                      (modify props (table-focus 'XML)
                              (lambda (m)
                                (just
                                 (cons (vline value: el)
                                       (unjust m '())))))))
                (table)
                (xml-element-children el))))
         (else (table)))
   children: (cond ((find-child ((xml xcal 'components))
                                (xml-element-children data))
                    => (lambda (el) (map sxml->vcomponent/object
                                    (xml-element-children el))))
                   (else '()))))

(define (sxml->vcomponent data)
  (define root
    (cond ((xml-document? data)
           (xml-document-root data))
          ((xml-element? data)
           data)
          (else (scm-error 'misc-error "sxml->vcomponent"
                           "Non-xml document given: ~s"
                           (list data) #f))))

  (sxml->vcomponent/object
   (if (tag-matches? root 'icalendar xcal)
       (car (xml-element-children root))
       root)))
