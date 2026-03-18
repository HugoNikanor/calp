(define-module (vcomponent media-type text calendar output)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent media-type common)
  :use-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module (hnh util optional)
  :use-module ((hnh util object) :select (record->list/filtered))
  :use-module (hnh util lens)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type version)
  :use-module (vcomponent type utc-offset)
  :use-module (vcomponent type unknown)
  :use-module (datetime)
  :use-module (web uri)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (ice-9 format)
  :use-module (base64)
  :export (vcomponent->icalendar
           serializers
           recur-rule->rrule-string
           escape-chars
           icalendar-wrap-length))

(define-once icalendar-wrap-length
  (make-parameter 70))

(define* (vcomponent->icalendar component optional: (port (current-output-port)))
  (typecheck component vcomponent?)
  (typecheck port port?)

  (with-output-to-port port
    (lambda ()
      (format #t "BEGIN:~a\r\n" (type component))
      (table->list (vcomponent-properties component) vline*->string)
      (map vcomponent->icalendar (vcomponent-children component))
      (format #t "END:~a\r\n" (type component)))))


(define (vline*->string key vlines)
  (typecheck key symbol?)
  (typecheck vlines (list-of vline?))
  ;; TODO if multi-valued-property, `(group-by (table-equal? (vline-parameters)))`
  ;; This can't work with the current implementation, since vline->string doesn't handle lists.
  (for vline in vlines
       (display (icalendar-linewrap (vline->string key vline)))
       (display "\r\n")))





(define (recur-rule->rrule-string _ rrule)

  (define (field->string field value)
    (case field
      [(wkst)
       (symbol->string (weekday->symbol value))]
      [(byday)
       (string-join (map byday->string value) ",")]
      [(freq recur-count interval)
       (format #f "~a" value)]
      [(until)
       (if (date? value)
           (date->string value "~Y~m~d")
           (datetime->string value "~Y~m~dT~H~M~S~Z"))]
      [else (format #f "~{~a~^,~}" value)]))

  (string-join
   (record->list/filtered
    (lambda (k v)
      (if (or (not v)
              (and (eq? k 'interval) (= v 1))
              (and (eq? k 'wkst) (= v mon)))
          #f
          (string-append
           (case k
             ((recur-count) "COUNT")
             (else (string-upcase (symbol->string k))))
           "=" (field->string k v))))
    rrule)
   ";"))

(define (escape-chars str)
  (define (escape char)
    (string #\\ char))
  (string-concatenate
   (map (lambda (c)
          (case c
            ((#\newline) (escape #\n))
            ((#\, #\; #\\) => escape)
            (else => string)))
        (string->list str))))


(define-once serializers
  (make-parameter
   (list (cons (@ (scheme base) bytevector?)
               (lambda (params v)
                 (values (bytevector->base64-string v)
                         (table-put params 'ENCODING "BASE64"))))
         (cons boolean? (lambda (_ v) (if v "TRUE" "FALSE")))
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? (lambda (_ v) (uri->string v)))
         (cons date? (lambda (_ v) (date->string v "~Y~m~d")))
         (cons datetime? (serialize-datetime "~Y~m~dT~H~M~S~Z"))
         (cons duration? (lambda (_ v) (duration->string v)))
         ;; Used for both FLOAT and INTEGER
         (cons number? (lambda (_ v) (number->string v)))
         (cons period?
               (lambda (p v)
                 ;; TODO TZID MUST be included here
                 (let ((start end params (serialize-period p v "~Y~m~dT~H~M~S~Z")))
                   (values (format #f "~a/~a" start end)
                           params))))
         (cons recur-rule? recur-rule->rrule-string)
         (cons string? (lambda (_ v) (escape-chars v)))
         ;; TODO TODO timezone
         (cons time? (lambda (_ v) (time->string v "~H~M~S")))
         (cons utc-offset? (lambda (_ v) (utc-offset->string v colon: "")))
         (cons unknown?
               (lambda (p v)
                 (values (from-unknown v)
                         ;; unknown-value is ALWAYS a string, and strings
                         ;; don't have a type indicator here
                         (if (and (unknown-type v)
                                  (not (string=? "TEXT" (unknown-type v))))
                             (table-put p 'VALUE (unknown-type v))
                             p)))))))

(define (ics-serialize parameters obj)
  (cond ((predicate-list-get (serializers) obj)
         => (lambda (serializer) (serializer parameters obj)))
        (else (scm-error 'misc-error "serialize"
                         "Unknown type stored in vline: ~s, failed to serialize ~s"
                         (list obj (length (serializers))) #f))))



;; Fold long lines to limit width.
;; Since this works in characters, but ics works in bytes
;; this will overshoot when faced with multi-byte characters.
;; But since the line wrapping is mearly a recomendation it's
;; not a problem.
;; Setting the wrap-len to slightly lower than allowed also help
;; us not overshoot.
(define* (icalendar-linewrap string key: wrap-len)
  (let ((wrap-len (or wrap-len (icalendar-wrap-length))))
    (cond [(< wrap-len (string-length string))
           (format #f "~a\r\n ~a"
                   (string-take string wrap-len)
                   (icalendar-linewrap (string-drop string wrap-len)))]
          [else string])))

(define (quote-parameter-value str)
  (cond
   ((string-index
     str
     (-> char-set:iso-control
         (char-set-delete #\tab)
         (char-set-adjoin #\")))
    => (lambda (idx)
         (scm-error 'misc-error "vline->string"
                    "Unrepresentable character present in parameter value: ~s, ~s"
                    (list str (string-ref str idx))
                    #f)))

   ((string-index str (char-set #\; #\: #\,))
    (format #f "\"~a\"" str))

   (else str)))


(define (vline->string key vline)
  (typecheck key symbol?)
  (typecheck vline vline?)

  (define v (vline-value vline))

  (with-output-to-string
    (lambda ()
      (display (-> key symbol->string string-upcase))
      (case key
        ;; TODO parameters for the special avlues
        ((GEO) (format #t ":~a;~a" (geo-latitude v) (geo-longitude v)))

        ((VERSION)
         (display ":")
         (display (string-join (map escape-chars (string-split (vcalendar-version->string v) #\;))
                               ";")))

        ((REQUEST-STATUS)
         (format #t ":~a;~a" (string-join (map number->string (statcode v)) ".")
                 (escape-chars (statdesc v)))
         (cond ((extdata v)
                => (lambda (v) (format #t ";~a" (escape-chars v))))))

        (else
         (call-with-values (lambda () (ics-serialize (vline-parameters vline) v))
           (lambda* (serialized optional: (params (vline-parameters vline)))
             (table->list
              ;; TODO I think I do `modify` here, to handle cases where an unknown type is passed through the system.
              ;; TODO ensure I actually handle unknown types correctly at the parse point, and rewrite this comment to match
              (modify params
                      (table-focus 'VALUE)
                      (lambda (specified)
                        (let ((apparent (apparent-type v)))
                          (if (eq? apparent (or (default-type key) 'TEXT))
                              (nothing)
                              (cond (apparent => (compose just symbol->string))
                                    (else specified))))))
              (lambda (key value)
                (format #t ";~a=~a" key (quote-parameter-value value))))

             (format #t ":~a" serialized)
             )))))))
