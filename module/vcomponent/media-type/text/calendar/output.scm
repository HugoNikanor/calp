(define-module (vcomponent media-type text calendar output)
  :use-module (vcomponent media-type types)
  :use-module (vcomponent)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util table)
  :use-module (hnh util optional)
  :use-module (hnh util lens)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type period)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type utc-offset)
  :use-module (vcomponent type version)
  :use-module (vcomponent type unknown)
  :use-module (datetime)
  :use-module (web uri)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export (vcomponent->icalendar
           serializers))


(define* (vcomponent->icalendar component optional: (port (current-output-port)))
  (typecheck component vcomponent?)
  (typecheck port port?)

  (with-output-to-port port
    (lambda ()
      (format #t "BEGIN:~a\r\n" (type component))
      (map vline*->string (table->list (vcomponent-properties component)))
      (map vcomponent->icalendar (vcomponent-children component))
      (format #t "END:~a\r\n" (type component)))))


(define (vline*->string pair)
  (typecheck pair (pair-of symbol? (list-of vline?)))
  ;; TODO if multi-valued-property, `(group-by (table-equal? (vline-parameters)))`
  ;; This can't work with the current implementation, since vline->string doesn't handle lists.
  (for vline in (cdr pair)
       (display (icalendar-linewrap (vline->string (car pair) vline)))
       (display "\r\n")))


(define (period->string v)
  (format #f "~a/~a"
          (datetime->string (period-start v)
                            "~Y~m~dT~H~M~S~Z")
          (if (datetime? (period-end v))
              (datetime->string (period-end v)
                                "~Y~m~dT~H~M~S~Z")
              (duration->string (period-end v)))) )

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
               (@ (base64) bytevector->base64-string))
         (cons boolean? (lambda (v) (if v "TRUE" "FALSE")))
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? uri->string)
         (cons date?
               (lambda (v) (date->string v "~Y~m~d")))
         ;; TODO TODO timezone
         (cons datetime?
               ;; NOTE We really should output TZID from param here, but
               ;; we first need to change so these writers can output
               ;; parameters.
               (lambda (v) (datetime->string v "~Y~m~dT~H~M~S~Z")))
         (cons duration? duration->string)
         ;; Used for both FLOAT and INTEGER
         (cons number? number->string)
         (cons period? period->string)
         (cons recur-rule? recur-rule->rrule-string)
         (cons string? escape-chars)
         ;; TODO TODO timezone
         (cons time? (lambda (v) (time->string v "~H~M~S")))
         (cons utc-offset? (lambda (v) (utc-offset->string v "~H~M~S")))
         (cons unknown? from-unknown))))

(define (serialize obj)
  (let loop ((pairs (serializers)))
    (cond ((null? pairs) #f)
          (((caar pairs) obj) ((cdar pairs) obj))
          (else (loop (cdr pairs))))))



;; Fold long lines to limit width.
;; Since this works in characters, but ics works in bytes
;; this will overshoot when faced with multi-byte characters.
;; But since the line wrapping is mearly a recomendation it's
;; not a problem.
;; Setting the wrap-len to slightly lower than allowed also help
;; us not overshoot.
(define* (icalendar-linewrap string key: (wrap-len 70))
  (cond [(< wrap-len (string-length string))
         (format #f "~a\r\n ~a"
                 (string-take string wrap-len)
                 (icalendar-linewrap (string-drop string wrap-len)))]
        [else string]))


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
         (cond ((version-min v)
                => (lambda (min) (format #t "~a;" (escape-chars min)))))
         (display (escape-chars (version-max v))))

        ((REQUEST-STATUS)
         (format #t ":~a;~a" (statcode v)
                 (escape-chars (statdesc v)))
         (cond ((extdata v)
                => (lambda (v) (format #t ";~a" (escape-chars v))))))

        (else
         (let ((serialized (serialize v)))

           (unless serialized
             (scm-error 'misc-error "vline->string"
                        "Unknown type stored in vline: ~s, failed to serialize"
                        (list vline) #f))

           ;; TODO quote:ing
           (map (lambda (pair) (format #t ";~a=~a" (car pair) (cdr pair)))
                (table->list
                 (modify (vline-parameters vline)
                         (table-focus 'VALUE)
                         (lambda (specified)
                           (let ((apparent (apparent-type v)))
                             (if (eq? apparent (or (default-type key) 'TEXT))
                                 (nothing)
                                 (cond (apparent => just)
                                       (else specified))))))))

           (format #t ":~a" serialized)
           )))))

  ;; If we have alternatives, splice them in here.
  ;; TODO -X-HNH-ALTERNATIVES isn't a thing anymore
  #;
  (cond [(prop component '-X-HNH-ALTERNATIVES)
         => (lambda (alts) (hash-map->list (lambda (_ comp)
                                        (unless (eq? component comp)
                                          (component->ical-string comp)))
                                      alts))]))






