(define-module (vcomponent media-type application calendar+xml output)
  :use-module (hnh util)
  :use-module (hnh util exceptions)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (vcomponent)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent media-type application calendar+xml types)
  :use-module (ice-9 match)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (srfi srfi-1)
  :use-module (calp translation)
  :use-module (calp namespaces)
  :use-module (sxml namespaced)
  :use-module (sxml namespaced util)
  :use-module (web uri)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (vcomponent type unknown)
  :export (vcomponent->sxcal))

;;; TODO why isn't `apparent-type` used?

(define serializers
  (make-parameter
   (list (cons (@ (scheme base) bytevector?) 'TODO)
         (cons boolean? (lambda (v) (list ((xml xcal 'boolean) (if v "true" "false")))))
         ;; Used for both URI and CAL-ADDRESS
         (cons uri? (compose list (xml xcal 'uri) uri->string))
         (cons date? (compose list (xml xcal 'date) date->string))
         (cons datetime? (compose list (xml xcal 'date-time) datetime->string))
         (cons duration? (compose list (xml xcal 'duration) duration->string))
         (cons exact-integer? (compose list (xml xcal 'integer) number->string))
         (cons number? (compose list (xml xcal 'float) number->string))
         (cons period? 'TODO)
         (cons recur-rule? (compose list (@@ (vcomponent type recurrence internal)
                                             recur-rule->rrule-sxml)))
         (cons string? (compose list (xml xcal 'text)))
         (cons time? (compose list (xml xcal 'time) time->string))
         (cons timespec?
               (lambda (v) (list
                       ((xml xcal 'utc-offset)
                        (string-append
                         (symbol->string (timespec-sign v))
                         (time->string (timespec-time v) "~H:~M:~S"))))))


         ;;
         (cons geo?
               (lambda (o)
                 (list
                  ((xml xcal 'geo)
                   ((xml xcal 'latitude)  (geo-latitude o))
                   ((xml xcal 'longitude) (geo-longitude o))))))
         ;; RFC 6321 specifies this as the only valid value
         ;; TODO actually serialize what we have instead
         (cons vcalendar-version? (const (list ((xml xcal 'version) "2.0"))))
         (cons request-status?
               (lambda (o)
                 `(
                   ,((xml xcal 'code) (string-join (statcode o) "."))
                   ,((xml xcal 'description) (statdesc o))
                   ,@(cond ((extdata o) => (lambda (data) (list ((xml xcal 'data) data))))
                           (else '())))))

         ;; TODO unkown type wrapper?
         (cons unknown? (compose list from-unknown)))))


;; Generate a complete xml representation of a given vline
(define (vline->value-tag key vline)
  (typecheck key symbol?)
  (typecheck vline vline?)

  (apply
   (xml xcal (downcase-symbol key))
   ;; TODO make this conditional
   (parameters-tag (vline-parameters vline))
   (let loop ((pairs (serializers)))
     (cond ((null? pairs) #f
                                        ; TODO do something here
            )
           (((caar pairs) (vline-value vline))
            ((cdar pairs) (vline-value vline)))
           (else (loop (cdr pairs)))))))



;; Generate an XML xcal:parameters tag from a table
;; (table-of string?) -> #<xml parameters>
(define (parameters-tag parameters)
  (apply (xml xcal 'parameters)
         (map (lambda (pair)
                (define-values (tag value) (car+cdr pair))

                (apply (xml xcal (downcase-symbol tag))
                       (map (lambda (v)
                              ;; TODO parameter types!!!! (rfc6321 3.5.)
                              ((xml xcal 'text) (->string v)))
                            value)))
              (table->list parameters))))

(define (vcomponent->sxcal component)
  (typecheck component vcomponent?)

  ((xml xcal (downcase-symbol (type component)))
   (apply (xml xcal 'properties)
          (concatenate
           (for (key . value) in (table->list (vcomponent-properties component))
                (map (lambda (v) (vline->value-tag key v))
                     value))))

   ;; TODO omit this if empty
   (apply (xml xcal 'components)
          (map vcomponent->sxcal (vcomponent-children component)))))
