(define-module (vcomponent parse component)
  :use-module (util)
  :use-module (util exceptions)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (vcomponent base)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (vcomponent parse types)
 )

(define-public (parse-calendar port)
  (parse (map tokenize (read-file port))))

;; port → (list string)
(define (read-file port)
 (let loop ((done '()))
   (let ((line (read-line port)))
     (if (eof-object? line)
         (reverse! done)
         (let ((line (string-trim-right line)))
           (loop
            (if (char=? #\space (string-ref line 0))
                ;; Line Wrapping
                ;; TODO if the line is split inside a unicode character
                ;; then this produces multiple broken unicode characters.
                ;; It could be solved by checking the start of the new line,
                ;; and the tail of the old line for broken char
                (cons (string-append (car done)
                                     (string-drop line 1))
                      (cdr done))
                (cons line done))))))))

;; (list string) → (list (key kv ... value))
(define (tokenize line)
  (define colon-idx (string-index line #\:))
  (define semi-idxs
    (let loop ((idx 0))
      (aif (string-index line #\; idx colon-idx)
           (cons it (loop (1+ it)))
           (list colon-idx (string-length line)))))
  (map (lambda (start end)
         (substring line (1+ start) end))
       (cons -1 semi-idxs)
       semi-idxs))

(define (x-property? symb)
  (string=? "X-" (string-take (symbol->string symb) 2)))

#;
'(ATTACH ATTENDEE CATEGORIES
         COMMENT CONTACT EXDATE
         REQUEST-STATUS RELATED-TO
         RESOURCES RDATE
         ;; x-prop
         ;; iana-prop
         )

(define (list-parser symbol)
  (let ((parser (get-parser symbol)))
    (lambda (params value)
      (map (lambda (v) (parser params v))
           (string-split value #\,)))))

(define* (enum-parser enum optional: allow-other)
  (let ((parser (get-parser 'TEXT)))
    (lambda (params value)
      (let ((vv (parser params value)))
        (when (list? vv)
          (error ""))
        (let ((v (symbol->string vv)))
          (unless (memv v enum)
            (warning ""))
          v)))))

;; params could be made optional, with an empty hashtable as default
(define (build-vline key value params)
  (let ((parser
         (cond
          [(hashq-ref params 'TYPE) => get-parser]

          [(memv key '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID  RDATE
                              CREATED DTSTAMP LAST-MODIFIED))
           (get-parser 'DATE-TIME)]

          [(memv key '(EXDATE))
           (list-parser 'DATE-TIME)]

          [(memv key '(DURATION))
           (get-parser 'DURATION)]

          [(memv key '(FREEBUSY))
           (list-parser 'PERIOD)]

          [(memv key '(CALSCALE METHOD PRODID  COMMENT DESCRIPTION
                             LOCATION STATUS SUMMARY TZID TZNAME
                             CONTACT RELATED-TO UID))
           (lambda (params value)
             (let ((v ((get-parser 'TEXT) params value)))
               (when (list? v)
                 (warning "List in non-list field"))
               v))]

          ;; TEXT, but allow a list
          [(memv key '(CATEGORIES RESOURCES))
           (get-parser 'TEXT)]

          [(memv key '(VERSION))
           (lambda (params value)
             (let ((v ((get-parser 'TEXT) params value)))
               (unless (string=? "2.0" v)
                 (warning "File of unsuported version. Proceed with caution"))))]

          [(memv key '(TRANSP))
           (enum-parser '(OPAQUE TRANSPARENT))]

          [(memv key '(CLASS))
           (enum-parser '(PUBLIC PRIVATE CONFIDENTIAL) #t)]

          ;; TODO
          [(memv key '(REQUEST-STATUS))]

          [(memv key '(ACTION))
           (enum-parser '(AUDIO DISPLAY EMAIL) #t)]

          [(memv key '(TZOFFSETFROM TZOFFSETTO))
           (get-parser 'UTC-OFFSET)]

          [(memv key '(ATTACH TZURL URL))
           (get-parser 'URI)]

          [(memv key '(PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE))
           (get-parser 'INTEGER)]

          [(memv key '(GEO))
           ;; two semicolon sepparated floats
           (lambda (params value)
             (let* (((left right) (string-split value #\;)))
               (cons ((get-parser 'FLOAT) params left)
                     ((get-parser 'FLOAT) params right))))]

          [(memv key '(RRULE))
           ;; TODO date/datetime
           (get-parser 'RECUR)]

          [(memv key '(ORGANIZER ATTENDEE))
           (get-parser 'CAL-ADDRESS)]

          [(x-property? key)
           (get-parser 'TEXT)]

          [else
           (warning "Unknown key ~a" key)
           (get-parser 'TEXT)])))
    (make-vline key (parser params value) params)))

;; (parse-itemline '("DTEND"  "20200407T130000"))
;; => DTEND
;; => "20200407T130000"
;; => #<hash-table 7f76b5f82a60 0/31>
(define (parse-itemline itemline)
  (define key (string->symbol (car itemline)))
  (define parameters (make-hash-table))
  (let loop ((rem (cdr itemline)))
    (if (null? (cdr rem))
        (values key (car rem) parameters )
        (let* ((kv (car rem))
               (idx (string-index kv #\=)))
          (hashq-set! parameters (string->symbol (substring kv 0 idx))
                      (substring kv (1+ idx)))
          (loop (cdr rem))))))


;; (list (key kv ... value)) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)
             (stack '()))
    (if (null? lst)
        stack
        (let ((head (car lst)))
          (cond [(string=? "BEGIN" (car head))
                 (loop (cdr lst) (cons (make-vcomponent (string->symbol (cadr head))) stack))]
                [(string=? "END" (car head))

                 (when (eq? (type (car stack)) 'VEVENT)

                   ;; This isn't part of the field values since we "need"
                   ;; the type of DTSTART for UNTIL to work.
                   ;; This could however be side steped by auto detecting
                   ;; @type{date}s vs @type{datetime}s in @function{parse-recurrence-rule}.
                   (when (attr (car stack) 'RRULE)
                     (set! (attr (car stack) 'RRULE)
                       ((@ (vcomponent recurrence) parse-recurrence-rule)
                        (attr (car stack) 'RRULE)
                        (if (date? (attr (car stack) 'DTSTART))
                            parse-ics-date parse-ics-datetime)))))

                 (loop (cdr lst)
                       (if (null? (cdr stack))
                           ;; return
                           (car stack)
                           (begin (add-child! (cadr stack) (car stack))
                                  (cdr stack))))]
                [else
                 (let* ((key value params (parse-itemline head)))
                   (call-with-values (lambda () (build-vline key value params))
                     (lambda vlines
                       (for vline in vlines
                            (define key (vline-key vline))

                            ;; See RFC 5545 p.53 for list of all repeating types
                            ;; (for vcomponent)
                            ;; TODO split on comman (,) here?
                            (if (memv key '(ATTACH ATTENDEE CATEGORIES
                                                COMMENT CONTACT EXDATE
                                                REQUEST-STATUS RELATED-TO
                                                RESOURCES RDATE
                                                ;; x-prop
                                                ;; iana-prop
                                                ))
                                (aif (attr* (car stack) key)
                                     (set! (attr* (car stack) key) (cons vline it))
                                     (set! (attr* (car stack) key) (list vline)))
                                ;; else
                                (set! (attr* (car stack) key) vline))))))

                 (loop (cdr lst) stack)])))))
