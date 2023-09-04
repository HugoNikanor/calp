(define-module (vcomponent formats ical parse)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (ice-9 format)
  :use-module (ice-9 curried-definitions)
  :use-module (hnh util exceptions)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-9 gnu)
  :use-module (vcomponent base)
  :use-module (vcomponent geo)
  :use-module (vcomponent formats common types)
  :use-module (calp translation)
  :use-module (hnh util lens)
  :use-module (hnh util table)
  :export (parse-calendar))

;;; TODO a few translated strings here contain explicit newlines. Check if that
;;;      is preserved through the translation.

(define string->symbol
  (let ((ht (make-hash-table 1000)))
    (lambda (str)
      (or (hash-ref ht str)
          (let ((symb ((@ (guile) string->symbol) str)))
            (hash-set! ht str symb)
            symb)))))

;; TODO rename to parse-vcomponent, or parse-ical (?).
(define (parse-calendar port)
  (parse (map tokenize (read-file port))))

(define-immutable-record-type <line>
  (make-line string file line)
  line?
  (string get-string)
  (file get-file)
  (line get-line))


;; port → (list <line>)
(define (read-file port)
  (define fname (port-filename port))
  (let loop ((line-number 1) (done '()))
    (let ((ostr (open-output-string)))
      (define ret
        (let loop ((line (read-line port)))
          (if (eof-object? line)
              'eof
              (let ((line (string-trim-right line #\return)))
               (let ((next (peek-char port)))
                 (display line ostr)
                 (cond ((eof-object? next)
                        'final-line)
                       ;; Line Wrapping
                       ;; If the first character on a line is space (whitespace?)
                       ;; then it's a continuation line, and should be merged
                       ;; with the one preceeding it.
                       ;; TODO if the line is split inside a unicode character
                       ;; then this produces multiple broken unicode characters.
                       ;; It could be solved by checking the start of the new line,
                       ;; and the tail of the old line for broken char
                       ;; TODO what about other leading whitespace?
                       ((char=? next #\space)
                        (read-char port) ; discard leading whitespace
                        (loop (read-line port)))
                       (else
                        ;; (unread-char next)
                        'line)))))))
      (case ret
        ((line)
         (let ((str (get-output-string ostr)))
           (close-port ostr)
           (loop (1+ line-number)
                 (cons (make-line str fname line-number)
                       done))))
        ((eof)
         (close-port ostr)
         (reverse! done))
        ((final-line)
         (let ((str (get-output-string ostr)))
           (close-port ostr)
           (reverse! (cons (make-line str fname line-number)
                           done))))))))

(define-immutable-record-type <tokens>
  (make-tokens metadata data)
  tokens?
  (metadata get-metadata) ; <line>
  (data get-data) ; (key kv ... value)
  )

;; <line> → <tokens>
(define (tokenize line-obj)
  (define line (get-string line-obj))
  (define colon-idx (string-index line #\:))
  (define semi-idxs
    (let loop ((idx 0))
      (aif (string-index line #\; idx colon-idx)
           (cons it (loop (1+ it)))
           (list colon-idx (string-length line)))))
  (make-tokens
   line-obj
   (map (lambda (start end)
          (substring line (1+ start) end))
        (cons -1 semi-idxs)
        semi-idxs)))


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

(define* (enum-parser enum optional: (allow-other #t))
  (let ((parser (compose car (get-parser 'TEXT))))
    (lambda (params value)
      (let ((vv (parser params value)))
        (when (list? vv)
          (scm-error 'parse-error "enum-parser"
                     (G_ "List in enum field")
                     #f #f))
        (let ((v (string->symbol vv)))
          (unless (memv v enum)
            (warning "~a ∉ { ~{~a~^, ~} }"
                     v enum))
          v)))))

;; params could be made optional, with an empty hashtable as default
(define (build-vline key value params)
  (let ((parser
         (cond
          [(and=> (table-get params 'VALUE) string->symbol) => get-parser]

          [(memv key '(COMPLETED DTEND DUE DTSTART RECURRENCE-ID RDATE
                              CREATED DTSTAMP LAST-MODIFIED
                              ;; only on VALARM
                              ACKNOWLEDGED
                              ))
           (get-parser 'DATE-TIME)]

          [(memv key '(EXDATE))
           (list-parser 'DATE-TIME)]

          [(memv key '(TRIGGER DURATION))
           (get-parser 'DURATION)]

          [(memv key '(FREEBUSY))
           (list-parser 'PERIOD)]

          [(memv key '(CALSCALE METHOD PRODID  COMMENT DESCRIPTION
                             LOCATION SUMMARY TZID TZNAME
                             CONTACT RELATED-TO UID))
           (lambda (params value)
             (let ((v ((get-parser 'TEXT) params value)))
               (unless (= 1 (length v))
                 (warning (G_ "List in non-list field: ~s") v))
               (string-join v ",")))]

          ;; TEXT, but allow a list
          [(memv key '(CATEGORIES RESOURCES))
           ;; TODO An empty value should lead to an empty set
           ;; currently it seems to lead to '("")
           (get-parser 'TEXT)]

          [(memv key '(VERSION))
           (lambda (params value)
             (let ((v (car ((get-parser 'TEXT) params value))))
               (unless (and (string? v) (string=? "2.0" v))
                 #f
                 ;; (warning "File of unsuported version. Proceed with caution")
                 )
               v))]

          [(memv key '(TRANSP))
           (enum-parser '(OPAQUE TRANSPARENT) #f)]

          [(memv key '(CLASS))
           (enum-parser '(PUBLIC PRIVATE CONFIDENTIAL))]

          [(memv key '(PARTSTAT))
           (enum-parser '(NEEDS-ACTION
                          ACCEPTED DECLINED
                          TENTATIVE DELEGATED
                          IN-PROCESS))]

          [(memv key '(STATUS))
           (enum-parser '(TENTATIVE
                          CONFIRMED CANCELLED
                          NEEDS-ACTION COMPLETED IN-PROCESS
                          DRAFT FINAL CANCELED))]

          [(memv key '(REQUEST-STATUS))
           (scm-error 'parse-error "build-vline"
                      (G_ "TODO Implement REQUEST-STATUS")
                      #f #f)]

          [(memv key '(ACTION))
           (enum-parser '(AUDIO DISPLAY EMAIL
                                NONE    ; I don't know where NONE is from
                                        ; but it appears to be prevelant.
                                ))]

          [(memv key '(TZOFFSETFROM TZOFFSETTO))
           (get-parser 'UTC-OFFSET)]

          [(memv key '(ATTACH TZURL URL))
           (get-parser 'URI)]

          [(memv key '(PERCENT-COMPLETE PRIORITY REPEAT SEQUENCE))
           (get-parser 'INTEGER)]

          [(memv key '(GEO))
           ;; two semicolon sepparated floats
           (lambda (params value)
             (let ((left right (apply values (string-split value #\;))))
               (make-geo ((get-parser 'FLOAT) params left)
                         ((get-parser 'FLOAT) params right))))]

          [(memv key '(RRULE))
           (get-parser 'RECUR)]

          [(memv key '(ORGANIZER ATTENDEE))
           (get-parser 'CAL-ADDRESS)]

          [(x-property? key)
           (compose car (get-parser 'TEXT))]

          [else
           (warning (G_ "Unknown key ~a") key)
           (compose car (get-parser 'TEXT))])))

    ;; If we produced a list create multiple VLINES from it.
    ;; NOTE that the created vlines share parameter tables.
    ;; TODO possibly allow vlines to reference each other, to
    ;; indicate that all these vlines are the same.
    (let ((parsed (parser params value)))
      (if (list? parsed)
          (apply values
                 (map (lambda (p) (vline key: key vline-value: p vline-parameters: params))
                      parsed))
       (vline key: key vline-value: parsed vline-parameters: params)))))

;; (parse-itemline '("DTEND"  "20200407T130000"))
;; => DTEND
;; => "20200407T130000"
;; => #<hash-table 7f76b5f82a60 0/31>
(define (parse-itemline itemline)
  (define key (string->symbol (car itemline)))
  ;; (define parameters (make-hash-table))
  (define-values (parameters value) (init+last (cdr itemline)))
  (values
   key value
   (fold (lambda (parameter table)
           (let ((idx (string-index parameter #\=)))
             ;; TODO lists in parameters
             (table-put table (string->symbol (substring parameter 0 idx))
                        (substring parameter (1+ idx)))))
         (table)
         parameters)))

(define ((warning-handler-proc token) fmt . args)
  (let ((linedata (get-metadata token)))
    (format
     #f
     ;; arguments:
     ;; linedata
     ;; ~?
     ;; source line
     ;; source file
     (G_ "WARNING parse error around ~a
  ~?
  line ~a ~a~%")
     (get-string linedata)
     fmt args
     (get-line linedata)
     (get-file linedata)
     )))

;;; Property keys which are allowed multiple times
(define repeating-properties
  '(ATTACH ATTENDEE CATEGORIES
           COMMENT CONTACT EXDATE
           REQUEST-STATUS RELATED-TO
           RESOURCES RDATE
           ;; x-prop
           ;; iana-prop
           ))

;; (list <tokens>) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)
             (stack '()))
    (if (null? lst)
        stack
        (let* ((token (car lst))
               (head (get-data token)))
          (catch 'parse-error
            (lambda ()
              (parameterize ((warning-handler (warning-handler-proc token)))
                (cond [(string=? "BEGIN" (car head))
                       (format (current-error-port) "BEGIN ~s~%" (cadr head))
                       (loop (cdr lst)
                             (cons (vcomponent type: (string->symbol (cadr head)))
                                   stack))]
                      [(string=? "END" (car head))
                       (format (current-error-port) "END ~s~%" (cadr head))
                       (loop (cdr lst)
                             (if (null? (cdr stack))
                                 ;; return
                                 stack
                                 (cons (add-child (cadr stack) (car stack))
                                       (cddr stack))))]
                      [else
                       (let ((k value params (parse-itemline head)))
                         (loop (cdr lst)
                               (let (((values . vlines) (build-vline k value params)))
                                 ;; TODO
                                 ;; (set! (vline-source vline)
                                 ;;   (get-metadata token))

                                 ;; See RFC 5545 p.53 for list of all repeating types
                                 ;; (for vcomponent)
                                 ;; TODO templetize this, and allow users to
                                 ;; set which types are list types, but also
                                 ;; validate this upon creation (elsewhere).
                                 (fold (lambda (vline stack)
                                         (modify stack car*
                                                 (lambda (comp)
                                                   (format (current-error-port)
                                                           "    stack=~s, comp=~s~%"
                                                           stack comp)
                                                   (if (memv (key vline) repeating-properties)
                                                       (aif (prop* comp (key vline))
                                                            (prop* comp (key vline) (cons vline it))
                                                            (prop* comp (key vline) (list vline)))
                                                       ;; else
                                                       (prop* comp (key vline) vline)))))
                                       stack vlines))))])))

            (lambda (err proc fmt fmt-args data)
              (let ((linedata (get-metadata token)))
                (display (format
                          #f
                          ;; arguments
                          ;; linedata
                          ;; ~?
                          ;; source line
                          ;; source file
                          (G_ "ERROR parse error around ~a
  ~?
  line ~a ~a
  Defaulting to string~%")
                          (get-string linedata)
                          fmt fmt-args
                          (get-line linedata)
                          (get-file linedata))
                         (current-error-port))
                (let ((k value params (parse-itemline head)))
                  (loop (cdr lst)
                        (modify stack car*
                                (lambda (c) (prop* c key
                                              (vline key: k
                                                     vline-value: value
                                                     vline-parameters: params)))))))))))))
