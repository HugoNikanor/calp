(define-module (vcomponent formats ical parse)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (ice-9 format)
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
          [(and=> (hashq-ref params 'VALUE) string->symbol) => get-parser]

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
                 (map (lambda (p) (make-vline key p params))
                      parsed))
       (make-vline key parsed params)))))

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
          ;; TODO lists in parameters
          (hashq-set! parameters (string->symbol (substring kv 0 idx))
                      (substring kv (1+ idx)))
          (loop (cdr rem))))))


;; (list <tokens>) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)
             (stack '()))
    (if (null? lst)
        stack
        (let* ((head* (car lst))
               (head (get-data head*)))
          (catch 'parse-error
            (lambda ()
             (parameterize
                 ((warning-handler
                   (lambda (fmt . args)
                     (let ((linedata (get-metadata head*)))
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
                        )))))
               (cond [(string=? "BEGIN" (car head))
                      (loop (cdr lst)
                            (cons (make-vcomponent (string->symbol (cadr head)))
                                  stack))]
                     [(string=? "END" (car head))
                      (loop (cdr lst)
                            (if (null? (cdr stack))
                                ;; return
                                (car stack)
                                (begin (add-child! (cadr stack) (car stack))
                                       (cdr stack))))]
                     [else
                      (let ((key value params (parse-itemline head)))
                        (call-with-values (lambda () (build-vline key value params))
                          (lambda vlines
                            (for vline in vlines
                                 (define key (vline-key vline))

                                 (set! (vline-source vline)
                                   (get-metadata head*))

                                 ;; See RFC 5545 p.53 for list of all repeating types
                                 ;; (for vcomponent)
                                 ;; TODO templetize this, and allow users to set which types are list types, but also validate this upon creation (elsewhere)
                                 (if (memv key '(ATTACH ATTENDEE CATEGORIES
                                                     COMMENT CONTACT EXDATE
                                                     REQUEST-STATUS RELATED-TO
                                                     RESOURCES RDATE
                                                     ;; x-prop
                                                     ;; iana-prop
                                                     ))
                                     (aif (prop* (car stack) key)
                                          (set! (prop* (car stack) key) (cons vline it))
                                          (set! (prop* (car stack) key) (list vline)))
                                     ;; else
                                     (set! (prop* (car stack) key) vline))))))

                      (loop (cdr lst) stack)])))
            (lambda (err proc fmt fmt-args data)
              (let ((linedata (get-metadata head*)))
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
                (let ((key value params (parse-itemline head)))
                  (set! (prop* (car stack) key)
                    (make-vline key value params))
                  (loop (cdr lst) stack)))))))))
