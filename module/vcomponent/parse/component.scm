(define-module (vcomponent parse component)
  :use-module (util)
  :use-module (util exceptions)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (vcomponent base)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9 gnu)
  :use-module (srfi srfi-26)
  :use-module (vcomponent parse types)
  :use-module (vcomponent geo)
 )

(define-public (parse-calendar port)
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
    (let ((line (read-line port)))
      (if (eof-object? line)
          (reverse! done)
          (let ((line (string-trim-right line)))
            (loop
             (1+ line-number)
             (if (char=? #\space (string-ref line 0))
                 ;; Line Wrapping
                 ;; TODO if the line is split inside a unicode character
                 ;; then this produces multiple broken unicode characters.
                 ;; It could be solved by checking the start of the new line,
                 ;; and the tail of the old line for broken char
                 (cons (make-line (string-append (get-string (car done))
                                                 (string-drop line 1))
                                  fname
                                  (get-line (car done)))
                       (cdr done))
                 (cons (make-line line fname line-number)
                       done))))))))

(define-immutable-record-type <tokens>
  (make-tokens metadata data)
  tokens?
  (metadata get-metadata) ; <line>
  (data get-data) ; (key kv ... value)
  )

;; (list <line>) → (list <tokens>)
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
  (let ((parser (compose car (get-parser 'TEXT))))
    (lambda (params value)
      (let ((vv (parser params value)))
        (when (list? vv)
          (throw 'parse-error "List in enum field"))
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
                 (warning "List in non-list field: ~s" v))
               (car v)))]

          ;; TEXT, but allow a list
          [(memv key '(CATEGORIES RESOURCES))
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
           (enum-parser '(OPAQUE TRANSPARENT))]

          [(memv key '(CLASS))
           (enum-parser '(PUBLIC PRIVATE CONFIDENTIAL) #t)]

          [(memv key '(PARTSTAT))
           (enum-parser '(NEEDS-ACTION
                          ACCEPTED DECLINED
                          TENTATIVE DELEGATED
                          IN-PROCESS)
                        #t)]

          [(memv key '(STATUS))
           (enum-parser '(TENTATIVE
                          CONFIRMED CANCELLED
                          NEEDS-ACTION COMPLETED IN-PROCESS
                          DRAFT FINAL CANCELED)
                        #t)]

          [(memv key '(REQUEST-STATUS))
           (throw 'parse-error "TODO Implement REQUEST-STATUS")]

          [(memv key '(ACTION))
           (enum-parser '(AUDIO DISPLAY EMAIL
                                NONE    ; I don't know where NONE is from
                                        ; but it appears to be prevelant.
                                )
                        #t)]

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
               (make-geo ((get-parser 'FLOAT) params left)
                         ((get-parser 'FLOAT) params right))))]

          [(memv key '(RRULE))
           (get-parser 'RECUR)]

          [(memv key '(ORGANIZER ATTENDEE))
           (get-parser 'CAL-ADDRESS)]

          [(x-property? key)
           (compose car (get-parser 'TEXT))]

          [else
           (warning "Unknown key ~a" key)
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
                        #f "WARNING parse error around ~a
  ~?
  line ~a ~a~%"
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
                      (let* ((key value params (parse-itemline head)))
                        (call-with-values (lambda () (build-vline key value params))
                          (lambda vlines
                            (for vline in vlines
                                 (define key (vline-key vline))

                                 (set! (vline-source vline)
                                   (get-metadata head*))

                                 ;; See RFC 5545 p.53 for list of all repeating types
                                 ;; (for vcomponent)
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

                      (loop (cdr lst) stack)])))
            (lambda (err fmt . args)
              (let ((linedata (get-metadata head*)))
                (display (format
                          #f "ERROR parse error around ~a
  ~?
  line ~a ~a
  Defaulting to string~%"
                          (get-string linedata)
                          fmt args
                          (get-line linedata)
                          (get-file linedata))
                         (current-error-port))
                (let* ((key value params (parse-itemline head)))
                  (set! (attr* (car stack) key)
                    (make-vline key value params))
                  (loop (cdr lst) stack)))))))))
