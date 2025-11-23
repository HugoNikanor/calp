(define-module (vcomponent media-type text calendar parse)
  :use-module (ice-9 curried-definitions)
  :use-module (ice-9 format)
  :use-module (ice-9 match)
  :use-module (ice-9 rdelim)
  :use-module (ice-9 regex)
  :use-module (hnh util exceptions)
  :use-module (hnh util)
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (srfi srfi-9 gnu)
  :use-module (vcomponent)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (vcomponent type period)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent media-type types)
  :use-module (calp translation)
  :use-module (hnh util lens)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util optional)
  :use-module (base64)
  :use-module (web uri)
  :use-module ((vcomponent type recurrence parse) :select (parse-day-spec))
  :export (
           icalendar->vcomponent
           multi-valued-properties
           parsers
           get-parser

           split-carefully

           parse-recurrence-rule
           parse-period
           parse-request-status
           ))

;;; TODO different parsers currently fail in different ways for invalid value.
;;; - Some throw exceptions, crashing the program
;;; - Some emit a warning, then wraps the raw value in an `unknown`
;;; - some may do something else entirely.

;;; BINARY throws on unknown encodnig, returns incorrect data on malformed data
;;; BOOLEAN never fails
;;; DATE throws on malformed date
;;; DATETIME throws on malformed date
;;; DURATION throws on malformed data
;;; FLOAT returns #f
;;; INTEGER return `(unknown <data>)`
;;; PERIOD throws on malformed data
;;; PERIOD throws on malformed data
;;; RECUR throws on malformed data
;;; TEXT never fails
;;; TIME throws on malformed data
;;; UTC-OFFSET throws on malformed data

;;; GEO throws
;;; VERSION never fails
;;; REQUEST-STATUS throws on malformed data


;;; (map tokenize)
;;; for each "real" line, extract the key, parse the parameters, and return the value unparsed
;;; (parse)
;;; build structure, and parse property values

(define (icalendar->vcomponent port)
  (parse (map tokenize (read-lines/folded port))))


;; (parse
;;  (map tokenize
;;       (bytevector->unfolded-lines
;;        (get-bytevector-all port))))

;; BINARY
(define (parse-binary props value)
  ;; p 30

  (values
   (case (string->symbol (or (table-get props 'ENCODING) "BASE64"))
     ((BASE64) (base64-string->bytevector value))
     (else => (lambda (enc) (scm-error 'misc-error "parse-binary"
                                  "Unknown encoding of binary data: ~s"
                                  (list enc) #f))))
   (table-remove props 'ENCODING)))

;; BOOLEAN
(define (parse-boolean _ value)
  (cond
   [(string=? "TRUE" value) #t]
   [(string=? "FALSE" value) #f]
   [else (warning (G_ "~a invalid boolean") (unknown value))]))



;; DATE-TIME
(define (parse-datetime props value)
  (values (modify (string->datetime value "~Y~m~dT~H~M~S~Z")
                  tz* (lambda (tz) (or tz (table-get props 'TZID))))
          (table-remove props 'TZID)))


;; INTEGER
(define (parse-integer _ value)
  (let ((n (string->number value)))
    (if (not (integer? n))
        (begin
          (warning (G_ "Non integer as integer"))
          (unknown value))
        n)))


;; PERIOD
(define* (parse-period props value optional: (dt-fmt "~Y~m~dT~H~M~S~Z"))
  (let ((left right (apply values (string-split value #\/))))
    (values (period start: (modify (string->datetime left dt-fmt)
                                   tz* (lambda (tz) (or tz (table-get props 'TZID))))
                    end: ((if (memv (string-ref right 0)
                                 '(#\P #\+ #\-))
                              string->duration
                              (lambda (v) (modify (string->datetime v dt-fmt)
                                             tz* (lambda (tz) (or tz (table-get props 'TZID))))))
                          right))
            (table-remove props 'TZID))))


;; TEXT
(define (parse-text _ value)
  (let loop ((rem (string->list value))
             (str '()))
    (match rem
      (() (reverse-list->string str))
      ((or (#\\ #\n rest ...) (#\\ #\N rest ...))
       (loop rest (cons #\newline str)))
      ((#\\ #\, rest ...) (loop rest (cons #\, str)))
      ((#\\ #\; rest ...) (loop rest (cons #\; str)))
      ((#\\ #\\ rest ...) (loop rest (cons #\\ str)))
      ((#\\ c rest ...)
       (warning (G_ "Non-escapable character: '~a'") c)
       (loop rest (cons c str)))
      ((#\, rest ...)
       (warning (G_ "Un-escaped '~a' encountered") #\,)
       (loop rest (cons #\, str)))
      ((#\; rest ...)
       (warning (G_ "Un-escaped '~a' encountered") #\;)
       (loop rest (cons #\; str)))
      ((c rest ...) (loop rest (cons c str))))))


;; UTC-OFFSET
;;; (@ (datetime timespec) parse-time-spec) parses timespecs as they
;;; appear in zoneinfo files.
(define (parse-utc-offset props value)
  (cond ((string-match "^([+-])([0-9]{4,6})$" value)
         => (lambda (m)
              (timespec
               (string->time (string-pad-right (match:substring m 2) 6 #\0)
                             "~H~M~S")
               (string->symbol (match:substring m 1))
               'utc)))
        (else (scm-error 'misc-error "parse-utc-offset"
                         "String not parsable as a UTC-OFFSET: ~s"
                         (list value) #f))))




(define* (string->number/throw string optional: (radix 10))
  (or (string->number string radix)
      (scm-error 'wrong-type-arg
                 "string->number/throw"
                 "Can't parse ~s as number in base ~a"
                 (list string radix) (list string radix))))

;; RFC 5545, Section 3.3.10. Recurrence Rule, states that the UNTIL value MUST have
;; the same type as the DTSTART of the event (date or datetime). I have seen events
;; in the wild which didn't follow this. I consider that an user error.
(define (parse-recurrence-rule _ str)
  (define result
    (fold
     (lambda (kv o)
       (let ((key (car kv))
             (val (cadr kv)))
         (let-lazy
          ((symb (string->symbol val))
           ;; NOTE until MUST have the same value type as DTSTART
           ;; on the object. Idealy we would save that type and
           ;; check it here. That however is impractical since we
           ;; might encounter the RRULE field before the DTSTART
           ;; field.
           (date (if (= 8 (string-length val))
                     (parse-ics-date val)
                     (string->datetime val "~Y~m~dT~H~M~S~Z")))
           (day (rfc->datetime-weekday (string->symbol val)))
           (days (map parse-day-spec (string-split val #\,)))
           (num  (string->number/throw val))
           (nums (map string->number/throw (string-split val #\,))))

          ;; It's an error to give BYHOUR and smaller for pure dates.
          ;; 3.3.10. p 41
          (case (string->symbol key)
            ((UNTIL)      (until      o date))
            ((COUNT)      (count      o num))
            ((INTERVAL)   (interval   o num))
            ((FREQ)       (freq       o symb))
            ((WKST)       (wkst       o day))
            ((BYSECOND)   (bysecond   o nums))
            ((BYMINUTE)   (byminute   o nums))
            ((BYHOUR)     (byhour     o nums))
            ((BYMONTH)    (bymonth    o nums))
            ((BYDAY)      (byday      o days))
            ((BYMONTHDAY) (bymonthday o nums))
            ((BYYEARDAY)  (byyearday  o nums))
            ((BYSETPOS)   (bysetpos   o nums))
            ((BYWEEKNO)   (byweekno   o nums))
            (else o)))))

     ;; obj
     (recur-rule)

     ;; ((key val) ...)
     (map (cut string-split <> #\=)
          (string-split str #\;))))

  ;; NOTE previously, we checked if freq actually had a value here.
  ;; Maybe do that again

  result)




;; A parser is a function with signature (table, string) → any
;; which takes the table of vline parameters, and the raw value,
;; and returns a parsed representation.
;; For example:
;;     ((get-parser 'DATE-TIME)
;;       (list->table '((TZID . "Europe/Stockholm")))
;;       "20201020T102030")
(define-once parsers
  (make-parameter
   (alist->table
    (list
     (cons 'BINARY parse-binary)
     (cons 'BOOLEAN parse-boolean)
     (cons 'CAL-ADDRESS (lambda (_ v) (string->uri v)))
     (cons 'DATE (lambda (_ v) (parse-ics-date v)))
     (cons 'DATE-TIME parse-datetime)
     (cons 'DURATION (lambda (_ v) (string->duration v)))
     ;; Note that this is overly permissive, and flawed.
     ;; Numbers such as @expr{1/2} is accepted as exact
     ;; rationals. Some floats are rounded.
     (cons 'FLOAT (lambda (_ v) (string->number v)))
     (cons 'INTEGER parse-integer)
     (cons 'PERIOD parse-period)
     (cons 'RECUR parse-recurrence-rule)
     (cons 'TEXT parse-text)
     ;; TODO time can have timezones...
     (cons 'TIME (lambda (_ v) (parse-ics-time v)))
     (cons 'URI (lambda (_ v) (string->uri v)))
     (cons 'UTC-OFFSET parse-utc-offset)))))

;;; Get iCalendar type parser by type name
(define (get-parser type)
  (table-get (parsers) type))

;;; TODO actually benchmark if this has any speed difference
(define string->symbol
  (let ((ht (make-hash-table 1000)))
    (lambda (str)
      (or (hash-ref ht str)
          (let ((symb ((@ (guile) string->symbol) str)))
            (hash-set! ht str symb)
            symb)))))


(define-immutable-record-type <line>
  (make-line string file line)
  line?
  (string get-string)                   ; string?
  (file get-file)                       ; string?
  (line get-line))                      ; exact-integer?


;; Like `read-line`, but both \n and \r\n are considered newlines, and trimmed
(define (read-line/crnl port)
  (let ((line (read-line port)))
    (if (eof-object? line)
        line
        (string-trim-right line #\return))))

;; TODO if the line is folded inside a unicode character
;; then this produces multiple broken unicode characters.
;; It could be solved by checking the start of the new line,
;; and the tail of the old line for broken char
(define (read-line/folded port)
  (let loop ((done (read-line/crnl port)) (lines 1))
    (if (memv (peek-char port) '(#\space #\tab))
        (begin
          (read-char port)              ; Discard continuation marker
          (loop (string-append done (read-line/crnl port))
                (1+ lines)))
        (values done lines))))


;; Read all (folded) lines in a file, and return a list of <line> objects.
(define (read-lines/folded port)
  (let loop ((line-number 1) (done '()))
    (let ((line line-count (read-line/folded port)))
      (if (eof-object? line)
          (reverse! done)
          (begin
            (loop (+ line-number line-count)
                  (cons (make-line line (port-filename port) line-number)
                        done)))))))

(define-immutable-record-type <tokens>
  (make-tokens metadata data)
  tokens?
  (metadata get-metadata) ; <line>
  (data get-data) ; (property-name (parameter-name . parameter-value) ... content)
  )

;; <line> → <tokens>
(define (tokenize line-obj)
  (make-tokens
   line-obj
   (call-with-input-string (get-string line-obj)
     (lambda (p)
       (define property-name (read-delimited ";:" p 'peek))
       (cons property-name
             (case (read-char p)
               ((#\:) (list (read-delimited "" p)))
               ((#\;)
                (let loop ()
                  (define parameter-name (read-delimited "=" p))
                  (define parameter-value
                    (if (char=? #\" (peek-char p))
                        (begin (read-char p)
                               (read-delimited "\"" p))
                        (read-delimited ";:" p 'peek)))
                  (cons (cons parameter-name parameter-value)
                        (case (read-char p)
                          ((#\:) (list (read-delimited "" p)))
                          ((#\;) (loop))))))))))))


(define multi-valued-properties
  (make-parameter
   '(CATEGORIES
     RESOURCES
     FREEBUSY
     EXDATE
     RDATE)))


(define (split-carefully str delim)
  (let loop ((rem (string->list str))
             (str '())
             (done '()))
    (match rem
      (() (reverse (cons (reverse-list->string str) done)))
      ((#\\ c rest ...) (loop rest (cons* c #\\ str)
                              done))
      ((c rest ...)
       (if (char=? c delim)
           (loop rest '() (cons (reverse-list->string str) done))
           (loop rest (cons c str) done))))))


(define (parse-request-status value)
  (define (parse-text str) ((get-parser 'TEXT) '() str))
  (apply (lambda* (statcode statdesc optional: extdata)
           (request-status
            statcode: (map string->number (string-split statcode #\.))
            statdesc: (parse-text statdesc)
            extdata: (and=> extdata parse-text)))
         (split-carefully value #\;)))


;; params could be made optional, with an empty table as default
;; Returns a list of vline objects.
;; For most types, this will be a single vline, but for
;; `multi-valued-properties`, it may be multiple.
;; For example,
;; (build-vlines 'CATEGORIES "A,B" (-> (table) (table-put 'LANG "EN")))
;; ⇒ #.(list (vline #:params (-> (table) (table-put 'LANG "EN")) #:value "A")
;;           (vline #:params (-> (table) (table-put 'LANG "EN")) #:value "B"))
(define (build-vlines key value params)
  (typecheck key symbol?)
  (typecheck value string?)
  (typecheck params table?)             ; (table-of string?)

  (define (parse-text str) ((get-parser 'TEXT) '() str))

  (define parser
    (or
     (cond
      ((eq? key 'GEO)
       (lambda (_ value)
         (apply (case-lambda ((y x) (geo x: x y: y))
                             (_ (scm-error 'misc-error "build-vlines"
                                           "Invalid GEO value: ~s"
                                           (list value) #f)))
                (map string->number (string-split value #\;)))))

      ((eq? key 'VERSION)
       (lambda (_ value)
         (apply (case-lambda
                  ((min max) (vcalendar-version min: (parse-text min) max: (parse-text max)))
                  ((max) (vcalendar-version max: (parse-text max))))
                (split-carefully value #\;))))

      ((eq? key 'REQUEST-STATUS) (lambda (_ value) (parse-request-status value)))

      ;; 1. Check if we have a VALUE parameter, and in that case use that
      ((and=> (table-get params 'VALUE) string->symbol) => get-parser)
      ;; 3. Retrieve the default type of the field
      ((default-type key) => get-parser)
      (else (get-parser 'TEXT)))
     (lambda (_ v) (unknown v))))

  ;; We remove the parameter VALUE, since we instead encode that into scheme types
  ;; (and most output formats explicitly forbid it from being included)
  ;; TODO TODO I believe special handling for multi-valued properties is incorrect.
  ;; I believe that all fields (or at least those of TEXT type) will
  ;; be split this way, unless otherwise stated.
  (if (memv key (multi-valued-properties))
      (map (lambda (value)
             (call-with-values (lambda () (parser params value))
               (lambda* (value optional: (params params))
                 (vline params: (table-remove params 'VALUE)
                        value: value))))
           (split-carefully value #\,))
      (list
       (call-with-values (lambda () (parser params value))
         (lambda* (value optional: (params params))
           (vline params: (table-remove params 'VALUE)
                  value: value))))))

(define ((warning-handler-proc token) fmt . args)
  (let ((linedata (get-metadata token)))
    (format
     #f
     ;; arguments:
     ;; linedata
     ;; ~?
     ;; source line
     ;; source file
     (G_ "Parse warning around ~a:~a (~s): ~?~%")
     (get-file linedata)
     (get-line linedata)
     (get-string linedata)
     fmt args

     )))

(define (update-property stack key vlines)
  (modify stack (lens-compose car* vcomponent-properties*
                              (table-focus key))
          (lambda (focus)
            (just (append (unjust focus '())
                          vlines)))))


;; (list <tokens>) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)                 ; Remaining tokens
             (stack (list (vcomponent type: 'DUMMY))))              ; Stack of vcomponent
    (if (null? lst)
        (-> stack car vcomponent-children car)
        (parameterize ((warning-handler (warning-handler-proc (car lst))))
          (match (get-data (car lst))
            (("BEGIN" type)
             (loop (cdr lst)
                   (cons (vcomponent type: (string->symbol type))
                         stack)))

            (("END" _)
             ;; TODO check that the correct object was closed
             (loop (cdr lst)
                   (cons (add-child (cadr stack) (car stack))
                         (cddr stack))))

            ((key (parameter-key . parameter-value) ... value)
             (define params (fold (lambda (k v params) (table-put params (string->symbol k) v))
                                  (table)
                                  parameter-key parameter-value))

             (catch 'parse-error
               (lambda ()
                 (loop (cdr lst)
                       (update-property stack
                                        (string->symbol key)
                                        (build-vlines (string->symbol key)
                                                      value params))))

               (lambda (err proc fmt fmt-args data)
                 (warning "Marking field as `unknown`")
                 (loop (cdr lst)
                       (update-property stack (list (vline value: (unknown data)
                                                           params: params))))))))))))
