(define-module (vcomponent parse old)
  :use-module (util)
  :use-module (util strbuf)
  :use-module (util exceptions)

  :use-module ((rnrs io ports) :select (get-u8))
  :use-module ((ice-9 textual-ports) :select (unget-char))

  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-26)

  :use-module (datetime)
  :use-module (datetime util)

  :use-module (vcomponent base)
  :use-module (vcomponent datetime)
  ;; export (parse-calendar)
)

(define-record-type <parse-ctx>
  (make-parse-ctx% filename row col ctx line-key param-key param-table)
  parse-ctx?
  (filename get-filename)               ; string
  (row get-row set-row!)                ; [0, ]
  (col get-col set-col!)                ; [1, )
  (ctx get-ctx set-ctx!)                ; '(key value param-name param-value escape)
  (line-key get-line-key set-line-key!) ; string
  (param-key get-param-key set-param-key!)       ; string
  (param-table get-param-table set-param-table!) ; hash-map
  )

(define (make-parse-ctx filename)
  (make-parse-ctx% filename 1 0 'key
                   #f #f (make-hash-table)))

(define (increment-column! ctx)
  (set-col! ctx (1+ (get-col ctx))))

(define (increment-row! ctx)
  (set-col! ctx 0)
  (set-row! ctx (1+ (get-row ctx))))

(define (ctx-dump-strings! ctx)
  (set-line-key! ctx "")
  (set-param-key! ctx "")
  ;; (set-param-table! ctx (make-hash-table))
  )




(define (fold-proc ctx c)
  ;; First extra character optional read is to get the \n if our line
  ;; ended with \r\n. Secound read is to get the first character of the
  ;; next line. The initial \r which might recide in @var{c} is discarded.
  (let ((pair (cons (if (char=? #\newline (integer->char c))
                         c (get-u8 (current-input-port)))
                        (get-u8 (current-input-port)))))
    (increment-row! ctx)
    (cond [(not (char=? #\newline (integer->char (car pair))))
           (error "Expected newline after CR")]

          ;; The standard (3.4, l. 2675) says that each icalobject must
          ;; end with CRLF. My files however does not. This means that
          ;; an EOF can immideately follow a \n\r pair. But this case is the
          ;; same as that we are at the end of line, so we spoof it and let
          ;; the regular parser loop handle it.
          [(eof-object? (cdr pair))
           'end-of-line]

          ;; Following line begins with a whitespace character,
          ;; meaning that we don't break the logical line here.
          [(memv (integer->char (cdr pair)) '(#\space #\tab))
           (increment-column! ctx)      ; since we just read the space
           'fold]

          [else
           ;; TODO check if this failed, and signal a writeback error
           (unget-char (current-input-port)
                       (integer->char (cdr pair)))

           'end-of-line])))

(define (handle-value! key vline strbuf)
  (case key
    ;; As far as I can tell the RFC says nothing about special
    ;; encoding for individual fields. It mentieons UTF-8, and
    ;; that transfer encoding should be set in the mime-headers.
    ;; That however seems like a breach of abstractions.
    ;; Currently I allow a CHARSET property on SUMMARY fields,
    ;; since I know that at least www.lysator.liu.se/alma/alma.cgi
    ;; uses it.
    [(SUMMARY)
     (cond [(and=> (prop vline 'CHARSET) car)
            => (lambda (encoding)
                 (set! (value vline)
                   (strbuf->string strbuf ((@ (rnrs io ports) make-transcoder)
                                           encoding))))])]

    [(DTSTART DTEND RECURRENCE-ID LAST-MODIFIED DTSTAMP EXDATE)

     ;; '("Africa/Ceuta" "Europe/Stockholm" "local")
     (let ((tz (or (and=> (prop vline 'TZID) car)
                   (and (string= "Z" (string-take-right (value vline) 1)) "UTC"))))

       (let ((type (and=> (prop vline 'VALUE) car)))
         (if (or (and=> type (cut string=? <> "DATE-TIME"))
                 (string-contains (value vline) "T"))
             ;; TODO TODO TODO
             ;; we move all parsed datetimes to local time here. This
             ;; gives a MASSIVE performance boost over calling get-datetime
             ;; in all procedures which want to guarantee local time for proper calculations.
             ;; 20s vs 70s runtime on my laptop.
             ;; We sohuld however save the original datetime in a file like X-HNH-DTSTART,
             ;; since we don't want to lose that information.
             (set! (value vline) (get-datetime (parse-ics-datetime (value vline) tz))
                   (prop vline 'VALUE) 'DATE-TIME)
             (set! (value vline) (parse-ics-date (value vline))
                   (prop vline 'VALUE) 'DATE)))
       )]))

;; Reads a vcomponent from the given port.
(define-public (parse-calendar port)
  ;; (report-time! "Parsing ~a" port)
  (with-input-from-port port
    (lambda ()
      (let ((component (make-vcomponent))
            (ctx (make-parse-ctx (port-filename port)))
            (strbuf (make-strbuf)))
        (parameterize ((warning-handler
                        (lambda (fmt . args)
                          (format #f
                                  "== PARSE WARNING ==
filename = ~a
row ~a	column ~a	ctx = ~a
~a ; ~a = ... : ...
~?~%~%"
                                  (get-filename ctx)
                                  (get-row ctx) (get-col ctx) (get-ctx ctx)
                                  (get-line-key ctx) (get-param-key ctx)
                                  fmt args))))
          (with-throw-handler #t
            (lambda ()
              (while #t
                (let ((c (get-u8 (current-input-port))))
                  (cond

                   ;; End of file
                   [(eof-object? c)
                    ;; == NOTE ==
                    ;; We never check the final line here. But since it
                    ;; ALWAYS should be "END:VCOMPONENT", and we do all
                    ;; the setup at creation this shouldn't be a problem.
                    (let ((component
                           (case (get-ctx ctx)
                             ;; Line ended before we came here, get the actual root
                             ;; component (instead of our virtual one:
                             [(key) (car (children component))]
                             ;; Line wasn't ended before we get here, so our current
                             ;; component is our "actual" root. NOTE that this never
                             ;; actually finalizes the root object, which matters if
                             ;; if do something with the finalizer below.
                             ;; At the time of writing we just set the parent.
                             [(value) component]
                             [else
                              => (lambda (a)
                                   (scm-error
                                    'wrong-type-arg "parse-break"
                                    (string-append
                                     "Bad context at end of file. "
                                     "Expected `key' or `value', got ~a")
                                    (list a) #f))])))
                      ;; == NOTE ==
                      ;; This sets to the VCALENDAR, which is correct,
                      ;; but the program later squashes together everything
                      ;; and drops this information.
                      (set! (attr component 'X-HNH-FILENAME) (get-filename ctx)
                            (parent component) #f)
                      (break component))]

                   ;; End of line
                   [(memv (integer->char c) '(#\return #\newline))
                    (case (fold-proc ctx c)
                      [(end-of-line)
                       (let ((str (strbuf->string strbuf)))
                         (cond [(and (eq? 'key (get-ctx ctx))
                                     (string-null? str))
                                ;; I believe that an empty line is against the standard
                                ;; in every way. But it's nice to handle it.
                                (warning "Unexpected completely empty line")]

                               [(eq? 'BEGIN (get-line-key ctx))
                                (let ((child (make-vcomponent (string->symbol str))))
                                  (add-child! component child)
                                  (set! component child))]

                               [(eq? (get-line-key ctx) 'END)

                                ;; Ensure that we have a DTEND
                                ;; TODO Objects aren't required to have a DTEND, or a DURATION.
                                ;; write fancier code which acknoledges this.
                                (when (and (eq? 'VEVENT (type component))
                                           (not (attr component 'DTEND)))
                                  (set! (attr component 'DTEND)
                                    (let ((start (attr component 'DTSTART)))
                                      ;; p. 54, 3.6.1
                                      ;; If DTSTART is a date then it's an all
                                      ;; day event. If DTSTART instead is a
                                      ;; datetime then the event has a length
                                      ;; of 0?
                                      (if (date? start)
                                          (date+ start (date day: 1))
                                          (datetime+ start (datetime time: (time hour: 1)))))))

                                (set! component (parent component))]

                               [else     ; Regular key-value line
                                (let ((key (get-line-key ctx))
                                      (vline (make-vline str (get-param-table ctx))))
                                  ;; Type specific processing
                                  (handle-value! key vline strbuf)

                                  ;; From RFC 5545 §3.6.1
                                  ;; DTEND and DURATION are mutually exclusive
                                  ;; DTSTART is required to exist while the other two are optional.

                                  ;; Allowed (some) repeated keys
                                  (if (memv key '(EXDATE ATTENDEE))
                                      (aif (attr* component key)
                                           ;; updates the current vline
                                           ;; NOTE that this discards any properties belonging to this object
                                           ;; TODO a more propper way to do it would be to store multiple vline
                                           ;; objects for a given key.
                                           (set! (value it) (cons (value vline) (value it)))
                                           (begin (mod! (value vline) list)
                                                  (set-vline! component key vline)))
                                      ;; Keys which aren't allowed to be repeated.
                                      (begin
                                        (awhen (attr* component key)
                                               (warning "Key ~a encountered more than once, overriding old value [~a] with [~a]"
                                                        key (value it) (value vline)))
                                        (set-vline! component key vline))))
                                (set-param-table! ctx (make-hash-table))])

                         (strbuf-reset! strbuf)
                         (ctx-dump-strings! ctx)
                         (set-ctx! ctx 'key))]
                      [(fold) 'noop]    ; Good case, here to catch errors in else
                      [else => (lambda (a) (error "Bad return from fold, unexpected" a))])]

                   ;; Escaped characters
                   [(char=? #\\ (integer->char c))
                    (case (integer->char (get-u8 (current-input-port)))
                      ;; Escape character '\' and escaped token sepparated by a newline
                      ;; (since the standard for some reason allows that (!!!))
                      ;; We are at least guaranteed that it's a folded line, so just
                      ;; unfold it and continue trying to find a token to escape.
                      [(#\return #\newline)
                       => (lambda (c)
                            (case (fold-proc ctx (char->integer c))
                              [(end-of-line)
                               (throw 'escape-error "ESC before not folded line")]
                              [(fold)
                               (increment-column! ctx)
                               (strbuf-append! strbuf (get-u8 (current-input-port)))]))]

                      [(#\n #\N) (strbuf-append! strbuf (char->integer #\newline))]
                      [(#\; #\, #\\) => (lambda (c) (strbuf-append! strbuf (char->integer c)))]
                      [else => (lambda (c) (warning "Non-escapable character: ~a" c))])
                    (increment-column! ctx)]

                   ;; Delimiter between param key and param value
                   [(and (eq? (get-ctx ctx) 'param-name)
                         (char=? #\= (integer->char c)))
                    (set-param-key! ctx (string->symbol (strbuf->string strbuf)))
                    (strbuf-reset! strbuf)
                    (set-ctx! ctx 'param-value)]

                   ;; Delimiter between parameters (;), or between
                   ;; "something" and attribute value (:)
                   [(and (memv (integer->char c) '(#\: #\;))
                         (memv (get-ctx ctx) '(param-value key)))
                    (case (get-ctx ctx)
                      [(param-value)
                       (hashq-set! (get-param-table ctx)
                                   (get-param-key ctx)
                                   (strbuf->string strbuf))
                       (strbuf-reset! strbuf)]
                      [(key)
                       (set-line-key! ctx (string->symbol (strbuf->string strbuf)))
                       (strbuf-reset! strbuf)])

                    (set-ctx! ctx (case (integer->char c)
                                    [(#\:) 'value]
                                    [(#\;) 'param-name]))]

                   ;; Regular character
                   [else
                    (strbuf-append! strbuf c)
                    (increment-column! ctx)]))))

            (lambda _
              ;; display is atomic, format isn't
              (display
               (format #f
                       "== PARSE ERROR ==
filename = ~a
row ~a	column ~a	ctx = ~a
~a ; ~a = ... : ...~%~%"
                       (get-filename ctx)
                       (get-row ctx) (get-col ctx) (get-ctx ctx)
                       (get-line-key ctx) (get-param-key ctx))))))))))


