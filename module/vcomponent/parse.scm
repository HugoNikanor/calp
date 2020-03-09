(define-module (vcomponent parse)
  :use-module ((rnrs io ports) :select (get-u8))
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (srfi srfi-26)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 textual-ports) :select (unget-char))
  :use-module ((ice-9 ftw) :select (scandir ftw))

  :use-module (util)
  :use-module (util time)
  :use-module (util strbuf)
  :use-module (vcomponent base)
  :use-module (vcomponent datetime)
  :use-module (datetime util)
  )

(use-modules ((rnrs base) #:select (assert)))




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

(define-macro (with-vline-tz object . body)
  `(let-env ((TZ (and=> (prop ,object 'TZID) car)))
            ,@body))




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

;; Reads a vcomponent from the given port.
(define-public (parse-calendar port)
  ;; (report-time! "Parsing ~a" port)
  (with-input-from-port port
    (lambda ()
      (let ((component (make-vcomponent))
            (ctx (make-parse-ctx (port-filename port)))
            (strbuf (make-strbuf)))
        (define (warning fmt . args)
          (display
           (format #f
                   "== PARSE WARNING ==
filename = ~a
row ~a	column ~a	ctx = ~a
~a ; ~a = ... : ...
~?~%~%"
                   (get-filename ctx)
                   (get-row ctx) (get-col ctx) (get-ctx ctx)
                   (get-line-key ctx) (get-param-key ctx)
                   fmt args)) )

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
                       ;; I believe that an empty line is against the standard
                       ;; in every whey. But it's nice to handle it.
                       (if (and (eq? 'key (get-ctx ctx))
                                (string-null? str))
                           (warning "Unexpected completely empty line")
                           (begin
                             (cond [(eq? 'BEGIN (get-line-key ctx))
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
                                          (if (date? start)
                                              (date+ start (date day: 1))
                                              (datetime+ start (datetime time: (time hour: 1)))))))

                                    (set! component (parent component))]

                                   [else
                                    ;; TODO repeated keys
                                    (let ((it (make-vline str (get-param-table ctx))))
                                      ;; Type specific processing
                                      (case (get-line-key ctx)
                                        [(DTSTART DTEND RECURRENCE-ID)

                                         ;; '("Africa/Ceuta" "Europe/Stockholm" "local")
                                         (let ((tz (or (and=> (prop it 'TZID) car)
                                                       (and (string= "Z" (string-take-right (value it) 1)) "UTC"))))

                                           (let ((type (and=> (prop it 'VALUE) car)))
                                             (if (or (and=> type (cut string=? <> "DATE-TIME"))
                                                     (string-contains (value it) "T"))
                                                 (set! (value it) (parse-datetime (value it) tz)
                                                       (prop it 'VALUE) 'DATE-TIME)
                                                 (set! (value it) (parse-date (value it))
                                                       (prop it 'VALUE) 'DATE))))])


                                      ;; From RFC 5545 §3.6.1
                                      ;; DTEND and DURATION are mutually exclusive
                                      ;; DTSTART is required to exist while the other two are optional.
                                      ;; None can appear more than once.

                                      (set-vline! component (get-line-key ctx) it))
                                    (set-param-table! ctx (make-hash-table))])))

                       (strbuf-reset! strbuf)
                       (ctx-dump-strings! ctx)
                       (set-ctx! ctx 'key))]
                    [(fold) 'noop] ; Good case, here to catch errors in else
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
                     (get-line-key ctx) (get-param-key ctx)))))))))




;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtd.
(define (parse-vdir path)
  (let ((/ (lambda args (string-join args file-name-separator-string 'infix))))
    (let ((color
           (catch 'system-error
             (lambda () (call-with-input-file (/ path "color") read-line))
             (const "#FFFFFF")))
          (name
           (catch 'system-error
             (lambda () (call-with-input-file (/ path "displayname") read-line))
             (const (basename path "/")))))

      (reduce (lambda (item calendar)

                (define-values (events other) (partition (lambda (e) (eq? 'VEVENT (type e)))
                                                         (children item)))

                ;; (assert (eq? 'VCALENDAR (type calendar)))
                (assert (eq? 'VCALENDAR (type item)))

                (for child in (children item)
                     (set! (attr child 'X-HNH-FILENAME)
                       (attr (parent child) 'X-HNH-FILENAME)))

                ;; NOTE The vdir standard says that each file should contain
                ;; EXACTLY one event. It can however contain multiple VEVENT
                ;; components, but they are still the same event.
                ;; In our case this means exceptions to reccurence rules, which
                ;; is set up here, and then later handled in rrule-generate.
                (case (length events)
                  [(0) (format (current-error-port)
                           "WARNING: No events in component~%~a~%"
                           (attr item 'X-HNH-FILENAME))]
                  [(1)
                   (let ((child (car events)))
                    (assert (memv (type child) '(VTIMEZONE VEVENT)))
                    (add-child! calendar child))]

                  ;; two or more
                  [else

                   ;; Sorting on SEQUENCE here would have been nice.
                   ;; But the patches can apparently share a sequence number
                   ;; of 0 with the original event!
                   ;; (╯°□°）╯ ┻━┻
                   (let* ((head (find (negate (extract 'RECURRENCE-ID))
                                      events))
                          (rest (delete head events eq?)))

                     (set! (attr head 'X-HNH-ALTERNATIVES)
                       (sort*! rest ;; HERE
                               date/-time< (extract 'RECURRENCE-ID)))
                     (add-child! calendar head))])

                ;; return
                calendar)
              (make-vcomponent)
              ((@ (ice-9 threads) par-map) (lambda (fname)
                     (let ((fullname (/ path fname)))
                       (let ((cal (call-with-input-file fullname
                                    parse-calendar)))
                         (set! (attr cal 'COLOR) color
                               (attr cal 'NAME) name)
                         cal)))
                   (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                             (string= "ics" (string-take-right s 3))))))))))

;; Parse a vdir or ics file at the given path.
(define-public (parse-cal-path path)
  (define st (stat path))
  (define cal
   (case (stat:type st)
     [(regular)
      (let ((comp (call-with-input-file path parse-calendar)))
        (set! (attr comp 'X-HNH-SOURCETYPE) "file")
        comp) ]
     [(directory)
      (report-time! "Parsing ~a" path)
      (let ((comp (parse-vdir path)))
        (set! (attr comp 'X-HNH-SOURCETYPE) "vdir")
        comp)]
     [(block-special char-special fifo socket unknown symlink)
      => (lambda (t) (error "Can't parse file of type " t))]))

    (unless (attr cal "NAME")
      (set! (attr cal "NAME")
        (or (attr cal "X-WR-CALNAME")
            "[NAMELESS]")))

    cal

  )



;; DEPRECATED
;; find all ics files in a tree, and does something with them
(define-public (read-tree path)
  (define list '())
  (ftw path
       (lambda (filename statinfo flag)
         (case flag
           [(regular)
            (case (stat:type statinfo)
              [(regular)
               (when (and (not (string= "." (string-take filename 1)))
                          (string= "ics" (string-take-right filename 3)))
                 (set! list (cons filename list)))
               #t]
              [else #t])]
           [(directory) #t]
           [else #f])))
  ((@ (ice-9 threads) n-par-map) 12
   (lambda (fname) (call-with-input-file fname parse-calendar))
   list))
