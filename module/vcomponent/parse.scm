(define-module (vcomponent parse)
  :use-module ((rnrs io ports) :select (get-u8))
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-19)
  :use-module (srfi srfi-19 setters)
  :use-module (srfi srfi-19 util)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 textual-ports) :select (unget-char))
  :use-module ((ice-9 ftw) :select (scandir ftw))

  :use-module (util)
  :use-module (util strbuf)
  :use-module (vcomponent base)
  :use-module (vcomponent datetime)
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
  (with-input-from-port port
    (lambda ()
      (let ((component (make-vcomponent))
            (ctx (make-parse-ctx (port-filename port)))
            (strbuf (make-strbuf)))
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
                           ;; component is our "actual" root.
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
                       (cond [(eq? 'BEGIN (get-line-key ctx))
                              (let ((child (make-vcomponent (string->symbol str))))
                                (add-child! component child)
                                (set! component child))]

                             [(eq? (get-line-key ctx) 'END)
                              (set! component (parent component))]

                             [else
                              ;; TODO repeated keys
                              (set-vline! component (get-line-key ctx)
                                          (make-vline str (get-param-table ctx)))
                              (set-param-table! ctx (make-hash-table))])

                       (strbuf-reset! strbuf)
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
                    [else => (lambda (c) (throw 'escape-error "Non-escapable character" c))])
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
            (format (current-error-port)
                    "== PARSE ERROR ==
filename = ~a
row ~a	column ~a	ctx = ~a
~a ; ~a = ... : ...~%~%"
                    (get-filename ctx)
                    (get-row ctx) (get-col ctx) (get-ctx ctx)
                    (get-line-key ctx) (get-param-key ctx))))))))



;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtracted from
;; the event DTSTART to get UTC time.

;; Goes through a vcomponent, finds all it's direct children of type VEVENT
;; and parses their DTSTART and DTEND attributes
(define (parse-dates! cal)
  "Parse all start times into scheme date objects."

  (for ev in (filter (lambda (o) (eq? 'VEVENT (type o))) (children cal))
       (let-env ((TZ (and=> (prop (attr* ev 'DTSTART) 'TZID) car)))
                (let*
                    ((dptr (attr* ev 'DTSTART))
                     (eptr (attr* ev 'DTEND))

                     (date (parse-datetime (value dptr)))
                     (end-date
                      (cond ;; [(attr ev 'DURATION) => (lambda (d) (add-duration ...))]
                       [(not eptr)
                        (let ((d (set (date-hour date) = (+ 1))))
                          (set! (attr ev 'DTEND) d
                                eptr (attr* ev 'DTEND))
                          d)]
                       [(value eptr) => parse-datetime]
                       [else
                        (set (date-hour date) = (+ 1))])))

                  (set! (value dptr) (date->time-utc date)
                        (value eptr) (date->time-utc end-date))

                  (when (prop (attr* ev 'DTSTART) 'TZID)
                    ;; Re-align date to have correect timezone. This is since time->date gives
                    ;; correct, but the code above may (?) fail to update the timezone.
                    (set! (zone-offset date) (zone-offset (time-utc->date (value dptr)))
                          (value dptr) (date->time-utc date)

                          ;; The standard says that DTEND must have the same
                          ;; timezone as DTSTART. Here we trust that blindly.
                          (zone-offset end-date) (zone-offset date)
                          (value eptr) (date->time-utc end-date)))))))


;; Takse a path to a vdir, and returns all ics files in the directory merged
;; together into a single vcalendar. The first found vcalendar is used as the
;; parent. Meaning other vcalendars are discarded.
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
                (assert (eq? 'VCALENDAR (type calendar)))
                (assert (eq? 'VCALENDAR (type item)))
                (for child in (children item)
                     (assert (memv (type child) '(VTIMEZONE VEVENT)))
                     (add-child! calendar child))
                calendar)
              (make-vcomponent)
              (map (lambda (fname)
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
      (let ((comp (parse-vdir path)))
        (set! (attr comp 'X-HNH-SOURCETYPE) "vdir")
        comp)]
     [(block-special char-special fifo socket unknown symlink)
      => (lambda (t) (error "Can't parse file of type " t))]))

    (parse-dates! cal)

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
