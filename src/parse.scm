
(define-module (parse)
  :use-module (rnrs io ports)
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-9)
  :use-module ((ice-9 textual-ports) :select (unget-char))
  :use-module ((ice-9 ftw) :select (scandir ftw)))



(define-record-type <vcomponent>
  (make-vcomponent% type children parent attributes)
  vcomponent?
  (type component-type)
  (children get-component-children set-component-children!)
  (parent get-component-parent set-component-parent!)
  (attributes get-component-attributes))

(define* (make-vcomponent #:optional (type 'VIRTUAL))
  (make-vcomponent% type '() #f (make-hash-table #x10)))

(define (add-child! parent child)
  (set-component-children! parent (cons child (get-component-children parent)))
  (set-component-parent! child parent))

(define* (get-attribute-value component key #:optional default)
  (cond [(hashq-ref (get-component-attributes component)
                    key #f)
         => cdr]
        [else default]))

(define (get-attribute component key)
  (hashq-ref (get-component-attributes component)
             key))

(define (set-attribute! component key value)
  (let ((ht (get-component-attributes component)))
   (cond [(hashq-ref ht key #f)
          => (lambda (pair) (set-cdr! pair value))]
         [else (hashq-set! ht key (cons (make-hash-table) value))])))



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



(define-record-type <strbuf>
  (make-strbuf% len bytes)
  strbuf?
  (len get-length set-length!)
  (bytes get-bytes set-bytes!))

(define (make-strbuf)
  (make-strbuf% 0 (make-u8vector #x1000)))

(define (strbuf-realloc! strbuf)
  (let* ((len (u8vector-length (get-bytes strbuf)))
         (nv (make-u8vector (ash len 1))))
    (bytevector-copy! (get-bytes strbuf) 0
                      nv 0 len)
    (set-bytes! strbuf nv)))

(define (strbuf->string strbuf)
  (let ((bv (make-u8vector (get-length strbuf))))
    (bytevector-copy! (get-bytes strbuf) 0
                      bv 0
                      (get-length strbuf))
    (bytevector->string bv (native-transcoder))))  ; TODO charset

(define (strbuf-reset! strbuf)
  (set-length! strbuf 0))

(define (strbuf-append! strbuf u8)
  (catch 'out-of-range
    (lambda ()
     (u8vector-set! (get-bytes strbuf)
                    (get-length strbuf)
                    u8))
    (lambda (err . args)
      (strbuf-realloc! strbuf)
      (strbuf-append! strbuf u8)))
  (set-length! strbuf (1+ (get-length strbuf))))



(define (fold-proc ctx c)
  ;; First extra character optionall read is to get the \n if our line
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

(define (parse-calendar port)
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
                  (break (case (get-ctx ctx)
                           [(key)             ; line ended
                            (let ((root-component
                                   (car (get-component-children component))))
                              (set-component-parent! root-component #f)
                              root-component)]
                           [(value)           ; still ending line
                            (set-component-parent! component #f)
                            component]
                           [else => (lambda (a)
                                      (scm-error 'wrong-type-arg "parse-break"
                                                 (string-append
                                                  "Bad context at end of file. "
                                                  "Expected `key' or `value', got ~a")
                                                 (list a) #f))]))]

                 ;; End of line
                 [(memv (integer->char c) '(#\return #\newline))
                  (case (fold-proc ctx c)
                    [(end-of-line)
                     (let ((str (strbuf->string strbuf)))
                       (cond [(string=? (get-line-key ctx) "BEGIN")
                              (let ((child (make-vcomponent (string->symbol str))))
                                (add-child! component child)
                                (set! component child))]

                             [(string=? (get-line-key ctx) "END")
                              (set! component (get-component-parent component))]

                             [else
                              (let ((ht (get-component-attributes component)))
                                ;; TODO repeated keys
                                (hashq-set! ht (string->symbol (get-line-key ctx))
                                            (cons (get-param-table ctx)
                                                  str))
                                (set-param-table! ctx (make-hash-table)))])

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
                 [(and (eq? (get-ctx ctx) 'panam-name) (char=? #\= (integer->char c)))
                  (set-param-key! ctx (strbuf->string strbuf))
                  (strbuf-reset! strbuf)
                  (set-ctx! ctx 'param-value)]

                 ;; Delimiter between parameters (;), or between
                 ;; "something" and attribute value (:)
                 [(memv (integer->char c) '(#\: #\;))
                  (case (get-ctx ctx)
                    [(param-value)
                     (hashq-set! (get-param-table ctx)
                                 (get-param-key ctx)
                                 (strbuf->string strbuf))
                     (strbuf-reset! strbuf)]
                    [(key)
                     (set-line-key! ctx (strbuf->string strbuf))
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



(define-public (read-vcalendar path)
  (define st (stat path))
  (case (stat:type st)
    [(regular) (list (call-with-input-file path parse-calendar))]
    [(directory)
     (map (lambda (fname)
            (call-with-input-file
                (string-append path file-name-separator-string fname)
              parse-calendar))
          (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                    (string= "ics" (string-take-right s 3))))))]
    [(block-special char-special fifo socket unknown symlink)
     => (lambda (t) (error "Can't parse file of type " t))]))


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
