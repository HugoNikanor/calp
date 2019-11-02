
(define-module (parse)
  :use-module (rnrs io ports)
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-9)
  :use-module ((ice-9 textual-ports) :select (unget-char))

  )



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
         => (lambda (p) (cdr p))]
        [else default]))

(define (get-attribute component key)
  (hashq-ref (get-component-attributes component) key))

(define (set-attribute! component key value)
  (let ((ht (get-component-attributes component)))
   (cond [(hashq-ref ht key #f)
          => (lambda (pair) (set-cdr! pair value))]
         [else (hashq-set! ht key (cons (make-hash-table) value))])))



(define contexts '(key value param-name param-value escape))

(define-record-type <parse-ctx>
  (make-parse-ctx% row col ctx line-key param-key param-table)
  parse-ctx?
  (row get-row set-row!)
  (col get-col set-col!)
  (ctx get-ctx set-ctx!)
  (line-key get-line-key set-line-key!)
  (param-key get-param-key set-param-key!)
  (param-table get-param-table set-param-table!)
  )

(define (make-parse-ctx)
  (make-parse-ctx% 0 0 'key
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
  (bytes get-bytes)
  )

(define (make-strbuf)
  (make-strbuf% 0 (make-u8vector #x1000))
  )

(define (strbuf->string strbuf)
  (let ((bv (make-u8vector (get-length strbuf))))
    (bytevector-copy! (get-bytes strbuf) 0
                      bv 0
                      (get-length strbuf))
    (bytevector->string bv (native-transcoder))))  ; TODO charset

(define (strbuf-reset! strbuf)
  (set-length! strbuf 0))

(define (strbuf-append! strbuf u8)
  (u8vector-set! (get-bytes strbuf)
                 (get-length strbuf)
                 u8)
  (set-length! strbuf (1+ (get-length strbuf))))

(define (fold-proc ctx c)
  (let ((pair (cons (if (= c (char->integer #\newline))
                        c (get-u8 (current-input-port)))
                    (get-u8 (current-input-port)))))
    (increment-row! ctx)
    (cond [(not (= (char->integer #\newline)
                   (car pair)))
           (throw 'fold-error "Expected newline after CR")]

          [(memv (integer->char (cdr pair)) '(#\space #\tab))
           (increment-column! ctx)
           'fold]

          [else
           ;; TODO check if this failed, and signal a writeback error
           (unget-char (current-input-port)
                       (integer->char (cdr pair)))

           'end-of-line]

          )))

(define (parse-file filename file root)
  (set-current-input-port file)
  (let ((component root)
        (ctx (make-parse-ctx))
        (strbuf (make-strbuf)))
    (catch #t
      (lambda ()
       (while #t
         (let ((c (get-u8 (current-input-port))))
           (cond

            ;; End of file
            [(eof-object? c)
             ;; TODO handle final line here
             (break)]

            ;; End of line
            [(memv (integer->char c) '(#\return #\newline))
             (case (fold-proc ctx c)
               [(error writeback-error) => (lambda (t) (throw t))]
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
                  (set-ctx! ctx 'key))])]

            ;; Escaped characters
            [(char=? (integer->char c) #\\)
             (let ((cc (integer->char (get-u8 (current-input-port)))))
               (case cc
                 ;; Escape character '\' and escaped token sepparated by a newline
                 ;; (since the standard for some reason allows that (!!!))
                 ;; We are at least guaranteed that it's a folded line, so just
                 ;; unfold it and continue trying to find a token to escape.
                 [(#\return #\newline)
                  (case (fold-proc ctx cc)
                    [(end-of-line) (throw 'escape-error "ESC before not folded line")]
                    [(fold)
                     (increment-column! ctx)
                     (strbuf-append! strbuf (get-u8 (current-input-port)))])]
                 [(#\n #\N)
                  (strbuf-append! strbuf (char->integer #\newline))]
                 [(#\; #\, #\\) => (lambda (c) (strbuf-append! strbuf c))]
                 [else (throw 'escape-error "Non-escapable character" cc)])
               (increment-column! ctx))]

            ;; Delimiter between param key and param value
            [(and (eq? (get-ctx ctx) 'panam-name) (char=? (integer->char c) #\=))
             (set-param-key! ctx (strbuf->string strbuf))
             (strbuf-reset! strbuf)
             (set-ctx! ctx 'param-value)]

            ;; Delimiter between parameters (;), or between "something" and attribute value (:)
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
             (set-ctx! ctx (case c
                             [(#\:) 'value]
                             [(#\;) 'param-name]))]

            ;; Regular character
            [else
             (strbuf-append! strbuf c)
             (increment-column! ctx)
             ]))))

     (lambda (err . args)
       (format #t "err = ~a~%ctx = ~a~%args = ~a~%"
               err ctx args)
       ))))



;;; These parts are more or less taken verbatim (with language trans-
;;; literation) from calendar.c. The code is horcrible from a scheme
;;; perspective. TODO replace it with propper code.

(define (open-ics path cal)
  (define f (open-input-file path))
  (parse-file path f cal))

(define (handle-dir cal path)
  'TODO
  ;; TODO
  )

(define (handle-file cal path)
  (set-attribute! cal 'X-HNH-SOURCETYPE "file")
  (open-ics path cal)
  )


(define (read-vcalendar root path)
  (define st (stat path))
  (case (stat:type st)
    [(regular) (handle-file root path)]
    [(directory) (handle-dir root path)]
    [(block-special char-special fifo socket unknown symlink)
     => (lambda (t) (throw t))])
  )

(define (parse-cal-path path)
  (define root (make-vcomponent))
  (read-vcalendar root path)
  root)



(define *path* "/home/hugo/.local/var/cal/STABEN/599ca4a2f8eda362aaac598c999321dcc8004780a1d5cef36019c7e421b70b08.ics")
(define root (parse-cal-path *path*))

(format #t "root = ~a~%" root)



