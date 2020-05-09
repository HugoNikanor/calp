(define-module (vcomponent parse new)
  :use-module (util)
  :use-module (util exceptions)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (vcomponent base)
  :use-module (datetime)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-26)
  :use-module ((ice-9 hash-table) :select (alist->hashq-table))
 )

(define-public (parse-calendar port)
  (let ((component (parse (map tokenize (read-file port)))))
    ;; (set! (attr component 'X-HNH-FILENAME) (or (port-filename port) "MISSING"))
    component))


;; (define f (open-input-file (car (glob "~/.local/var/cal/Calendar/c17*"))))

;; port → (list string)
(define (read-file port)
 (let loop ((done '()))
   (let ((line (read-line port)))
     (if (eof-object? line)
         (reverse! done)
         (let ((line (string-trim-right line)))
           (loop
            (if (char=? #\space (string-ref line 0))
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

;; params could be made optional, with an empty hashtable as default
(define (build-vline key value params)
  (case key
    [(DTSTART DTEND RECURRENCE-ID LAST-MODIFIED DTSTAMP EXDATE)

     ;; '("Africa/Ceuta" "Europe/Stockholm" "local")
     (let ((tz (or (hashq-ref params 'TZID)
                   (and (string= "Z" (string-take-right value 1)) "UTC"))))

       (let ((type (hashq-ref params 'VALUE)))
         (if (or (and=> type (cut string=? <> "DATE-TIME"))
                 (string-index value #\T))
             ;; TODO TODO TODO
             ;; we move all parsed datetimes to local time here. This
             ;; gives a MASSIVE performance boost over calling get-datetime
             ;; in all procedures which want to guarantee local time for proper calculations.
             ;; 20s vs 70s runtime on my laptop.
             ;; We sohuld however save the original datetime in a file like X-HNH-DTSTART,
             ;; since we don't want to lose that information.
             (let ((datetime (parse-ics-datetime value tz)))
               (hashq-set! params 'VALUE 'DATE-TIME)
               (values (make-vline key (get-datetime datetime) params)
                       (make-vline (symbol-append 'X-ORIGINAL- key) datetime params)))
             (begin (hashq-set! params 'VALUE 'DATE)
                    (make-vline key (parse-ics-date value) params)))))]

    [else
     (make-vline key
                 (list->string
                  (let loop ((rem (string->list value)))
                    (if (null? rem)
                        '()
                        (if (char=? #\\ (car rem))
                            (case (cadr rem)
                              [(#\n #\N) (cons #\newline (loop (cddr rem)))]
                              [(#\; #\, #\\) => (lambda (c) (cons c (loop (cddr rem))))]
                              [else => (lambda (c) (warning "Non-escapable character: ~a" c)
                                          (loop (cddr rem)))])
                            (cons (car rem) (loop (cdr rem)))))))
                 params)]))

;; (parse-itemline '("DTEND" "TZID=Europe/Stockholm" "VALUE=DATE-TIME" "20200407T130000"))
;; => (DTEND "20200407T130000" #<hash-table 7f88fb094d80 2/31>)
;; (parse-itemline '("DTEND"  "20200407T130000"))
;; => (DTEND "20200407T130000" #<hash-table 7f88facafd20 0/31>)
;; ⇒ (DTEND . #<<vline> value: #<<datetime> date: 2020-04-07 time: 13:00:00 tz: #f>
;;                      parameters: #<hash-table 7f88fc1207a0 2/31>>
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

                 ;; TODO This is an ugly hack until the rest of the code is updated
                 ;; to work on events without an explicit DTEND attribute.
                 (when (and (eq? (type (car stack)) 'VEVENT)
                            (not (attr (car stack) 'DTEND)))
                   (set! (attr (car stack) 'DTEND)
                     (let ((start (attr (car stack) 'DTSTART)))
                       ;; p. 54, 3.6.1
                       ;; If DTSTART is a date then it's an all
                       ;; day event. If DTSTART instead is a
                       ;; datetime then the event has a length
                       ;; of 0?
                       (if (date? start)
                           (date+ start (date day: 1))
                           (datetime+ start (datetime time: (time hour: 1)))))))

                 (loop (cdr lst)
                       (if (null? (cdr stack))
                           ;; return
                           (car stack)
                           ;; TODO link parent here?
                           (begin (add-child! (cadr stack) (car stack))
                                  (cdr stack))
                           ))]
                [else
                 (let* ((key value params (parse-itemline head)))
                   (call-with-values (lambda () (build-vline key value params))
                     (lambda vlines
                       (for vline in vlines
                            (define key (vline-key vline))

                            ;; Which types are allowed to be given multiple times
                            (if (memv (vline-key vline) '(EXDATE ATTENDEE))
                                (aif (attr* (car stack) key)
                                     (set! (attr* (car stack) key) (cons vline it))
                                     (set! (attr* (car stack) key) (list vline)))
                                ;; else
                                (set! (attr* (car stack) key) vline))))))

                 (loop (cdr lst) stack)])))))




;; Repeated keys ('(EXDATE ATTENDEE))
