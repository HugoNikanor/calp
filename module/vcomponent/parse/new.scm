(define-module (vcomponent parse new)
  :use-module (util)
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
    (link-parents! component)
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


;; (parse-itemline '("DTEND" "TZID=Europe/Stockholm" "VALUE=DATE-TIME" "20200407T130000"))
;; ⇒ #<<vline> value: "20200407T130000" parameters: #<hash-table 7f4294c913a0 2/31>>
;; (define (parse-itemline itemline)
;;   (define all
;;     (reverse
;;      (let loop ((rem (cdr itemline)))
;;        (if (null? (cdr rem))
;;            rem                             ; (list (car rem))
;;            (let* ((kv (car rem))
;;                   (idx (string-index kv #\=)))
;;              (cons (cons (string->symbol (substring kv 0 idx))
;;                          ;; NOTE handle value parsing here?
;;                          (substring kv (1+ idx)))
;;                    (loop (cdr rem))))))))

;;   (make-vline% (car all) (alist->hashq-table (cdr all))))

(define (handle-value! key vline)
  (case key
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
                   (prop vline 'VALUE) 'DATE))))])
  vline)

;; (parse-itemline '("DTEND" "TZID=Europe/Stockholm" "VALUE=DATE-TIME" "20200407T130000"))
;; ⇒ (DTEND . #<<vline> value: #<<datetime> date: 2020-04-07 time: 13:00:00 tz: #f>
;;                      parameters: #<hash-table 7f88fc1207a0 2/31>>
(define (parse-itemline itemline)
  (define key (string->symbol (car itemline)))
  (let loop ((rem (cdr itemline))
             (done '()))
    (if (null? (cdr rem))
        ;; TODO repeated keys
        (cons key
              (handle-value!
               key (make-vline (car rem)
                               (alist->hashq-table done))))
        (let* ((kv (car rem))
               (idx (string-index kv #\=)))
          (loop (cdr rem)
                (cons (cons (string->symbol (substring kv 0 idx))
                            (substring kv (1+ idx)))
                      done))))))


(define (make-component type . children-and-attributes)
  (let* ((children attributes (partition vcomponent? children-and-attributes)))
    ((@@ (vcomponent base) make-vcomponent%) type children #f (alist->hashq-table attributes))))

;; (list (key kv ... value)) → <vcomponent>
(define (parse lst)
  (let loop ((lst lst)
             (stack '()))
    (if (null? lst)
        stack
        (let ((head (car lst)))
          (cond [(string=? "BEGIN" (car head))
                 (loop (cdr lst) (cons (list (string->symbol (cadr head))) stack))]
                [(string=? "END" (car head))
                 (loop (cdr lst)
                       (let* ((frame (reverse (car stack)))
                              (component (apply make-component frame)))
                         (if (null? (cdr stack))
                             component
                             (cons (cons component (cadr stack))
                                   (cddr stack)))))]
                [else
                 (loop (cdr lst)
                       (cons (cons (parse-itemline head)
                                   (car stack))
                             (cdr stack)))])))))

(define (link-parents! component)
  (for child in (children component)
       ((@@ (vcomponent base) set-component-parent!) child component)
       (link-parents! child)))



;; DTEND when missing in VEVENT
;; Repeated keys ('(EXDATE ATTENDEE))
