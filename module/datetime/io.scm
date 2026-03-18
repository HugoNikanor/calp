(define-module (datetime io)
  :use-module (datetime core)
  :use-module (datetime timezone)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util serialize)
  :use-module (hnh util type)
  :use-module ((hnh util env) :select (with-locale1))
  :use-module (ice-9 format)
  :use-module (ice-9 i18n)
  :use-module (ice-9 match)
  :use-module (ice-9 regex)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :export (
           datetime->string
           datetime->http-date
           date->string
           time->string

           parse-month
           string->datetime
           string->time
           string->date
           parse-ics-date
           parse-ics-time
           parse-ics-datetime
           parse-iso-date
           parse-iso-time
           parse-iso-datetime

           parse-freeform-datetime

           week-day-name
           )
  :re-export (locale-month locale-month-short))


;;; Output


(define* (week-day-name week-day-number optional: truncate-to
                        key: (locale %global-locale))

  ;; NOTE this allows days larger than 7 (sunday if counting from monday).
  (let ((str (locale-day (1+ (modulo week-day-number 7)) locale)))
    ;; I also know about the @var{locale-day-short} method, but I need
    ;; strings of length 2.
    (if truncate-to
        (string-take str truncate-to)
        str)))




(define* (datetime->string
          datetime
          optional:
          (fmt "~1T~3~z")
          (locale %global-locale)
          key: allow-unknown?)
  (define date (datetime-date datetime))
  (define time (datetime-time datetime))
  (with-output-to-string
    (lambda ()
      (fold (lambda (token state)
              (case state
                ((#\~)
                 (case token
                   ((#\~) (display "~"))
                   ((#\H) (format #t "~2'0d" (hour time)))
                   ((#\k) (format #t "~2' d" (hour time)))
                   ((#\M) (format #t "~2'0d" (minute time)))
                   ((#\S) (format #t "~2'0d" (second time)))
                   ((#\Y) (format #t "~4'0d" (year date)))
                   ((#\m) (format #t "~2'0d" (month date)))
                   ((#\d) (format #t "~2'0d" (day date)))
                   ((#\e) (format #t "~2' d" (day date)))
                   ;; Should be same as ~_d
                   ((#\s) (display (datetime->unix-time datetime))) ; epoch time!
                   ((#\1) (display (datetime->string datetime "~Y-~m-~d")))
                   ((#\3) (display (datetime->string datetime "~H:~M:~S")))
                   ((#\A) (display (week-day-name (week-day date)   locale: locale)))
                   ((#\a) (display (week-day-name (week-day date) 3 locale: locale)))
                   ((#\B) (display (locale-month       (month date) locale)))
                   ((#\b) (display (locale-month-short (month date) locale)))
                   ((#\Z) (when (equal? "UTC" (tz datetime)) (display "Z")))
                   ((#\z) (display
                           (cond ((not (tz datetime)) "") ; local time
                                 ((string=? "UTC" (tz datetime))
                                  "Z")  ; special case
                                 (else
                                  ;; TODO change here
                                  "TODO"
                                  #;
                                  (let ((offset _ (query-timezone datetime)))
                                    "TODO"
                                    ;; (timespec->string offset 'm)
                                    )))))
                   ;; date(1) has the following
                   ;; %z ⇒ -0400 (numeric offset)
                   ;; %:z ⇒ -04:00 (numeric offset, colons)
                   ;; %::s ⇒ -04:00:00 (numeric offset, force precission)
                   ;; %Z ⇒ EDT (name of timezone)
                   ;; %z and %Z also exists in strftime(3)
                   (else (unless allow-unknown?
                           (scm-error 'misc-error "datetime->string"
                                      "Invalid format token ~a"
                                      (list token)
                                      #f))))
                 #f)
                (else (unless (char=? #\~ token) (display token)) token)))
            #f
            (string->list fmt)))))

(define* (date->string date optional: (fmt "~1") (locale %global-locale)
                       key: allow-unknown?)
  (datetime->string (datetime date: date)
                    fmt locale
                    allow-unknown?: allow-unknown?))

(define* (time->string time optional: (fmt "~3") (locale %global-locale)
                       key: allow-unknown?)
  (datetime->string (datetime time: time)
                    fmt locale
                    allow-unknown?: allow-unknown?))


;;; Input

(define* (parse-month str optional: (locale %global-locale))
  "Get month number from a (shortened) monthname.
Returns -1 on failure"
  (or
   (find (lambda (n)
           (define name (locale-month n locale))
           (define len (min (string-length name)
                            (string-length str)))
           (string-locale-ci=? (string-take str len)
                               (string-take name len)
                               locale))
         (iota 12 1))
   -1))

(define* (string->datetime string optional: (format-specifier "~Y-~m-~dT~H:~M:~S~Z")
                           (locale %global-locale)
                           key: return-trailing)

  (define (err fmt . args)
    ;; TODO throw a unique error type?
    (scm-error 'misc-error "string->datetime"
               (string-append "When parsing ~s as ~s; " fmt)
               (cons* string format-specifier args)
               (list string format-specifier)))

  (let loop* ((str (string->list string))
              (fmt (string->list format-specifier))
              (dt (datetime))
              (ampm identity))

    (define* (loop str fmt dt optional: (ampm ampm))
      (loop* str fmt dt ampm))

    (cond [(and (null? str) (null? fmt))
           (if return-trailing
               (values (ampm dt) '())
               (ampm dt))]
          [(null? str)
           ;; TODO it would be preferable to error out here. However, that fails for
           ;; optional specifiers (e.g. ~Z).
           ;; Also see the disabled test in "Premature end of string to parse"
           (if return-trailing
               (values (ampm dt) '())
               (ampm dt))
           #; (err "Premature end of string, trailing fmt: ~s" fmt)]
          [(null? fmt)
           (if return-trailing
               (values (ampm dt) str)
               (err "trailing characters: ~s" str))]
          [(and (eq? #\~ (car fmt))
                (null? (cdr fmt)))
           (err "Stray ~ at end of fmt")]
          [(eq? #\~ (car fmt))
           (case (cadr fmt)
             [(#\~) (if (eq? #\~ (car str))
                        (loop (cdr str)
                              (cddr fmt)
                              dt)
                        (err "mismatched symbol, expected ~s got ~s" #\~ (car str)))]
             [(#\Z)
              ;; TODO more timezone support
              ;; RFC 3339 explictly allows a literal 'Z', or an offset on the form
              ;; [+-]~H:~M (§5.6)
              ;; TODO read ISO 8601 and check if a standard way to give "human"
              ;; timezones (such as Europe/Stockholm) exists
              (if (eq? #\Z (car str))
                  (loop (cdr str)
                        (cddr fmt)
                        (tz dt "UTC"))
                  (loop str
                        (cddr fmt)
                        dt))]
             ;; AM/PM
             [(#\p)
              (cond ((string-match "^([AaPp])[.]?[Mm][.]?" (list->string str))
                     => (lambda (m)
                          (loop (drop str (match:end m))
                                (cddr fmt)
                                dt
                                (case (string-ref (match:substring m 1) 0)
                                  ((#\a #\A)
                                   (lambda (dt)
                                     (modify dt (lens-compose time* hour*)
                                             (lambda (x) (if (= x 12) 0 x)))))
                                  ((#\p #\P)
                                   (lambda (dt)
                                     (modify dt (lens-compose time* hour*)
                                             (lambda (x) (if (= x 12)
                                                        x (+ x 12))))))))
                          ))
                    ;; fail here?
                    (else (loop str (cddr fmt) dt)))
              ]
             ;; month by name
             [(#\b #\B #\h)
              (let ((head post
                          (match (cddr fmt)
                            (()                   (values str '()))
                            ;; Manual check so remaining cases becomes clearer
                            ((#\~)                (err "Unexpected ~ at end of fmt"))
                            ((#\~ #\~ rest ...)   (span (lambda (c) (not (eqv? #\~ c))) str))
                            ;; Dissalowed, since we otherwise have no idea where the month name ends.
                            ((#\~ rest ...)       (err "Can't have format specifier directly after month by name"))
                            ((next-char rest ...) (span (lambda (c) (not (eqv? c next-char))) str)))))
                (loop post
                      (cddr fmt)
                      (set dt (lens-compose date* month*)
                           (parse-month (list->string head) locale))))]
             [(#\H #\M #\S #\m #\d)
              ;; This captures both the possibility of a date with a single digit,
              ;; e.g. 7 may, but also compact, digits only, form without delimiters,
              ;; e.g. --0507,
              (let* ((pre post (span-upto 2 char-numeric? str))
                     (num (-> pre list->string string->number)))
                (unless num
                  (err "Missing '~~~a' integer. Tail: ~s"
                       (cadr fmt) (list->string str)))
                (loop
                 post
                 (cddr fmt)
                 (let ((lens
                        (case (cadr fmt)
                          [(#\H) (lens-compose time* hour*)]
                          [(#\M) (lens-compose time* minute*)]
                          [(#\S) (lens-compose time* second*)]
                          [(#\m) (lens-compose date* month*)]
                          [(#\d) (lens-compose date* day*)])))
                   (set dt lens num))))]

             [(#\Y)
              (let* ((pre post (span-upto 4 char-numeric? str))
                     (num (-> pre list->string string->number)))
                (loop
                 post
                 (cddr fmt)
                 (set dt (lens-compose date* year*) num)))]

             [else (err "Unimplemented or incorrect parse token ~S" str)])]
          [else
           (if (eq? (car str) (car fmt))
               (loop (cdr str)
                     (cdr fmt)
                     dt)
               (err "Mismatched symbol, expected ~s got ~s" (car fmt) (car str)))])))


;; TODO both string->time and string->date accepts format tokens which are invalid for them.
;; Should this be filtered out?

(define* (string->time str optional: (fmt "~H:~M:~S") (locale %global-locale)
                       key: return-trailing)
  (call-with-values
      (lambda () (string->datetime str fmt locale return-trailing: return-trailing))
    (case-lambda ((dt) (datetime-time dt))
                 ((dt rem) (values (datetime-time dt) rem)))))

(define* (string->date str optional: (fmt "~Y-~m-~d") (locale %global-locale)
                       key: return-trailing)
  (call-with-values
      (lambda () (string->datetime str fmt locale return-trailing: return-trailing))
    (case-lambda ((dt) (datetime-date dt))
                 ((dt rem) (values (datetime-time dt) rem)))))

(define (parse-ics-date str)
  (string->date str "~Y~m~d"))

(define (parse-ics-time str)
  (string->time str "~H~M~S"))

(define (parse-ics-datetime str)
  (string->datetime str "~Y~m~dT~H~M~S~Z"))

(define (parse-iso-date str)
  (string->date str))

(define (parse-iso-time str)
  (string->time str))

(define (parse-iso-datetime str)
  (string->datetime str))

(define (parse-freeform-datetime str)
  (parse-iso-datetime str))

(define (datetime->http-date dt)
  (typecheck dt datetime?)
  (typecheck (tz dt) (equal? "UTC"))
  (with-locale1
   LC_TIME "C"
   (lambda ()
     (datetime->string dt "~a, ~d ~b ~Y ~H:~M:~S GMT"))))
