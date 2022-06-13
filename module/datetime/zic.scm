;;; Commentary:
;; Zoneinfo Compiler.
;;
;; Compiles plain-text zoneinfo files to guile data.
;; Replaces zic(8), since I need the "raw" recurrence rules.
;;
;; For a source of data see:
;; https://data.iana.org/time-zones/tz-link.html or
;; https://github.com/eggert/tz.
;;
;; See zic(8) for data format
;;; Code:
(define-module (datetime zic)
  :use-module ((hnh util)
               :select (awhen group set when sort* iterate group-by let*))
  :use-module ((hnh util exceptions) :select (warning))
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu)
  :use-module ((vcomponent recurrence internal)
               :select (byday make-recur-rule bymonthday))
  :use-module (calp translation)
  )


;; returns a <zoneinfo> object
(define-public (read-zoneinfo ports-or-filenames)
  (parsed-zic->zoneinfo
   (concatenate
    (map (lambda (port-or-filename)
           (if (port? port-or-filename)
               (parse-zic-file port-or-filename)
               (call-with-input-file port-or-filename parse-zic-file)))
         ports-or-filenames))))




;; <day-name> := [0..6]

(define-immutable-record-type <rule>    ; EXPORTED
  ;; type should always be "-"
  (make-rule name from to #|type|# in on at save letters)
  rule?
  (name rule-name)                      ; string
  (from rule-from)                       ; int (year) | 'minimum | 'maximum
  (to rule-to)                           ; int (year) | 'minimum | 'maximum
  (in rule-in)                           ; int (month number)
  (on rule-on)                           ; int (month day) | ('last <day-name>) | (['< | '>] <day-name> int)
  (at rule-at)                           ; <timespec>
  (save rule-save)                       ; <timespec>
  (letters rule-letters)                 ; string
  )

(export rule? rule-name rule-from rule-to rule-in rule-on rule-at rule-save rule-letters)

(define-immutable-record-type <zone-entry> ; EXPORTED
  (make-zone-entry stdoff rule format until)
  zone-entry?
  (stdoff zone-entry-stdoff)                   ; <timespec>
  (rule zone-entry-rule)                       ; #f | symbol | <timespec>
  (format zone-entry-format)                   ; string
  (until zone-entry-until))                    ; <datetime> | #f

(export zone-entry? zone-entry-stdoff zone-entry-rule zone-entry-format zone-entry-until)


(define-immutable-record-type <zone>    ; INTERNAL
  (make-zone name entries)
  zone?
  (name zone-name)                      ; string
  (entries zone-entries))               ; (list <zone-entry>)

(define-immutable-record-type <link>    ; INTERNAL
  (make-link name target)
  link?
  (name link-name)                      ; string
  (target link-target))                 ; string

(define-immutable-record-type <zoneinfo>  ; EXPORTED
  (make-zoneinfo rules zones)
  zoneinfo?
  (rules zoneinfo-rules)                ; (map symbol (list <rule>))
  (zones zoneinfo-zones))               ; (map string (list <zone-entry>))

(export zoneinfo?)

;; @example
;; (get-zone zoneinfo "Europe/Stockholm")
;; @end example
(define-public (get-zone zoneinfo name)
  (or (hash-ref (zoneinfo-zones zoneinfo) name)
      (scm-error 'misc-error "get-zone" "No zone ~a" (list name) #f)))

;; @example
;; (get-rule zoneinfo 'EU)
;; @end example
(define-public (get-rule zoneinfo name)
  (or (hashq-ref (zoneinfo-rules zoneinfo) name)
      (scm-error 'misc-error "get-rule" "No rule ~a" (list name) #f)))



;; takes an (abriviated) month name, and returns the
;; number of that month.
(define (month-name->number name)
  (cond
   [(string-prefix? name "January")   1]
   [(string-prefix? name "February")  2]
   [(string-prefix? name "Mars")      3]
   [(string-prefix? name "April")     4]
   [(string-prefix? name "May")       5]
   [(string-prefix? name "June")      6]
   [(string-prefix? name "July")      7]
   [(string-prefix? name "August")    8]
   [(string-prefix? name "September") 9]
   [(string-prefix? name "October")  10]
   [(string-prefix? name "November") 11]
   [(string-prefix? name "December") 12]
   [else (scm-error 'misc-error "month-name->number"
                    "Unknown month ~s" (list name)
                    #f)]))


(define (string->weekday name)
  (cond
   [(string-prefix? name "Monday")    mon]
   [(string-prefix? name "Tuesday")   tue]
   [(string-prefix? name "Wednesday") wed]
   [(string-prefix? name "Thursday")  thu]
   [(string-prefix? name "Friday")    fri]
   [(string-prefix? name "Saturday")  sat]
   [(string-prefix? name "Sunday")    sun]
   [else (scm-error 'misc-error "string->weekday"
                    "Unknown week day ~s"
                    (list name) #f)]))


(define (parse-from str)
  (cond
   [(string-prefix? str "minimum") 'minimum]
   [(string-prefix? str "maximum") 'maximum]
   [else (string->number str)]))


;; used for ON field
(define (parse-day-spec string)
  (cond [(string-prefix? "last" string)
         (list 'last (string->weekday (string-drop string 4)))]
        [(string-every char-set:digit string)
         (string->number string)]
        [(string-index string #\=)
         => (lambda (idx)
              (list (symbol (string-ref string (1- idx)))
                    (string->weekday (substring string 0 (1- idx)))
                    (string->number (substring string (1+ idx)))
                    ))]))


(define* (parse-until year optional: (month "Jan") (day "1") (tm "-"))
  ;; I'm pretty sure that the until rule never has a negative time component
  (let ((timespec (parse-time-spec tm)))
    (datetime date: (date year:  (string->number year)
                          month: (month-name->number month)
                          day:   (string->number day))
              time: (timespec-time timespec)
              tz: (case (timespec-type timespec)
                    [(#\s) (warning (_ "what even is \"Standard time\"‽")) ""]
                    [(#\w) #f]
                    ;; Since we might represent times before UTC existed
                    ;; this is a bit of a lie. But it should work.
                    [(#\u #\g #\z) "UTC"]))))


(define (parse-zone . args)
  (let* (((stdoff rule format . until) args))
    (make-zone-entry
     (parse-time-spec stdoff)           ; stdoff
     (cond [(string=? "-" rule) #f]     ; rule
           [(char-alphabetic? (string-ref rule 0))
            (string->symbol rule)]
           [else (parse-time-spec rule)])
     format                             ; format
     (if (null? until)                  ; until
         #f (apply parse-until until)))))





;; strip comments from a single line
(define (strip-comments str)
 (or (and=> (string-index str #\#)
            (lambda (idx) (string-take str idx)))
     str))

;; tokenize a single line
(define (tokenize line)
  (remove string-null? (string-split line char-set:whitespace)))

;; Returns a list of zones, rules, and links
(define (parse-zic-file port)
  (let loop ((done '()) (continued #f))
    ;; NOTE
    ;; whitespace and #\# are techically allowed in names, if the name
    ;; is quoted. There however doesn't appear to be ANY quoted strings
    ;; in the zoneinfo db.
    (let ((str (read-line port)))
      (if (eof-object? str)
          done
          (let* ((tokens (tokenize (strip-comments str))))
            (cond [(null? tokens) (loop done continued)]
                  [continued
                   ;; Zone-continuation
                   (let* (((name entries) continued)
                          (zone-entry (apply parse-zone tokens))
                          (zone-entries (cons zone-entry entries)))
                    (if (zone-entry-until zone-entry)
                        (loop done (list name zone-entries))
                        (loop (cons (make-zone name (reverse zone-entries))
                                    done)
                              #f)))]
                  [else
                   (let* (((type . args) tokens))
                     (case (string->symbol type)

                       [(Rule)
                        (let* (((name from to type in on at save letters) args))
                          (let ((parsed-from (parse-from from)))
                            (loop
                             (cons
                              (make-rule (string->symbol name) ; name
                                         parsed-from           ; from
                                         ;; to
                                         (if (string-prefix? to "only")
                                             ;; parsed-from
                                             'only
                                             (parse-from to))
                                         (month-name->number in) ; in
                                         (parse-day-spec on)     ; on
                                         (parse-time-spec at)    ; at
                                         (parse-time-spec save '(#\s #\d)) ; save
                                         (if (string= letters "-") ; letters
                                             "" letters))
                              done) #f)))]

                       [(Zone)
                        (let* ((zone-entry (apply parse-zone (cdr args)))
                               (zones (list zone-entry)))
                          (if (zone-entry-until zone-entry)
                              (loop done (list (car args) zones))
                              (loop (cons (make-zone (car args) (reverse zones))
                                          done)
                                    #f)))]

                       [(Link)
                        (let* (((target name) args))
                          (loop (cons (make-link name target)
                                      done) #f))]

                       [else
                        ;; NOTE an earlier version of the code the parsers for those.
                        ;; They were removed since they were unused, uneeded, and was
                        ;; technical dept.
                        (scm-error 'misc-error "parse-zic-file"
                                   (_ "Invalid key ~s. Note that leap seconds and expries rules aren't yet implemented.")
                                   (list type)
                                   #f)]
                       ))]))))))


;; Takes a list of zones, rules, and links (as provided by parse-zic-file), and
;; returns a zoneinfo object
(define (parsed-zic->zoneinfo lst)

  (define zones (make-hash-table))
  (define rules (make-hash-table))

  (let ((groups (group-by (lambda (item)
                            (cond [(rule? item) 'rule]
                                  [(zone? item) 'zone]
                                  [(link? item) 'link]
                                  [else (warning "Unknown item type ~a" item) #f]))
                          lst)))

    ;; group rules and put in map
    (awhen (assoc-ref groups 'rule)
      (for-each (lambda (group)
                  (hashq-set! rules (car group) (sort* (cadr group) (lambda (a b) (if (eq? 'minimum) #t (< a b)))
                                                       rule-from)))
                (group-by rule-name (car it))))

    ;; put zones in map
    (awhen (assoc-ref groups 'zone)
      (for-each (lambda (zone)
                  (hash-set! zones (zone-name zone) (zone-entries zone)))
                (car it)))

    ;; resolve links to extra entries in the zone map
    (awhen (assoc-ref groups 'link)
      (for-each (lambda (link)
                  (let* ((name (link-name link))
                         (target (link-target link))
                         (target-item (hash-ref zones target #f)))
                    (if (not target-item)
                        (warning (_ "Unresolved link, target missing ~a -> ~a") name target)
                        (hash-set! zones name target-item))))
                (car it)))

    (make-zoneinfo rules zones)))




;; The first time this rule was/will be applied
(define-public (rule->dtstart rule)
  ;; NOTE 'minimum and 'maximum represent the begining and end of time.
  ;; since I don't have a way to represent those ideas I just set a very
  ;; high and a very low year here. What 'maximum even entails for a start
  ;; time is not noted in the spec.
  (define d (date year: (case (rule-from rule)
                          ((minimum) 0)
                          ((maximum) 9999)
                          (else (rule-from rule)))
                  month: (rule-in rule)
                  day: 1))

  (define dt
    (datetime
     date:
     (let ((on (rule-on rule)))
       (cond [(number? on)
              (set (day d) on)]
             [(eq? 'last (car on))
              (iterate (lambda (d) (date- d (date day: 1)))
                       (lambda (d) (eqv? (cadr on) (week-day d)))
                       (set (day d) (days-in-month d)))]
             [else                      ; < | >
              (let* (((<> wday base-day) on))
                (iterate (lambda (d) ((if (eq? '< <>)
                                     date- date+)
                                 d (date day: 1)))
                         (lambda (d) (eqv? wday (week-day d)))
                         (set (day d) base-day)))]))
     tz: (case (timespec-type (rule-at rule))
           ((#\w) #f)
           ((#\s) (warning (_ "what even is \"Standard time\"‽")) #f)
           ((#\u #\g #\z) "UTC"))))

  (let ((timespec (rule-at rule)))
    ((case (timespec-sign timespec)
       ((+) datetime+)
       ((-) datetime-))
     dt
     (datetime time: (timespec-time timespec)))
    ))

(define-public (rule->rrule rule)
  (if (eq? 'only (rule-to rule))
      #f
      (let ((base (make-recur-rule
                   freq: 'YEARLY
                   interval: 1
                   bymonth: (list (rule-in rule))
                   until: (let ((to  (rule-to rule)))
                            (case to
                              ((maximum) #f)
                              ((minimum) (scm-error 'misc-error "rule->rrule"
                                                    (_ "Check your input")
                                                    #f #f))
                              (else
                               ;; NOTE I possibly need to check the start of
                               ;; the next rule to know when this rule really
                               ;; ends.
                               (datetime
                                date: (date year: to month: 1 day: 1))))))))


        (cond [(number? (rule-on rule))
               (set (bymonthday base)
                    (list (rule-on rule)))]

              [(eqv? 'last (car (rule-on rule)))
               (set (byday base) (list (cons -1 (cadr (rule-on rule)))))]

              [else
               ;; Sun<=25
               ;; Sun>=8
               (let* (((<> wday base-day) (rule-on rule)))
                 (when (eq? '< <>)
                   (warning (_ "Counting backward for RRULES unsupported")))
                 ;; NOTE this only realy works when base-day = 7n + 1, n ∈ N
                 ;; something like Sun>=5 is hard to fix, since we can only
                 ;; say which sunday in the month we want (first sunday,
                 ;; second sunday, ...).
                 (set (byday base)
                      (list
                       (cons (ceiling-quotient base-day 7)
                             wday))))]))))

;; special case of format which works with %s and %z
(define-public (zone-format fmt-string arg)
  (let ((idx (string-index fmt-string #\%)))
    (case (string-ref fmt-string (1+ idx))
      [(#\s) (string-replace fmt-string arg
                             idx (+ idx 2))]
      [(#\z)
       ;; NOTE No zones seem to currently use %z formatting.
       ;; '%z' is NOT a format string, but information about another format string.
       (warning (_ "%z not yet implemented"))
       fmt-string]

      [else (scm-error 'misc-error "zone-format"
                       ;; first slot is the errornous character,
                       ;; second is the whole string, third is the index
                       ;; of the faulty character.
                       (_ "Invalid format char ~s in ~s at position ~a")
                       (list (string-ref fmt-string (1+ idx))
                             fmt-string
                             (1+ idx))
                       #f)])))
