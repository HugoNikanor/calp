;;; Commentary:
;; Zoneinfo Compiler.
;;
;; Compiles plain-text zoneinfo files to guile data.
;; Replaces zic(8), since I need the "raw" recurrence rules.
;;
;; For a source of data see:
;; https://data.iana.org/time-zones/tz-link.html or
;; https://github.com/eggert/tz.
;;; Code:
(define-module (datetime zic)
  :use-module (util)
  :use-module (util exceptions)
  :use-module (datetime)
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-9)
  :use-module (srfi srfi-9 gnu))


(define-public (read-zoneinfo . ports-or-filenames)
  (parsed-zic->zoneinfo
   (concatenate (map (lambda (port-or-filename)
                       (if (port? port-or-filename)
                           (parse-zic-file port-or-filename)
                           (call-with-input-file port-or-filename parse-zic-file))))
                ports-or-filenames)))



(define-immutable-record-type <timespec> ; EXPORTED
  (make-timespec time sign type)
  timespec?
  (time timespec-time)                  ; <time>
  (sign timespec-sign)                  ; '+ | '-
  ;; types:
  ;; w - wall clock time (local time)
  ;; s - standard time without daylight savings adjustments
  ;; u, g, z - Universal time
  (type timespec-type))                 ; char

(export timespec? timespec-time timespec-sign timespec-type)

;; <day-name> := 'mon | 'tue | 'wed | 'thu | 'sat | 'sun

(define-immutable-record-type <rule>    ; EXPORTED
  ;; type should always be "-"
  (make-rule name from to #|type|# in on at save letters)
  rule?
  (name rule-name)                      ; string
  (from rule-from)                       ; int (year) | 'minimum | 'maximum
  (to rule-to)                           ; int (year) | 'minimum | 'maximum
  (in rule-in)                           ; int (month number)
  (on rule-on)                           ; int (month day) | ('last <day-name>) | (<day-name> int ['< | '>])
  (at rule-at)                           ; <timespec>
  (save rule-save)                       ; <timespec>
  (letters rule-letters)                 ; #f | string
  )

(export rule? rule-name rule-from rule-to rule-in rule-on rule-at rule-save rule-letters)

(define-immutable-record-type <zone-entry> ; EXPORTED
  (make-zone-entry stdoff rule format until)
  zone-entry?
  (stdoff zone-entry-stdoff)                   ; <timespec>
  (rule zone-entry-rule)                       ; #f | symbol | <timespec>
  (format zone-entry-format)                   ; string
  (until zone-entry-until))                    ; <datetime>

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

(define-immutable-record-type <leap-second>  ; "UNUSED"
  (make-leap-second when correction r/s)
  leap-second?
  (when leap-second-when)
  (correction leap-second-correction)
  (r/s leap-second-r/s))

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
      (error "No zone ~a" name)))

;; @example
;; (get-rule zoneinfo 'EU)
;; @end example
(define-public (get-rule zoneinfo name)
  (or (hashq-ref (zoneinfo-rules zoneinfo) name)
      (error "No rule ~a" name)))



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
   [else (error "Unknown month" name)]))


(define (string->weekday name)
  (cond
   [(string-prefix? name "Monday")    'mon]
   [(string-prefix? name "Tuesday")   'tue]
   [(string-prefix? name "Wednesday") 'wed]
   [(string-prefix? name "Thursday")  'thu]
   [(string-prefix? name "Friday")    'fri]
   [(string-prefix? name "Saturday")  'sat]
   [(string-prefix? name "Sunday")    'sun]
   [else (error "Unknown week day" name)]))


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
              (list (string->weekday (substring string 0 (1- idx)))
                    (string->number (substring string (1+ idx)))
                    (symbol (string-ref string (1- idx)))))]))


(define (parse-time string)
  (apply (lambda* (hour optional: (minute "0") (second "0"))
           (time hour: (string->number hour)
                 minute: (string->number minute)
                 ;; discard sub-seconds
                 second: (string->number (car (string-split second #\.)))))
         (string-split string #\:)))



(define* (parse-time-spec string optional: (suffixes '(#\s #\w #\u #\g #\z)))
  (let* ((type string
          (cond [(string-rindex string (list->char-set suffixes))
                 => (lambda (idx)
                      (values (string-ref string idx)
                              (substring string 0 idx)))]
                [else (values #\w string)])))
    (cond [(string=? "-"  string)
           (make-timespec (time) '+ type)]
          [(string-prefix? "-" string)
           (make-timespec (parse-time (string-drop string 1))
                          '- type)]
          [else
           (make-timespec (parse-time string)
                          '+ type)])))


(define* (parse-until year optional: (month "Jan") (day "1") (tm "-"))
  ;; I'm pretty sure that the until rule never has a negative time component
  (let ((timespec (parse-time-spec tm)))
    (datetime date: (date year:  (string->number year)
                          month: (month-name->number month)
                          day:   (string->number day))
              time: (timespec-time timespec)
              tz: (case (timespec-type timespec)
                    [(#\s) #| aoeuoeu oeu aoeuaoeuht aoeu htns|# ""]
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
                                             parsed-from
                                             (parse-from to))
                                         (month-name->number in) ; in
                                         (parse-day-spec on)     ; on
                                         (parse-time-spec at)    ; at
                                         (parse-time-spec save '(#\s #\d)) ; save
                                         (if (string= letters "-") ; letters
                                             #f letters))
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

                       ;; Leap and Exprires mostly ignored

                       [(Leap)
                        (let* (((year month day time correction r/s) args))
                          (loop (cons
                                 (make-leap-second
                                  (datetime
                                   date: (date year: (string->number year)
                                               month: (month-name->number month)
                                               day: (string->number day))
                                   time: (parse-iso-time time))
                                  correction
                                  (cond
                                   [(string-prefix? r/s "Stationary") 'stationary]
                                   [(string-prefix? r/s "Rolling") 'rolling]
                                   [else (error "Invalid r/s" r/s)]))
                                 done) #f))]

                       [(Expires)
                        (let* (((year month day time) args))
                          ;; TODO class here
                          (loop
                           (cons (datetime
                                  date: (date year: (string->number year)
                                              month: (month-name->number month)
                                              day: (string->number day))
                                  time: (parse-iso-time time))
                                 done) #f))]
                       [else
                        (error "Something")]
                       ))]))))))


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
                  (hashq-set! rules (car group) (sort* (cadr group) < rule-from)))
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
                         (target-item (hash-ref zones link #f)))
                    (if (not target)
                        (warning "Unresolved link, target missing ~a -> ~a " name target)
                        (hash-set! zones name target-item))))
                (car it)))

    (make-zoneinfo rules zones)))


;; special case of format which works with %s and %z
(define-public (zone-format fmt-string arg)
  (let ((idx (string-index fmt-string #\%)))
    (case (string-ref fmt-string (1+ idx))
      [(#\s) (string-replace fmt-string arg
                             idx (+ idx 2))]
      [(#\z)
       ;; NOTE No zones seem to currently use %z formatting.
       (warning "%z not yet implemented")
       fmt-string]

      [else (error "Invalid format char")])))
