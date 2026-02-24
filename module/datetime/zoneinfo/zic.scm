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
(define-module (datetime zoneinfo zic)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util object)
  :use-module (hnh util type)
  :use-module (hnh util lens)
  :use-module (hnh util exceptions)
  :use-module (datetime core)
  :use-module (datetime duration)
  :use-module (datetime arithmetic)
  :use-module (datetime timespec)
  :use-module (datetime zoneinfo types)
  :use-module (datetime zoneinfo intermediary)
  :use-module (ice-9 match)
  :use-module (ice-9 rdelim)
  :use-module (calp translation)
  :export (read-zoneinfo
           execute-day-spec))


;; returns an intermediary object ready to be compiled into the real zoneinfo
(define (read-zoneinfo . ports-or-filenames)
  (parsed-zic->intermediary
   (concatenate
    (map (lambda (port-or-filename)
           (if (port? port-or-filename)
               (parse-zic-file port-or-filename)
               (call-with-input-file port-or-filename parse-zic-file)))
         ports-or-filenames))))



;;; TODO write tests for this, zic(8) gives the following:
;; 5        the fifth of the month
;; lastSun  the last Sunday in the month
;; lastMon  the last Monday in the month
;; Sun>=8   first Sunday on or after the eighth
;; Sun<=25  last Sunday on or before the 25th

(define (execute-day-spec base-date day-spec)
  (match day-spec
    ((? number? on) (day base-date on))
    (('last n)
     (iterate (lambda (d) (date- d (duration day: 1)))
              (lambda (d) (eqv? n (week-day d)))
              (day base-date (days-in-month base-date))))
    (((? (lambda (x) (memv x '(< >))) <>) wday base-day)
     (iterate (lambda (d) ((if (eq? '< <>)
                          date- date+)
                      d (duration day: 1)))
              (lambda (d) (eqv? wday (week-day d)))
              (day base-date base-day)))))



;; INTERNAL
(define-type (zone)
  (zone-name    type: string?)
  (zone-entries type: (list-of zone-entry?)))

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
  (let ((timespec (parse-time-spec tm))
        (base-date (date year:  (string->number year)
                         month: (month-name->number month)
                         day:   1)))

    ;; TODO TODO
    ;; I believe tm can't be negative (since that would be written as
    ;; a positive value the previous day). However, it can be in any of wall,
    ;; utc, or standard time (defaulting to wall)
    ;; HOWEVER, UNTIL follows the same rules as AT from Rule records,
    ;; which CAN be negative
    ;; We DON'T store that in the TZ component of the datetime object,
    ;; since that is reserved for timezone names
    ;; (even though utc could be coded as UTC, and wall as #f, that
    ;; leaves standard time).
    ;; Instead, we should return a new type, datetime-spec
    (cons (or (timespec-type timespec) 'wall)
          (datetime date: (execute-day-spec base-date (parse-day-spec day))
                    time: (timespec-time timespec)))))


(define (parse-zone stdoff rule format . until)
  (zone-entry
   stdoff: (parse-time-spec stdoff)
   rule: (cond [(string=? "-" rule)
                (timespec (time) '+ 'standard)]
               [(char-alphabetic? (string-ref rule 0))
                (string->symbol rule)]
               [else
                (let ((s (parse-time-spec rule)))
                  (modify s timespec-type*
                          (lambda (t)
                            (or t (if (time-zero? (timespec-time s))
                                      'standard 'daylight)))))])
   format: format
   until: (if (null? until)
              #f (apply parse-until until))))



;; strip comments from a single line
(define (strip-comments str)
  (or (and=> (string-index str #\#)
             (lambda (idx) (string-take str idx)))
      str))

;; Returns a list of zones, rules, and links
(define (parse-zic-file port)
  (define lineno 0)
  (let loop ((done '()) (continued #f))
    ;; NOTE
    ;; whitespace and #\# are techically allowed in names, if the name
    ;; is quoted. There however doesn't appear to be ANY quoted strings
    ;; in the zoneinfo db.
    (let ((str (read-line port)))
      (set! lineno = 1+)
      ;; (format (current-error-port) "line ~a: ~s~%" lineno str)
      (if (eof-object? str)
          done
          (let ((tokens (string-tokenize (strip-comments str))))
            (cond [(null? tokens) (loop done continued)]
                  [continued
                   ;; Zone-continuation
                   (let* ((name (car continued))
                          (entries (cadr continued))
                          (zone-entry (apply parse-zone tokens))
                          (zone-entries (cons zone-entry entries)))
                    (if (zone-entry-until zone-entry)
                        (loop done (list name zone-entries))
                        (loop (cons (zone zone-name: name
                                          zone-entries: (reverse zone-entries))
                                    done)
                              #f)))]
                  [else
                   (match tokens
                     (("Rule" name from to type in on at save letters)
                      (let* ((parsed-from (parse-from from))
                             (rule
                              (zi-rule rule-name: (string->symbol name)
                                       rule-from: parsed-from
                                       rule-to: (if (string-prefix? to "only")
                                                    ;; parsed-from
                                                    'only
                                                    (parse-from to))
                                       rule-in: (month-name->number in)
                                       rule-on: (parse-day-spec on)
                                       rule-at: (modify (parse-time-spec at)
                                                        timespec-type*
                                                        (lambda (t) (or t 'wall)))
                                       rule-save:
                                       (let ((s (parse-time-spec save)))
                                         (modify s timespec-type*
                                                 (lambda (t)
                                                   (or t (if (time-zero? (timespec-time s))
                                                             'standard 'daylight)))))
                                       rule-letters: (if (string= letters "-")
                                                         "" letters))))
                        (loop (cons rule done)
                              #f)))
                     (("Zone" name args ...)
                      (let* ((zone-entry (apply parse-zone args))
                             (zones (list zone-entry)))
                        (if (zone-entry-until zone-entry)
                            (loop done (list name zones))
                            (loop (cons (zone zone-name: name
                                              zone-entries: (reverse zones))
                                        done)
                                  #f))))

                     (("Link" target name)
                      (loop (cons (zone-link name: name target: target)
                                  done) #f))
                     ;; There may exist a parser Leap and Expires in the git history
                     (("Leap" _ ...)
                      (throw 'not-implemented (G_ "Leap seconds aren't yet implemented")))
                     (("Expires" _ ...)
                      (throw 'not-implemented (G_ "Leap seconds aren't yet implemented")))
                     (_
                      (scm-error 'misc-error "parse-zic-file"
                                 (G_ "Invalid key ~s.")
                                 (list (car tokens))
                                 #f)))]))))))



;; Takes a list of zones, rules, and links (as provided by parse-zic-file), and
;; returns a zoneinfo object
(define (parsed-zic->intermediary lst)
  (let ((groups (group-by (lambda (item)
                            (cond [(zi-rule? item) 'rule]
                                  [(zone? item) 'zone]
                                  [(zone-link? item) 'link]
                                  [else (warning "Unknown item type ~a" item) #f]))
                          lst)))

    (parsed-zic-intermediary
     links: (or (assoc-ref groups 'link) '())
     zones: (map (lambda (zone) (cons (zone-name zone) (zone-entries zone)))
                 (or (assoc-ref groups 'zone) '()))
     rules: (cond ((assoc-ref groups 'rule)
                   => (lambda (rules)
                        (map (lambda (group)
                               (cons (car group)
                                     (sort* (cdr group)
                                            (lambda (a b) (if (eq? 'minimum) #t (< a b)))
                                            rule-from)))
                             (group-by rule-name rules))))
                  (else '())))
    ))
