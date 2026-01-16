
;;; calp tz -f "Europe/Stockholm" 10:00
;;; ⇒ 09:00

;;; calp tz [convert] -f "Europe/Stockholm" -t "UTC" 2020-01-01T10:00
;;; ⇒ 09:00

;;; calp tz dump --zone Europe/Stockholm
;;; ⇒ <all zone entries for Europe/Stockholm>

;;; calp tz dump --rule EU
;;; ⇒ <all rule entries for EU>

(define-module (calp entry-points tz)
  :use-module (srfi srfi-71)
  :use-module (calp translation)
  :use-module (datetime)
  :use-module (datetime zoneinfo)
  :use-module (datetime timespec)
  :use-module (datetime timezone)
  :use-module (datetime localtime)
  :use-module (hnh util)
  :use-module (hnh util path)
  :use-module (hnh util options)
  :use-module ((xdg basedir) :prefix xdg-)
  :use-module (ice-9 pretty-print)
  :use-module (ice-9 getopt-long)
  :use-module (ice-9 match)
  ;; TODO actually check locale, and use the right one
  :use-module ((text numbers en) :select (number->string-ordinal))
  :export (main %description))

(define %description (G_ "Query the zoneinfo database."))

(define opt-spec
  `((help (single-char #\h)
          (description ,(G_ "Print this help.")))
    (tzdb-source
     (value #t)
     (description ,(G_ "Where to get timezone data from. Valid values currently are `cache' and `vendored'.")))))

(define convert-opt-spec
  `((from (value #t) (single-char #\f)
          (description ,(G_ "Timezone for input date, defaults to UTC.")))
    (to (value #t) (single-char #\t)
        (description ,(G_ "Timezone for output date, defaults to UTC.")))
    (input-format
     (value #t)
     (description ,(G_ "Format of input date, see string->datetime for format.")))
    (output-format
     (value #t)
     (description ,(G_ "Format to output result in, see datetime->string for format.")))))

(define (print-help)
  (format #t "Usage:~%")
  ;; TODO run this through the markup system
  (format #t "  calp tz [options] dump {--zone z | --rule r} ...~%")
  (format #t "  calp tz [options] list zone-limiters ...~%")
  (format #t "  calp tz [options] convert [convert-options ...]~%"))

(define (main args)

  (define opts (getopt-long args (getopt-opt opt-spec)
                            stop-at-first-non-option: #t))

  (when (option-ref opts 'help #f)
    (print-help)
    (newline)
    ;; TODO why doesn't the markup system include <h> tags?
    (format #t "Common tz flags~%")
    (format #t "===============~%")
    (print-arg-help opt-spec)
    (format #t "dump flags~%")
    (format #t "----------~%")
    (print-arg-help
     ;; Options hard-coded here, since we don't use the option system in the actual code
     ;; (since getopt-long doesn't support repeating options)
     `((zone (value #t) (description ,(G_ "Zone to include in the dump, repeatable.")))
       (rule (value #t) (description ,(G_ "Rule to include in the dump, repeatable.")))))
    (format #t "list flags~%")
    (format #t "----------~%")
    (format #t "convert flags~%")
    (format #t "-------------~%")
    (print-arg-help convert-opt-spec)
    (throw 'return))

  (define intermediary
    (case (string->symbol (option-ref opts 'tzdb-source "vendored"))
      ((cache)
       (apply read-zoneinfo
              ((@ (glob) glob)
               (format #f "~~/.cache/calp/tzdata/{~a}"
                       (string-join '("africa" "antarctica" "asia"
                                      "australasia" "europe" "northamerica"
                                      "southamerica" "etcetera"
                                      "factory" "backward")
                                    ",")))))

      ((vendored)
       (@ (datetime timezone vendored-tzdb) zoneinfo-intermediary))

      (else
       ;; TODO allow custom cached version
       => (lambda (it) (scm-error 'misc-error "entry-point tz"
                             "Invalid intermediary source: ~s"
                             (list it) #f)))))

  ;; ----------------------------------------

  ;; Parse intermediary into real zoneinfo.
  ;; The vendored comes with this pre-bundled, but this avoid implicit
  ;; dependencies, and allows the same control flow irregardless of
  ;; where we get the intermediary from.
  (define zoneinfo (intermediary->zoneinfo intermediary))

  ;; install the relevant zoneinfo
  ((@ (datetime timezone) zoneinfo) zoneinfo)

  (define trailers (option-ref opts '() '()))

  (if (null? trailers)
      (begin (print-help)
             (throw 'return))
      (case (string->symbol (car trailers))
        ((dump)    (run-dump zoneinfo trailers))
        ((list)    (run-list intermediary trailers))
        ((convert) (run-convert zoneinfo trailers))
        (else (print-help)
              (format (current-error-port) (G_ "Unknown mode of operation: ~s~%")
                      (car trailers))
              (throw 'return)))))


(define (dump-zone zone-entries)
  (for-each (lambda (entry)
              (format #t (G_ "stdoff: ~a, rule: ~a, name: ~a, until: ~a~%")
                      (timespec->string (zone-entry-stdoff entry))
                      (let ((r (zone-entry-rule entry)))
                        (cond ((symbol? r) r)
                              ((timespec? r) (timespec->string r))
                              (else "-")))
                      (zone-entry-format entry)
                      (cond ((zone-entry-until entry)
                             => (lambda (u)
                                  (format #f "~a~a"
                                          (datetime->string (cdr u))
                                          (car u))))
                            (else "#f"))))
            zone-entries))

(define (dump-rule rule-entries)
  (for-each
   (lambda (rule)
     (format #t
             ;; This is on the format
             ;; "$RULE_NAME: $DATE_RANGE, $CHANGEOVER, at
             ;; $TIME_TYPE $TIME, set local offset to $TIME"
             ;; For example
             ;; "EU: 1979-1995, on the last Sunday of September, at
             ;; UTC +01:00:00u, set local offset to +00:00:00s".
             (G_ "~a: ~a, ~a, at ~a ~a, set local offset to ~a~%")

             ;; Name ($RULE_NAME)
             (rule-name rule)

             ;; Year interval ($DATE_RANGE)
             (case (rule-to rule)
               ((maximum) (format #f "~a-" (rule-from rule)))
               ((only) (format #f "~a" (rule-from rule)))
               (else (format #f "~a-~a"
                             (rule-from rule)
                             (rule-to rule))))

             ;; Changeover date
             (let ((monthname (date->string (date month: (rule-in rule)) "~B")))
               (match (rule-on rule)
                 ((? number? on)
                  (format #f
                          ;; "on the third of october" ($CHANGEOVER)
                          (G_ "on the ~a of ~a")
                          (number->string-ordinal on)
                          monthname))
                 (('last day)
                  (format #f
                          ;; "on the last sunday of october" ($CHANGEOVER)
                          (G_ "on the last ~a of ~a")
                          (week-day-name day)
                          monthname))
                 (('> week-day month-day)
                  (format #f
                          ;; "First sunday on or after 3 october" ($CHANGEOVER)
                          (G_ "on the first ~a on or after ~a ~a")
                          (week-day-name week-day)
                          month-day monthname))
                 (('< week-day month-day)
                  (format #f
                          ;; last sunday on or before 3 october" ($CHANGEOVER)
                          (G_ "on the last ~a on or before ~a ~a")
                          (week-day-name week-day)
                          month-day monthname))))


             (case (timespec-type (rule-at rule))
               ((standard) (G_ "unmodified local time"))
               ((wall) (G_ "local time"))
               ((utc) "UTC")
               (else => (lambda (c) (format #f (G_ "time type '~a'") c))))

             ;; Changeover time
             (timespec->string (rule-at rule))

             ;; Time change
             (timespec->string (rule-save rule))))
   rule-entries))


(define (run-dump zoneinfo args)
  (let loop ((args (cdr args)))
    (cond ((null? args) 'x)
          ((string=? "--zone" (car args))
           ;; TODO also check if intermediary if this is a real zone,
           ;; or an aliased one. The above example indicates that Stockholm
           ;; has used Soviet timezone rules, since we are actually looking
           ;; at Europe/Berlin

           ;; $ ./calp tz  dump --zone Europe/Stockholm
           ;; förskjutning: +00:53:28, regel: +00s, namn: LMT, t.o.m.: 1893-04-01T00:00:00
           ;; förskjutning: +01, regel: C-Eur, namn: CE%sT, t.o.m.: 1945-05-24T02:00:00
           ;; förskjutning: +01, regel: SovietZone, namn: CE%sT, t.o.m.: 1946-01-01T00:00:00
           ;; förskjutning: +01, regel: Germany, namn: CE%sT, t.o.m.: 1980-01-01T00:00:00
           ;; förskjutning: +01, regel: EU, namn: CE%sT, t.o.m.: #f

           (dump-zone (get-zone zoneinfo (cadr args)))
           (loop (cddr args)))
          ((string=? "--rule" (car args))
           (dump-rule (get-rule zoneinfo (string->symbol (cadr args))))
           (loop (cddr args)))
          (else
           (format (current-error-port)
                   (G_ "Unknown argument: ~s~%") (car args))
           (loop (cdr args))))))


(define (run-list intermediary args)
  (for-each
   (lambda (entry) (format #t "~a~%" entry))
   (hash-map->list (lambda (a _) a)
                   (zoneinfo-zones
                    (intermediary->zoneinfo
                     (if (null? (cdr args))
                         intermediary
                         (apply limit-intermediary intermediary
                                (cdr args)))))
                   )))


(define (run-convert zoneinfo args)
  (define opts (getopt-long args (getopt-opt convert-opt-spec)))

  ;; NOTE this assumes a timezone named "UTC" is available in the database
  (define-values (input-zone-name output-zone-name)
    (match (cons (option-ref opts 'from #f)
                 (option-ref opts 'to #f))
      ((#f . #f) (values "UTC" (get-localtime)))
      ((from . #f) (values from (get-localtime)))
      ((#f . to) (values "UTC" to))
      ((from . to) (values from to))))


  (define trailers (option-ref opts '() '()))
  (define input-datetimes
    (if (null? trailers)
        (list (current-datetime))
        (map
         (lambda (x)
           (string->datetime
            x (option-ref opts 'input-format "~Y-~m-~dT~H:~M:~S")))
         trailers)))

  (for input-datetime in input-datetimes
       (let* ((utc input-offset pretty-input-name
                   (zone->utc (-> input-datetime (tz input-zone-name))))
              (output-datetime output-offset pretty-output-name
                               (utc->zone utc output-zone-name))
              (output-fmt
               (option-ref opts 'output-format "~Y-~m-~dT~H:~M:~S")))
         ;; 2025-12-09T04:33:18+01:00 (CET)
         (format #t "~a~a (~a) = ~a~a (~a)~%"
                 (datetime->string input-datetime output-fmt)
                 (timespec->string (timespec-type input-offset #f) 'm)
                 pretty-input-name
                 (datetime->string output-datetime output-fmt)
                 (timespec->string (timespec-type output-offset #f) 'm)
                 pretty-output-name))))
