(define-module (datetime zoneinfo)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (calp translation)
  :use-module (datetime zoneinfo types)
  :use-module (datetime zoneinfo intermediary)
  :use-module (datetime zoneinfo zic)
  ;; :use-module (datetime io)
  :use-module (srfi srfi-71)
  :use-module (datetime core)
  :use-module (ice-9 match)
  :use-module (ice-9 format)
  :export (zone-format)
  :re-export (
              ;; Types
              zi-rule
              zi-rule?
              rule-name rule-from rule-to rule-in
              rule-on rule-at rule-save rule-letters

              zone-entry zone-entry?
              zone-entry-stdoff zone-entry-rule
              zone-entry-format zone-entry-until

              zone-link zone-link?
              link-name link-target

              zoneinfo?

              ;; TODO rewrite these to return non-mutable references,
              ;; probably by running hash-map->list internally
              zoneinfo-zones zoneinfo-rules
              cached-zone-expansions

              get-zone get-rule

              ;; Intermediary
              intermediary->zoneinfo
              limit-intermediary

              parsed-zic-intermediary
              parsed-zic-intermediary?

              ;; Zic
              read-zoneinfo
              execute-day-spec
              )
  )

;; special case of `format` which works with %s and %z
;; TODO rename to something like zone-printf
;; - `fmt-string' is per zic(8)'s Zone
;;   Being a format string optionally containing the placeholders
;;   - `%s', to be filled with the LETTERS field from the active rule
;;   - `%z', to be filled with the UTC offset for the given rule
;; - `arg' is what will be spliced on `%s`
;; - `utc-offset' is used when an `%z` is encountered
(define* (zone-format fmt-string arg offset optional: (time-type 'standard))
  (typecheck fmt-string string?)
  (typecheck arg string?)
  (typecheck offset rational?)
  (typecheck time-type (memv '(standard daylight)))

  (define (offset->string offset colon)
    (with-output-to-string
      (lambda ()
        (display (if (negative? offset) "-" "+"))
        (let* ((h r (floor/ (abs offset) 3600))
               (m s (floor/ r 60)))
          (format #t "~2'0d" h)
          (unless (zero? r)
            (display colon)
            (format #t "~2'0d" m)
            (unless (zero? s)
              (display colon)
              (format #t "~2'0d" s)))))))

  (let loop ((remaining
              (string->list
               (cond ((string-contains fmt-string "/")
                      => (lambda (idx)
                           (case time-type
                             ((standard) (substring fmt-string 0 idx))
                             ((daylight) (substring fmt-string (1+ idx))))))
                     (else fmt-string)))))
    (match remaining
      ('() "")
      ((#\% #\: #\z rest ...)
       (string-append (offset->string offset ":")
                      (loop rest)))
      ((#\% #\z rest ...)
       (string-append (offset->string offset "")
                      (loop rest)))
      ((#\% #\s rest ...) (string-append arg (loop rest)))
      ((#\% #\% rest ...) (string-append "%" (loop rest)))
      ((#\% c rest ...) (scm-error 'misc-error "zone-format"
                                   (G_ "Invalid format char %~s in ~s")
                                   (list c fmt-string)
                                   #f))
      ((c rest ...) (string-append (string c) (loop rest))))))
