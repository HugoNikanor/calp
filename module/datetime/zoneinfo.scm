(define-module (datetime zoneinfo)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (calp translation)
  :use-module (datetime zoneinfo types)
  :use-module (datetime zoneinfo intermediary)
  :use-module (datetime zoneinfo zic)
  ;; :use-module (datetime io)
  :use-module (datetime core)
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
(define (zone-format fmt-string arg utc-offset)
  (typecheck fmt-string string?)
  (typecheck arg string?)
  (typecheck utc-offset rational?)

  (cond ((string-index fmt-string #\%)
         => (lambda (idx)
              (string-replace fmt-string
               (case (string-ref fmt-string (1+ idx))
                 [(#\s) arg]

                 [(#\z)
                  ;; TODO format time properly
                  (format #f "~a~s"
                          (if (negative? utc-offset) "-" "+")
                          ;; TODO only have some components
                          (seconds->time (abs utc-offset)))
                  ;; (timespec->string (-> utc-offset (timespec-type #f))
                  ;;                   delimiter: "")
                  ]

                 ;; Not standard, but it feels like good faith to have it
                 [(#\%) "%"]

                 [else (scm-error 'misc-error "zone-format"
                                  ;; first slot is the errornous character,
                                  ;; second is the whole string, third is the index
                                  ;; of the faulty character.
                                  (G_ "Invalid format char ~s in ~s at position ~a")
                                  (list (string-ref fmt-string (1+ idx))
                                        fmt-string
                                        (1+ idx))
                                  #f)])
               idx (+ idx 2))))
        (else fmt-string)))
