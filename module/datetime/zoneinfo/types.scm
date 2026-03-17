(define-module (datetime zoneinfo types)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (datetime core)
  :export (
           zi-rule zi-rule?
           rule-name rule-from rule-to rule-in
           rule-on rule-at rule-save rule-letters

           zone-entry zone-entry?
           zone-entry-stdoff zone-entry-rule
           zone-entry-format zone-entry-until

           zone-link zone-link?
           link-name link-target

           zoneinfo
           zoneinfo?

           ;; TODO rewrite these to return non-mutable references,
           ;; probably by running hash-map->list internally
           zoneinfo-zones zoneinfo-rules
           cached-zone-expansions

           get-zone get-rule
           )
  )


;; <day-name> := [0..6]

(define-type (zi-rule)                  ; EXPORTED
  (rule-name    type: symbol?)
  (rule-from    type: integer?) ; year
  (rule-to      type: (or integer? ; year
                          (memv '(only maximum))))

  ;; type should always be "-"
  ;; (rule-type type: (eq? "-") default: "-")

  (rule-in      type: integer?); month number, jan = 1
  (rule-on      type: (or integer? ; month day
                          (tuple-of (eq? 'last)
                                    (memv (weekday-list sun)))
                          (tuple-of (memv '(< >))
                                    (memv (weekday-list sun))
                                    integer?)))
  ;; Defaults to wall time
  (rule-at      type: (pair-of (memv '(utc standard wall))
                               rational?))
  ;; Defaults to standard if number is 0, daylight otherwise
  (rule-save    type: (pair-of (memv '(standard daylight))
                               rational?))
  (rule-letters type: string?))


(define-type (zone-entry)               ; EXPORTED
  (zone-entry-stdoff keyword: stdoff type: rational?)
  ;; Direct numeric values follows same rules as the rule-save
  ;; field of zi-rule above. E.g. a value of 0 defaluts to
  ;; standard, and daylight othervise.
  ;; The source string "-" maps to `(cons 'standard 0)`
  (zone-entry-rule   keyword: rule
                     type: (or symbol?
                               (pair-of (memv '(standard daylight))
                                        rational?)))
  (zone-entry-format keyword: format type: string?)
  ;; Defaults to wall time
  (zone-entry-until  keyword: until
                     type: (or false? (pair-of (memv '(utc standard wall))
                                               unzoned-datetime?))))



(define-type (zone-link)                ; Exported
  (link-name   type: string? keyword: name)
  (link-target type: string? keyword: target))

;; Exported here, for use within the (datetime zoneinfo) namespace,
;; but not re-exported by (datetime zoneinfo).
(define-type (zoneinfo)
  ;; (map symbol? (list <rule>)
  (zoneinfo-rules type: hash-table? keyword: rules)
  ;; (map string? (list <zone-entry>))
  (zoneinfo-zones type: hash-table? keyword: zones)

  ;; zone identifier ("Europe/Stockholm") to stream of all it's expanded rules
  (cached-zone-expansions type: hash-table? default: (make-hash-table))
  )




;; @example
;; (get-zone zoneinfo "Europe/Stockholm")
;; @end example
(define (get-zone zoneinfo name)
  (typecheck name string?)
  (or (hash-ref (zoneinfo-zones zoneinfo) name)
      (scm-error 'misc-error "get-zone" "No zone `~a'" (list name) #f)))

;; @example
;; (get-rule zoneinfo 'EU)
;; @end example
(define (get-rule zoneinfo name)
  (typecheck name symbol?)
  (or (hashq-ref (zoneinfo-rules zoneinfo) name)
      (scm-error 'misc-error "get-rule" "No rule ~s" (list name) #f)))
