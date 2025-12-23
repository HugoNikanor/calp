(define-module (datetime zoneinfo types)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (datetime core)
  :use-module (datetime timespec)
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

           get-zone get-rule
           )
  )


;; <day-name> := [0..6]

(define-type (zi-rule)                  ; EXPORTED
  (rule-name    type: symbol?)
  (rule-from    type: (or integer? ; year
                          ))
  (rule-to      type: (or integer? ; year
                          (memv '(only maximum))))

  ;; type should always be "-"
  ;; (rule-type type: (eq? "-") default: "-")

  (rule-in      type: integer?); month number
  (rule-on      type: (or integer? ; month day
                     (tuple-of (eq? 'last)
                               (memv (weekday-list sun)))
                     (tuple-of (memv '(< >))
                               (memv (weekday-list sun))
                               integer?)))
  (rule-at      type: timespec?)
  (rule-save    type: timespec?)
  (rule-letters type: string?))

;;; TODO zone-entry collision

(define-type (zone-entry)               ; EXPORTED
  ;; NOTE the letter for this timespec doesn't matter
  (zone-entry-stdoff keyword: stdoff type: timespec?)
  (zone-entry-rule   keyword: rule   type: (or symbol? timespec?))
  (zone-entry-format keyword: format type: string?)
  (zone-entry-until  keyword: until  type: (or false? datetime?)))



(define-type (zone-link)                ; Exported
  (link-name   type: string? keyword: name)
  (link-target type: string? keyword: target))

;; Exported here, for use within the (datetime zoneinfo) namespace,
;; but not re-exported by (datetime zoneinfo).
(define-type (zoneinfo)
  (zoneinfo-rules type: hash-table? keyword: rules)             ; (map symbol? (list <rule>)
  (zoneinfo-zones type: hash-table? keyword: zones)             ; (map string? (list <zone-entry>))
  )




;; @example
;; (get-zone zoneinfo "Europe/Stockholm")
;; @end example
(define (get-zone zoneinfo name)
  (or (hash-ref (zoneinfo-zones zoneinfo) name)
      (scm-error 'misc-error "get-zone" "No zone ~a" (list name) #f)))

;; @example
;; (get-rule zoneinfo 'EU)
;; @end example
(define (get-rule zoneinfo name)
  (or (hashq-ref (zoneinfo-rules zoneinfo) name)
      (scm-error 'misc-error "get-rule" "No rule ~a" (list name) #f)))
