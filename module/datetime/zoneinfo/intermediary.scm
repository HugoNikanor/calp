(define-module (datetime zoneinfo intermediary)
  :use-module (hnh util)
  :use-module (hnh util type)
  :use-module (hnh util object)
  :use-module (hnh util lens)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (datetime zoneinfo types)
  :export (
           intermediary->zoneinfo
           limit-intermediary

           parsed-zic-intermediary
           parsed-zic-intermediary?
           ;; intermediary-rules intermediary-rules*
           ;; intermediary-zones intermediary-zones*
           ;; intermediary-links intermediary-links*
           )
  )


(define-type (parsed-zic-intermediary)
  (intermediary-rules keyword: rules type: (list-of (pair-of symbol? (list-of zi-rule?))))
  (intermediary-zones keyword: zones type: (list-of (pair-of string? (list-of zone-entry?))))
  (intermediary-links keyword: links type: (list-of zone-link?)))


(define (resolve-link intermediary root-link)
  (typecheck intermediary parsed-zic-intermediary?)
  (typecheck root-link zone-link?)

  (cons root-link
   (let loop ((link root-link))
     (cond
      ((find (lambda (candidate)
               (string=? (link-target link) (link-name candidate)))
             (intermediary-links intermediary))
       => (lambda (link) (cons link (loop link))))
      ((assoc (link-target link) (intermediary-zones intermediary))
       => list)
      (else
       (scm-error 'msic-error "resolve-link"
                  "Broken link found in zic intermediary: ~s, starting from ~s"
                  (list root-link link) #f))))))

;;; TODO test this
(define (limit-intermediary intermediary . zone-names)
  (typecheck intermediary parsed-zic-intermediary?)
  (typecheck zone-names (list-of string?))
  ;; select all links matching any of zones
  (define link-roots
    (filter (lambda (candidate) (member (link-name candidate) zone-names))
            (intermediary-links intermediary)))

  ;; select all links while resolving to zones
  (define-values (links indirect-zones)
    (car+cdr
     (fold (lambda (root-link st)
             (fold (lambda (entry st)
                     (cond ((zone-link? entry)
                            (modify st car*
                                    (lambda (links)
                                      (lset-adjoin (lambda (a b) (string=? (link-name a) (link-name b)))
                                                   links entry))))
                           (else
                            (modify st cdr*
                                    (lambda (zones)
                                      (lset-adjoin (lambda (a b) (string=? (car a) (car b)))
                                                   zones entry))))))
                   st (resolve-link intermediary root-link)))
           (cons '() '())
           link-roots)))


  (define zones
    (lset-union
     (lambda (a b) (string=? (car a) (car b)))
     indirect-zones
     (filter (lambda (p) (member (car p) zone-names))
             (intermediary-zones intermediary))))

  (define rule-names
    (fold (lambda (zone rule-names)
            (apply lset-adjoin eq? rule-names
                   (filter symbol? (map zone-entry-rule (cdr zone)))))
          '()
          zones))


  (define rules
    (filter (lambda (rule) (memv (car rule) rule-names))
            (intermediary-rules intermediary)))

  (parsed-zic-intermediary
   rules: rules
   zones: zones
   links: links
   ))

(define (intermediary->zoneinfo intermediary)
  (typecheck intermediary parsed-zic-intermediary?)

  (define zones (make-hash-table))
  (define rules (make-hash-table))

  ;; group rules and put in map
  (for-each (lambda (group) (hashq-set! rules (car group) (cdr group)))
            (intermediary-rules intermediary))

  ;; put zones in map
  (for-each (lambda (zone) (hash-set! zones (car zone) (cdr zone)))
            (intermediary-zones intermediary))

  ;; resolve links to extra entries in the zone map
  (for-each (lambda (link)
              (hash-set! zones (link-name link)
                         (cdr (last (resolve-link intermediary link)))))
            (intermediary-links intermediary))

  (zoneinfo rules: rules zones: zones))
