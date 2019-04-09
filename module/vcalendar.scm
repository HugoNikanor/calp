(define-module (vcalendar)
  #:use-module (vcalendar primitive)
  #:use-module (vcalendar datetime)
  #:use-module (vcalendar recur)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (util)
  #:export (make-vcomponent)
  #:re-export (repeating?))

(define (parse-dates! cal)
  "Parse all start times into scheme date objects."
  (for-each-in (children cal 'VEVENT)
               (lambda (ev)
                 (mod! (attr ev "DTSTART") parse-datetime)
                 (mod! (attr ev "DTEND")   parse-datetime)))
  cal)


(define-public (type-filter t lst)
  (filter (lambda (e) (eqv? t (type e)))
          lst))

(define* (children component #:optional only-type)
  (let ((childs (%vcomponent-children component)))
    (if only-type
        (type-filter only-type childs)
        childs)))
(export children)

(define (get-attr component attr)
  (%vcomponent-get-attribute
   component
   (as-string attr)))

(define (set-attr! component attr value)
  (set-car! (get-attr component (as-string attr))
            value))

(define-public attr*
  (make-procedure-with-setter
   get-attr set-attr!))

(define-public attr
  (make-procedure-with-setter
   (compose car get-attr) set-attr!))

;; value
(define-public v
  (make-procedure-with-setter car set-car!))

(define-public prop
  (make-procedure-with-setter
   (lambda (attr-obj prop-key)
     (hashq-ref (cdr attr-obj) prop-key))
   (lambda (attr-obj prop-key val)
     (hashq-set! (cdr attr-obj) prop-key val))))

(define-public (properties component attr-key)
  (hash-map->list cons (cdr (attr component (as-string attr-key)))))

;; (define-public type %vcomponent-get-type)
(define-public type (make-procedure-with-setter
                     %vcomponent-get-type
                     %vcomponent-set-type!))
(define-public parent %vcomponent-parent)
(define-public push-child! %vcomponent-push-child!)
(define-public (attributes component) (map string->symbol (%vcomponent-attribute-list component)))

(define-public copy-vcomponent %vcomponent-shallow-copy)

(define-public filter-children! %vcomponent-filter-children!)

(define-public (extract field)
  (lambda (e) (attr e field)))

(define-public (extract* field)
  (lambda (e) (attr* e field)))

(define-public (search cal term)
  (cdr (let ((events (filter (lambda (ev) (eq? 'VEVENT (type ev)))
                             (children cal))))
         (find (lambda (ev) (string-contains-ci (car ev) term))
               (map cons (map (extract "SUMMARY")
                              events)
                    events)))))

(define-public (key=? k1 k2)
  (eq? (as-symb k1)
       (as-symb k2)))

(define* (make-vcomponent #:optional path)
  (if (not path)
      (%vcomponent-make)
      (let* ((root (%vcomponent-make path))
             (component
              (parse-dates!
               (case (string->symbol (or (attr root "X-HNH-SOURCETYPE") "no-type"))
                 ;; == Single ICS file ==
                 ;; Remove the abstract ROOT component,
                 ;; returning the wanted VCALENDAR component
                 ((file)
                  (car (%vcomponent-children root)))

                 ;; == Assume vdir ==
                 ;; Also removes the abstract ROOT component, but also
                 ;; merges all VCALENDAR's children into the first
                 ;; VCALENDAR, and return that VCALENDAR.
                 ;;
                 ;; TODO the other VCALENDAR components might not get thrown away,
                 ;; this since I protect them from the GC in the C code.
                 ((vdir)
                  (reduce (lambda (cal accum)
                            (for-each (lambda (component)
                                        (case (type component)
                                          ((VTIMEZONE)
                                           (let ((zones (children accum 'VTIMEZONE)))
                                             (unless (find (lambda (z)
                                                             (string=? (attr z "TZID")
                                                                       (attr component "TZID")))
                                                           zones)
                                               (%vcomponent-push-child! accum component))))
                                          (else (%vcomponent-push-child! accum component))))
                                      (%vcomponent-children cal))
                            accum)
                          '() (%vcomponent-children root)))

                 ((no-type) (throw 'no-type))

                 (else (throw 'something))))))

        (set! (attr component "NAME")
              (attr root      "NAME"))
        (set! (attr component "COLOR")
              (attr root      "COLOR"))
        component)))
