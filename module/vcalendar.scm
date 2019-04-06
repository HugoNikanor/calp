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

(define (set-attr! component attr value)
  (%vcomponent-set-attribute!
   component
   (as-string attr)
   value))

(define (get-attr component attr)
  (%vcomponent-get-attribute
   component
   (as-string attr)))

(define (get-property component attr prop)
  (%vcomponent-get-property
   component
   (as-string attr)
   (as-string prop)))

(define (set-property! component attr prop val)
  (%vcomponent-set-property!
   component
   (as-string attr)
   (as-string prop)
   val))

(define-public prop
  (make-procedure-with-setter
   get-property
   set-property!))

(define-public (properties component attr)
  (%vcomponent-property-list component (as-string attr)))

;; Enables symmetric get and set:
;; (set! (attr ev "KEY") 10)
(define-public attr (make-procedure-with-setter get-attr set-attr!))

;; (define-public type %vcomponent-get-type)
(define-public type (make-procedure-with-setter
                     %vcomponent-get-type
                     %vcomponent-set-type!))
(define-public parent %vcomponent-parent)
(define-public push-child! %vcomponent-push-child!)
(define-public (attributes component) (map string->symbol (%vcomponent-attribute-list component)))

(define-public copy-vcomponent %vcomponent-shallow-copy)

(define-public filter-children! %vcomponent-filter-children!)

(define-public (search cal term)
  (cdr (let ((events (filter (lambda (ev) (eq? 'VEVENT (type ev)))
                             (children cal))))
         (find (lambda (ev) (string-contains-ci (car ev) term))
               (map cons (map (cut get-attr <> "SUMMARY")
                              events)
                    events)))))

(define-public (extract field)
  (cut get-attr <> field))

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
                                           (let ((zones (children cal 'VTIMEZONE)))
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
