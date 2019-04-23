(define-module (vcalendar)
  #:use-module (vcalendar primitive)
  #:use-module (vcalendar datetime)
  #:use-module (vcalendar recurrence)
  #:use-module (vcalendar timezone)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-19 util)
  #:use-module (srfi srfi-19 setters)
  #:use-module (srfi srfi-26)
  #:use-module ((ice-9 optargs) #:select (define*-public))
  #:use-module (util)
  #:export (make-vcomponent)
  #:re-export (repeating?))

;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtracted from
;; the event DTSTART to get UTC time.

(define string->time-utc
  (compose date->time-utc parse-datetime))

(define (parse-dates! cal)
  "Parse all start times into scheme date objects."

  (for tz in (children cal 'VTIMEZONE)
       (for-each (lambda (p) (mod! (attr p "DTSTART") string->time-utc))
                 (children tz))

       ;; TZSET is the generated recurrence set of a timezone
       (set! (attr tz 'X-HNH-TZSET)
             (make-tz-set tz)))

  (for ev in (children cal 'VEVENT)
       (define date     (parse-datetime (attr ev 'DTSTART)))
       (define end-date (parse-datetime (attr ev 'DTEND)))

       (set! (attr ev "DTSTART") (date->time-utc date)
             (attr ev "DTEND")   (date->time-utc end-date))

       (when (prop (attr* ev 'DTSTART) 'TZID)
         (set! (zone-offset date) (get-tz-offset ev)
               (attr ev 'DTSTART) (date->time-utc date)

               ;; The standard says that DTEND must have the same
               ;; timezone as DTSTART. Here we trust that blindly.
               (zone-offset end-date) (zone-offset date)
               (attr ev 'DTEND) (date->time-utc end-date))))

  ;; Return
  cal)


(define-public (type-filter t lst)
  (filter (lambda (e) (eqv? t (type e)))
          lst))

(define*-public (children component #:optional only-type)
  (let ((childs (%vcomponent-children component)))
    (if only-type
        (type-filter only-type childs)
        childs)))

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

;; Returns the properties of attribute as an assoc list.
;; @code{(map car <>)} leads to available properties.
(define-public (properties attrptr)
  (hash-map->list cons (cdr attrptr)))

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
