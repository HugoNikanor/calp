(define-module (vcalendar)
  #:use-module (vcalendar primitive)
  #:use-module (vcalendar datetime)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (util))

(define (parse-dates! cal)
;;; Parse all start times into scheme date objects.
  (for-each-in (children cal 'VEVENT)
               (lambda (ev)
                 (transform-attr! ev "DTSTART" parse-datetime)
                 (transform-attr! ev "DTEND"   parse-datetime)))
  cal)

(define-public (make-vcomponent path)
  (parse-dates!
   (if (string-ci=? ".ics" (string-take-right path 4))
       ;; == Single ICS file ==
       ;; Remove the abstract ROOT component,
       ;; returning the wanted VCALENDAR component
       (car (%vcomponent-children
             (%vcomponent-make path)))
       ;; == Assume vdir ==
       ;; Also removes the abstract ROOT component, but also
       ;; merges all VCALENDAR's children into the first
       ;; VCALENDAR, and return that VCALENDAR.
       ;;
       ;; TODO the other VCALENDAR components might not get thrown away,
       ;; this since I protect them from the GC in the C code.
       (reduce (lambda (cal accum)
                 (for-each (cut %vcomponent-push-child! accum <>)
                           (%vcomponent-children cal))
                 accum)
               '() (%vcomponent-children (%vcomponent-make path))))))

(define-public (type-filter t lst)
  (filter (lambda (e) (eqv? t (type e)))
          lst))

(define* (children component #:optional only-type)
  (let ((childs (%vcomponent-children component)))
    (if only-type
        (type-filter only-type childs)
        childs)))
(export children)

(define-public set-attr! %vcomponent-set-attribute!)
(define-public get-attr %vcomponent-get-attribute)

;; Enables symmetric get and set:
;; (set! (attr ev "KEY") 10)
(define-public attr (make-procedure-with-setter get-attr set-attr!))

(define-public type %vcomponent-type)
(define-public parent %vcomponent-parent)
(define-public push-child! %vcomponent-push-child!)
(define-public attributes %vcomponent-attribute-list)

(define-public (transform-attr! ev field transformer)
  "Apply transformer to field in ev, and store the result back."
  #;
  (set-attr! ev field
             (transformer
              (get-attr ev field)))

  ;; TODO make transform C primitive.
  ;; Halfing the lookups.
  (set! (attr ev field)
        (transformer (attr ev field))))

;; { (attr ev field) := (transformer (attr ev field)) }

(define-public copy-vcomponent %vcomponent-shallow-copy)
