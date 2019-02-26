(define-module (vcalendar)
  #:use-module (vcalendar primitive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (make-vcomponent children set-attr! get-attr type
                            type-filter
                            transform-attr! push-child!))

(define (make-vcomponent path)
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
              '() (%vcomponent-children (%vcomponent-make path)))))

(define (type-filter t lst)
  (filter (lambda (e) (eqv? t (type e)))
          lst))

(define* (children component #:optional only-type)
  (let ((childs (%vcomponent-children component)))
    (if only-type
        (type-filter only-type childs)
        childs)))

(define set-attr! %vcomponent-set-attribute!)
(define get-attr %vcomponent-get-attribute)
(define type %vcomponent-type)
(define parent %vcomponent-parent)
(define push-child! %vcomponent-push-child!)

(define (transform-attr! ev field transformer)
  "Apply transformer to field in ev, and store the result back."
  (set-attr! ev field
             (transformer
              (get-attr ev field))))
