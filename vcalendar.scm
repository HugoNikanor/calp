(define-module (vcalendar)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (make-vcomponent children set-attr! get-attr type
                            transform-attr! push-child!))

(setenv "LD_LIBRARY_PATH" (dirname (current-filename)))
(load-extension "libguile-calendar" "init_lib")

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
      (reduce (lambda (cal accum)
                (for-each (cut %vcomponent-push-child! accum <>)
                          (%vcomponent-children cal))
                accum)
              '() (%vcomponent-children (%vcomponent-make path)))))

(define children %vcomponent-children)
(define set-attr! %vcomponent-set-attribute!)
(define get-attr %vcomponent-get-attribute)
(define type %vcomponent-type)

(define push-child! %vcomponent-push-child!)

(define (transform-attr! ev field transformer)
  "Apply transformer to field in ev, and store the result back."
  (set-attr! ev field
             (transformer
              (get-attr ev field))))
