(define-module (vcomponent data-stores vdir)
  :use-module (hnh util)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module (srfi srfi-71)
  :use-module ((srfi srfi-88) :select ())
  :use-module (hnh util path)
  :use-module ((vcomponent formats ical) :select (serialize deserialize))
  :use-module ((ice-9 ftw) :select (scandir))
  :use-module (ice-9 rdelim)
  :use-module (srfi srfi-1)
  :use-module (vcomponent base)
  :export ())

(define-class <vdir-data-store> (<calendar-data-store>)
  (path getter: path
        init-keyword: path:)
  (loaded-calendar accessor: loaded-calendar
                   init-value: #f)
  (uid-map accessor: uid-map
           init-value: #f)
  )

(define (make-vdir-store path)
  (make <vdir-data-store> path: path))

(define* (get-attribute path key key: dflt)
  (catch 'system-error
    (lambda () (call-with-input-file (path-append path key) read-line))
    (const dflt)))


(define-method (get-all (this <vdir-data-store>))
  (let ((files (scandir (path this) (lambda (item) (string-ci=? "ics" (filename-extension item)))))
        (calendar
         (fold (swap add-child)
               (set-properties (vcomponent type: 'VCALENDAR)
                               (cons 'NAME  (get-attribute (path this) "displayname"))
                               (cons 'COLOR (get-attribute (path this) "color" "#FFFFFF")))
               (append-map (lambda (file)
                             (define cal
                               (call-with-input-file (path-append (path this) file)
                                 deserialize))
                             (unless (eq? 'VCALENDAR (type cal))
                               (scm-error 'misc-error "get-all<vdir-data-store>"
                                          "Unexpected top level component. Expected VCALENDAR, got ~a. In file ~s"
                                          (list (type cal) file) '()))
                             (each cal children
                                   (lambda (child)
                                     (prop child '-X-HNH-FILENAME file))))
                           files))))
    (set! (loaded-calendar this) calendar)
    calendar))


(define-method (get-by-uid (this <vdir-data-store>) (uid <string>))
  (unless (uid-map this)
    (let ((cal
           (or (loaded-calendar this)
               (get-all this))))
      (define ht (make-hash-table))
      (for-each (lambda (ev) (hash-set! ht (uid ev) ev))
                (children cal))
      (set! (uid-map this) ht)))
  (hash-ref m uid #f))


(define (wrap-for-output . vcomponents)
  (fold (swap add-child)
        (set-properties (vcomponent type: 'VCALENDAR)
                        (cons 'VERSION "2.0")
                        (cons 'PRODID (prodid))
                        (cons 'CALSCALE "GREGORIAN"))
        vcomponents))

(define-method (queue-write (this <vdir-data-store>) vcomponent)
  ;; TODO Multiple components
  (let ((filename
         (cond ((prop vcomponent '-X-HNH-FILENAME)
                => identity)
               (else
                (format #f "~a.ics" (prop vcomponent 'UID))))))
    (with-atomic-output-to-file (path-append (path this) filename)
      (lambda () (serialize (wrap-for-output vcomponent) (current-output-port))))))

(define-method (flush (this <vdir-data-store>))
  (sync))

;; (define (get-in-date-interval ))
