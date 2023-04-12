(define-module (vcomponent data-stores vdir)
  :use-module (hnh util)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module (srfi srfi-71)
  :use-module ((srfi srfi-88) :select ())
  :use-module (hnh util path)
  :use-module ((vcomponent formats ical) :select (serialize deserialize))
  :use-module ((ice-9 ftw) :select (scandir))
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
        (calendar (make-vcomponent 'VCALENDAR)))
    (set! (prop calendar 'NAME)  (get-attribute (path this) "displayname")
          (prop calendar 'COLOR) (get-attribute (path this) "color" "#FFFFFF"))
    (for-each (lambda (item) (reparent! calendar item))
              (append-map (lambda (file)
                            (define cal
                              (call-with-input-file (path-append (path this) file)
                                deserialize))
                            (unless (eq? 'VCALENDAR (type cal))
                              (scm-error 'misc-error "get-all<vdir-data-store>"
                                         "Unexpected top level component. Expected VCALENDAR, got ~a. In file ~s"
                                         (list (type cal) file)))
                            (for-each (lambda (child)
                                        (set! (prop child '-X-HNH-FILENAME) file))
                                      (children cal))
                            )
                          files))
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
  (let ((calendar (make-vcomponent 'VCALENDAR)))
    (set! (prop calendar 'VERSION) "2.0"
          (prop calendar 'PRODID) (prodid)
          (prop calendar 'CALSCALE) "GREGORIAN")
    (for-each (lambda (vcomponent) (reparent! calendar vcomponent))
              vcomponents)
    calendar))

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
