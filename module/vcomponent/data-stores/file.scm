;;; Commentary:
;;; Data store which stores all events in a single file
;;;
;;; NOTE this data store is not recommended for advanced
;;; operations. For example, it only allows one instance of each timezone
;;; (per TZID), meaning that conflicting definitions will overwrite each
;;; other. It also drops any data stored as properties on the containing
;;; VCALENDAR object on any submitted VEVENT.
;;; Code:

(define-module (vcomponent data-stores file)
  :use-module (oop goops)
  :use-module (srfi srfi-1)
  :use-module ((srfi srfi-45) :select (delay force))
  :use-module ((srfi srfi-71))
  :use-module ((srfi srfi-88) :select ())
  :use-module (vcomponent)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent type version)
  :use-module (vcomponent formats)
  :use-module ((vcomponent create) :select (vcalendar))
  :use-module (hnh util)
  :use-module (hnh util bimap)
  :use-module (hnh util exceptions)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util uuid)
  :use-module (calp util config)
  :use-module (xattr)
  :use-module (ice-9 regex)
  :use-module ((scheme base) :select (string->utf8 utf8->string))
  :export (create-instance)
  )

;;; TODO inotify on the file, in case another program modifies it

(define-config xattr-prefix "user.calp"
  pre: (ensure string?))

(define-config xattr-href-prefix
  ;; TODO is this evaluated at time of use, or time of define?
  ;; Its only useful if it's at time of use.
  (string-append (xattr-prefix) ".href")
  pre: (ensure string?))

;;; Return the xattr key used to reference a given href
(define (href-xattr href)
  (string-append (xattr-href-prefix) href))

;;; Evaluates `expr` and returns its value, if it throws an ENOENT
;;; error, instead return `deflt`. All other errors are re-thrown.
(define-syntax-rule (enoent-or expr dflt)
  (catch 'system-error
    (lambda () expr)
    (lambda (err proc fmt args data)
      (if (= ENOENT (car data))
          dflt
          (throw err proc fmt args data)))))

(define-class <file-internals> ()
  ;; Place in filesystem this file is stored
  (path getter: path init-keyword: path: init-value: #f)

  ;; How the data should be serialized.
  ;; (iCalendar, xCal, ...)
  (data-format init-keyword: format:
               getter: data-format
               init-value: #f)

  (root-object  accessor: %root-object
                setter: %set-root-object!
                )                       ; type: vcomponent?
  (event-by-uid getter: %event-by-uid init-form: (make-hash-table))
  (tz-by-tzid   getter: %tz-by-tzid   init-form: (make-hash-table))

  (href-uid-map getter: href-uid-map init-form: (bimap))

  (pending-xattr-operations
   getter: pending-xattr-operations
   init-form: (make-hash-table))
  )

(define (queue-set-xattr! self key value)
  (typecheck self (is-a? <file-internals>))
  (hash-set! (pending-xattr-operations self)
             key (cons 'set value)))

(define (queue-remove-xattr! self key)
  (typecheck self (is-a? <file-internals>))
  (hash-set! (pending-xattr-operations self)
             key
             '(delete)))

(define (execute-queued-xattr! self)
  (typecheck self (is-a? <file-internals>))
  (hash-for-each
   (lambda (key op)
     (case (car op)
       ((set) (set-xattr! (path self) key (cdr op)))
       ((delete) (remove-xattr! (path self) key))))
   (pending-xattr-operations self))
  (hash-clear! (pending-xattr-operations self)))

(define-class <file-data-store> (<calendar-data-store>)
  (path        getter: path        init-keyword: path:  init-value: #f)
  (data-format getter: data-format init-keyword: media: init-value: #f)
  (internals   getter: internals))

(define (vcalendar? x)
  (and (vcomponent? x)
       (eq? 'VCALENDAR (type x))))

(define-method (initialize (self <file-data-store>) args)
  (next-method)
  (slot-set! self 'internals
             (delay (make <file-internals>
                      path:   (path self)
                      format: (data-format self)))))

(define* (create-instance key: path media)
  (typecheck path string?)
  (typecheck media string?)

  (define-values (media-type media-parameters)
    (car+cdr ((@ (web http) parse-header) 'content-type media)))

  ;; TODO currently, we ignore the first half of the content type.
  ;; This should be changed once data format modules are moved into a proper module hierarchy.
  (define media-module
    (string->symbol (list-ref (string-split (symbol->string media-type) #\/) 1)))

  (make <file-data-store>
    path: path
    media: (module-ref (resolve-interface `(vcomponent formats ,media-module))
                       'format)))

(define-method (initialize (self <file-internals>) args)
  (next-method)

  (typecheck (path self) string?)

  (define dflt (vcalendar
                prodid: ((@ (calp) prodid))
                version: (vcalendar-version max: "2.0")
                calscale: "GREGORIAN"))

  (define root
    (enoent-or
     (cond ((string=? "/dev/stdout" (path self))
            dflt)
           ((string=? "/dev/stdin"  (path self))
            ((parser (data-format self)) (current-input-port)))
           (else
            (call-with-input-file (path self)
              (parser (data-format self)))))
     dflt))

  (unless (vcalendar? root)
    (scm-error 'misc-error "initialize<file-data-store>"
               "Root object MUST be a vcalendar. Got ~s"
               (%root-object self) #f))

  ;; TODO ensure appropriate calendar version
  ;; TODO ensure appropriate calendar calscale

  (set! (%root-object self)
    (vcomponent-children root '()))


  (let ((groups (group-by type (vcomponent-children root))))
    (for component in (or (assoc-ref groups 'VEVENT) '())
         (define uid (prop1 component 'UID))
         (hash-set! (%event-by-uid self) uid
                    (cons component (hash-ref (%event-by-uid self) uid '()))))

    (for component in (or (assoc-ref groups 'VTIMEZONE) '())
         ;; If multiple definitions exists for a time zone, last one wins
         (define tzid (prop1 component 'TZID))
         (hash-set! (%tz-by-tzid self) tzid
                    component)))

  (when (hash-ref (%event-by-uid self) #f)
    (warning "One or more components with no UID in ~s, ignoring" (path self))
    ;; We remove invalid entries, since they can't be referenced, and
    ;; it makes the rest of the code cleaner.
    (hash-remove! (%event-by-uid self) #f))

  (when (hash-ref (%tz-by-tzid self) #f)
    (warning "One or more timezones without TZID in ~s" (path self)))

  (for-each (lambda (name)
              (awhen (string-match (format #f "^~a.(.*)$"
                                           (regexp-quote (xattr-href-prefix)))
                                   name)
                     (let* ((href (match:substring it 1))
                            ;; Fallback to ensure string, since we
                            ;; might get swapped out here
                            (uid
                             (cond ((enoent-or (get-xattr (path self) name) #f)
                                    => utf8->string)
                                   (else ""))))
                       (set-left! (href-uid-map self) href uid))))
            (enoent-or
             (list-xattr (path self))
             '()))

  ;; dangling-uids     :: uids referenced by a href, but not present in the file
  ;; referenced-uids   :: uids in the file, properly referenced by a href
  ;; unreferenced-uids :: events in the file, which lacks an href
  (let* ((dangling-uids referenced-uids
                        (lset-diff+intersection
                         string=?
                         (bimap->list (href-uid-map self) (lambda (_ a) a))
                         (hash-map->list (lambda (a _) a) (%event-by-uid self))))
         (unreferenced-uids
          (lset-difference string=?
                           (hash-map->list (lambda (a _) a) (%event-by-uid self))
                           referenced-uids)))

    ;; for each dangling uid, remove the xattr
    (for-each (lambda (uid)
                (queue-remove-xattr!
                 self
                 (href-xattr (get-right (href-uid-map self) uid))))
              dangling-uids)

    ;; for each unreferenced-uids, create a new href
    (for-each (lambda (uid)
                (queue-set-xattr!
                 self
                 ;; ics extension not needed, but looks good
                 (href-xattr (string-append uid ".ics"))
                 (string->utf8 uid)))
              unreferenced-uids)))


(define-method (list-entries (this <file-data-store>))
  (define int (force (internals this)))
  (for (uid . event) in (hash-map->list cons (%event-by-uid int))
       (cons (format #f "~a.ics" uid)
             (wrap-components (%root-object int) (%tz-by-tzid int) event))))

(define-method (store-color (this <file-data-store>))
  (let ((root (%root-object (force (internals this)))))
    (or (prop1 root 'COLOR)
        (prop1 root 'X-APPLE-CALENDAR-COLOR))))

;;; TODO
;;; If COLOR exists, simply update that
;;; If X-APPLE-CALENDAR-COLOR exists:
;;;     update it
;;;     add COLOR property
;;; Otherwise
;;;     add COLOR property
;; (define-method (set-color! (this <file-data-store>) value)
;;   (let ((root (ensure-root! this)))
;;     )
;;   )

(define-method (store-displayname (this <file-data-store>))
  (let ((root (%root-object (force (internals this)))))
    (or (prop1 root 'NAME)
        (prop1 root 'X-WR-CALNAME))))

(define-method (store-description (this <file-data-store>))
  (let ((root (%root-object (force (internals this)))))
    (or (prop1 root 'DESCRIPTION)
        (prop1 root 'X-WR-CALDESC))))

;;; TODO {set,remove}-store-{displayname,description,color}!


(define (component-vlines component)
  (->> component vcomponent-properties table->list (map cdr) concatenate))

(define (referenced-timezones component)
  (filter-map (lambda (vline)
                (table-get (vline-parameters vline) 'TZID))
              (component-vlines component)))





;; Given a set of VEVENT components, wrap them in the VCALENDAR
;; wrapper set up with the creation of this store, and append all
;; referenced timezones also found in this store.
(define (wrap-components root timezones components)
  (let* ((timezone-names
          (-> (map referenced-timezones components)
              concatenate
              (sort string<)
              unique))
         (timezone-components
          (filter-map (lambda (name)
                        (or (hash-ref timezones name)
                            (begin (warning "WARNING: missing timezone: ~s~%" name)
                                   #f)))
                      timezone-names)))
    (vcomponent-children
     root
     (append timezone-components components))))

(define-method (get-by-href (this <file-data-store>) href)
  (define int (force (internals this)))
  (cond ((get-left (href-uid-map int) href)
         => (lambda (uid) (get-by-uid this uid)))
        (else #f)))

(define-method (get-by-uid (this <file-data-store>) uid)
  (define int (force (internals this)))
  (cond ((hash-ref (%event-by-uid int) uid)
         => (lambda (evs) (wrap-components (%root-object int)
                                      (%tz-by-tzid int)
                                      evs)))
        (else #f)))


(define-method (put-event! (this <file-data-store>) href component)
  (typecheck component vcomponent?)

  ;; - assert component is a VCALENDAR
  (unless (eq? 'VCALENDAR (type component))
    (scm-error 'misc-error "put-event!<file-data-store>"
               "Can only put calendar objects, got: ~s"
               (list (type component))
               #f))

  (define groups (group-by type (vcomponent-children component)))

  ;; - assert that at least one vevent exists
  (unless (assoc-ref groups 'VEVENT)
    (scm-error 'misc-error "put-event!<file-data-store>"
               "At least one VEVENT component must exist"
               '() #f))

  ;; - assert all VEVENT children share the same UID
  ;;   NOTE this throws if any component lacks an UID
  (unless (apply string=? (map (extract1 'UID) (assoc-ref groups 'VEVENT)))
    (scm-error 'misc-error "put-event!<file-data-store>"
               "Not all VEVENT components have the same id"
               '() #f))

  ;; - TODO assert all referenced VTIMEZONEs exist in the child set
  ;; (concatenate (map referenced-timezones (assoc-ref groups 'VEVENT)))

  ;; - TODO assert that VCALENDAR properties are compatible with this store
  ;;   + version: 2.0
  ;;   + calscale: GREGORIAN

  ;; - update `event-by-uid`
  (define int (force (internals this)))
  (define uid (prop1 (car (assoc-ref groups 'VEVENT)) 'UID))
  (hash-set! (%event-by-uid int) uid
             (assoc-ref groups 'VEVENT))

  ;; - update `tz-by-tzid` (possible checking that the provided
  ;;   timezone declarations are semantically equivalent to the
  ;;   pre-existing ones)

  (for tz in (or (assoc-ref groups 'VTIMEZONE) '())
       (hash-set! (%tz-by-tzid int) (prop1 tz 'TZID) tz))

  (queue-set-xattr! int (href-xattr href)
                    (string->utf8 uid))


  )

(define-method (remove-by-href! (store <file-data-store>) href)
  (define int (force (internals store)))
  (define uid (get-left (href-uid-map store) href))
  (when href
    (remove-by-uid! store uid)

    (queue-remove-xattr!
     int
     (string-append (xattr-href-prefix)
                    "." href))))

;;; Note: this method doesn't update the href map (by design)
(define-method (remove-by-uid! (store <file-data-store>) uid)
  (define int (force (internals store)))
  (hash-remove! int (%event-by-uid uid)))

(define (merged-calendar this)
  (define int (force (internals this)))
  (wrap-components (%root-object int)
                   (%tz-by-tzid int)
                   (concatenate (hash-map->list
                                 (lambda (_ v) v)
                                 (%event-by-uid int)))))

(define-method (flush! (this <file-data-store>))
  (define (run port)
    ((serializer (data-format this))
     (merged-calendar this)
     port)
    (unless (zero? (port-column port))
      (newline port)))

  (cond ((string=? "/dev/stdout" (path this))
         (run (current-output-port)))
        ((string=? "/dev/stdin" (path this))
         (scm-error 'misc-error "flush!<file-data-store>"
                    "Can't write to stdin"
                    '() #f))
        (else
         ;; TODO atomic output? (currently that doesn't preserve xattrs)
         (call-with-output-file (path this) run)
         (execute-queued-xattr! (force (internals this))))))
