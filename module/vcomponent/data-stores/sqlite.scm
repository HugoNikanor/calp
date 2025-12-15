(define-module (vcomponent data-stores sqlite)
  :use-module (oop goops)
  :use-module (vcomponent)
  :use-module (vcomponent data-stores common)
  :use-module (vcomponent media-type common)
  :use-module (vcomponent type duration)
  :use-module (vcomponent type period)
  :use-module (vcomponent type unknown)
  :use-module (vcomponent type recurrence)
  :use-module (vcomponent type geo)
  :use-module (vcomponent type version)
  :use-module (vcomponent type request-status)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util uuid)
  :use-module (web uri)
  :use-module ((web query) :select (encode-query-parameters))
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (sxml namespaced)
  :export (create-instance)
  )


(catch 'misc-error
  (lambda ()
    (use-modules (sqlite3))
    (provide 'data-store-sqlite))
  (lambda args 'no-op))

(define-class <sqlite-data-store> (<calendar-data-store>)
  (path getter: path
        init-keyword: path:
        init-value: #f)
  (db accessor: database)
  )


(define-method (initialize (self <sqlite-data-store>) args)
  (next-method)
  (typecheck (path self) string?)
  (set! (database self) (sqlite-open (path self)))

  (init-db (database self))

  (let ((stmt (sqlite-prepare (database self) "SELECT component, href FROM href")))
    (begin1
     (sqlite-map
      (lambda (v) (hash-set! (href-by-id self)
                        (-> (vector-ref v 0)
                            number->string string->symbol)
                        (vector-ref v 1)))
      stmt)
     (sqlite-finalize stmt))))

(define* (create-instance key: path)
  (make <sqlite-data-store> path: path))

(define-method (store-uri (store <sqlite-data-store>))
  (build-uri 'store
             host: "sqlite"
             path: (path store)))

(define (init-db db)
  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS component
( id INTEGER PRIMARY KEY AUTOINCREMENT
, type TEXT NOT NULL
, parent INTEGER REFERENCES component(id)
)")

  (sqlite-exec db "CREATE INDEX IF NOT EXISTS component_type ON component(type)")

  ;; Recursive component lookups are crazy slow without this
  (sqlite-exec
   db "CREATE INDEX IF NOT EXISTS component_parent ON component(parent)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS property
( id INTEGER PRIMARY KEY AUTOINCREMENT
, property TEXT NOT NULL
, component INTEGER NOT NULL REFERENCES component(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
, type TEXT
-- Any value type is accepted, see documentation
-- for valid values corresponding to each property
, value NOT NULL
)")

  (sqlite-exec db "CREATE INDEX IF NOT EXISTS property_property ON property(property)")
  ;; (sqlite-exec db "CREATE INDEX IF NOT EXISTS prop_idx ON property (property, component)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS parameter
( id INTEGER PRIMARY KEY AUTOINCREMENT
, parameter TEXT NOT NULL
, value TEXT NOT NULL
, property INTEGER NOT NULL REFERENCES property(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
)")

  ;; (sqlite-exec db "CREATE INDEX IF NOT EXISTS param_idx ON PARAMETER (parameter, property)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS duration
( id INTEGER PRIMARY KEY AUTOINCREMENT
, series INTEGER NOT NULL
, property INTEGER NOT NULL REFERENCES property(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
, content TEXT NOT NULL
)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS period
( id INTEGER PRIMARY KEY AUTOINCREMENT
, property INTEGER NOT NULL REFERENCES property(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
, end
)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS href
(href TEXT NOT NULL PRIMARY KEY
, component INTEGER NOT NULL REFERENCES component(id)
  ON DELETE CASCADE
  ON UPDATE CASCADE
)")


  ;; TODO view to find root component from component id

  (sqlite-exec db "
CREATE VIEW IF NOT EXISTS component_trace AS
WITH RECURSIVE trace (root, id, type, parent) AS
(
SELECT id AS root, id, type, parent FROM component
WHERE parent IS NULL
UNION
SELECT trace.root, c.id, c.type, c.parent
FROM component c
INNER JOIN trace ON c.parent = trace.id
)
SELECT root, id, type, parent FROM trace")

  (sqlite-exec db "CREATE INDEX IF NOT EXISTS property_component ON property(component)")
  (sqlite-exec db "CREATE INDEX IF NOT EXISTS parameter_property ON parameter(property)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS metadata
( key TEXT PRIMARY KEY NOT NULL
, value TEXT
)")

  )

;; (define-method (get-all (this <sqlite-data-store>))
;;   (throw 'not-implemented))

(define (vector-car v)
  (vector-ref v 0))

(define (call-with-sqlite-transaction db proc)
  (let ((id (uuid)))
    (catch #t
      (lambda ()
        (sqlite-exec db (format #f "SAVEPOINT '~a'" id))
        (begin1
         (proc db)
         (sqlite-exec db (format #f "RELEASE SAVEPOINT '~a'" id))) )
      (lambda args
        (sqlite-exec db (format #f "ROLLBACK TRANSACTION TO SAVEPOINT '~a'" id))
        (sqlite-exec db (format #f "RELEASE SAVEPOINT '~a'" id))
        (apply throw args)))))

(define (get-metadata db key)
  (let ((stmt (sqlite-prepare db "SELECT value FROM metadata WHERE key = :key")))
    (sqlite-bind-arguments stmt key: key)
    (begin1 (and=> (sqlite-step stmt) vector-car)
            (sqlite-finalize stmt))))

(define (set-metadata! db key value)
  (let ((stmt (sqlite-prepare
               db "INSERT OR REPLACE INTO metadata (key, value) VALUES (:key, :value)")))
    (sqlite-bind-arguments stmt key: key value: value)
    (sqlite-step stmt)
    (sqlite-finalize stmt)))

(define (remove-metadata! db key)
  (let ((stmt (sqlite-prepare db "DELETE FROM metadata WHERE key = :key")))
    (sqlite-bind-arguments stmt key: key)
    (sqlite-step stmt)
    (sqlite-finalize stmt)))

;;; TODO language
(define-method (store-displayname (this <sqlite-data-store>))
  (get-metadata (database this) "displayname"))

(define-method (set-store-displayname! (this <sqlite-data-store>) name)
  (set-metadata! (database this) "displayname" name))

(define-method (remove-store-displayname! (this <sqlite-data-store>))
  (remove-metadata! (database this) "displayname"))

;;; TODO language
(define-method (store-description (store <sqlite-data-store>))
  (get-metadata (database store) "description"))

(define-method (set-store-description! (store <sqlite-data-store>) desc)
  (set-metadata! (database store) "description" desc))

(define-method (remove-store-description! (store <sqlite-data-store>))
  (remove-metadata! (database store) "description"))

(define-method (store-color (this <sqlite-data-store>))
  (get-metadata (database this) "color"))

(define-method (set-store-color! (this <sqlite-data-store>) name)
  (set-metadata! (database this) "color" name))

(define-method (remove-store-color! (store <sqlite-data-store>))
  (remove-metadata! (database store) "color"))

(define-method (store-calendar-timezone (store <sqlite-data-store>))
  (call-with-sqlite-transaction
   (database store)
   (lambda (db)
    (cond ((get-metadata db "calendar-timezone")
           => (lambda (id)
                (assoc-ref (get-entries db "c.root = :id" id: id) #f)))
          (else #f)))))

;; (define-method (write-all! (this <sqlite-data-store>) component)
;;   (call-with-sqlite-transaction
;;    (database this)
;;    (lambda (db) (write-component! db component))))

;;; TODO this is the only put-event! which actually uses href currently,
;;; Ensure all uses href
(define-method (put-event! (this <sqlite-data-store>) href component
                           )
  ;; TODO where is component UID conflicts handled?
  ;; TODO check if the given HREF refers to a component with a different UID (RFC 4791 §5.3.2.1.)
  (call-with-sqlite-transaction
   (database this)
   (lambda (db)
     ;; TODO (remove-component! db ())
     (define component-id (write-component! db component))
     (define stmt
       (sqlite-prepare db "INSERT OR REPLACE INTO href (href, component) VALUES (:href, :component)"))
     (sqlite-bind-arguments stmt href: href component: component-id)
     (sqlite-step stmt)
     (sqlite-finalize stmt))))

(define (duration->sqlite-time-offset dur)
  (define ± (duration-sign dur))

  (cond ((duration-week? dur)
         (list (format #f "~a~a days" ± (* 7 (duration-week-count dur)))))
        ((duration-datetime? dur)
         (append
          (cond ((duration-day dur)
                 (lambda (x) (and (number? x) (not (zero? x))))
                 => (lambda (d) (list (format #f "~a~a days" ± d))))
                (else '()))
          (cond ((duration-time dur)
                 (lambda (x) (and (time? x) (not (time-zero? x))))
                 => (lambda (t)
                      (map (lambda (p) (format #f "~a~a ~a" ± (cdr p) (car p)))
                           (remove (compose zero? cdr)
                                   `((hours   . ,(hour t))
                                     (minutes . ,(minute t))
                                     (seconds . ,(second t)))))))
                (else '()))))
        (else '())))

;;; TODO this should use a parameter, to allow custom types
(define (sqlite-serialize key vline)
  (typecheck key symbol?)
  (typecheck vline vline?)
  ;; Checking for differences between apparent and actual type is NOT needed, since
  ;; we store type information regardless.
  (let ((v (vline-value vline)))
    (cond (((@ (scheme base) bytevector?) v) (values 'BINARY v))
          ((boolean? v)        (values 'BOOLEAN (if v 1 0)))
          ;; NOTE this also captures CAL-ADDRESS
          ((uri? v)            (values 'URI (uri->string v)))
          ((date? v)           (values 'DATE (date->string v "~Y-~m-~d")))
          ((datetime? v)
           (values 'DATE-TIME
                   (datetime->string v "~Y-~m-~d ~H:~M:~S")
                   (if (tz v)
                       (table-put (vline-parameters vline)
                                  'TZID (tz v))
                       (vline-parameters vline))))
          ((duration? v)       (values 'DURATION (duration->string v)))
          ((exact-integer? v)  (values 'INTEGER v))
          ;; gulie-sqlite is weird, and REQUIRES exact numbers.
          ;; Possibly file a bug report with them
          ((number? v)         (values 'FLOAT (inexact->exact v)))

          ((period? v)
           (let ((start end params (serialize-period (vline-parameters vline) v "~Y-~m-~d ~H:~M:~S~Z")))

             (values 'PERIOD
                     (format #f "~a/~a" start end)
                     params)))

          ((recur-rule? v)
           (values 'RECUR
                   ((@ (vcomponent media-type text calendar output) recur-rule->rrule-string)
                    (vline-parameters vline)
                    v)))
          ((string? v)         (values 'TEXT v))
          ;; TODO timezone
          ((time? v) (values 'TIME (time->string v "~H:~M:~S")))

          ((timespec? v)
           (values 'UTC-OFFSET (timespec->string v)))

          ;; `X-` prefix to GEO and VERSION, since the standard
          ;; claims them as FLOAT and TEXT respectively, but they have
          ;; special handling, effectively making them their own types.
          ((geo? v)
           (values 'X-GEO (format #f "~a;~a" (geo-latitude v) (geo-longitude v))))
          ((vcalendar-version? v)
           (values 'X-VERSION (string-append (cond ((version-min v)
                                                   => (lambda (vv) (string-append vv ";")))
                                                  (else ""))
                                             (version-max v))))
          ((request-status? v)
           (values 'X-REQUEST-STATUS
                   (with-output-to-string
                     (lambda ()
                       (define escape-chars
                         (@ (vcomponent media-type text calendar output) escape-chars))
                       ;; NOTE this is identical to the code in text/calendar, bar a colon
                       (format #t "~a;~a" (string-join (map number->string (statcode v)) ".")
                               (escape-chars (statdesc v)))
                       (cond ((extdata v)
                              => (lambda (v) (format #t ";~a" (escape-chars v)))))))))

          ((unknown? v)
           (values (cond ((unknown-type v) => string->symbol)
                         (else 'UNKNOWN))
                   (from-unknown v)))

          (else
           (scm-error 'misc-error "sqlite-serialize"
                      "Don't know how to serialize following for SQLite: ~s"
                      (list v) #f)))))


(define-method (remove-by-href! (store <sqlite-data-store>) href)
  (define stmt
    (sqlite-prepare
     (database store)
     "DELETE FROM component WHERE id IN (SELECT id FROM component_trace WHERE href = :href"))
  (sqlite-bind-arguments stmt href: href)
  (sqlite-step stmt)
  (sqlite-finalize stmt))

(define* (write-component! db component optional: parent)
  (call-with-sqlite-transaction
   db
   (lambda (db)
     (define component-id
       (let ((stmt
              (sqlite-prepare
               db "INSERT INTO component (type, parent) VALUES (?, ?) RETURNING id")))
         (sqlite-bind-arguments stmt (symbol->string (type component)) parent)
         (begin1 (vector-car (sqlite-step stmt))
                 (sqlite-finalize stmt))))


     (let ((property-stmt (sqlite-prepare db "
INSERT INTO property (property, component, type, value)
VALUES (?, ?, ?, ?)
RETURNING id"))
           (param-stmt (sqlite-prepare db "
INSERT INTO parameter (parameter, value, property)
VALUES (?, ?, ?)
")))

       (for (key . vlines) in (table->list (vcomponent-properties component))
            (for vline in vlines
                 (define-values (type serialized parameters)
                   (call-with-values (lambda () (sqlite-serialize key vline))
                     (lambda* (t s optional: (p (vline-parameters vline)))
                       (values t s p))))

                 (sqlite-bind-arguments property-stmt (symbol->string key)
                                        component-id (symbol->string type) serialized)
                 (let ((property-id
                        (begin1 (vector-car (sqlite-step property-stmt))
                                (sqlite-reset property-stmt))))

                   ;; TODO if value is a duration
                   ;; insert into `duration` table
                   ;; TODO if value is a period
                   ;; Insert into `period` table

                   (for (key . value) in (table->list parameters)
                        (sqlite-bind-arguments param-stmt (symbol->string key) value property-id)
                        (sqlite-step param-stmt)
                        (sqlite-reset param-stmt))

                   )))
       (sqlite-finalize property-stmt)
       (sqlite-finalize param-stmt))

     (for-each (lambda (child)
                 (write-component! db child component-id))
               (vcomponent-children component))

     component-id)))


(define-once parsers
 (make-parameter
   (alist->table
    (list
     (cons 'BINARY (lambda (_ v) v))
     (cons 'BOOLEAN (lambda (_ v) (not (= v 0))))

     ;; NOTE this might not be used, if CAL-ADDRESS uris are coded as URIs.
     (cons 'CAL-ADDRESS (lambda (_ v) (string->uri v)))

     (cons 'DATE (lambda (_ v) (string->date v)))

     ;; TODO datetime is always stored as is,
     ;; with datetime stored in a paremeter.
     (cons 'DATE-TIME (lambda (params v)
                        (values (tz (string->datetime v "~Y-~m-~d ~H:~M:~S")
                                    (table-get params 'TZID))
                                (table-remove params 'TZID))))

     (cons 'DURATION (lambda (_ v) (string->duration v)))

     (cons 'FLOAT (lambda (_ v) v))
     (cons 'INTEGER (lambda (_ v) v))

     (cons 'PERIOD (lambda (p v)
                     ((@ (vcomponent media-type text calendar parse-semantics) parse-period)
                      p v "~Y-~m-~d ~H:~M:~S~Z")))
     (cons 'RECUR (@ (vcomponent media-type text calendar parse-semantics) parse-recurrence-rule))

     (cons 'TEXT (lambda (_ v) v))

     ;; TODO timezone
     (cons 'TIME (lambda (_ v) (string->time v "~H:~M:~S")))

     (cons 'UTC-OFFSET (lambda (_ v) (parse-time-spec v)))

     (cons 'URI (lambda (_ v) (string->uri v)))

     (cons 'X-GEO (lambda (_ v) (let ((p (string-split v #\;)))
                             (geo y: (string->number (list-ref p 0))
                                  x: (string->number (list-ref p 1))))))
     (cons 'X-VERSION (lambda (_ v)
                        (apply (case-lambda
                                 ((min max)
                                  (vcalendar-version min: min max: max))
                                 ((max)
                                  (vcalendar-version max: max)))
                               (string-split v #\;))))

     (cons 'X-REQUEST-STATUS
           (lambda (_ v) ((@ (vcomponent media-type text calendar parse-semantics) parse-request-status) v)))


     )
    )))

(define (get-parser type)
  (table-get (parsers) type))

(define-method (get-by-href (this <sqlite-data-store>) href)
  (cdar
   (get-entries (database this)
                "href = :href"
                href: href)))

(define-method (list-entries (this <sqlite-data-store>))
  ;; Only doing a single SQL lookup (instead of running get-entries
  ;; for each href) takes the runtime on a store with ~2000 elements
  ;; from 80s down to 5s, most of which is spent in the garbage collector.
  (get-entries (database this) "true"))

;;; TODO bad things happen on no-match
(define (get-entries db filter . filter-args)
 ;; We group parameters, since propreties may be really large, while
 ;; component data is tiny. As it's currently written, parameters
 ;; can't contain record or unit separator characters anywhere.
 (define stmt (sqlite-prepare db (format #f "
SELECT
  c.type
, c.parent
, p.property
, p.component
, p.type
, p.value
, group_concat(parameter.parameter || char(0x1F) || parameter.value, char(0x1E))
-- , href.href
FROM component_trace c
-- TODO this fails for components with 0 properties
RIGHT JOIN property p ON c.id = p.component
FULL OUTER JOIN parameter ON parameter.property = p.id
LEFT JOIN href ON href.component = root
WHERE ~a
GROUP by p.id" filter)))

 (apply sqlite-bind-arguments stmt filter-args)

 (define-values (ids components)
   (car+cdr
    (sqlite-fold
     (lambda (record state)
       (let ((component-type      (string->symbol (vector-ref record 0)))
             (parent-id           (string->symbol (format #f "~a" (vector-ref record 1))))
             (property-name       (string->symbol (vector-ref record 2)))
             (component-id        (-> (vector-ref record 3) number->string string->symbol))
             (property-type       (string->symbol (vector-ref record 4)))
             (property-value      (vector-ref record 5))
             (property-parameters (vector-ref record 6)))

         (define parameters*
           (aif property-parameters
                (alist->table
                 (map (lambda (record)
                        (let ((pair (string-split record #\us)))
                          (cons (string->symbol (car pair))
                                (string-join (cdr pair) (string #\us)))))
                      (string-split it #\rs)))
                (table)))

         (define-values (value parameters)
           (call-with-values
               (lambda ()
                 ((or (get-parser property-type)
                      (lambda (_ v)
                        (unknown
                         ;; As long as we never put in anything except
                         ;; strings, we will never get anything other back
                         v
                         (and (not (eq? 'UNKNOWN property-type))
                              (symbol->string property-type))
                         )))
                  parameters*
                  property-value))
             (lambda* (v optional: (p parameters*))
               (values v p))))

         (-> state
             ;; - For component referenced by ID, set property
             ;;   and parameters by property-name
             (modify
              (lens-compose cdr* (table-focus component-id))
              (lambda (m-component)
                (just
                 (modify (unjust m-component (vcomponent type: component-type))
                         (prop* property-name)
                         (lambda (m-prop)
                           (just
                            (cons (vline params: parameters value: value)
                                  (unjust m-prop '()))))))))
             (modify (lens-compose car* (table-focus parent-id))
                     (lambda (m) (just (lset-adjoin eq? (unjust m '()) component-id)))))))
     (cons (table) (table))
     stmt)))

 (sqlite-finalize stmt)

 ;; TODO possibly cache this list
 (define href-by-id (make-hash-table))
 (let ((stmt (sqlite-prepare db "SELECT component, href FROM href")))
   (sqlite-map (lambda (v) (hash-set! href-by-id (string->symbol (format #f "~a" (vector-ref v 0)))
                                 (vector-ref v 1)))
               stmt))

 ;; TODO TODO we actually only fetch components which have at least one property.
 ;; That means that the following (semantically invalid) iCalendar
 ;; stream crashes, since there is no #f key.
 ;;   BEGIN:VCALENDAR
 ;;     BEGIN:VEVENT
 ;;       UID:2134566
 ;;     END:VEVENT
 ;;   END:VCALENDAR
 (for id in (table-get ids (string->symbol "#f"))
      (cons (hash-ref href-by-id id)
            (let recurse ((id id))
              (vcomponent-children (table-get components id)
                                   (map recurse (or (table-get ids id) '()))))
            )))


(define-method (list-entries/shallow (store <sqlite-data-store>))
  (let ((stmt (sqlite-prepare (database store) "SELECT href FROM href")))
    (begin1 (sqlite-map (lambda (v) (cons (vector-ref v 0) 'x))
                        stmt)
            (sqlite-finalize stmt))))

(define-method (entry-count (store <sqlite-data-store>))
  (let ((stmt (sqlite-prepare (database store) "SELECT count(1) FROM href")))
    (begin1 (vector-car (sqlite-step stmt))
            (sqlite-finalize stmt))))

(define-method (flush! (this <sqlite-data-store>))
  ;; TODO possible commit any pending transactions here
  'noop)

