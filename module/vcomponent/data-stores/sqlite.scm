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
  :use-module (srfi srfi-43)
  :use-module (srfi srfi-71)
  :use-module (srfi srfi-88)
  :use-module (hnh util)
  :use-module (hnh util lens)
  :use-module (hnh util optional)
  :use-module (hnh util table)
  :use-module (hnh util type)
  :use-module (hnh util named-type)
  :use-module (hnh util uuid)
  :use-module (web uri)
  :use-module ((web query) :select (encode-query-parameters))
  :use-module (datetime)
  :use-module (datetime timespec)
  :use-module (sxml namespaced)
  :export (create-instance)
  )

;;; Find "dangling" components. E.g. root components with no href
;; sqlite> select * from component full outer join href on component.id = href.component where component.parent is null and href is null;

(catch 'misc-error
  (lambda ()
    (use-modules (sqlite3))
    (provide 'data-store-sqlite))
  (lambda args 'no-op))



;; Iteration 1 of a macro for binding scheme variables from columns in an sqlite statement.
;; This version returns simply binds each column name to its index in the query, leaving the
;; user to do all the "heavy" lifting. This solves the problem with changing column indices,
;; but becomes overly verbose.
;; @example
;; (define stmt (sqlite-prepare db "SELECT 1 AS a, 2 AS b, 3 AS c"))
;; (define record (sqlite-step stmt))
;; (with-sqlite-columns
;;  stmt (a b c)
;;  (list (vector-ref record a)
;;        (vector-ref record b)
;;        (vector-ref record c)))
;; @end example
;; (define-syntax-rule (with-sqlite-columns stmt (column ...) body ...)
;;   (let ((column-names (sqlite-column-names stmt)))
;;     (let ((column (vector-index
;;                    (lambda (s) (string= s (symbol->string (quote column))))
;;                    column-names)) ...)
;;       body ...)))


;; Second iteration of a macro for binding SQLite procedures into scheme values.
;; This version combines the column name resolution with an anaphoric let statement.
;; This allows for convenient usage.
;; Reason record is taken separately from stmt is for cases where
;; another method of retrieving the records are prefered. Note however
;; that the record MUST have come from the statement provided.
;; @example
;; (with-sqlite-columns
;;  stmt (sqlite-step stmt)
;;  ((a (number->string it))
;;   (b (number->boolean it)))
;;  (list a b))
;; @end example
(define-syntax (with-sqlite-columns stx)
  (syntax-case stx ()
    ((_ stmt record ((column definition) ...) body ...)
     (with-syntax ((it (datum->syntax stx 'it)))
       #`(let ((column-names (sqlite-column-names stmt)))
           (let #,(map (lambda (stx)
                         (syntax-case stx ()
                           ((c d)
                            #`(c ((lambda (it) d)
                                  (vector-ref record
                                              (vector-index
                                               (lambda (s) (string=
                                                       s #,(-> #'c syntax->datum symbol->string)))
                                               column-names)))))))
                       #'((column definition) ...))
             body ...))))))

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




(define* (delimited->table str key: (rs #\rs) (us #\us))
  (typecheck str string?)
  (if (string-null? str)
      (table)
      (fold (lambda (line t)
              (let ((pair (string-split line us)))
                (table-put t (string->symbol (car pair))
                           (string-join (cdr pair) (string us)))))
            (table)
            (string-split str rs))))

(define (unordered-superset-of lst target)
  (lset<= equal? target lst))


(define (vector-car v)
  (vector-ref v 0))




(define-class <sqlite-data-store> (<calendar-data-store>)
  (path getter: path
        init-keyword: path:
        init-value: #f)
  (db accessor: database)
  (href-by-id getter: href-by-id
              init-form: (make-hash-table))
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
                (assoc-ref (get-entries/helper store "c.root = :id" id: id) #f)))
          (else #f)))))

;; (define-method (write-all! (this <sqlite-data-store>) component)
;;   (call-with-sqlite-transaction
;;    (database this)
;;    (lambda (db) (write-component! db component))))

(define-method (put-event! (this <sqlite-data-store>) href component)
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
     (sqlite-finalize stmt)
     (hash-set! (href-by-id this)
                (-> component-id number->string string->symbol)
                href))))

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
  (call-with-sqlite-transaction
   (database store)
   (lambda (db)
     (let ((stmt (sqlite-prepare
                  db "DELETE FROM href WHERE href = :href RETURNING id")))
       ;; TODO clean up dangling components, something like
       ;; SELECT id FROM component_trace WHERE root = :id
       ;; DELETE FROM property WHERE component IN (...)
       ;; DELETE FROM component WHERE id IN (...)
       (sqlite-bind-arguments stmt href: href)
       (sqlite-step stmt)
       (hash-remove! (href-by-id store) (-> (sqlite-step stmt)
                                            vector-car number->string string->symbol))
       (sqlite-finalize stmt)
       ))))

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
  (let ((matches (get-entries/helper this "href = :href" href: href)))
    (if (null? matches)
        #f
        (cdar matches))))

;;; returns (list-of (pair-of href vcalendar))
(define-method (list-entries (this <sqlite-data-store>))
  ;; Only doing a single SQL lookup (instead of running get-entries
  ;; for each href) takes the runtime on a store with ~2000 elements
  ;; from 80s down to 5s, most of which is spent in the garbage collector.
  (get-entries/helper this "true"))





;;; Returns 2 values:
;;; - a table specifying component children, with parents ids as keys, and lists of child ids as values
;;; - a table of components, with component ids as keys, and actuall components as values
(define (retrieve-components-and-ids stmt)
  ;; (typecheck stmt stmt?) ; stmt? isn't exported from (sqlite3)
  (typecheck (vector->list (sqlite-column-names stmt))
             (unordered-superset-of '("component-type" "component-id" "parent-id"
                                      "property-name" "property-type" "property-value"
                                      "parameters*")))

  (define root-symbol (gensym "root"))

  (let ((ids components
             (car+cdr
              (sqlite-fold
               (lambda (record state)
                 (with-sqlite-columns
                  stmt record
                  ((component-type (string->symbol it))
                   (component-id   (-> it number->string string->symbol))
                   (parent-id      (cond ((number? it)
                                          (-> it number->string string->symbol))
                                         (else root-symbol)))
                   (property-name  (string->symbol it))
                   (property-type  (string->symbol it))
                   (property-value it)
                   (parameters* (delimited->table (or it ""))))

                  (define active-parser
                    (or (get-parser property-type)
                        (lambda (_ v)
                          (unknown
                           ;; As long as we never put in anything except
                           ;; strings, we will never get anything other back
                           v
                           (and (not (eq? 'UNKNOWN property-type))
                                (symbol->string property-type))))))

                  (-> state
                      ;; - For component referenced by ID, set property
                      ;;   and parameters by property-name
                      (modify
                       (lens-compose cdr* (table-focus component-id))
                       (lambda (m-component)
                         (just
                          (modify
                           ;; Create component if needed
                           (unjust m-component (vcomponent type: component-type))
                           ;; Attach the current property

                           ;; Note that the value returned from the
                           ;; database might be @code{#f}. This ONLY
                           ;; happens for components with no properties.
                           ;; Actual boolean values are stored as integers.
                           (prop* property-name)
                           (lambda (m-prop)
                             (cond (property-value
                                    (call-with-values
                                        (lambda () (active-parser parameters* property-value))
                                      (lambda* (value optional: (parameters parameters*))
                                        (just (cons (vline params: parameters value: value)
                                                    (unjust m-prop '()))))))
                                   (else m-prop)))))))
                      (modify (lens-compose car* (table-focus parent-id))
                              (lambda (m) (just (lset-adjoin eq? (unjust m '()) component-id)))))))
               (cons (table (named-type (list-of symbol?)))
                     (table (named-type vcomponent?)))
               stmt))))
    (values root-symbol ids components)))

(define (build-component-trees root-symbol id-table component-table)
  (typecheck root-symbol symbol?)
  (typecheck id-table (table-of (list-of symbol?)))           ; (table-of (list-of symbol?))
  (typecheck component-table (table-of vcomponent?))    ; (table-of vcomponent?)

  (cond ((table-get id-table root-symbol)
         => (lambda (ids)
              (map (lambda (root-id)
                     (cons root-id
                           (let recurse ((id root-id))
                             (-> (table-get component-table id)
                                 (vcomponent-children
                                  (map recurse (or (table-get id-table id) '())))))))
                   ids)))
        (else '())))

;;; DEPRECATED
;;; It's query method is overly limited, AND it doesn't even work properly.
;;; Setting filter to "pr.value like '%blot%'" fails, even if properties matching that exists.
;;; Either way, returns a list of matching entries,
;;; meaning an empty list on no match
(define (get-entries/helper store filter . filter-args)
  ;; We group parameters, since propreties may be really large, while
  ;; component data is tiny. As it's currently written, parameters
  ;; can't contain record or unit separator characters anywhere.

  (apply get-entries store (format #f "
SELECT
  c.type      AS [component-type]
, c.parent    AS [parent-id]
, c.id        AS [component-id]
, pr.property AS [property-name]
, pr.type     AS [property-type]
, pr.value    AS [property-value]
, group_concat(pa.parameter || char(0x1F) || pa.value, char(0x1E))
    AS [parameters*]
-- start special
FROM component_trace c
-- end special
FULL OUTER JOIN property  pr ON c.id = pr.component
FULL OUTER JOIN parameter pa ON pr.id = pa.property
-- start special
LEFT JOIN href ON href.component = root
WHERE ~a
-- end special
GROUP BY pr.id" filter)
         filter-args))

;;; returns (list-of (pair-of href vcalendar))
(define-method (get-entries (store <sqlite-data-store>)
                            (query <string>)
                            . args)

  (define stmt (sqlite-prepare (database store) query))
  (apply sqlite-bind-arguments stmt args)
  (let ((root-id ids components (retrieve-components-and-ids stmt)))
    (begin1
     (for (id . component) in (build-component-trees root-id ids components)
          (cons (hash-ref (href-by-id store) id)
                component))
     (sqlite-finalize stmt))))

(define-method (list-entries/shallow (store <sqlite-data-store>))
  (hash-map->list (lambda (_ href) href)
                  (href-by-id store)))

(define-method (entry-count (store <sqlite-data-store>))
  (hash-count (const #t) (href-by-id store)))

(define-method (flush! (this <sqlite-data-store>))
  ;; TODO possible commit any pending transactions here
  ;; This could also help with performance, as long as we don't commit in add-entry.
  ;; That would however require us to have a "master" transaction running at all times
  ;; (since add-entry creates it's own (sub) transaction)
  'noop)

;;; returns (stream-of (pair-of href object))
(define-method (entries-in-interval (store <sqlite-data-store>)
                                    start end)
  (typecheck start datetime?)
  (typecheck end   datetime?)

  ;; TODO log level debug
  (format (current-error-port) "<DEBUG> entries-in-interval ~s, ~s - ~s~%"
          (uri->string (store-uri store)) start end)

;;; TODO duration
;;; TODO entries with only start

   ;; NOTE we sort in Guile instead of SQLite, since all the grouping
   ;; makes it hard to access the start time.
  (define regular-events
    (filter (negate (compose recurring? cdr))
            (get-entries store "
-- start special
WITH base_components (id) AS
    (
    -- Instances where DTSTART is in interval
    SELECT DISTINCT component FROM property
        WHERE property = 'DTSTART'
        AND value BETWEEN :start AND :end
    UNION
    -- Instances where DTEND is in interval
    SELECT DISTINCT component FROM property
        WHERE property = 'DTEND'
        AND value BETWEEN :start AND :end
    -- TODO DURATION
    )
   , root_components (id) AS
     -- TODO does this join actually do anything?
     (SELECT t.root FROM base_components c
        -- LEFT JOIN property p ON p.component = c.id
        INNER JOIN component_trace t ON c.id = t.id
        WHERE t.type = 'VEVENT'
        --   AND p.property = 'DTSTART'
        -- ORDER BY p.value
)
-- end special
SELECT
  c.type      AS [component-type]
, c.parent    AS [parent-id]
, c.id        AS [component-id]
, pr.property AS [property-name]
, pr.type     AS [property-type]
, pr.value    AS [property-value]
, group_concat(pa.parameter || char(0x1F) || pa.value, char(0x1E))
   AS [parameters*]
-- start special
 FROM root_components
INNER JOIN component_trace c ON root_components.id = c.root
-- end special
FULL OUTER JOIN property  pr ON c.id = pr.component
FULL OUTER JOIN parameter pa ON pr.id = pa.property
-- TODO why is this needed? How do we manage to join property lines without component
WHERE [component-id] IS NOT NULL
GROUP BY pr.id
-- ORDER BY pr.value"
                         start: (datetime->string start)
                         end: (datetime->string end))))


  ;; TODO this can match components without href.
  ;; This happens when a href entry is removed, but the underlying
  ;; component is kept.
  (define recurring-events
    (get-entries store "
WITH base_components (id) AS (
    SELECT distinct t.root
    FROM property p
    INNER JOIN component_trace t ON p.component = t.id
    WHERE (p.property = 'RDATE' OR p.property = 'RRULE') AND t.type = 'VEVENT'
)
-- end special
SELECT
  c.type      AS [component-type]
, c.parent    AS [parent-id]
, c.id        AS [component-id]
, pr.property AS [property-name]
, pr.type     AS [property-type]
, pr.value    AS [property-value]
, group_concat(pa.parameter || char(0x1F) || pa.value, char(0x1E))
   AS [parameters*]
 FROM base_components
INNER JOIN component_trace c ON base_components.id = c.root
FULL OUTER JOIN property  pr ON c.id = pr.component
FULL OUTER JOIN parameter pa ON pr.id = pa.property
-- TODO why is this needed? How do we manage to join property lines without component
WHERE [component-id] IS NOT NULL
GROUP BY pr.id"))

  (define result
   ((expand-and-interleave-recurrences start end)
    recurring-events regular-events))

  ;; TODO log level debug
  (format (current-error-port) "<DEBUG> Entries gotten ~s~%"
          (uri->string (store-uri store)))

  result)



