(define-module (vcomponent data-stores sqlite)
  :use-module (oop goops)
  :use-module (vcomponent data-stores common)
  :use-module (srfi srfi-71)
  :use-module ((srfi srfi-88) :select ())
  :use-module (vcomponent)
  :use-module ((vcomponent formats ical) :prefix #{ical:}#)
  :use-module ((hnh util) :select (aif))
  )


(catch 'misc-error
  (lambda ()
    (use-modules (sqlite3))
    (provide 'data-store-sqlite))
  (lambda args 'no-op))

;; (define (sqlite-exec db str)
;;   (display str)
;;   ((@ (sqlite3) sqlite-exec) db str))

(define-class <sqlite-data-store> (<calendar-data-store>)
  (database accessor: database)
  (name init-keyword: name: getter: calendar-name)
  )

(define (initialize-database db)
    ;;; Setup Content type

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS content_type
( id INTEGER PRIMARY KEY AUTOINCREMENT
, name TEXT NOT NULL
)")

  (let ((stmt (sqlite-prepare db "
INSERT OR IGNORE INTO content_type
( name ) VALUES ( ? )")))
    (for-each (lambda (content-type)
                (sqlite-reset stmt)
                (sqlite-bind-arguments stmt )
                (sqlite-step stmt))
              '("ical"
                "xcal"
                "jcal")))

    ;;; Setup calendar

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS calendar
( id INTEGER PRIMARY KEY AUTOINCREMENT
, name TEXT NOT NULL
)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS calendar_properties
( id INTEGER PRIMARY KEY AUTOINCREMENT
, calendar INTEGER NOT NULL
, key TEXT NOT NULL
, value TEXT NOT NULL
, FOREIGN KEY (calendar) REFERENCES calendar(id)
)")

  ;; INSERT INTO calendar_properties (id, key, value)
  ;; VALUES ( (SELECT id FROM calendar WHERE name = 'Calendar')
  ;;        , 'color'
  ;;        , '#1E90FF')

    ;;; Setup event

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS event
( uid TEXT PRIMARY KEY
, content_type INTEGER NOT NULL
, content TEXT NOT NULL
, calendar INTEGER NOT NULL
, FOREIGN KEY (content_type) REFERENCES content_type(id)
, FOREIGN KEY (calendar) REFERENCES calendar(id)
)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS event_instances
( id INTEGER PRIMARY KEY AUTOINCREMENT
, event TEXT NOT NULL
, start DATETIME NOT NULL
, end DATETIME
, FOREIGN KEY (event) REFERENCES event(uid)
)")

  (sqlite-exec db "
CREATE TABLE IF NOT EXISTS event_instances_valid_range
( start DATETIME NOT NULL
, end DATETIME NOT NULL
)")
  )

(define-method (initialize (this <sqlite-data-store>) args)
  (next-method)
  (if (calendar-name this)
      (set! (database this) (sqlite-open (path this)))
      (let ((path db-name
             (aif (string-rindex (path this) #\#)
                  (values (substring (path this) 0 it)
                          (substring (path this) (1+ it)))
                  (scm-error 'misc-error "(initialize <sqlite-data-store>)"
                             "Target calendar name not specified"
                             '() #f))))
        (set! (database this) (sqlite-open path))
        (slot-set! this 'name db-name)))

  (initialize-database (database this)))


(define-method (get-calendar (this <sqlite-data-store>))
  (let ((db (database this))
        (calendar (vcomponent type: 'VCALENDAR)))
    (let ((stmt (sqlite-prepare db "
SELECT key, value FROM calendar_properties cp
LEFT JOIN calendar c ON cp.calendar = c.id
WHERE c.name = ?
")))
      (sqlite-bind-arguments stmt (calendar-name this))
      (sqlite-fold (lambda (row calendar)
                     (let ((key (vector-ref row 0))
                           (value (vector-ref row 1)))
                       (set-property! calendar
                                      (string->symbol key)
                                      value))
                     calendar)
                   calendar
                   stmt))

    (let ((stmt (sqlite-prepare db "
SELECT content_type.name, content
FROM event
LEFT JOIN calendar ON event.calendar = calendar.id
LEFT JOIN content_type ON event.content_type = content_type.id
WHERE calendar.name = ?
")))
      (sqlite-bind-arguments stmt (calendar-name this))
      (sqlite-fold (lambda (row calendar)
                     (case (string->symbol (vector-ref row 0))
                       ((ical)
                        (add-child! calendar
                                    (call-with-input-string (vector-ref row 1)
                                      ics:deserialize))
                        calendar)
                       (else
                        (scm-error 'misc-error "(get-calendar <sqlite-data-store>)"
                                   "Only iCal data supported, got ~a"
                                   (list (vector-ref row 0)) #f)
                        ))
                     )
                   calendar
                   stmt))

    calendar))


#;
(define-method (get-by-uid (this <sqlite-data-store>) (uid <string>))
  (let ((stmt (sqlite-prepare db "
SELECT name, content
FROM event
LEFT JOIN content_type ON event.content_type = content_type.id
WHERE event.uid = ?")))
    (sqlite-bind-arguments stmt uid)
    (cond ((sqlite-step stmt)
           => (lambda (record)
                (case (string->symbol (vector-ref content 0))
                  ((ics)
                   ;; TODO dispatch to higher instance
                   )
                  (else
                   (scm-error 'value-error "get-by-uid"
                              "Can only deserialize ics (uid=~s)"
                              (list uid) #f)))

                ))
          (else
           ;; TODO possibly throw no-such-value
           #f
           ))

    )
  )
