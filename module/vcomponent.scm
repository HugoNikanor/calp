(define-module (vcomponent)
  :use-module (util)
  :use-module (util app)
  :use-module (util config)
  :use-module (srfi srfi-1)
  :use-module (srfi srfi-41)
  :use-module (srfi srfi-41 util)
  :use-module (datetime)
  :use-module (datetime util)
  :use-module (vcomponent base)
  :use-module (vcomponent parse)
  :use-module ((vcomponent recurrence) :select (generate-recurrence-set repeating?))
  :use-module ((vcomponent datetime) :select (ev-time<?))
  :re-export (make-vcomponent
              parse-cal-path parse-calendar))

(re-export-modules (vcomponent base))

(define-config calendar-files '()
  "Which files to parse. Takes a list of paths or a single string which will be globbed."
  pre: (lambda (v)
         (cond [(list? v) v]
               [(string? v) ((@ (glob) glob) v)]
               [else #f])))

(define-public (load-calendars calendar-files)
  (map parse-cal-path calendar-files))


(define-method (init-app calendar-files)
  (setf 'calendars (load-calendars calendar-files))

  (setf 'events
        (concatenate
         ;; TODO does this drop events?
         (map (lambda (cal) (filter (lambda (o) (eq? 'VEVENT (type o)))
                               (children cal)))
              (getf 'calendars))))

  (setf 'fixed-and-repeating-events
        (let* ((repeating regular (partition repeating? (getf 'events))))

          ;; (report-time! "Sorting")
          ;; NOTE There might be instances where we don't care if the
          ;; collection if sorted, but for the time beieng it's much
          ;; easier to always sort it.
          (list
           (sort*! regular   date/-time<? (extract 'DTSTART))
           (sort*! repeating date/-time<? (extract 'DTSTART)))))

  (setf 'fixed-events (car (getf 'fixed-and-repeating-events)))
  (setf 'repeating-events (cadr (getf 'fixed-and-repeating-events)))

  (setf 'event-set
        (interleave-streams
         ev-time<?
         (cons (list->stream (getf 'fixed-events))
               (map generate-recurrence-set (getf 'repeating-events)))))

  (setf 'uid-map
        (let ((ht (make-hash-table)))
          (for-each (lambda (event) (hash-set! ht (attr event 'UID) event)) (getf 'events))
          ht)))

(define-method (fixed-events-in-range start end)
  (filter-sorted (lambda (ev) ((in-date-range? start end)
                          (as-date (attr ev 'DTSTART))))
                 (getf 'fixed-events)))

(define-method (get-event-by-uid uid)
  (hash-ref (getf 'uid-map) uid))




(use-modules (output ical)
             (ice-9 popen)
             ((ice-9 rdelim) :select (read-line))
             ((rnrs io ports) :select (call-with-port))
             )


(define (uuidgen)
  (call-with-port (open-input-pipe "uuidgen")
                  read-line))


(define / file-name-separator-string)

(define-public (calendar-import calendar event)
  (case (attr calendar 'X-HNH-SOURCETYPE)
    [(file)
     (error "Importing into direct calendar files not supported")]

    [(vdir)
     (let* ((uid (or (attr event 'UID) (uuidgen)))
            ;; copy to enusre writable string
            (tmpfile (string-copy (string-append (attr calendar 'X-HNH-DIRECTORY)
                                                 / ".calp-" uid "XXXXXX")))
            (port (mkstemp! tmpfile)))
       (set! (attr event 'UID) uid)
       (with-output-to-port port
         (lambda () (print-components-with-fake-parent (list event))))
       ;; does close flush?
       (force-output port)
       (close-port port)
       (rename-file tmpfile (string-append (attr calendar 'X-HNH-DIRECTORY)
                                           / uid ".ics"))
       uid)]

    [else
     (error "Source of calendar unknown, aborting.")
     ]))
