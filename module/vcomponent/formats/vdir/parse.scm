;;; Commentary:
;; Code for parsing vdir's and icalendar files.
;; This module handles the finding of files, while
;; (vcomponent formats parse ical) handles reading data from icalendar files.
;;; Code:

(define-module (vcomponent formats vdir parse)
  :use-module (srfi srfi-1)

  :use-module ((ice-9 hash-table) :select (alist->hash-table))
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 ftw) :select (scandir ftw))

  :use-module (calp util)
  :use-module (calp util exceptions)
  :use-module (vcomponent base)

  :use-module (vcomponent formats ical parse)
  )




;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO properties can be subtd.
(define-public (parse-vdir path)
  ;; TODO empty files here cause "#<eof>" to appear in the output XML, which is *really* bad.
  (let ((color
         (catch 'system-error
           (lambda () (call-with-input-file (path-append path "color") read-line))
           (const "#FFFFFF")))
        (name
         (catch 'system-error
           (lambda () (call-with-input-file (path-append path "displayname") read-line))
           (const #f))))

    (reduce (lambda (item calendar)

              (define-values (events other) (partition (lambda (e) (eq? 'VEVENT (type e)))
                                                       (children item)))


              ;; (assert (eq? 'VCALENDAR (type calendar)))
              (assert (eq? 'VCALENDAR (type item)))

              (for child in (children item)
                   (set! (prop child '-X-HNH-FILENAME)
                     (prop (parent child) '-X-HNH-FILENAME)))

              ;; NOTE The vdir standard says that each file should contain
              ;; EXACTLY one event. It can however contain multiple VEVENT
              ;; components, but they are still the same event.
              ;; In our case this means exceptions to reccurence rules, which
              ;; is set up here, and then later handled in rrule-generate.
              ;; NOTE These events also share UID, but are diferentiated
              ;; by RECURRENCE-ID. As far as I can tell this goes against
              ;; the standard. Section 3.8.4.4.
              (case (length events)
                [(0) (warning "No events in component~%~a"
                              (prop item '-X-HNH-FILENAME))]
                [(1)
                 (let ((child (car events)))
                   (assert (memv (type child) '(VTIMEZONE VEVENT)))
                   (add-child! calendar child))]

                ;; two or more
                [else
                 ;; Sequence numbers on their own specifies revisions of a
                 ;; single compenent, incremented by a central authorative
                 ;; source. In that case simply picking the version with the
                 ;; highest SEQUENCE number would suffice. However, for
                 ;; recurring events where each instance is its own VEVENT
                 ;; they also signify something.
                 ;; TODO Neither version is handled here (or anywhere else).


                 ;; Multiple VEVENT objects can share a UID if they have
                 ;; different RECURRENCE-ID fields. This signifies that they
                 ;; are instances of the same event, similar to RDATE.
                 ;; Here we first check if we have a component which contains
                 ;; an RRULE or lacks a RECURRENCE-ID, and uses that as base.
                 ;; Otherwise we just take the first component as base.
                 ;; 
                 ;; All alternatives (and the base) is added the the -X-HNH-ALTERNATIVES
                 ;; property of the base object, to be extracted where needed.
                 (let* ((head (or (find (extract 'RRULE) events)
                                  (find (negate (extract 'RECURRENCE-ID)) events)
                                  (car events)))
                        (rest (delete head events eq?)))

                   (set! (prop head '-X-HNH-ALTERNATIVES)
                     (alist->hash-table
                      (map cons
                           ;; head is added back to the collection to simplify
                           ;; generation of recurrences. The recurrence
                           ;; generation assumes that the base event either
                           ;; contains an RRULE property, OR is in the
                           ;; -X-HNH-ALTERNATIVES set. This might produce
                           ;; duplicates, since the base event might also
                           ;; get included through an RRULE. This however
                           ;; is almost a non-problem, since RDATES and RRULES
                           ;; can already produce duplicates, meaning that
                           ;; we need to filter duplicates either way.
                           (map (extract 'RECURRENCE-ID) (cons head rest))
                           (cons head rest))))
                   (add-child! calendar head))])

              ;; return
              calendar)
            (make-vcomponent)
            (map #; (@ (ice-9 threads) par-map)
             (lambda (fname)
               (let ((fullname (path-append path fname)))
                 (let ((cal (call-with-input-file fullname
                              parse-calendar)))
                   (set! (prop cal 'COLOR) color
                         (prop cal 'NAME) name
                         (prop cal '-X-HNH-FILENAME) fullname)
                   cal)))
             (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                       (string= "ics" (string-take-right s 3)))))))))

