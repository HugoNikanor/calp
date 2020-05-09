;;; Commentary:
;; Code for parsing vdir's and icalendar files.
;; This module handles the finding of files, while
;; (vcomponent parse component) handles reading data from icalendar files.
;;; Code:

(define-module (vcomponent parse)
  :use-module (rnrs bytevectors)
  :use-module (srfi srfi-1)

  :use-module ((ice-9 hash-table) :select (alist->hash-table))
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 ftw) :select (scandir ftw))

  :use-module (util)
  :use-module (util time)
  :use-module (util exceptions)
  :use-module (vcomponent base)

  :use-module (vcomponent parse component)
  :re-export (parse-calendar)
  )




;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO attribute can be subtd.
(define (parse-vdir path)
  (let ((/ (lambda args (string-join args file-name-separator-string 'infix))))
    (let ((color
           (catch 'system-error
             (lambda () (call-with-input-file (/ path "color") read-line))
             (const "#FFFFFF")))
          (name
           (catch 'system-error
             (lambda () (call-with-input-file (/ path "displayname") read-line))
             (const #f))))

      (reduce (lambda (item calendar)

                (define-values (events other) (partition (lambda (e) (eq? 'VEVENT (type e)))
                                                         (children item)))


                ;; (assert (eq? 'VCALENDAR (type calendar)))
                (assert (eq? 'VCALENDAR (type item)))

                (for child in (children item)
                     (set! (attr child 'X-HNH-FILENAME)
                       (attr (parent child) 'X-HNH-FILENAME)))

                ;; NOTE The vdir standard says that each file should contain
                ;; EXACTLY one event. It can however contain multiple VEVENT
                ;; components, but they are still the same event.
                ;; In our case this means exceptions to reccurence rules, which
                ;; is set up here, and then later handled in rrule-generate.
                ;; NOTE These events also share UID, but are diferentiated
                ;; by RECURRENCE-ID. As far as I can tell this goes against
                ;; the standard. Section 3.8.4.4.
                ;; TODO Also make this grouping when reading in a whole
                ;; icalendar-file (one with multiple events)
                (case (length events)
                  [(0) (warning "No events in component~%~a"
                           (attr item 'X-HNH-FILENAME))]
                  [(1)
                   (let ((child (car events)))
                    (assert (memv (type child) '(VTIMEZONE VEVENT)))
                    (add-child! calendar child))]

                  ;; two or more
                  [else

                   ;; Sorting on SEQUENCE here would have been nice.
                   ;; But the patches can apparently share a sequence number
                   ;; of 0 with the original event!
                   ;; (╯°□°）╯ ┻━┻
                   (let* ((head (find (negate (extract 'RECURRENCE-ID))
                                      events))
                          (rest (delete head events eq?)))

                     (set! (attr head 'X-HNH-ALTERNATIVES)
                       (alist->hash-table
                        (map cons
                             (map (extract 'RECURRENCE-ID) rest)
                             rest))
                       #;
                       (sort*! rest ;; HERE
                               date/-time< (extract 'RECURRENCE-ID)))
                     (add-child! calendar head))])

                ;; return
                calendar)
              (make-vcomponent)
              (map #; (@ (ice-9 threads) par-map)
               (lambda (fname)
                 (let ((fullname (/ path fname)))
                   (let ((cal (call-with-input-file fullname
                                parse-calendar)))
                     (set! (attr cal 'COLOR) color
                           (attr cal 'NAME) name
                           (attr cal 'X-HNH-FILENAME) fullname)
                     cal)))
               (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                         (string= "ics" (string-take-right s 3))))))))))

;; Parse a vdir or ics file at the given path.
(define-public (parse-cal-path path)
  (define st (stat path))
  (define cal
    (case (stat:type st)
      [(regular)
       (let ((comp (call-with-input-file path parse-calendar)))
         (set! (attr comp 'X-HNH-SOURCETYPE) "file")
         comp) ]
      [(directory)
       (report-time! "Parsing ~a" path)
       (let ((comp (parse-vdir path)))
         (set! (attr comp 'X-HNH-SOURCETYPE) "vdir")
         comp)]
      [(block-special char-special fifo socket unknown symlink)
       => (lambda (t) (error "Can't parse file of type " t))]))

  (unless (attr cal "NAME")
    (set! (attr cal "NAME")
      (or (attr cal "X-WR-CALNAME")
          (string-append "[" (basename path) "]"))))

  cal

  )



;; DEPRECATED
;; find all ics files in a tree, and does something with them
(define-public (read-tree path)
  (define list '())
  (ftw path
       (lambda (filename statinfo flag)
         (case flag
           [(regular)
            (case (stat:type statinfo)
              [(regular)
               (when (and (not (string= "." (string-take filename 1)))
                          (string= "ics" (string-take-right filename 3)))
                 (set! list (cons filename list)))
               #t]
              [else #t])]
           [(directory) #t]
           [else #f])))
  ((@ (ice-9 threads) n-par-map) 12
   (lambda (fname) (call-with-input-file fname parse-calendar))
   list))
