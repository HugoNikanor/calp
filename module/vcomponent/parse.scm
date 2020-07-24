;;; Commentary:
;; Code for parsing vdir's and icalendar files.
;; This module handles the finding of files, while
;; (vcomponent parse ical) handles reading data from icalendar files.
;;; Code:

(define-module (vcomponent parse)
  :use-module (srfi srfi-1)

  :use-module ((ice-9 hash-table) :select (alist->hash-table))
  :use-module ((ice-9 rdelim) :select (read-line))
  :use-module ((ice-9 ftw) :select (scandir ftw))

  :use-module (util)
  :use-module (util time)
  :use-module (util exceptions)
  :use-module (vcomponent base)

  :use-module (vcomponent parse ical)
  :re-export (parse-calendar)
  )




;; All VTIMEZONE's seem to be in "local" time in relation to
;; themselves. Therefore, a simple comparison should work,
;; and then the TZOFFSETTO properties can be subtd.
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

                   ;; Sorting on SEQUENCE here would have been nice.
                   ;; But the patches can apparently share a sequence number
                   ;; of 0 with the original event!
                   ;; (╯°□°）╯ ┻━┻
                   (let* ((head (find (negate (extract 'RECURRENCE-ID))
                                      events))
                          (rest (delete head events eq?)))

                     (set! (prop head '-X-HNH-ALTERNATIVES)
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
                     (set! (prop cal 'COLOR) color
                           (prop cal 'NAME) name
                           (prop cal '-X-HNH-FILENAME) fullname)
                     cal)))
               (scandir path (lambda (s) (and (not (string= "." (string-take s 1)))
                                         (string= "ics" (string-take-right s 3))))))))))

;; Parse a vdir or ics file at the given path.
(define-public (parse-cal-path path)
  ;; TODO check (access? path R_OK) ?
  (define st (stat path))
  (define cal
    (case (stat:type st)
      [(regular)
       (let ((comp (call-with-input-file path parse-calendar)))
         (set! (prop comp '-X-HNH-SOURCETYPE) 'file)
         comp) ]
      [(directory)
       (report-time! "Parsing ~a" path)
       (let ((comp (parse-vdir path)))
         (set! (prop comp '-X-HNH-SOURCETYPE) 'vdir
               (prop comp '-X-HNH-DIRECTORY) path)
         comp)]
      [(block-special char-special fifo socket unknown symlink)
       => (lambda (t) (error "Can't parse file of type " t))]))

  (unless (prop cal "NAME")
    (set! (prop cal "NAME")
      (or (prop cal "X-WR-CALNAME")
          (string-append "[" (basename path) "]"))))

  cal)

