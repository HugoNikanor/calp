#!/usr/bin/guile \
-e main -s
!#
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/module"))

(use-modules (vcomponent)
             ((vcomponent recurrence parse) :select (parse-recurrence-rule))
             ((vcomponent formats xcal output) :select (vcomponent->sxcal ns-wrap))
             ((vcomponent formats ical output) :select (component->ical-string))
             (vcomponent datetime)
             (datetime)
             ((datetime instance) :select (zoneinfo))
             (hnh util)
             (hnh util uuid)
             (ice-9 format)
             (ice-9 popen)
             (ice-9 threads)
             ((srfi srfi-88) :select (keyword->string))
             (sxml simple)
             )

(define (vevent . rest)
  (define v (make-vcomponent 'VEVENT))

  (let loop ((rem rest))
    (unless (null? rem)
      (let ((symb (-> (car rem)
                      keyword->string
                      string-upcase
                      string->symbol)))
        (set! (prop v symb)
          (case symb
            ;; [(DTSTART EXDATE) (parse-ics-datetime (cadr rem))]
            [(RRULE) (parse-recurrence-rule (cadr rem))]
            [else (cadr rem)]))
        ;; hack for multi valued fields
        (when (eq? symb 'EXDATE)
          (set! (prop* v symb) = list)))
      (loop (cddr rem))))

  v)

(define ev
  (vevent
    summary: "Test Event #1"
    uid: (uuid)
    dtstart: #2021-12-21T10:30:00
    dtend: #2021-12-21T11:45:00
    dtstamp: (current-datetime)
    ))

(set!
  (param (prop* ev 'DTSTART) 'TZID) "Europe/Stockholm"
  (param (prop* ev 'DTEND)   'TZID) "Europe/Stockholm")

(define zoneinfo
  (zoneinfo->vtimezone (zoneinfo '("tzdata/europe"))
                       "Europe/Stockholm" ev))

(define cal (make-vcomponent 'VCALENDAR))

(set!
  (prop cal 'PRODID) "-//hugo//calp TEST//EN"
  (prop cal 'VERSION) "2.0")

(add-child! cal zoneinfo)
(add-child! cal ev)

(define sxcal
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
          ,(ns-wrap (vcomponent->sxcal cal))))

(define (main args)
  (for-each (lambda (fmt)
              (define parts (map string->symbol (string-split fmt #\:)))
              (case (car parts)
                ((sxcal)
                 (if (memv 'pretty (cdr parts))
                   (format #t "~y" sxcal)
                   (begin (write sxcal) (newline))))
                ((ical) (component->ical-string cal))
                ((xml)
                 (let ((pipe (open-output-pipe
                               (string-join
                                 (append '("cat")
                                         (if (memv 'pretty (cdr parts)) '("xmllint --format -") '())
                                         (if (memv 'color (cdr parts))  '("highlight -Oansi --syntax=xml") '()))
                                 "|"))))
                 (sxml->xml sxcal pipe)
                 (close-pipe pipe)
                 (newline)))
                ((newline) (newline))
                (else (format #t "Unknown mode [~a]~%" (car parts)))))
              (cdr args)))

;; (write sxcal)
;;
;; (newline)
;; (newline)
;;
;; (format #t "~y" sxcal)
;;
;; (newline)
;;
;; (let ((pipe (open-pipe* OPEN_WRITE "highlight" "-Oansi" "--syntax=xml")))
;;   ((@ (sxml simple) sxml->xml) sxcal pipe)
;;   (close-pipe pipe))
;; (newline)
;;
;; (let ((pipe (open-pipe "xmllint --format - | highlight -Oansi --syntax=xml"
;;                        OPEN_WRITE
;;                        )))
;;   (sxml->xml sxcal pipe)
;;   (close-pipe pipe))
;; (newline)
;;
;;
;; (newline)
;; (component->ical-string cal)
