#!/usr/bin/env bash
# -*- mode: scheme; geiser-scheme-implementation: guile -*-

here=$(dirname $(realpath $0))
export here
. "$(dirname $(dirname "$here"))/env"

exec $GUILE -s "$0" "$@"
!#

(use-modules (srfi srfi-64)
             (srfi srfi-88)
             (vcomponent)
             (vcomponent create)
             (datetime)
             (datetime timespec)
             ((hnh util) :select (for print-and-return))
             (hnh test testrunner))


(verbose? #t)
(test-runner-factory construct-test-runner)

(define component
  (vcomponent
   'VCALENDAR
   version: "2.0"
   prodid: "-//PIMUTILS.ORG//NONSGML khal / icalendar //EN"
   (list
    (vcomponent
     'VEVENT
     summary: "Backhäfv"
     dtstart: (with-parameters tzid: "Europe/Stockholm"
                               value: "DATE-TIME"
                               #2018-09-07T17:00:00)
     dtend: (with-parameters tzid: "Europe/Stockholm"
                             value: "DATE-TIME"
                             #2018-09-07T18:00:00)
     dtstamp: (with-parameters value: "DATE-TIME"
                               #2018-09-07T15:42:23Z)
     uid: "ZSGUP6BTM52UV42SEWBICHSS63V8DYQX5TSZ"
     sequence: 0)
    (vcomponent
     'VTIMEZONE
     tzid: "Europe/Stockholm"
     (list (vcomponent
            'STANDARD
            dtstart: (with-parameters value: "DATE-TIME"
                                      #2018-10-28T02:00:00)
            tzname: "CET"
            tzoffsetfrom: (make-timespec #02:00 '+ #\z)
            tzoffsetto: (make-timespec #01:00 '+ #\z))
           (vcomponent
            'DAYLIGHT
            dtstart: (with-parameters value: "DATE-TIME"
                                      #2018-03-25T03:00:00)
            tzname: "CEST"
            tzoffsetfrom: (make-timespec #01:00 '+ #\z)
            tzoffsetto: (make-timespec #02:00 '+ #\z))))
    )))

(add-to-load-path (getenv "here"))

(test-begin "Serialization Formats")



(for test in '(ical xcal)
     (test-group (format #f "Format: ~a" test)
       (let ((interface (resolve-interface (list test))))
         (let ((component-str   (module-ref interface 'component-str))
               (serialize       (module-ref interface 'serialize))
               (deserialize     (module-ref interface 'deserialize))
               (sanitize-string (module-ref interface 'sanitize-string)))

           (test-equal "Serialize"
             (sanitize-string component-str)
             (sanitize-string
              (call-with-output-string
                (lambda (p) (serialize component p)))))

           (test-equal "Deserialized object serializes back into source"
             (sanitize-string component-str)
             (sanitize-string
              (call-with-output-string
                (lambda (p)
                  (serialize
                   (call-with-input-string
                       component-str deserialize)
                   p)))))


           (test-assert "Serialized string can still be read back in"
             (vcomponent?
              (let* ((obj1 (call-with-input-string component-str deserialize))
                     (str2 (call-with-output-string (lambda (p) (serialize obj1 p))))
                     (obj2 (call-with-input-string str2 deserialize)))
                obj2)))))))


(test-end)
