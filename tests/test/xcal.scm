;;; Commentary:
;; Basic tests of xcal convertion.
;; Currently only checks that events survive a round trip.
;;; Code:

(define-module (test xcal)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  :use-module ((vcomponent formats ical parse)
               :select (parse-calendar))
  :use-module ((hnh util) :select (->))
  :use-module ((vcomponent base)
               :select (parameters prop* children)))

;;; Some different types, same parameters

(define ev
  (call-with-input-string
    "BEGIN:VCALENDAR
VERSION:2.0
PRODID:-//calparse-test
BEGIN:VEVENT
SUMMARY:Test event
DTSTART;TZID=Europe/Stockholm:20200625T133000
DTEND:20200625T143000Z
DTSTAMP:20200609T131418Z
UID:1
SEQUENCE:0
CREATED:20200609T081725Z
DESCRIPTION:Short description
LAST-MODIFIED:20200609T081725Z
STATUS;X-TEST-PARAM=10:CONFIRMED
TRANSP:OPAQUE
END:VEVENT
END:VCALENDAR"
    parse-calendar))

(define twice-converted
  (-> ev vcomponent->sxcal sxcal->vcomponent))

;;; NOTE both these tests may fail since neither properties nor parameters are ordered sorted.

(test-equal
  "c->x & c->x->c->x"
  (vcomponent->sxcal ev)
  (vcomponent->sxcal twice-converted))

(test-equal
  "xcal parameters"
  '((X-TEST-PARAM "10"))
  (parameters
    (prop* (car (children twice-converted)) 'STATUS)))


