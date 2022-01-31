;;; Commentary:
;; Basic tests of xcal convertion.
;; Currently only checks that events survive a round trip.
;;; Code:

(((vcomponent formats xcal parse) sxcal->vcomponent)
 ((vcomponent formats xcal output) vcomponent->sxcal)
 ((vcomponent formats ical parse) parse-calendar)
 ((hnh util) ->)
 ((vcomponent base)
  parameters prop* children)
 )

;;; Some different types, same parameters

(define ev
  (call-with-input-string "BEGIN:VCALENDAR
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
  (-> ev
      vcomponent->sxcal
      sxcal->vcomponent))

;;; NOTE both these tests may fail since neither properties nor parameters are ordered sorted.

(test-equal "c->x & c->x->c->x"
  (vcomponent->sxcal ev)
  (vcomponent->sxcal twice-converted))

(test-equal "xcal parameters"
  '((X-TEST-PARAM "10"))
  (parameters (prop* (car (children twice-converted))
                     'STATUS)))
