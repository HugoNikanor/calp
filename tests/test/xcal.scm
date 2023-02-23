;;; Commentary:
;; Basic tests of xcal convertion.
;; Currently only checks that events survive a round trip.
;;; Code:

(define-module (test xcal)
  :use-module (srfi srfi-64)
  :use-module (srfi srfi-88)
  :use-module (datetime)
  :use-module ((vcomponent create)
               :select (vcalendar vevent with-parameters))
  :use-module ((vcomponent formats xcal parse)
               :select (sxcal->vcomponent))
  :use-module ((vcomponent formats xcal output)
               :select (vcomponent->sxcal))
  :use-module ((hnh util) :select (->))
  :use-module ((vcomponent base)
               :select (parameters prop* children)))

;;; Some different types, same parameters

(define ev
  (vcalendar
   version: "2.0"
   prodid: "-//calparse-test"
   (list (vevent
          summary: "Test event"
          dtstart: (with-parameters
                    tzid: "Europe/Stockholm"
                    #2020-06-25T13:30:00)
          dtend: #2020-06-25T14:30:00Z
          dtstamp: #2020-06-09T13:14:18Z
          uid: "1"
          sequence: 0
          created: #2020-06-09T08:17:25Z
          description: "Short description"
          last-modified: #2020-06-09T08:17:25Z
          status: (with-parameters
                   x-test-param: 10
                   'CONFIRMED)
          transp: 'OPAQUE))))

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


